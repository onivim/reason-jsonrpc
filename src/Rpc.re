module IntMap =
  Map.Make({
    type t = int;
    let compare = compare;
  });

type message =
  | Request(int, Request.t)
  | Notification(Notification.t)
  | Response(int, Response.t)
  | Unknown(string);

type t = {
  input: in_channel,
  output: out_channel,
  mutable nextRequestId: int,
  mutable pendingRequests: IntMap.t(responseHandler),
  messageMutex: Mutex.t,
  writeMutex: Mutex.t,
  messageHandler: (message, t) => unit,
  mutable shouldClose: bool,
  mutable pendingMessages: list(message),
}
and responseHandler = (Response.t, t) => unit;

type closeHandler = unit => unit;
type notificationHandler = (Notification.t, t) => unit;
type requestHandler = (Request.t, t) => result(Yojson.Safe.t, string);
type errorMessageHandler = string => unit;

let _send = (rpc, json: Yojson.Safe.t) => {
  let str = Yojson.Safe.to_string(json);

  let length = String.length(str);
  let contentLengthString =
    "Content-Length: " ++ string_of_int(length) ++ "\r\n";

  Mutex.lock(rpc.writeMutex);
  output_string(rpc.output, contentLengthString);
  output_string(rpc.output, "\r\n");
  output_string(rpc.output, str);
  flush(rpc.output);
  Mutex.unlock(rpc.writeMutex);
};

let _sendResponse = (rpc, msg, id) => {
  let response = `Assoc([("id", `Int(id)), ("result", msg)]);
  _send(rpc, response);
};

let sendNotification = (rpc, method, msg) => {
  let response = `Assoc([("method", `String(method)), ("params", msg)]);
  _send(rpc, response);
};

let sendRequest = (rpc, method, msg, cb) => {
  let id = rpc.nextRequestId;
  rpc.nextRequestId = rpc.nextRequestId + 1;
  rpc.pendingRequests = IntMap.add(id, cb, rpc.pendingRequests);
  let request =
    `Assoc([
      ("id", `Int(id)),
      ("method", `String(method)),
      ("params", msg),
    ]);
  _send(rpc, request);
};

let _parse: string => message =
  msg => {
    let p = Yojson.Safe.from_string(msg);

    switch (Notification.is(p), Request.is(p), Response.is(p)) {
    | (true, _, _) =>
      let result = Notification.parse(p);
      Notification(result);
    | (_, true, _) =>
      let id = p |> Yojson.Safe.Util.member("id") |> Yojson.Safe.Util.to_int;
      Request(id, Request.parse(p));
    | (_, _, true) =>
      let id = p |> Yojson.Safe.Util.member("id") |> Yojson.Safe.Util.to_int;
      Response(id, Response.parse(p));
    | _ => Unknown(msg)
    };
  };

let pump = rpc =>
  if (!rpc.shouldClose) {
    Mutex.lock(rpc.messageMutex);
    List.iter(v => rpc.messageHandler(v, rpc), rpc.pendingMessages);
    rpc.pendingMessages = [];
    Mutex.unlock(rpc.messageMutex);
  };

let noop = (_) => ();

let start =
    (
      ~onNotification: notificationHandler,
      ~onRequest: requestHandler,
      ~onClose: closeHandler,
      ~onError=noop,
      input: in_channel,
      output: out_channel,
    ) => {
  set_binary_mode_in(input, true);
  set_binary_mode_out(output, true);

  let messageHandler = (msg, rpc) => {
    switch (msg) {
    | Notification(v) => onNotification(v, rpc)
    | Request(id, v) =>
      switch (onRequest(v, rpc)) {
      | Ok(result) => _sendResponse(rpc, result, id)
      | Error(msg) => onError(msg)
      | exception (Yojson.Json_error(msg)) => onError(msg);
      }
    | Response(id, r) =>
      let cb = IntMap.find_opt(id, rpc.pendingRequests);
      switch (cb) {
      | Some(c) => c(r, rpc)
      | None => ()
      };

      rpc.pendingRequests = IntMap.remove(id, rpc.pendingRequests);
    | Unknown(payload) => onError("Unknown message: " ++ payload);
    };
  };

  let messageMutex = Mutex.create();
  let writeMutex = Mutex.create();

  let rpc: t = {
    input,
    output,
    messageMutex,
    writeMutex,
    messageHandler,
    nextRequestId: 0,
    shouldClose: false,
    pendingMessages: [],
    pendingRequests: IntMap.empty,
  };

  let queueMessage = (m: message) => {
    Mutex.lock(messageMutex);
    rpc.pendingMessages = [m, ...rpc.pendingMessages];
    Mutex.unlock(messageMutex);
  };

  let _ =
    Thread.create(
      () => {
        let id = Unix.descr_of_in_channel(stdin);
        while (!rpc.shouldClose) {
          Thread.wait_read(id);

          switch (Preamble.read(input)) {
          | exception End_of_file => rpc.shouldClose = true
          | preamble =>
            let len = preamble.contentLength;

            /* Read message */
            let buffer = Bytes.create(len);
            let read = ref(0);
            while (read^ < len) {
              let n = Stdlib.input(input, buffer, read^, len - read^);
              read := read^ + n;
            };

            let str = Bytes.to_string(buffer);
            let result = _parse(str);

            queueMessage(result);
            ();
          };
        };
        onClose();
      },
      (),
    );

  rpc;
};

let stop = (rpc: t) => rpc.shouldClose = true;
