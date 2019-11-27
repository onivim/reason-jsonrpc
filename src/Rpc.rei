type t;

type notificationHandler = (Notification.t, t) => unit;
type requestHandler = (Request.t, t) => result(Yojson.Safe.t, string);
type closeHandler = unit => unit;
type errorMessageHandler = string => unit;
type scheduler = (unit => unit) => unit;

let start:
  (
    ~onNotification: notificationHandler,
    ~onRequest: requestHandler,
    ~onClose: closeHandler,
    ~onError: errorMessageHandler=?,
    ~scheduler: scheduler=?,
    in_channel,
    out_channel
  ) =>
  t;

let sendNotification: (t, string, Yojson.Safe.t) => unit;

type responseHandler = (Response.t, t) => unit;

let sendRequest: (t, string, Yojson.Safe.t, responseHandler) => unit;

let stop: t => unit;
