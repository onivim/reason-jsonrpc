type t = {
  method: string,
  params: Yojson.Safe.t,
};

let is = (msg: Yojson.Safe.t) =>
  Utility.hasMethod(msg) && !Utility.hasId(msg);

let parse = (msg: Yojson.Safe.t) => {
  let method =
    msg |> Yojson.Safe.Util.member("method") |> Yojson.Safe.Util.to_string;

  let params = msg |> Yojson.Safe.Util.member("params");

  {method, params};
};

let show = (v: t) => {
  "[Notification - " ++ v.method ++ "]: " ++ Yojson.Safe.to_string(v.params);
};
