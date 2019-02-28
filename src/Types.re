module Request = {
  type t = (string, Yojson.Safe.json);

  let is = (msg: Yojson.Safe.json) =>
    Utility.hasMethod(msg) && Utility.hasId(msg);

  let parse = (msg: Yojson.Safe.json) => {
    let method =
      msg |> Yojson.Safe.Util.member("method") |> Yojson.Safe.Util.to_string;

    let params = msg |> Yojson.Safe.Util.member("params");

    (method, params);
  };
};

module Notification = {
  type t = {
    method: string,
    params: Yojson.Safe.json,
  };

  let is = (msg: Yojson.Safe.json) =>
    Utility.hasMethod(msg) && !Utility.hasId(msg);

  let parse = (msg: Yojson.Safe.json) => {
    let method =
      msg |> Yojson.Safe.Util.member("method") |> Yojson.Safe.Util.to_string;

    let params = msg |> Yojson.Safe.Util.member("params");

    {method, params};
  };

  let show = (v: t) => {
    "[Notification - " ++ v.method ++ "]: " ++ Yojson.Safe.to_string(v.params);
  };
};
