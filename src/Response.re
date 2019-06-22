type t = result(Yojson.Safe.json, string);

let is = (msg: Yojson.Safe.json) =>
  !Utility.hasMethod(msg) && Utility.hasId(msg);

let parse = (msg: Yojson.Safe.json) => {
  let result =
    Utility.hasResult(msg)
      ? {
        let result = msg |> Yojson.Safe.Util.member("result");
        Ok(result);
      }
      : {
        let error =
          msg |> Yojson.Safe.Util.member("error") |> Yojson.Safe.to_string;
        Error(error);
      };

  result;
};
