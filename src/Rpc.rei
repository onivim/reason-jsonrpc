type t;

type notificationHandler = (Notification.t, t) => unit;
type requestHandler = (Request.t, t) => result(Yojson.Safe.json, string);

let start: (~onNotification:notificationHandler, ~onRequest:requestHandler, in_channel, out_channel) => t;

let sendNotification = (t, string, Yojson.Safe.json) => unit;

let stop: (t) => unit;
