type t;

type notificationHandler = (Notification.t, t) => unit;
type requestHandler = (Request.t, t) => result(Yojson.Safe.json, string);
type closeHandler = unit => unit;

let start: (~onNotification:notificationHandler, ~onRequest:requestHandler, ~onClose:closeHandler, in_channel, out_channel) => t;

let sendNotification: (t, string, Yojson.Safe.json) => unit;

/*
 * Calling 'pump' is required to handle pending notifications and requests
 */
let pump: t => unit;

let stop: (t) => unit;
