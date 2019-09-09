type t;

type notificationHandler = (Notification.t, t) => unit;
type requestHandler = (Request.t, t) => result(Yojson.Safe.t, string);
type closeHandler = unit => unit;
type errorMessageHandler = string => unit;

let start:
  (
    ~onNotification: notificationHandler,
    ~onRequest: requestHandler,
    ~onClose: closeHandler,
    ~onError: errorMessageHandler=?,
    in_channel,
    out_channel
  ) =>
  t;

let sendNotification: (t, string, Yojson.Safe.t) => unit;

type responseHandler = (Response.t, t) => unit;

let sendRequest: (t, string, Yojson.Safe.t, responseHandler) => unit;

/*
 * Calling 'pump' is required to handle pending notifications and requests
 */
let pump: t => unit;

let stop: t => unit;
