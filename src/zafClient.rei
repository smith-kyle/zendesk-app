type product;

type location;

type subdomain;

type account;

type context;

type requestOptions;

type hashbackStatus;

let onAppRegistered: (unit => unit) => unit;

let makeRequest: requestOptions => Js.Promise.t(Js.Json.t);

let getContext: unit => Js.Promise.t(context);

let getSubdomain: unit => Js.Promise.t(option(subdomain));

let getHashbackStatus: unit => Js.Promise.t(hashbackStatus);
