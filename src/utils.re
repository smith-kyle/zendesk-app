type guid =
  | Guid(string);

type url =
  | Url(string);

[@bs.deriving jsConverter]
type requestMethod = [
  | [@bs.as "CONNECT"] `Connect
  | [@bs.as "DELETE"] `Delete
  | [@bs.as "GET"] `Get
  | [@bs.as "HEAD"] `Head
  | [@bs.as "OPTIONS"] `Options
  | [@bs.as "PATCH"] `Patch
  | [@bs.as "POST"] `Post
  | [@bs.as "PUT"] `Put
  | [@bs.as "TRACE"] `Trace
];

let createGuid = guidString => Guid(guidString);

let guidToString = guid =>
  switch guid {
  | Guid(guidString) => guidString
  };

let createUrl = urlString => Url(urlString);

let urlToString = url =>
  switch url {
  | Url(urlString) => urlString
  };
