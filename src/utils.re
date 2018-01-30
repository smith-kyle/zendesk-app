type guid =
  | Guid(string);

type url =
  | Url(string);

type requestMethod =
  | Connect
  | Delete
  | Get
  | Head
  | Options
  | Patch
  | Post
  | Put
  | Trace;

let createGuid = guidString => Guid(guidString);

let createUrl = urlString => Url(urlString);

let encodeRequestMethod = verbType =>
  switch verbType {
  | Connect => "CONNECT"
  | Delete => "DELETE"
  | Get => "GET"
  | Head => "HEAD"
  | Options => "OPTIONS"
  | Patch => "PATCH"
  | Post => "POST"
  | Put => "PUT"
  | Trace => "TRACE"
  };

let decodeRequestMethod = verbString =>
  switch verbString {
  | "CONNECT" => Connect
  | "DELETE" => Delete
  | "GET" => Get
  | "HEAD" => Head
  | "OPTIONS" => Options
  | "PATCH" => Patch
  | "POST" => Post
  | "PUT" => Put
  | "TRACE" => Trace
  | e => raise(Failure("Unknown requestMethod: " ++ e))
  };
