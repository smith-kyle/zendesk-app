type product =
  | Support
  | Chat;

type location =
  | Background
  | Modal
  | NavBar
  | NewTicketSidebar
  | OrganizationSidebar
  | TicketEditor
  | TicketSidebar
  | TopBar
  | UserSidebar;

type subdomain =
  | Subdomain(string);

type account = {subdomain: option(subdomain)};

type context = {
  instanceGuid: Utils.guid,
  product,
  account: option(account),
  location,
  ticketId: int
};

type requestOptions = {
  url: Utils.url,
  secure: bool,
  _type: Utils.requestMethod
};

type hashbackStatus = {
  hasOauthToken: bool,
  subdomain: option(subdomain)
};

module Decode = {
  let product = productString =>
    switch productString {
    | "support" => Support
    | "chat" => Chat
    | e => raise(Failure("Unknown product: " ++ e))
    };
  let location = locationString =>
    switch locationString {
    | "background" => Background
    | "modal" => Modal
    | "nav_bar" => NavBar
    | "new_ticket_sidebar" => NewTicketSidebar
    | "organization_sidebar" => OrganizationSidebar
    | "ticket_editor" => TicketEditor
    | "ticket_sidebar" => TicketSidebar
    | "top_bar" => TopBar
    | "user_sidebar" => UserSidebar
    | e => raise(Failure("Unknown product: " ++ e))
    };
  let subdomain = maybeSubdomainString =>
    switch maybeSubdomainString {
    | Some(subdomainString) => Some(Subdomain(subdomainString))
    | None => None
    };
  let account = json =>
    Json.Decode.{
      subdomain: json |> optional(field("subdomain", string)) |> subdomain
    };
  let context = json =>
    Json.Decode.{
      instanceGuid: json |> field("instanceGuid", string) |> Utils.createGuid,
      product: json |> field("product", string) |> product,
      account: json |> optional(field("account", account)),
      location: json |> field("location", string) |> location,
      ticketId: json |> field("ticketId", int)
    };
  let hashbackStatus = json =>
    Json.Decode.{
      hasOauthToken: json |> field("hasOauthToken", bool),
      subdomain: json |> optional(field("subdomain", string)) |> subdomain
    };
};

type zafClient;

[@bs.scope "ZAFClient"] [@bs.val] external init : unit => zafClient = "";

[@bs.send]
external requestContext : zafClient => Js.Promise.t(Js.Json.t) = "context";

[@bs.send]
external request :
  (
    zafClient,
    {
      .
      "url": string,
      "secure": Js.boolean,
      "_type": string
    }
  ) =>
  Js.Promise.t(Js.Json.t) =
  "";

[@bs.send]
external on_app_registered :
  (zafClient, [@bs.as "app.registered"] _, unit => unit) => unit =
  "on";

let zafClientInstance = init();

let onAppRegistered = callback =>
  on_app_registered(zafClientInstance, callback);

let makeRequest = ({url, secure, _type}) =>
  request(
    zafClientInstance,
    Utils.(
      {
        "url": urlToString(url),
        "secure": Js.Boolean.to_js_boolean(secure),
        "_type": requestMethodToJs(_type)
      }
    )
  );

let getContext = () =>
  Js.Promise.(
    zafClientInstance
    |> requestContext
    |> then_(json => json |> Decode.context |> resolve)
  );

let accountLens =
  Rationale.Lens.(
    make(
      context => context.account,
      (account, context) => {...context, account}
    )
    >>- optional({subdomain: None})
  );

let subdomainLens =
  Rationale.Lens.(
    make(
      (account: account) => account.subdomain,
      (subdomain, _) => {subdomain: subdomain}
    )
  );

let getSubdomain = () =>
  Js.Promise.(
    getContext()
    |> then_(context =>
         context
         |> Rationale.Lens.(view(accountLens >>- subdomainLens))
         |> resolve
       )
  );

let getHashbackStatus = () =>
  Js.Promise.(
    makeRequest(
      Utils.{
        url:
          Url(
            "https://api.hashback.io/feedback/zendesk/status?token={{setting.token}}"
          ),
        secure: true,
        _type: `Get
      }
    )
    |> then_(json => json |> Decode.hashbackStatus |> resolve)
  );
