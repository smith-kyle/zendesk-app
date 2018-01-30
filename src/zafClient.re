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
      url: Utils.url,
      secure: bool,
      type_: Utils.requestMethod
    }
  ) =>
  Js.Promise.t(Js.Json.t) =
  "";

[@bs.send]
external on_app_registered :
  (zafClient, [@bs.as "app.registered"] _, unit => unit) => unit =
  "on";

let getContext = zafClient =>
  Js.Promise.(
    requestContext(zafClient)
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
      account => account.subdomain,
      (subdomain, account) => {...account, subdomain}
    )
  );

let getSubdomain = zafClient =>
  Js.Promise.(
    getContext(zafClient)
    |> then_(context =>
         context
         |> Rationale.Lens.(view(accountLens >>- subdomainLens))
         |> resolve
       )
  );
