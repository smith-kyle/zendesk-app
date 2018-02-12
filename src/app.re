type action =
  | ClearError
  | ConfigureHashbackForSubdomain
  | LoadInitialData
  | UpdateData(
      option(Js.Promise.error),
      bool,
      option(ZafClient.subdomain),
      option(ZafClient.subdomain)
    )
  | UpdateErrorTimeoutId(option(Js.Global.timeoutId));

type state = {
  error: option(Js.Promise.error),
  isLoading: bool,
  hasOauthToken: bool,
  subdomainOnHashback: option(ZafClient.subdomain),
  subdomainOnZendesk: option(ZafClient.subdomain),
  errorTimeoutId: option(Js.Global.timeoutId)
};

let initialState = {
  error: None,
  isLoading: true,
  hasOauthToken: false,
  subdomainOnHashback: None,
  subdomainOnZendesk: None,
  errorTimeoutId: None
};

let component = ReasonReact.reducerComponent("App");

let hashbackIsCorrectlyConfigured =
  <div className="c-callout c-callout--success">
    <strong className="c-callout__title">
      (ReasonReact.stringToElement("Hashback is configured correctly"))
    </strong>
    <p className="c-callout__paragraph">
      (
        ReasonReact.stringToElement(
          "Your Zendesk app will now update new tickets with analysis from Hashback."
        )
      )
    </p>
  </div>;

let hashbackRequiresConfiguration = self =>
  <div className="c-callout c-callout--warning">
    <strong className="c-callout__title">
      (ReasonReact.stringToElement("Hashback requires some configuration"))
    </strong>
    <p className="c-callout__paragraph">
      (
        ReasonReact.stringToElement(
          "Click the below button to finish the setup for your Hashback account."
        )
      )
    </p>
    <div className="my-20 l-btn-group">
      <a
        className="c-btn c-btn--medium c-btn--primary"
        onClick=((_) => self.ReasonReact.send(ConfigureHashbackForSubdomain))
        role="button"
        target="_top">
        (ReasonReact.stringToElement("Configure Hashback"))
      </a>
    </div>
  </div>;

let navigateToExternalPage: string => unit = [%bs.raw
  {| function (url) { window.location.href = url; } |}
];

external castToJson : Js.Promise.error => Js.Json.t = "%identity";

let parseErrorMessage = error =>
  Json.Decode.(error |> castToJson |> optional(field("responseText", string)));

let getServerErrorResponse = error =>
  switch (parseErrorMessage(error)) {
  | Some(responseText) => responseText
  | None => "Unknown server error."
  };

let make = _children => {
  ...component,
  initialState: () => initialState,
  didMount: ({state}) =>
    ReasonReact.UpdateWithSideEffects(
      state,
      ({send}) => ZafClient.onAppRegistered(() => send(LoadInitialData))
    ),
  reducer: (action, state) =>
    switch action {
    | ClearError => ReasonReact.Update({...state, error: None})
    | ConfigureHashbackForSubdomain =>
      ReasonReact.SideEffects(
        (
          ({state}) =>
            switch state {
            | {subdomainOnZendesk: Some(ZafClient.Subdomain(subdomain))} =>
              navigateToExternalPage(
                "https://app.hashback.io/settings/integrate/zendesk/"
                ++ subdomain
              )
            | _ => ()
            }
        )
      )
    | LoadInitialData =>
      ReasonReact.UpdateWithSideEffects(
        initialState,
        (
          self =>
            Js.Promise.(
              all2((ZafClient.getSubdomain(), ZafClient.getHashbackStatus()))
              |> then_(
                   ((subdomainOnZendesk, {ZafClient.hasOauthToken, subdomain})) =>
                   self.send(
                     UpdateData(
                       None,
                       hasOauthToken,
                       subdomain,
                       subdomainOnZendesk
                     )
                   )
                   |> resolve
                 )
              |> catch(error => {
                   self.send(UpdateData(Some(error), false, None, None));
                   Js.log("Error: " ++ getServerErrorResponse(error));
                   Js.Global.setTimeout(() => self.send(ClearError), 5000)
                   |> (id => self.send(UpdateErrorTimeoutId(Some(id))));
                   resolve();
                 })
              |> ignore
            )
        )
      )
    | UpdateData(error, hasOauthToken, subdomainOnHashback, subdomainOnZendesk) =>
      ReasonReact.Update({
        ...state,
        error,
        isLoading: false,
        hasOauthToken,
        subdomainOnHashback,
        subdomainOnZendesk
      })
    | UpdateErrorTimeoutId(errorTimeoutId) =>
      ReasonReact.Update({...state, errorTimeoutId})
    },
  render: ({state} as self) =>
    <div className="hashback-app">
      (
        switch state {
        | {isLoading: true} =>
          <section> (ReasonReact.stringToElement("Loading...")) </section>
        | {error: Some(error)} =>
          <section>
            <div>
              (
                ReasonReact.stringToElement(
                  "There was an error when checking account status. Please try again..."
                )
              )
            </div>
            <div>
              (
                ReasonReact.stringToElement(
                  "Server error: " ++ getServerErrorResponse(error)
                )
              )
            </div>
          </section>
        | {
            isLoading: false,
            hasOauthToken: true,
            subdomainOnHashback:
              Some(ZafClient.Subdomain(subdomainOnHashback)),
            subdomainOnZendesk: Some(ZafClient.Subdomain(subdomainOnZendesk))
          } =>
          subdomainOnHashback == subdomainOnZendesk ?
            hashbackIsCorrectlyConfigured : hashbackRequiresConfiguration(self)
        | _ => hashbackRequiresConfiguration(self)
        }
      )
    </div>
};
