type action =
  | ConfigureHashbackForSubdomain
  | LoadInitialData
  | UpdateData(bool, option(ZafClient.subdomain), option(ZafClient.subdomain));

type state = {
  isLoading: bool,
  hasOauthToken: bool,
  subdomainOnHashback: option(ZafClient.subdomain),
  subdomainOnZendesk: option(ZafClient.subdomain)
};

let initialState = {
  isLoading: true,
  hasOauthToken: false,
  subdomainOnHashback: None,
  subdomainOnZendesk: None
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

let make = _children => {
  ...component,
  initialState: () => initialState,
  didMount: ({state}) =>
    ReasonReact.UpdateWithSideEffects(
      state,
      ({send}) => ZafClient.onAppRegistered(() => send(LoadInitialData))
    ),
  reducer: (action, _) =>
    switch action {
    | ConfigureHashbackForSubdomain =>
      ReasonReact.SideEffects(
        (
          ({state}) =>
            switch state {
            | {subdomainOnZendesk: Some(ZafClient.Subdomain(subdomain))} =>
              ReasonReact.Router.push(
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
                     UpdateData(hasOauthToken, subdomain, subdomainOnZendesk)
                   )
                   |> resolve
                 )
              |> ignore
            )
        )
      )
    | UpdateData(hasOauthToken, subdomainOnHashback, subdomainOnZendesk) =>
      ReasonReact.Update({
        isLoading: false,
        hasOauthToken,
        subdomainOnHashback,
        subdomainOnZendesk
      })
    },
  render: ({state} as self) =>
    <div className="hashback-app">
      (
        switch state {
        | {isLoading: true} =>
          <section> (ReasonReact.stringToElement("Loading...")) </section>
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
