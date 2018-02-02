[%bs.raw {|require('./app.css')|}];

type action =
  | LoadInitialData
  | UpdateData(bool, option(ZafClient.subdomain), option(ZafClient.subdomain));

type state = {
  isLoading: bool,
  hashbackHasOauthToken: bool,
  subdomainOnHashback: option(ZafClient.subdomain),
  subdomainOnZendesk: option(ZafClient.subdomain)
};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    isLoading: true,
    hashbackHasOauthToken: false,
    subdomainOnHashback: None,
    subdomainOnZendesk: None
  },
  reducer: (action, state) =>
    switch action {
    | LoadInitialData =>
      ReasonReact.UpdateWithSideEffects(
        {...state, isLoading: true},
        (
          self =>
            Js.Promise.(
              all2((ZafClient.getSubdomain(), ZafClient.getHashbackStatus()))
              |> then_(((subdomainOnZendesk, {hasOauthToken, subdomain})) =>
                   self.send(
                     UpdateData(hasOauthToken, subdomain, subdomainOnZendesk)
                   )
                   |> resolve
                 )
              |> ignore
            )
        )
      )
    | UpdateData(
        hashbackHasOauthToken,
        subdomainOnHashback,
        subdomainOnZendesk
      ) =>
      ReasonReact.Update({
        isLoading: false,
        hashbackHasOauthToken,
        subdomainOnHashback,
        subdomainOnZendesk
      })
    },
  render: _self =>
    <div className="App">
      <div className="App-header">
        <h2> (ReasonReact.stringToElement("yep")) </h2>
      </div>
      <p className="App-intro">
        (ReasonReact.stringToElement("To get started, edit"))
        <code> (ReasonReact.stringToElement(" src/app.re ")) </code>
        (ReasonReact.stringToElement("and save to reload."))
      </p>
    </div>
};
