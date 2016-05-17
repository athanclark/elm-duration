## Elm-Duration

A helper component for issuing actions every browser frame with some easing functions,
over some time duration.

```elm
type alias MyModel =
  { someComponent : MyComponent
  , duration : Duration
  }

initMyModel : MyModel
initMyModel =
  { someComponent = initMyComponent
  , duration = initDuration
  }

type MyMsg
  = ComponentMsg MyComponentMsg
  | DurationMsg DurationMsg

updateMyModel : MyMsg -> MyModel -> (MyModel, Cmd MyMsg)
updateMyModel action model =
  case action of
    ComponentMsg a ->
      let (newComponent, eff) = updateComponent a model.someComponent
      in  ( { model | someComponent = newComponent }
          , Cmd.map ComponentMsg eff
          )
    DurationMsg a ->
      let (newDuration, eff) = updateDuration
                                 [(outQuad, ComponentMsg << MyComponentVisibility)]
                                 (2 * second) a model.duration
      in  ( { model | duration = newDuration }
          , Cmd.map (handleDurationResults DurationMsg) eff
          )

subscriptions : MyModel -> Sub MyMsg
subscriptions = Sub.map DurationMsg durationSubscriptions

view : MyModel -> Html MyMsg
view model =
  div []
    [ viewMyComponent model.someComponent
    , button [ onClick <| DurationMsg Start
             ] [text "Start animation"]
    ]

```
