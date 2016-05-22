module Duration exposing
  ( Model
  , init
  , Msg (Start)
  , handle
  , update
  , subscriptions
  )

{-|

## Duration State

@docs Duration

@docs initDuration

## Duration Initialization

@docs DurationMsg

## Duration Evaluation

@docs updateDuration

@docs durationSubscriptions

### Result Handling

@docs DurationResults

@docs handleDurationResults


-}

import Time exposing (Time)
import Task
import AnimationFrame as Anim


{-| The state of the duration -}
type alias Model a =
  { elapsed      : Maybe Time
  , onCompletion : Cmd a
  }

{-| The initial state of the duration -}
init : Model a
init =
  { elapsed      = Nothing
  , onCompletion = Cmd.none
  }

{-| Actions of the duration; just use `Start` to kick it off -}
type Msg a
  = Start (Cmd a -> Cmd a)
  | Tick Time

{-| Given a way to handle duration messages, you can handle the results -}
handle : (Msg a -> a) -> Result (Msg a) a -> a
handle f m =
  case m of
    Err x -> f x
    Ok x  -> x

{-| Given a set of _parallel_ animations and their actions to issue, and
    a duration the animation should play over, create an update function.
    Note that the `Float`s in this function **must** be between `0 <= x <= 1` -
    just like [easing-functions](http://package.elm-lang.org/packages/elm-community/easing-functions/1.0.1).
    Also note that there's an optional action called when the animation is completed.
-}
update : (Time -> Cmd a)
      -> Time
      -> Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update actions duration mMainAction action model =
  case action of
    Start withCompletion ->
      case model.elapsed of
        Just _ ->
          ( { model | onCompletion = withCompletion model.onCompletion }
          , Cmd.none
          )
        Nothing ->
          ( { model | onCompletion = withCompletion model.onCompletion }
          , Task.perform Debug.crash (Err << Tick) Time.now
          )
    Tick now ->
      case model.elapsed of
        Nothing ->
          ( { model | elapsed = Just now }
          , actions 0
          )
        Just past ->
          if now - past > duration
          then ( { model | elapsed = Nothing }
               , Cmd.map Ok <| Cmd.batch
                   [ actions 1
                   , model.onCompletion
                   ]
               )
          else ( model
               , let current = (now - past) / duration
                 in Cmd.map Ok <| actions current
               )


{-| The subscriptions for the duration - attaching to the current time every
    browser screen refresh.
-}
subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  case model.elapsed of
    Nothing -> Sub.none
    Just _  -> Anim.times Tick
