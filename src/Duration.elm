module Duration exposing
  ( Duration
  , initDuration
  , DurationMsg (Start)
  , DurationResults
  , handleDurationResults
  , updateDuration
  , durationSubscriptions
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
type alias Duration =
  { elapsed : Maybe Time
  }

{-| The initial state of the duration -}
initDuration : Duration
initDuration =
  { elapsed = Nothing
  }

{-| Actions of the duration; just use `Start` to kick it off -}
type DurationMsg
  = Start
  | Tick Time

{-| Either another duration message, or the action you'd want issued -}
type DurationResults a
  = More DurationMsg
  | Issue a

{-| Given a way to handle duration messages, you can handle the results -}
handleDurationResults : (DurationMsg -> a) -> DurationResults a -> a
handleDurationResults f m =
  case m of
    More x -> f x
    Issue x -> x

{-| Given a set of _parallel_ animations and their actions to issue, and
    a duration the animation should play over, create an update function.
    Note that the `Float`s in this function **must** be between `0 <= x <= 1` -
    just like [easing-functions](http://package.elm-lang.org/packages/elm-community/easing-functions/1.0.1).
    Also note that there's an optional action called when the animation is completed.
-}
updateDuration : List (Float -> Float, Float -> a)
              -> Time
              -> Maybe a
              -> DurationMsg
              -> Duration
              -> (Duration, Cmd (DurationResults a))
updateDuration animations duration mMainAction action model =
  case action of
    Start ->
      case model.elapsed of
        Just _ ->
          ( model
          , Cmd.none
          )
        Nothing ->
          ( model
          , Task.perform Debug.crash (More << Tick) Time.now
          )
    Tick now ->
      case model.elapsed of
        Nothing -> -- fail silently
          ( initDuration
          , Cmd.none
          )
        Just past ->
          if now - past > duration
          then ( initDuration
               , Cmd.batch <|
                   ( List.map -- complete them
                       (\(_, act) -> Task.perform Debug.crash Issue <|
                                       Task.succeed <| act 1)
                       animations
                   ) ++ [ case mMainAction of
                            Nothing -> Cmd.none
                            Just a  -> Task.perform Debug.crash Issue <|
                                         Task.succeed a
                        ]
               )
          else ( { model | elapsed = Just now }
               , let current = now - past / duration
                 in  Cmd.batch <| List.map
                       (\(anim,act) -> Task.perform Debug.crash Issue <|
                                         Task.succeed <| act <| anim current)
                       animations
               )


{-| The subscriptions for the duration - attaching to the current time every
    browser screen refresh.
-}
durationSubscriptions : Duration -> Sub DurationMsg
durationSubscriptions model =
  case model.elapsed of
    Nothing -> Sub.none
    Just _  -> Anim.times Tick
