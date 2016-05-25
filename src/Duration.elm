module Duration exposing
  ( Model
  , init
  , Msg (Forward, Reverse, Toggle)
  , update
  , subscriptions
  )

{-|

## Duration State

@docs Model

@docs init

## Duration Initialization

@docs Msg

## Duration Evaluation

@docs update

@docs subscriptions


-}

import Time exposing (Time)
import Task
import AnimationFrame as Anim


type Direction
  = Forwards
  | Backwards

{-| The state of the duration -}
type alias Model b =
  { elapsed      : Maybe (Time, Direction)
  , onCompletion : Cmd b
  }

{-| The initial state of the duration -}
init : Model b
init =
  { elapsed      = Nothing
  , onCompletion = Cmd.none
  }

{-| Either trigger the transition to explicitly go `Forward`, `Reverse`,
    or `Toggle` its playback, where `b` is the effect to be played when
    the transition finishes.
-}
type Msg b
  = Forward (Cmd b -> Cmd b)
  | Reverse (Cmd b -> Cmd b)
  | Toggle  (Cmd b -> Cmd b)
  | Tick Time


{-| Given a time-indexed command and the length of time the animation
    should play over, create an update function.
-}
update : (Time -> Cmd a)
      -> Time
      -> Msg b
      -> Model b
      -> (Model b, Cmd (Result a b))
update actions duration action model =
  case action of
    Forward withCompletion ->
      case model.elapsed of
        Nothing ->
          ( { model | onCompletion = withCompletion model.onCompletion
                    , elapsed      = Just (0, Forwards)
            }
          , Cmd.map Err <| actions 0
          )
        Just (oldTime, direction) ->
          case direction of
            Forwards ->
              ( { model | onCompletion = withCompletion model.onCompletion }
              , Cmd.none
              )
            Backwards ->
              ( { model | onCompletion = withCompletion model.onCompletion
                        , elapsed      = Just (oldTime, Forwards)
                }
              , Cmd.none
              )
    Reverse withCompletion ->
      case model.elapsed of
        Nothing ->
          ( { model | onCompletion = withCompletion model.onCompletion
                    , elapsed      = Just (duration, Backwards)
            }
          , Cmd.map Err <| actions duration
          )
        Just (oldTime, direction) ->
          case direction of
            Backwards ->
              ( { model | onCompletion = withCompletion model.onCompletion }
              , Cmd.none
              )
            Forwards ->
              ( { model | onCompletion = withCompletion model.onCompletion
                        , elapsed      = Just (oldTime, Backwards)
                }
              , Cmd.none
              )
    Toggle withCompletion ->
      case model.elapsed of
        Nothing ->
          ( { model | onCompletion = withCompletion model.onCompletion }
          , Cmd.none
          )
        Just (oldTime, direction) ->
          ( { model | onCompletion = withCompletion model.onCompletion
                    , elapsed = Just ( oldTime
                                     , case direction of
                                         Forwards -> Backwards
                                         Backwards -> Forwards
                                     )
            }
          , Cmd.none
          )
    Tick diff ->
      case model.elapsed of
        Nothing -> Debug.crash "Somehow in bad state"
        Just (oldTime, direction) ->
          case direction of
            Forwards  ->
              let newTime = oldTime + diff
              in if newTime >= duration
              then
                ( { model | elapsed = Nothing }
                , Cmd.batch
                    [ Cmd.map Err <| actions duration
                    , Cmd.map Ok <| model.onCompletion
                    ]
                )
              else
                ( { model | elapsed = Just (newTime, direction) }
                , Cmd.map Err <| actions newTime
                )
            Backwards ->
              let newTime = oldTime - diff
              in if newTime <= 0
              then
                ( { model | elapsed = Nothing }
                , Cmd.batch
                    [ Cmd.map Err <| actions 0
                    , Cmd.map Ok <| model.onCompletion
                    ]
                )
              else
                ( { model | elapsed = Just (newTime, direction) }
                , Cmd.map Err <| actions newTime
                )


{-| The subscriptions for the duration - every time the browser screen refreshes. -}
subscriptions : Model b -> Sub (Msg b)
subscriptions model =
  case model.elapsed of
    Nothing -> Sub.none
    Just _  -> Anim.diffs Tick
