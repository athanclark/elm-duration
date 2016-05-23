module Duration.Revert exposing
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
type alias Model a =
  { elapsed      : Maybe (Time, Direction)
  , onCompletion : Cmd a
  }

{-| The initial state of the duration -}
init : Model a
init =
  { elapsed      = Nothing
  , onCompletion = Cmd.none
  }

{-| 
-}
type Msg a
  = Forward (Cmd a -> Cmd a)
  | Reverse (Cmd a -> Cmd a)
  | Toggle  (Cmd a -> Cmd a)
  | Tick Time


{-| Given a time-indexed command and the length of time the animation should play over, create an update function. -}
update : (Time -> Cmd a)
      -> Time
      -> Msg a
      -> Model a
      -> (Model a, Cmd a)
update actions duration action model =
  case action of
    Forward withCompletion ->
      case model.elapsed of
        Nothing ->
          ( { model | onCompletion = withCompletion model.onCompletion
                    , elapsed      = Just (0, Forwards)
            }
          , actions 0
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
          , actions duration
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
                    [ actions duration
                    , model.onCompletion
                    ]
                )
              else
                ( { model | elapsed = Just (newTime, direction) }
                , actions newTime
                )
            Backwards ->
              let newTime = oldTime - diff
              in if newTime <= 0
              then
                ( { model | elapsed = Nothing }
                , Cmd.batch
                    [ actions 0
                    , model.onCompletion
                    ]
                )
              else
                ( { model | elapsed = Just (newTime, direction) }
                , actions newTime
                )


{-| The subscriptions for the duration - every time the browser screen refreshes. -}
subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  case model.elapsed of
    Nothing -> Sub.none
    Just _  -> Anim.diffs Tick
