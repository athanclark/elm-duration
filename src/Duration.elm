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

@docs Model

@docs init

## Duration Initialization

@docs Msg

## Duration Evaluation

@docs update

@docs subscriptions

### Result Handling

@docs handle


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

{-| Actions of the duration; just use `Start` to initiate it. You can supply some
    method for issuing actions upon completion of the duration: To make it _always_
    issue some action when completed, use `Start <| always <| Task.perform xx identity <| Task.succeed SomeAction`.
    To additively add more actions upon completion, use `Start <| \x -> Cmd.batch [x, myCommand]`.
    Note that this will adjust the action _during_ the duration, and __is__ mutable and thus
    may cause race conditions - _use wisely_.
-}
type Msg a
  = Start (Cmd a -> Cmd a)
  | Tick Time

{-| Given a way to handle duration messages, you can handle the results -}
handle : (Msg a -> a) -> Result (Msg a) a -> a
handle f m =
  case m of
    Err x -> f x
    Ok x  -> x

{-| Given a time-indexed command and the length of time the animation should play over, create an update function. -}
update : (Time -> Cmd a)
      -> Time
      -> Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update actions duration action model =
  case action of
    Start withCompletion ->
      case model.elapsed of
        Just _ ->
          ( let onCompletion' = withCompletion model.onCompletion
            in  { model | onCompletion = onCompletion' }
          , Cmd.none
          )
        Nothing ->
          ( let onCompletion' = withCompletion model.onCompletion
            in  { model | onCompletion = onCompletion' }
          , Task.perform Debug.crash (Err << Tick) Time.now
          )
    Tick now ->
      case model.elapsed of
        Nothing ->
          ( { model | elapsed = Just now }
          , Cmd.map Ok <| actions 0
          )
        Just past ->
          if now - past > duration
          then ( { model | elapsed = Nothing }
               , Cmd.map Ok <| Cmd.batch
                   [ actions duration
                   , model.onCompletion
                   ]
               )
          else ( model
               , Cmd.map Ok <| actions <| now - past
               )


{-| The subscriptions for the duration - every time the browser screen refreshes. -}
subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  case model.elapsed of
    Nothing -> Sub.none
    Just _  -> Anim.times Tick
