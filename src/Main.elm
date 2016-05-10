module Main where

import Char exposing (KeyCode)
import Graphics.Element exposing (Element, show)
import Keyboard exposing (keysDown)
import List
import Set
import Time exposing (Time)

type Key = Up | Down | Left | Right | DigLeft | DigRight

toKey : KeyCode -> Maybe Key
toKey code =
  case Char.toUpper (Char.fromCode code) of
    'T' -> Just DigLeft
    'Y' -> Just Up
    'U' -> Just DigRight
    'G' -> Just Left
    'H' -> Just Down
    'J' -> Just Right
    _ -> Nothing

type Input = InputKey (Maybe Key) | InputTime Time

inputs : Signal Input
inputs =
  let currentKey = Signal.map (Set.toList >> List.filterMap toKey >> oneOnly) keysDown
      oneOnly xs = if List.length xs == 1 then List.head xs else Nothing
  in Signal.mergeMany [ Signal.map InputKey currentKey
                      , Signal.map InputTime (Time.fps 15)
                      ]

type alias GameState
  = { key      : Maybe Key
    , position : Int
    }

main : Signal Element
main =
  let initialState : GameState
      initialState = { key = Nothing, position = 0 }

      view : GameState -> Element
      view st = show { position = st.position }

      step : Input -> GameState -> GameState
      step inp st = case inp of
        InputKey k ->
          { st | key = k }
        InputTime t ->
          let pos = case st.key of
                      Just Left  -> st.position - 1
                      Just Right -> st.position + 1
                      _          -> st.position
          in { st | position = pos }

  in inputs |> Signal.foldp step initialState |> Signal.map view
