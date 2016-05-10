module Main where

import Char
import Graphics.Element exposing (Element, show)
import Keyboard
import List
import Set
import Task exposing (Task)

type Key = Up | Down | Left | Right | DigLeft | DigRight

toKey : Char.KeyCode -> Maybe Key
toKey code =
  case Char.toUpper (Char.fromCode code) of
    'T' -> Just DigLeft
    'Y' -> Just Up
    'U' -> Just DigRight
    'G' -> Just Left
    'H' -> Just Down
    'J' -> Just Right
    _ -> Nothing

keys : Signal (Maybe Key)
keys =
  let oneOnly xs = if List.length xs == 1 then List.head xs else Nothing
  in Signal.map (Set.toList >> List.filterMap toKey >> oneOnly) Keyboard.keysDown

type alias GameState
  = { key : Maybe Key
    , position : Int }

main : Signal Element
main =
  let state0 : GameState
      state0 = { key = Nothing, position = 0 }

      step : Maybe Key -> GameState -> GameState
      step k st =
        let pos = case k of
                    Just Left -> st.position - 1
                    Just Right -> st.position + 1
                    _ -> st.position
        in { st | key = k, position = pos }

  in keys |> Signal.foldp step state0 |> Signal.map show
