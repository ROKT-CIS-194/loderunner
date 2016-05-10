module LodeRunner.Input where

import Char exposing (KeyCode)
import Keyboard exposing (keysDown)
import Set
import Time exposing (Time)
import Dict exposing (Dict)

import LodeRunner.Types exposing (..)

-- All the types of input, currently key presses/releases, and frame events
type Input
  = InputKey (Maybe Key)
  | InputTime Time

type alias KeyBindings
  = Dict Char Key

inputs : KeyBindings -> Int -> Signal Input
inputs bindings fps =
  let currentKey = Signal.map (Set.toList >> List.filterMap parseKey >> oneOnly) keysDown
      parseKey code = Dict.get (Char.toUpper (Char.fromCode code)) bindings
      oneOnly xs = if List.length xs == 1 then List.head xs else Nothing
  in Signal.mergeMany [ Signal.map InputKey currentKey
                      , Signal.map InputTime (Time.fps fps)
                      ]
