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

main : Signal Element
main =
  Signal.map show keys
