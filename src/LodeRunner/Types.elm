module LodeRunner.Types where

import Array exposing (Array)
import Graphics.Collage as Gfx

-- Relevent key presses
type Key
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | DigLeft
  | DigRight

-- Visual states the player and enemies can be in
type ActorState
  = Standing
  | RunningLeft
  | RunningRight
  | DiggingLeft
  | DiggingRight
  | Climbing
  | Falling

-- Sprite types for display
type Sprite
  = Player ActorState
  | Enemy ActorState

-- Block types that make up the level
type Block
  = Wall
  | Ladder
  | Background

-- All state contained in the game between frames
type alias GameState
  = { player : { x : Float, y : Float, state : ActorState }
    , enemies : List { x : Float, y : Float, state : ActorState }
    , dimensions : { w : Int, h : Int }
    , grid : Array Block
    }
