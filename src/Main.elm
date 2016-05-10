module Main where

import Array exposing (Array)
import Color
import Dict exposing (Dict)
import Graphics.Element exposing (Element, show)
import Graphics.Collage as Gfx

import LodeRunner.Input as Inp
import LodeRunner.Types exposing (..)
import LodeRunner.View as View

main : Signal Element
main =
  let blockSize = 40
      fps = 30

      step f inp (key, st) = case inp of
        Inp.InputKey k  -> (k, st)
        Inp.InputTime t -> (key, f key st)

  in Inp.inputs keyBindings fps
       |> Signal.foldp (step stepper) (Nothing, initialState)
       |> Signal.map (snd >> View.view viewSprite viewBlock blockSize)

initialState : GameState
initialState =
  { player = { x = 0, y = 1, state = Standing }
  , enemies = [ { x = 1, y = 1, state = Standing }
              , { x = 2, y = 1, state = Standing }
              ]
  , dimensions = { w = 3, h = 3 }
  , grid = Array.fromList [ Background, Wall, Background
                          , Background, Wall, Background
                          ,       Wall, Wall, Background
                          ]
  }

keyBindings : Inp.KeyBindings
keyBindings = Dict.fromList
  [ ('J', MoveLeft)
  , ('L', MoveRight)
  , ('I', MoveUp)
  , ('K', MoveDown)
  , ('U', DigLeft)
  , ('O', DigRight)
  ]

viewSprite : View.BlockSize -> Sprite -> Gfx.Form
viewSprite blockSize s =
  let sz = toFloat blockSize
      actor r g b = Gfx.filled (Color.rgb r g b) (Gfx.rect (sz / 4) sz)
  in case s of
       Player _ -> actor 255 255 255
       Enemy _  -> actor 255   0   0

viewBlock : View.BlockSize -> Block -> Gfx.Form
viewBlock blockSize b =
  let sz = toFloat blockSize
      box r g b = Gfx.filled (Color.rgb r g b) (Gfx.rect sz sz)
  in case b of
       Wall       -> box 200 200 255
       Ladder     -> box   0 255 255
       Background -> box   0   0   0

stepper : Maybe Key -> GameState -> GameState
stepper key state =
  let player = state.player
      x' = case key of
        Just MoveLeft  -> state.player.x - 0.1
        Just MoveRight -> state.player.x + 0.1
        _              -> state.player.x
      y' = case key of
        Just MoveUp    -> state.player.y + 0.1
        Just MoveDown  -> state.player.y - 0.1
        _              -> state.player.y
      p = { player | x = x', y = y' }
      es = List.indexedMap (\i e -> { e | x = x' + toFloat i + 1 }) state.enemies
  in { state | player = p, enemies = es }
