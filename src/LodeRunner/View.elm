module LodeRunner.View where

import Array exposing (Array)
import Graphics.Element as E
import Graphics.Collage as Gfx
import Graphics.Element exposing (Element)

import LodeRunner.Types exposing (..)

type alias BlockSize = Int

view : (BlockSize -> Sprite -> Gfx.Form)
     -> (BlockSize -> Block -> Gfx.Form)
     -> BlockSize
     -> GameState -> Element
view viewSprite viewBlock blockSize state =
  let blocks =
        indexArray state.dimensions state.grid
          |> Array.map (renderBlock viewBlock blockSize state.dimensions)
          |> Array.toList

      player =
        [renderSprite viewSprite blockSize (Player state.player.state) state.player]

      enemies =
        List.map (renderSprite viewSprite blockSize (Enemy state.player.state))
                 state.enemies

  in List.concat [blocks, player, enemies]
       |> List.map (Gfx.move ( globalOffset blockSize state.dimensions.w
                             , globalOffset blockSize state.dimensions.h ))
       |> Gfx.collage (state.dimensions.w * blockSize) (state.dimensions.h * blockSize)

globalOffset : Int -> Int -> Float
globalOffset blockSize x =
  negate (toFloat (blockSize * x) / 2)

renderSprite : (BlockSize -> Sprite -> Gfx.Form) -> BlockSize -> Sprite
             -> { x : Float, y : Float, state : ActorState } -> Gfx.Form
renderSprite viewSprite blockSize sprite abc =
  let offset =
        ( toFloat blockSize * 0.5 + (abc.x * toFloat blockSize)
        , toFloat blockSize * 0.5 + (abc.y * toFloat blockSize) )
  in Gfx.move offset (viewSprite blockSize sprite)

renderBlock : (BlockSize -> Block -> Gfx.Form) -> BlockSize -> { w : Int, h : Int }
            -> { x : Int, y : Int, payload : Block } -> Gfx.Form
renderBlock viewBlock blockSize dims block =
  let offset =
        ( toFloat (block.x * blockSize) + toFloat blockSize * 0.5
        , toFloat ((dims.h - block.y - 1) * blockSize) + toFloat blockSize * 0.5)
  in Gfx.move offset (viewBlock blockSize block.payload)

indexArray : { w : Int, h : Int } -> Array a -> Array { x : Int, y : Int, payload : a }
indexArray dims array =
  let sliceRow y = let a = y * dims.w in Array.slice a (a + dims.w) array
      expandRow y = Array.indexedMap (\x a -> { x = x, y = y, payload = a })
  in List.repeat dims.h ()
      |> List.indexedMap (\y _ -> sliceRow y |> expandRow y)
      |> List.foldl Array.append Array.empty
