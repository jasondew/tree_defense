module Maps where

import Models exposing (..)

import Array exposing (Array)

defaultMap : Map
defaultMap =
  Array.fromList [
    Array.fromList [Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass],
    Array.fromList [Grass, Grass, Grass, Grass,  Road,  Road,  Road, Grass, Grass, Grass],
    Array.fromList [Grass, Grass, Grass, Grass,  Road, Grass,  Road, Grass, Grass, Grass],
    Array.fromList [Road,   Road, Grass, Grass,  Road, Grass,  Road, Grass, Grass, Grass],
    Array.fromList [Grass,  Road, Grass, Grass,  Road, Grass,  Road, Grass, Grass, Grass],
    Array.fromList [Grass,  Road, Grass, Grass,  Road, Grass,  Road,  Road,  Road,  Road],
    Array.fromList [Grass,  Road, Grass, Grass,  Road, Grass, Grass, Grass, Grass, Grass],
    Array.fromList [Grass,  Road,  Road,  Road,  Road, Grass, Grass, Grass, Grass, Grass],
    Array.fromList [Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass],
    Array.fromList [Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass]
  ]
