module Models where

import Array exposing (Array)
import Color exposing (Color)

type alias Model = {
  state: State,
  outcome: Maybe Outcome,
  map: Map,
  creeps: List Creep,
  towers: List Tower,
  projectiles: List Projectile,
  lives: Int,
  money: Int
}

type State =
  Play |
  Pause

type Outcome =
  Won |
  Lost

type alias Map = Array (Array Tile)

type alias Creep = {
  position: Maybe Position,
  previousPosition: Maybe Position,
  delay: Delay,
  color: Color,
  health: Int
}

type alias Tower = {
  position: Position,
  radius: Int,
  damage: Int
}

type alias Projectile = {
  tower: Tower,
  creep: Creep,
  position: Float
}

type alias Position = (Int, Int)

type Tile =
  Grass |
  Road

type alias Delay = Int
