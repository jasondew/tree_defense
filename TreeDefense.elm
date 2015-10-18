module TreeDefense where

import Graphics.Element exposing (Element, show)
import Graphics.Collage as Collage exposing (Form, Shape)
import Color exposing (Color)
import Time exposing (Time)
import Array exposing (Array)
import Text
import Window
import Debug

main : Signal Element
main =
  Signal.map2 view model Window.dimensions


-- MODEL


type alias Model = {
  map: Map,
  creeps: List Creep,
  towers: List Tower,
  projectiles: List Projectile
}

type alias Map = Array (Array Tile)

type alias Creep = {
  position: Position,
  previousPosition: Maybe Position,
  delay: Maybe Int,
  color: Color
}

type alias Tower = {
  position: Position,
  radius: Int
}

type alias Projectile = {
  source: Tower,
  target: Creep
}

type alias Position = (Int, Int)

type Tile =
  Grass |
  Road

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

defaultCreeps : List Creep
defaultCreeps =
  [
    Creep (0, 3) Nothing Nothing Color.lightBlue,
    Creep (0, 3) Nothing (Just 2) Color.lightRed,
    Creep (0, 3) Nothing (Just 7) Color.lightGreen
  ]

initialModel : Model
initialModel =
  Model defaultMap defaultCreeps [Tower (2, 6) 1, Tower (5, 2) 2] []

model : Signal Model
model =
  Signal.foldp update initialModel inputs


-- UPDATE


type Action =
  NoOp |
  Tick Time

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Tick time ->
      let
        updatedCreeps = updateCreeps model.map model.creeps
      in
        { model |
          creeps <- Debug.watch "creeps" updatedCreeps,
          projectiles <- Debug.watch "projectiles" (List.filterMap (projectile updatedCreeps) (Debug.watch "towers" model.towers))
        }

projectile : List Creep -> Tower -> Maybe Projectile
projectile creeps tower =
  let
    creepsInRange = List.filter (inRange tower) creeps
  in
    List.head creepsInRange
    |> Maybe.map (Projectile tower)

inRange : Tower -> Creep -> Bool
inRange tower creep =
  distance tower.position creep.position <= tower.radius

distance : Position -> Position -> Int
distance (x1, y1) (x2, y2) =
  (x1 - x2) ^ 2 + (y1 - y2) ^ 2
  |> toFloat
  |> sqrt
  |> round

updateCreeps : Map -> List Creep -> List Creep
updateCreeps map creeps =
  List.filterMap (updateCreep map) creeps

updateCreep : Map -> Creep -> Maybe Creep
updateCreep map creep =
  case creep.delay of
    Just delay ->
      Just { creep | delay <- if delay == 0 then Nothing else Just (delay - 1) }
    Nothing ->
      case inBounds map creep.position of
        True ->
          case nextPosition map creep of
            Just newPosition ->
              Just { creep |
                     position <- newPosition,
                     previousPosition <- Just creep.position
                   }
            Nothing ->
              Just creep
        False ->
          Nothing

inBounds : Map -> Position -> Bool
inBounds map (x, y) =
  let
    columns = Array.length map
    rows = Array.length
           <| Maybe.withDefault Array.empty
           <| Array.get 0 map
  in
    x < (columns - 1) && y < (rows - 1)

nextPosition : Map -> Creep -> Maybe Position
nextPosition map creep =
  let
    (x, y) = creep.position
    up     = ( x,  y+1)
    down   = ( x,  y-1)
    right  = (x+1,  y )
  in
    List.map (\position -> (position, tileAt map position)) [up, down, right]
    |> List.filter (traversable creep.previousPosition)
    |> List.map fst
    |> List.head                    -- TODO: pick one at random

traversable : Maybe Position -> (Position, Maybe Tile) -> Bool
traversable maybePreviousPosition (position, maybeTile) =
  case maybeTile of
    Just tile ->
      tile == Road && case maybePreviousPosition of
                        Just previousPosition -> position /= previousPosition
                        Nothing -> True
    Nothing ->
      False

tileAt : Map -> Position -> Maybe Tile
tileAt map (x, y) =
  Maybe.andThen (Array.get y map) (Array.get x)


-- SIGNALS


mailbox : Signal.Mailbox Action
mailbox =
  Signal.mailbox NoOp

actions : Signal Action
actions =
  mailbox.signal

address : Signal.Address Action
address =
  mailbox.address

clockTicks : Signal Action
clockTicks =
  Signal.map Tick <| Time.fps 5

inputs : Signal Action
inputs =
  Signal.mergeMany [actions, clockTicks]


-- VIEW


view : Model -> (Int, Int) -> Element
view model (width, height) =
  List.filterMap creepView model.creeps
  |> List.append (List.map towerView model.towers)
  |> List.append [mapView model.map]
  |> Collage.collage width height

towerView : Tower -> Form
towerView tower =
  let
    base = Collage.circle (tileSize / 4)
           |> Collage.filled Color.darkGray
           |> Collage.move (translate tower.position)
    halo = Collage.circle (tileSize * (toFloat tower.radius + 0.5))
           |> Collage.outlined Collage.defaultLine
           |> Collage.move (translate tower.position)
  in
    Collage.group [base, halo]

creepView : Creep -> Maybe Form
creepView creep =
  case creep.delay of
    Just _ ->
      Nothing
    Nothing ->
      Collage.circle (tileSize / 4)
      |> Collage.filled creep.color
      |> Collage.move (translate creep.position)
      |> Just

mapView : Map -> Form
mapView map =
  Array.toIndexedList map
  |> List.map columnView
  |> List.concat
  |> Collage.group

columnView : (Int, Array Tile) -> List Form
columnView (y, column) =
  Array.toIndexedList column
  |> List.map (\(x, tile) -> tileView (x, y) tile)

tileView : Position -> Tile -> Form
tileView position tile =
  let
    color = case tile of
      Grass -> Color.darkGreen
      Road  -> Color.brown
    background = Collage.rect tileSize tileSize
                 |> Collage.filled color
    label = Collage.text
            <| Text.fromString
            <| "(" ++ (toString <| fst position) ++ "," ++ (toString <| snd position) ++ ")"
  in
    Collage.group [background] -- , label]
    |> Collage.move (translate position)

translate : Position -> (Float, Float)
translate (x, y) =
  (toFloat x * tileSize - 400, toFloat -y * tileSize + 400)

tileSize : Float
tileSize = 60
