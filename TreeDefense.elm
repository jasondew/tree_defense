module TreeDefense where

import Graphics.Element as Element
import Graphics.Collage as Collage exposing (Form, Shape)
import Graphics.Input as Input
import Color exposing (Color)
import Time exposing (Time)
import Array exposing (Array)
import Text
import Window
import Debug
import Mouse

main : Signal Element.Element
main =
  Signal.map2 (view actions.address) model Window.dimensions


-- MODEL


type alias Model = {
  state: State,
  map: Map,
  creeps: List Creep,
  towers: List Tower,
  projectiles: List Projectile,
  lives: Int
}

type State =
  Play |
  Pause

type alias Map = Array (Array Tile)

type alias Creep = {
  position: Position,
  previousPosition: Maybe Position,
  delay: Maybe Int,
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
  creep: Creep
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
    Creep (0, 3) Nothing Nothing Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 1) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 2) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 3) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 4) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 5) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 10) Color.red 20,
    Creep (0, 3) Nothing (Just 15) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 16) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 17) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 18) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 19) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 20) Color.lightBlue 10
  ]

initialModel : Model
initialModel =
  Model Pause defaultMap defaultCreeps [Tower (2, 6) 1 2, Tower (5, 2) 2 1] [] 10

model : Signal Model
model =
  Signal.foldp update initialModel inputs


-- UPDATE


type Action =
  NoOp |
  StateChange State |
  Click Position |
  Tick Time

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    StateChange newState ->
      { model | state <- Debug.watch "state" newState }

    Click position ->
      let
        alreadyExists = List.any (\tower -> tower.position == position) model.towers
        outsideMap = List.any (\x -> x >= Array.length model.map) [fst position, snd position]
      in
        if alreadyExists || outsideMap
        then model
        else { model | towers <- (Tower position 1 2) :: model.towers }

    Tick time ->
      case model.state of
        Play ->
          let
            updatedCreeps = List.filterMap (updateCreep model.map) model.creeps
            escapedCreepCount = List.length model.creeps - List.length updatedCreeps
            projectiles = List.filterMap (projectile updatedCreeps) (Debug.watch "towers" model.towers)
            newLives = Debug.watch "lives" (model.lives - escapedCreepCount)
          in
            if newLives > 0
            then
              { model |
                creeps <- Debug.watch "creeps" (creepsAfterProjectiles projectiles updatedCreeps),
                projectiles <- Debug.watch "projectiles" projectiles,
                lives <- newLives
              }
            else
              { model | lives <- 0 }
        Pause ->
          model

creepsAfterProjectiles : List Projectile -> List Creep -> List Creep
creepsAfterProjectiles projectiles creeps =
  List.filterMap (creepAfterProjectiles projectiles) creeps

creepAfterProjectiles : List Projectile -> Creep -> Maybe Creep
creepAfterProjectiles projectiles creep =
  let
    firingTowers = projectiles
                   |> List.filter (\projectile -> projectile.creep == creep)
                   |> List.map .tower
    totalDamage = firingTowers
                  |> List.map .damage
                  |> List.sum
  in
    if totalDamage > creep.health
    then Nothing
    else Just { creep | health <- creep.health - totalDamage }

projectile : List Creep -> Tower -> Maybe Projectile
projectile creeps tower =
  let
    creepsInRange = List.filter (inRange tower) creeps
  in
    List.head creepsInRange
    |> Maybe.map (Projectile tower)

updateCreep : Map -> Creep -> Maybe Creep
updateCreep map creep =
  case creep.delay of
    Just delay ->
      Just { creep | delay <- if delay == 0 then Nothing else Just (delay - 1) }
    Nothing ->
      case nextPosition map creep of
        Just newPosition ->
          Just { creep |
                 position <- newPosition,
                 previousPosition <- Just creep.position
               }
        Nothing ->
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

inRange : Tower -> Creep -> Bool
inRange tower creep =
  distance tower.position creep.position <= tower.radius

distance : Position -> Position -> Int
distance (x1, y1) (x2, y2) =
  (x1 - x2) ^ 2 + (y1 - y2) ^ 2
  |> toFloat
  |> sqrt
  |> round

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


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

clockTicks : Signal Action
clockTicks =
  Signal.map Tick <| Time.fps 5

mouseClicks : Signal Action
mouseClicks =
  Signal.sampleOn Mouse.clicks Mouse.position
  |> Signal.map inverseTranslate
  |> Signal.map Click

inputs : Signal Action
inputs =
  Signal.mergeMany [actions.signal, clockTicks, mouseClicks]


-- VIEW


view : Signal.Address Action -> Model -> (Int, Int) -> Element.Element
view address model (width, height) =
  let
    mapSize = (round tileSize) * (Array.length model.map)
    graphics = List.filterMap creepView model.creeps
               |> List.append (List.map towerView model.towers)
               |> List.append (List.map projectileView model.projectiles)
               |> List.append [mapView model.map]
               |> Collage.group
               |> Collage.move ((toFloat -mapSize/2) + tileSize, (toFloat mapSize/2) - tileSize)
  in
    [
      Collage.collage mapSize mapSize [graphics],
      Element.flow Element.down [
        Element.container 150 75 Element.midBottom (controlsView address model.state),
        Element.container 150 50 Element.middle (Element.centered (Text.fromString ("♥ " ++ (toString model.lives))))
      ]
    ]
    |> Element.flow Element.right

projectileView : Projectile -> Form
projectileView projectile =
  let
    lineStyle = Collage.defaultLine
  in
    Collage.segment (translate projectile.tower.position)
                    (translate projectile.creep.position)
    |> Collage.traced lineStyle

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

controlsView : Signal.Address Action -> State -> Element.Element
controlsView address state =
  let
    element = case state of
                Play ->
                  Input.button (Signal.message address (StateChange Pause)) "Pause"
                Pause ->
                  Input.button (Signal.message address (StateChange Play)) "Play"
  in
    element

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
  (toFloat x * tileSize, toFloat -y * tileSize)

inverseTranslate : (Int, Int) -> Position
inverseTranslate (x, y) =
  (
    truncate ((toFloat x - tileSize / 2) / tileSize),
    truncate ((toFloat y - tileSize / 2) / tileSize)
  )

tileSize : Float
tileSize = 60
