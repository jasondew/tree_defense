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
import Touch

main : Signal Element.Element
main =
  Signal.map2 (view actions.address) model Window.dimensions


-- MODEL


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
  creep: Creep,
  position: Float
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
    Creep (0, 3) Nothing Nothing   Color.lightBlue 10,
    Creep (0, 3) Nothing (Just  1) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just  2) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just  3) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 15) Color.red 20,
    Creep (0, 3) Nothing (Just 20) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 21) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 22) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 23) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 24) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 25) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 35) Color.red 20,
    Creep (0, 3) Nothing (Just 45) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 46) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 47) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 48) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 49) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 50) Color.lightBlue 10,
    Creep (0, 3) Nothing (Just 60) Color.darkRed 30,
    Creep (0, 3) Nothing (Just 70) Color.darkPurple 50
  ]

initialModel : Model
initialModel =
  Model Pause Nothing defaultMap defaultCreeps [] [] 10 200

model : Signal Model
model =
  Signal.foldp update initialModel inputs


-- UPDATE


type Action =
  NoOp |
  StateChange State |
  Click Position |
  Reset |
  CreepTick Time |
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
        towerCost = 50
        cantAfford = towerCost > model.money
        alreadyExists = List.any (\tower -> tower.position == position) model.towers
        onGrass = tileAt model.map position == Just Grass
      in
        if cantAfford || alreadyExists || not onGrass
        then model
        else { model |
               towers <- (Tower position 1 2) :: model.towers,
               money <- model.money - towerCost
             }

    Reset ->
      initialModel

    CreepTick time ->
      case model.state of
        Play ->
          let
            movedCreeps = List.filterMap (updateCreep model.map) model.creeps
            projectiles = List.filterMap (projectile movedCreeps) (Debug.watch "towers" model.towers)
            updatedCreeps = creepsAfterProjectiles projectiles movedCreeps
            escapedCreepCount = List.length model.creeps - List.length movedCreeps
            killedCreepCount = List.length movedCreeps - List.length updatedCreeps
            newLives = Debug.watch "lives" (model.lives - escapedCreepCount)
            remainingCreeps = List.length model.creeps
          in
            if newLives > 0
            then
              if remainingCreeps > 0
              then
                { model |
                  creeps <- Debug.watch "creeps" updatedCreeps,
                  projectiles <- Debug.watch "projectiles" projectiles,
                  lives <- newLives,
                  money <- model.money + killedCreepCount * 10
                }
              else
                { model | projectiles <- [], outcome <- Just Won }
            else
              { model | lives <- 0, outcome <- Just Lost }
        Pause ->
          model

    Tick time ->
      case model.state of
        Play ->
          let
            updatedProjectiles = Debug.watch "projectiles" <| List.map updateProjectile model.projectiles
          in
            { model | projectiles <- updatedProjectiles }
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
    |> Maybe.map (\creep -> Projectile tower creep 0)

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

updateProjectile : Projectile -> Projectile
updateProjectile projectile =
  { projectile | position <- projectile.position + 1 }

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
  Signal.map Tick <| Time.fps framesPerSecond

creepTicks : Signal Action
creepTicks =
  Signal.map CreepTick <| Time.fps creepVelocity

mouseClicks : Signal Action
mouseClicks =
  Signal.sampleOn Mouse.clicks Mouse.position
  |> Signal.map inverseTranslate
  |> Signal.map Click

touches : Signal Action
touches =
  Signal.map (\touch -> Click (touch.x, touch.y)) Touch.taps

inputs : Signal Action
inputs =
  Signal.mergeMany [actions.signal, creepTicks, clockTicks, mouseClicks, touches]


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
    Element.flow Element.right [
      Element.layers [
        Collage.collage mapSize mapSize [graphics],
        Element.container mapSize mapSize Element.middle (wonOrLostView model.outcome)
      ],
      Element.flow Element.down [
        Element.container 150 110 Element.midBottom <| Element.flow Element.down (controlsView address model.state),
        Element.container 150 50 Element.middle (Element.centered (Text.fromString ("♥ " ++ (toString model.lives)))),
        Element.container 150 50 Element.middle (Element.centered (Text.fromString ("$ " ++ (toString model.money))))
      ]
    ]

projectileView : Projectile -> Form
projectileView projectile =
  let
    percentage : Float
    percentage = projectile.position / (60 / 5)
    (from_x, from_y) = projectile.tower.position
    (to_x, to_y) = projectile.creep.position
    current = (ease from_x to_x percentage, ease from_y to_y percentage)
  in
    Element.image (round (tileSize / 4)) (round (tileSize / 4)) "assets/projectile.png"
    |> Collage.toForm
    |> Collage.move (translateFloat current)

towerView : Tower -> Form
towerView tower =
  let
    image = Element.image tileSize tileSize "assets/tree-elm-grown.png"
            |> Collage.toForm
            |> Collage.move (translate tower.position)
    lineStyle = Collage.dotted Color.charcoal
    halo = Collage.circle (tileSize * (toFloat tower.radius + 0.5))
           |> Collage.outlined { lineStyle | width <- 1.5 }
           |> Collage.move (translate tower.position)
  in
    Collage.group [image, halo]

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

controlsView : Signal.Address Action -> State -> List Element.Element
controlsView address state =
  let
    play_or_pause = case state of
                      Play ->
                        Input.button (Signal.message address (StateChange Pause)) "Pause"
                      Pause ->
                        Input.button (Signal.message address (StateChange Play)) "Play"
    reset = Input.button (Signal.message address Reset) "Play Again"
  in
    [play_or_pause, reset]

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
    imageName = case tile of
      Grass -> "assets/grass.png"
      Road  -> "assets/road.png"
  in
    Element.image tileSize tileSize imageName
    |> Collage.toForm
    |> Collage.move (translate position)

wonOrLostView : Maybe Outcome -> Element.Element
wonOrLostView maybeOutcome =
  let
    message outcome = case outcome of
                        Won  -> "YOU WON!"
                        Lost -> "GAME OVER"
    color outcome = case outcome of
                      Won  -> Color.lightGreen
                      Lost -> Color.lightRed
  in
    case maybeOutcome of
      Just outcome ->
        Text.fromString (message outcome)
        |> Text.bold
        |> Text.height (tileSize * 1.5)
        |> Text.color (color outcome)
        |> Element.centered
      Nothing ->
        Element.empty

ease : Int -> Int -> Float -> Float
ease fromInt toInt percentage =
  let
    from = toFloat fromInt
    to   = toFloat toInt
  in
    from + (to - from) * percentage

translate : Position -> (Float, Float)
translate (x, y) =
  (toFloat x * tileSize, toFloat -y * tileSize)

translateFloat : (Float, Float) -> (Float, Float)
translateFloat (x, y) =
  (x * tileSize, -y * tileSize)

inverseTranslate : (Int, Int) -> Position
inverseTranslate (x, y) =
  (
    truncate ((toFloat x - tileSize / 2) / tileSize),
    truncate ((toFloat y - tileSize / 2) / tileSize)
  )


-- CONSTANTS


tileSize : number
tileSize = 75

framesPerSecond : number
framesPerSecond = 60

creepVelocity : number
creepVelocity = 5
