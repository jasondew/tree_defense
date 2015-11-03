module TreeDefense where

import Models exposing (..)
import Levels exposing (levelOne)
import Maps exposing (defaultMap)

import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage exposing (Form, Shape)
import Graphics.Input as Input
import Color exposing (Color)
import Time exposing (Time)
import Array exposing (Array)
import Text
import Debug
import Mouse
import Touch

main : Signal Element
main =
  Signal.map (view actions.address) model


-- MODEL


initialModel : Model
initialModel =
  Model Pause Nothing defaultMap levelOne [] [] 10 150

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
  if creep.delay == 0 then
    case nextPosition map creep of
      Just newPosition ->
        Just { creep |
               position <- Just newPosition,
               previousPosition <- creep.position
             }
      Nothing ->
        Nothing
  else
    Just { creep | delay <- creep.delay - 1 }

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
  case creep.position of
    Just position ->
      distance tower.position position <= tower.radius
    Nothing ->
      False

distance : Position -> Position -> Int
distance (x1, y1) (x2, y2) =
  (x1 - x2) ^ 2 + (y1 - y2) ^ 2
  |> toFloat
  |> sqrt
  |> round

initialPosition : Map -> Position
initialPosition map =
  let
    column = Array.map (\row -> Maybe.withDefault Grass <| Array.get 0 row) map
    row = Array.toIndexedList column
          |> List.filter (\(index, tile) -> tile == Road)
          |> List.head
          |> Maybe.withDefault (0, Road)
          |> fst
  in
    (0, row)

nextPosition : Map -> Creep -> Maybe Position
nextPosition map creep =
  case creep.position of
    Just (x, y) ->
      let
        up     = ( x,  y+1)
        down   = ( x,  y-1)
        right  = (x+1,  y )
      in
        List.map (\position -> (position, tileAt map position)) [up, down, right]
        |> List.filter (traversable creep.previousPosition)
        |> List.map fst
        |> List.head                    -- TODO: pick one at random
    Nothing ->
      Just (initialPosition map)

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


view : Signal.Address Action -> Model -> Element
view address model =
  Element.flow Element.right [
    Element.layers [
      Element.container mapSize mapSize Element.topLeft (gameView model)
    , Element.container mapSize mapSize Element.middle (wonOrLostView model.outcome)
    ]
  , Element.spacer 10 mapSize
  , Element.flow Element.down [
      Element.container panelWidth              50 Element.middle  titleView
    , Element.container panelWidth              50 Element.middle  instructionsView
    , Element.container panelWidth             100 Element.middle  (statusView model)
    , Element.container panelWidth (mapSize - 250) Element.topLeft legendView
    , Element.container panelWidth              50 Element.middle  (controlsView address model.state)
    ]
  ]

gameView : Model -> Element
gameView model =
  List.filterMap creepView model.creeps
    |> List.append (List.map towerView model.towers)
    |> List.append (List.map projectileView model.projectiles)
    |> List.append [mapView model.map]
    |> Collage.collage mapSize (Debug.watch "mapSize" mapSize)

projectileView : Projectile -> Form
projectileView projectile =
  let
    percentage : Float
    percentage = projectile.position / (60 / 5)
    (from_x, from_y) = projectile.tower.position
    (to_x, to_y) = Maybe.withDefault (0, 0) projectile.creep.position
    current = (ease from_x to_x percentage, ease from_y to_y percentage)
  in
    Element.image (round (tileSize / 4)) (round (tileSize / 4)) "assets/projectile.png"
    |> Collage.toForm
    |> Collage.move (translateFloat current)

towerView : Tower -> Form
towerView tower =
  let
    image = Element.image (round tileSize) (round tileSize) "assets/tree-elm-grown.png"
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
  if creep.delay == 0 then
    case creep.position of
      Just position ->
        Element.image (round tileSize) (round tileSize) "assets/ant.png"
        |> Collage.toForm
        |> Collage.move (translate position)
        |> Collage.rotate (creepRotation creep)
        |> Just
      Nothing ->
        Nothing
  else
    Nothing

creepRotation : Creep -> Float
creepRotation creep =
  let
    defaultRotation = degrees 0
  in
    case creep.previousPosition of
      Just previousPosition ->
        case creep.position of
          Just position ->
            let
              (lastX, lastY) = previousPosition
              (x, y) = position
              delta = (x - lastX, y - lastY)
            in
              case delta of
                ( 0,  1) -> degrees 270
                ( 0, -1) -> degrees 90
                ( 1,  0) -> degrees 0
          Nothing -> defaultRotation
      Nothing -> defaultRotation

titleView : Element
titleView =
  Text.fromString "Tree Defense"
  |> Text.bold
  |> Text.height 30
  |> Element.centered

instructionsView : Element
instructionsView =
  let
    format string = Text.fromString string
                    |> Text.height 12
                    |> Element.centered
                    |> Element.container panelWidth 15 Element.middle
  in
    Element.flow Element.down [
      format "Click to plant trees that defend"
    , format "against the invading ant horde!"
    ]

controlsView : Signal.Address Action -> State -> Element
controlsView address state =
  let
    play_or_pause = case state of
                      Play ->
                        Input.button (Signal.message address (StateChange Pause)) "Pause"
                      Pause ->
                        Input.button (Signal.message address (StateChange Play)) "Play"
    reset = Input.button (Signal.message address Reset) "Play Again"
  in
    play_or_pause `Element.beside` reset

statusView : Model -> Element
statusView model =
  Text.fromString ("♥" ++ (toString model.lives) ++ "  $" ++ (toString model.money))
  |> Text.height 25
  |> Element.centered

legendView : Element
legendView =
  let
    header string = Text.fromString string
                    |> Text.bold
                    |> Element.centered
    rule = [Collage.filled Color.black (Collage.rect panelWidth 1)]
           |> Collage.collage panelWidth 4
           |> Element.container panelWidth 4 Element.topLeft
  in
    Element.flow Element.down [
      header "Trees"
    , rule
    , legendItemView "tree-elm-grown" "Elm tree" "$50"
    , Element.spacer panelWidth 40
    , header "Enemies"
    , rule
    , legendItemView "ant" "Ant" "♥10"
    , legendItemView "ant" "Fire Ant" "♥20"
    , legendItemView "ant" "Cow Ant" "♥30"
    ]

legendItemView : String -> String -> String -> Element
legendItemView imageName title cost =
  let
    size = tileSize / 3
           |> round
    image = Element.image size size ("assets/" ++ imageName ++ ".png")
    text string = Text.fromString string
                  |> Text.height (tileSize / 3)
                  |> Element.centered
    titleElement = text title
    costElement = text cost
    space = panelWidth -
            Element.widthOf image -
            Element.widthOf titleElement -
            Element.widthOf costElement
  in
    Element.flow Element.right [
      image
    , titleElement
    , Element.spacer space 10
    , costElement
    ]

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
    Element.image (round tileSize) (round tileSize) imageName
    |> Collage.toForm
    |> Collage.move (translate position)

wonOrLostView : Maybe Outcome -> Element
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
  translateFloat (toFloat x, toFloat y)

translateFloat : (Float, Float) -> (Float, Float)
translateFloat (x, y) =
  (
     x * tileSize - mapSize / 2.0 + tileSize / 2.0
  , -y * tileSize + mapSize / 2.0 - tileSize / 2.0
  )

inverseTranslate : (Int, Int) -> Position
inverseTranslate (x, y) =
  (
    truncate (toFloat x / tileSize)
  , truncate (toFloat y / tileSize)
  )


-- CONSTANTS


mapSize : number
mapSize = 600

panelWidth : number
panelWidth = 200

gridSize : number
gridSize = 10

tileSize : number
tileSize = mapSize / gridSize

framesPerSecond : number
framesPerSecond = 60

creepVelocity : number
creepVelocity = 5
