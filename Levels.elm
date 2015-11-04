module Levels where

import Models exposing (..)
import Creeps exposing (..)

levelOne : List Creep
levelOne =
  level [
    ( 2, Just ant)
  , (10, Nothing)
  , ( 4, Just ant)
  , (10, Nothing)
  , ( 8, Just ant)
  , (10, Nothing)
  , (10, Just ant)
  , (10, Nothing)
  , ( 1, Just fireAnt)
  , (10, Nothing)
  , ( 2, Just fireAnt)
  , ( 5, Nothing)
  , ( 3, Just fireAnt)
  , ( 5, Nothing)
  , ( 1, Just cowAnt)
  , ( 5, Nothing)
  , ( 2, Just cowAnt)
  , ( 5, Nothing)
  , ( 3, Just cowAnt)
  ]

level : List (Int, Maybe (Int -> Creep)) -> List Creep
level list =
  list
  |> List.map (\(repetitions, creep) -> List.repeat repetitions creep)
  |> List.concat
  |> List.indexedMap (\delay maybeCreep -> Maybe.map ((|>) delay) maybeCreep)
  |> List.filterMap identity
