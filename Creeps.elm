module Creeps where

import Models exposing (..)

import Color exposing (Color)

ant : Delay -> Creep
ant delay =
  Creep Nothing Nothing delay Color.black 10

fireAnt : Delay -> Creep
fireAnt delay =
  Creep Nothing Nothing delay Color.red 20

cowAnt : Delay -> Creep
cowAnt delay =
  Creep Nothing Nothing delay Color.darkRed 30
