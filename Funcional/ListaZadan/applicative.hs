module Applicative where

-- Kontrawariantny
data COV a = COV ( Int -> a ) deriving ( Functor )

-- This is ... weirrddd