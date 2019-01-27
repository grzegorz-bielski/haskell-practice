{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

val0 = (* 9) 6
val1 = head [(0,"doge"),(1,"kitteh")]
val2 = head [(0 :: Integer ,"doge"),(1,"kitteh")]
val3 = if False then True else False
val4 = length [1, 2, 3, 4, 5]
val5 = (length [1, 2, 3, 4]) > (length "TACOCAT")

x=5 
y=x+5 
w = y * 10
f=4/y