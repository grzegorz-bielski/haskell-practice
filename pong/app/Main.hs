module Main where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Game state
data Game = Game
  { ballLoc :: (Float, Float)   -- ^ Ball location vector
  , ballVel :: (Float, Float)   -- ^ Ball velocity vector
  , player1 :: Float            -- ^ player 1 height
  , player2 :: Float            -- ^ player 2 height
  } deriving (Show)

intialState :: Game
intialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (1, -3)
  , player1 = 40
  , player2 = -80
  }

render :: Game -> Picture
render game = pictures [ ball, walls, 
                     mkPaddle rose 120 $ player1 game,
                     mkPaddle orange (-129) $ player2 game]
 where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    wall :: Float -> Picture
    wall offset =
        translate 0 offset $
          color wallColor $
            rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
     [ translate x y $ color col $ rectangleSolid 26 86
     , translate x y $ color paddleColor $ rectangleSolid 20 80
     ]

    paddleColor = light (light blue)

-- todo; anim
main :: IO ()
main = display window background render