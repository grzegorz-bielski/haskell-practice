module Main where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100

paddleWidth, paddleHeight, paddleBorderOffset, paddlePos, radius :: Float
paddleWidth = 20
paddleHeight = 80
paddleBorderOffset = 6
paddlePos = 120
radius = 10

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

initialState :: Game
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (30, 100)
  , player1 = 0
  , player2 = 0
  }


render :: Game -> Picture
render game = pictures [ ball, walls, 
                     mkPaddle rose paddlePos $ player1 game,
                     mkPaddle orange (-paddlePos) $ player2 game]
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
     [ translate x y $ color col $ rectangleSolid (paddleWidth + paddleBorderOffset) (paddleHeight + paddleBorderOffset)
     , translate x y $ color paddleColor $ rectangleSolid paddleWidth paddleHeight
     ]

    paddleColor = light (light blue)

moveBall :: Float -> Game -> Game
moveBall seconds game = game { ballLoc = (x', y') }
      where
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        x' = x + vx * seconds
        y' = y + vy * seconds

fps :: Int
fps = 60

type Radius = Float
type Position = (Float, Float)

verticalCollision :: Position -> Radius -> Bool
verticalCollision (x, y) radius = topCollision || bottomCollision
  where
    halfHeight = fromIntegral height / 2
    topCollision = y - radius <= -halfHeight
    bottomCollision = y + radius >= halfHeight


horizontalCollision :: Position -> Radius -> Bool
horizontalCollision (x, y) radius =  leftCollision || rightCollision
  where
    halfWidth = fromIntegral width / 2
    leftCollision = x - radius <= -halfWidth
    rightCollision = x + radius >= halfWidth

wallBounce :: Game -> Game
wallBounce game = game { ballVel = (vx', vy') }
  where
    (vx, vy) = ballVel game

    vy' = if verticalCollision (ballLoc game) radius then -vy else vy
    vx' =  if horizontalCollision (ballLoc game) radius then -vx else vx

      

paddleCollisionHorizontal :: Game -> Float -> Bool
paddleCollisionHorizontal game radius = player1Collision || player2Collision
  where 
    getMinMax :: Float -> (Float, Float)
    getMinMax playerPos = (minPos, maxPos)
        where heightWithOffset = paddleHeight + paddleBorderOffset / 2
              minPos = playerPos -  heightWithOffset / 2
              maxPos = minPos + heightWithOffset

    touchedPaddleVertically :: Float -> (Float, Float) -> Bool
    touchedPaddleVertically coord (posMin, posMax) = coord + radius >= posMin && coord - radius <= posMax

    halfWidth = paddleWidth / 2
    (x, y) = ballLoc game

    crossedX1 = x - radius <= (-paddlePos) + halfWidth
    crossedX2 = x - radius >= paddlePos - paddleWidth

    player1Collision = crossedX1 && (touchedPaddleVertically y (getMinMax $ player1 game))
    player2Collision = crossedX2 && (touchedPaddleVertically y (getMinMax $ player2 game))

paddleBounce :: Game -> Game
paddleBounce game = game { ballVel = (vx', vy') }
  where
    (vx, vy) = ballVel game
    vy' = vy -- not implemented

    vx' = if paddleCollisionHorizontal game radius then -vx else vx

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'r') _ _ _) game = game { ballLoc = (0,0) }
handleKeys _ game = game

update ::  Float -> Game -> Game
update seconds = wallBounce . paddleBounce . moveBall seconds

main :: IO ()
main = play window background fps initialState render handleKeys update