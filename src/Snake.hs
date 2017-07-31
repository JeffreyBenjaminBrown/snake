{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (randomRIO)

-- Types

data Game = Game
  { _snake  :: Snake     -- ^ snake as a sequence of points in R2
  , _dir    :: Direction -- ^ direction
  , _food   :: [Coord]     -- ^ location of the food
  , _dead   :: Bool      -- ^ game over flag
  , _paused :: Bool      -- ^ paused flag
  , _score  :: Int       -- ^ score
  , _frozen :: Bool      -- ^ freeze to disallow duplicate turns between time steps
  } deriving (Eq, Show)

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 13
width  = 13

-- Functions

-- | Step forward in time
step :: Game -> IO Game
step g = fromMaybe (return g) $ do
  guard $ not $ g ^. paused || g ^. dead
  let g' = g & frozen .~ False
  (pure <$> die g') <|> eatFood g' <|> (pure <$> move g')

-- | Possibly die if next head position is disallowed
die :: Game -> Maybe Game
die g = do
  guard $ bodyHit || borderHit
  return $ g & dead .~ True
  where bodyHit   = nh g `elem` g ^. snake
        borderHit = outOfBounds (nh g)

-- | Possibly eat food if next head position is food
eatFood :: Game -> Maybe (IO Game)
  -- Why did I change this from Maybe Game to Maybe (IO Game)?
  -- It happened at commit a6b54962a8aaadcd5882193404eaeb3fa6951b1b
  -- where I changed it from one food to two.
eatFood g = do
  guard $ elem (nh g) (g ^. food)
  return $ do
    -- Abbreviated. Before, ng was defined with food and score updates,
    -- then the next food was pulled, and then the return value created.
    -- This seems to do the same thing.
    nf <- nextFood g
    return $ g & food .~ nf
               & score %~ (+10)
               & snake %~ (nh g <|)

-- | Move snake along in a marquee fashion
move :: Game -> Maybe Game
move g = Just $ g & snake %~ (mv . S.viewr)
  where
    mv (EmptyR) = error "Snakes can't be empty!"
    mv (s :> _) = nh g <| s

-- | Get next head location of the game's snake
nh :: Game -> Coord
nh g = nextHead (g ^. dir) (g ^. snake)

-- | Get next head position of a snake in a particular direction
nextHead :: Direction -> Snake -> Coord
nextHead d = go . S.viewl
  where
    go (EmptyL) = error "Snakes can't be empty!"
    go (a :< _)
      | d == North = a & _y %~ (+1)
      | d == South = a & _y %~ (subtract 1)
      | d == East  = a & _x %~ (+1)
      | d == West  = a & _x %~ (subtract 1)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game
turn d g =
  if g ^. frozen
     then g
     else g & dir %~ turnDir d
            & paused .~ False
            & frozen .~ True

-- no turning backwards
turnDir :: Direction -> Direction -> Direction
turnDir n c
  | c `elem` [North, South] && n `elem` [East, West] = n
  | c `elem` [East, West] && n `elem` [North, South] = n
  | otherwise                                        = c

outOfBounds :: Coord -> Bool
outOfBounds c = any (< 1) c || c ^. _x > width || c ^. _y > height

-- | Get a valid next food coordinate
nextFood :: Game -> IO [Coord]
  -- In the original, this is Game -> Game, through the magic of Sequence.
nextFood g = do
  c1 <- randomCoord
  c2 <- randomCoord
  let s = g ^. snake
  if (c1 `elem` s  &&  c2 `elem` s)
     then nextFood g
     else return [c1, c2]

randomCoord :: IO Coord
randomCoord = V2 <$> randomRIO (1, width)
                 <*> randomRIO (1, height)

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  let g = Game { _snake = S.fromList [V2 0 y | y <- [1..10]]
               , _dir = North
               , _food = [V2 0 0, V2 5 5]
               , _score = 0
               , _dead = False
               , _paused = True
               , _frozen = False }
  nf <- nextFood g
  return $ g & food .~ nf
