{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Snake

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))


-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Icon = Snake | Food | Black


-- App definition

app :: App Game Tick Name
app = App { appDraw = ui
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 1
    -- was 10, but I think 1 is enough, because the writes (explicit below)
    -- and reads (implicit in event loop) happen at the same rate, 1 per loop
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- determines how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = liftIO (step g) >>= continue
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g


-- Drawing

ui :: Game -> [Widget Name]
ui g =
  [ C.center $ padRight (Pad 2) (statsWidget g) <+> gridWidget g ]
  -- that's a +, not a *, in the < >

statsWidget :: Game -> Widget Name
statsWidget g = hLimit 15
  $ vBox [ scoreWidget (g ^. score)
         , padTop (Pad 2) $ gameOverWidget (g ^. dead)
         ]

scoreWidget :: Int -> Widget Name
scoreWidget n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

gameOverWidget :: Bool -> Widget Name
gameOverWidget dead =
  if dead
     then withAttr gameOverAttr $ vBox
          $ map (C.hCenter . str)
          $ ["NOW YOU ARE","IN HEAVEN"]
     else emptyWidget

gridWidget :: Game -> Widget Name
gridWidget g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r
                   | r <- [height, height-1 .. 1] ]
    cellsInRow y = [iconWidget . iconAt $ (V2 x y)
                   | x <- [1..width]]
    iconAt c
      | c `elem` g ^. snake = Snake
      | c `elem` g ^. food  = Food
      | otherwise           = Black

iconWidget :: Icon -> Widget Name
iconWidget Snake = withAttr snakeAttr cw
iconWidget Food  = withAttr foodAttr cw
iconWidget Black = withAttr blackAttr cw

cw :: Widget Name -- "current widget"?
cw = str "  " -- two spaces looks like a square
  -- make it one space, and the whole board becomes skinny

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.black `on` V.blue)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.green`V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, blackAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
blackAttr = "blackAttr"
