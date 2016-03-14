{-# LANGUAGE TupleSections #-}
module Main where

import           Data.List          (find, group)
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust)
import           Data.Tuple         (swap)
import           System.Environment (getArgs)
import           System.Exit        (die)

------------------------------------------------------------------------------
-- Cave layout
------------------------------------------------------------------------------

type Coord = (Int, Int)
data Element = Empty | Wall | Water
             deriving Eq

data Cave = Cave { caveContents :: Map Coord Element }

elementAt :: Cave -> Coord -> Element
elementAt cave coord = M.findWithDefault Empty coord (caveContents cave)

caveBottomRight :: Cave -> (Int, Int)
caveBottomRight cave = (xmax, ymax)
  where
    (xmax, ymax) = swap . maximum $ swap <$> coords
    coords = M.keys (caveContents cave)

caveRows :: Cave -> [[Element]]
caveRows cave = map row [0..ymax]
  where
    row y = map (elementAt cave . (,y)) [0..xmax]
    (xmax, ymax) = caveBottomRight cave

caveColumns :: Cave -> [[Element]]
caveColumns cave = map col [0..xmax]
  where
    col x = map (elementAt cave . (x,)) [0..ymax]
    (xmax, ymax) = caveBottomRight cave

waterPositions :: Cave -> [Coord]
waterPositions cave = [ pos | (pos, el) <- M.toList (caveContents cave), el == Water ]

below :: Coord -> Coord
below (x, y) = (x, y + 1)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

data Depth = Flowing | Depth Int

caveDepths :: Cave -> [Depth]
caveDepths = map summarise . caveColumns
  where
    summarise cells = case waterAndBelow of
      [] -> Depth 0
      w:rest -> case find (are Empty) rest of
        Just _  -> Flowing
        Nothing -> Depth (length w)
      where
        waterAndBelow = dropWhile (not . are Water) $ group cells
        are el = (el ==) . head

------------------------------------------------------------------------------
-- Read and write caves
------------------------------------------------------------------------------

instance Show Element where
  show Empty = " "
  show Wall  = "#"
  show Water = "~"

elementFromChar :: Char -> Maybe Element
elementFromChar c = case c of
  ' ' -> Just Empty
  '#' -> Just Wall
  '~' -> Just Water
  _ -> Nothing

instance Show Cave where
  show cave = unlines (map (concatMap show) (caveRows cave))

readCave :: [String] -> Either String Cave
readCave []   = Left "no cave data"
readCave rows = Right $ Cave contents
  where
    contents = M.fromList positions
    positions = concatMap f (zip rows [0..])
      where f (line, y) = map g (zip line [0..])
              where g (ch, x) = ((x, y), fromJust (elementFromChar ch))

instance Show Depth where
  show Flowing = "~"
  show (Depth n)  = show n

------------------------------------------------------------------------------
-- Flowing water
------------------------------------------------------------------------------

flow :: Cave -> Coord -> Coord -> Cave
flow cave from to = Cave $ M.insert to Water $ M.insert from Empty $ caveContents cave

flowedPosition :: Cave -> Coord -> Maybe Coord
flowedPosition cave coord =
  case elementAt cave (below coord) of
    Empty -> Just (below coord)
    _     -> case elementAt cave (right coord) of
      Empty -> Just (right coord)
      _     -> Nothing

replenish :: Coord -> (Cave, [Coord]) -> (Cave, [Coord])
replenish inlet (cave, flowing) =
  (Cave $ M.insert inlet Water $ caveContents cave, flowing ++ [inlet])

flow1 :: (Cave, [Coord]) -> (Cave, [Coord])
flow1  (cave, flowing) = foldl f (cave, []) flowing
  where
    f (c, nxt) pos = case flowedPosition c pos of
      Just newpos -> (flow c pos newpos, nxt ++ [newpos])
      Nothing -> (c, nxt)

flowN :: Int -> Cave -> Cave
flowN units cave = fst. head . drop units $ iterate (replenish inlet . flow1) (cave, [inlet])
  where
    inlet = head $ waterPositions cave

------------------------------------------------------------------------------
-- Problems
------------------------------------------------------------------------------

data Problem = Problem { problemWaterUnits :: Int
                       , problemCave       :: Cave }
             deriving Show

readProblem :: String -> Either String Problem
readProblem input =
  case lines input of
    (num:("":rest)) ->
       return . Problem (read num) =<< readCave rest
    _ -> Left "malformed problem"

solve :: Problem -> Cave
solve p = flowN (problemWaterUnits p - 1) (problemCave p)

simpleCave = do
  Right prob <- readProblem <$> readFile "examples/simple_cave.txt"
  return $ problemCave prob

solveFile :: FilePath -> IO ()
solveFile file = do
  problem <- readProblem <$> readFile file
  case problem of
    Left err -> die err
    Right prob ->
      let solved = solve prob in do
        print solved
        putStrLn $ unwords (show <$> caveDepths solved)

main :: IO ()
main = do
  (file:_) <- getArgs
  solveFile file
