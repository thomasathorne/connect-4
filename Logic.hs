{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module Main where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Control.Applicative
import Data.Array
import Data.List
import Data.Char

size, aiDepth :: Int
size = 8
aiDepth = 4

--------------

type A a = Array Int a

data Player = Red | Blue 
 deriving (Eq, Show)

switch Red = Blue
switch Blue = Red

data Game = Game { _board :: A (A (Maybe Player))
                 , _turn :: Player
                 } deriving (Show, Eq)

makeLenses ''Game

infixl 8 ^??
(^??) dat trav = join $ dat ^? trav

emptyColumn = array (1,size) $ zip [1..size] $ repeat Nothing
emptyBoard = array (1,size) $ zip [1..size] $ repeat emptyColumn
gameStart = Game emptyBoard Blue

newtype Count f a = Count { unCount :: State Int (f a) }
instance (Functor f) => Functor (Count f) where
 fmap f c = Count $ (fmap.fmap) f $ unCount c 
instance (Applicative f) => Applicative (Count f) where
 pure x = Count $ return $ pure x
 f <*> x = Count $ do f' <- unCount f
                      x' <- unCount x
                      return $ f' <*> x'

column, row, fDiag, bDiag :: Int -> Traversal' Game (Maybe Player)
column n = board . ix n . traversed
row n = board . traversed . ix n 
fDiag n = diagonalise n (board.traversed)
bDiag n = diagonalise n (backwards $ board.traversed)

diagonalise :: Int -> Traversal' b (A a) -> Traversal' b a
diagonalise n trav app dat = evalState (unCount $ trav step dat) n
 where step arr = Count $ do 
        m <- get
        put (m+1)
        return (ix m app arr)

-----------------------------

pDisplay :: Game -> IO ()
pDisplay g = do
  forM_ [size,size-1..1] $ \n -> forM (g^..row n) displayBlock >> putStr "\n"

display :: StateT Game IO ()
display = get >>= io.pDisplay

displayBlock :: Maybe Player -> IO ()
displayBlock Nothing = putStr " -" 
displayBlock (Just Red) = putStr " X"
displayBlock (Just Blue) = putStr " O"

------------------

data Finally a = Currently Int | Finally Int deriving (Show, Eq)
instance Functor Finally where
 fmap f (Currently x) = Currently x
 fmap f (Finally x) = Finally x
instance Applicative Finally where
 pure a = Currently 0
 (Currently x) <*> (Currently y) = Currently (x + y)
 (Finally x) <*> _ = Finally x
 (Currently x) <*> (Finally y) = Finally (x + y)

play :: (Monad m) => Int -> StateT Game m Int
play n = do
 spot <- get >>= return . column n (\mb -> if mb == Nothing then Finally 1 else Currently 1)
 case spot of
  Currently _ -> return 0
  Finally m -> do p <- use turn
                  board.ix n.ix m .= Just p 
                  turn %= switch
                  return m

pPlay :: Int -> Game -> (Int, Game)
pPlay n = runState (play n)

---------------------------------------

pCheckWinner :: Int -> Int -> Game -> Maybe Player
pCheckWinner n m g = fromEither $ do
 checkOn $ bDiag (m + n - size) 
 checkOn $ fDiag (m + 1 - n)
 checkOn $ column n 
 checkOn $ row m
   where checkOn trav = case g^..trav of
                         ls | replicate 4 (Just Red) `isInfixOf` ls -> Left Red
                            | replicate 4 (Just Blue) `isInfixOf` ls -> Left Blue
                            | otherwise -> Right ()

checkWinner :: (Monad m) => Int -> Int -> StateT Game m (Maybe Player)
checkWinner n m = gets $ pCheckWinner n m

fromEither :: Either a () -> Maybe a
fromEither (Left x) = Just x
fromEither (Right ()) = Nothing

----------------------------------------

main :: IO ()
main = evalStateT runGame gameStart

runGame :: StateT Game IO ()
runGame = do
 g <- get
 display
 p <- getPlay
 q <- play p
 mw <- checkWinner p q 
 case (q,mw) of
  (0,_) -> io (putStrLn "You can't play there.") >> runGame
  (_,Nothing) -> runGame
  (_,Just Red) -> display >> io (putStrLn "I win!! HA HA HA!")
  (_,Just Blue) -> display >> io (putStrLn "Oh, I guess you win this one.")

getPlay :: StateT Game IO Int
getPlay = do 
 pl <- use turn
 case pl of
  Blue -> do
    io $ putStrLn "It is your turn."
    s <- io getLine
    case s of
     "c" -> aiPlay
     _ | all isDigit s -> return $ read s
       | otherwise -> io (putStrLn "Not a valid play.") >> getPlay
  Red -> do 
    io $ putStrLn "It is my turn."
    aiPlay

io = liftIO

-------------------------------

data Status = Win [Int] | Unknown [Int] | Loss
 deriving (Show, Eq)

opp :: Status -> Status
opp (Win _) = Loss
opp (Unknown _) = Unknown []
opp Loss = Win []

flatten :: [(Int,Status)] -> Status
flatten [] = Loss
flatten ((n,Loss):rest) = flatten rest
flatten ((n,Unknown _):rest) = case flatten rest of
  Win ls -> Win ls
  Unknown ls -> Unknown (n:ls)
  Loss -> Unknown [n]
flatten ((n,Win _):rest) = case flatten rest of
  Win ls -> Win (n:ls)
  _ -> Win [n]

aiPlay :: StateT Game IO Int
aiPlay = do
 stat <- gets (analyse aiDepth)
 io $ putStrLn $ show stat
 case stat of
  Win ls -> return $ head ls
  Unknown ls -> return $ head ls
  Loss -> return 11

analyse :: Int -> Game -> Status
analyse 1 g = flatten $ do
 n <- [1..size]
 case pPlay n g of
  (0,_) -> []
  (m,g') -> case pCheckWinner n m g' of
             Just _ -> [(n,Win [])]
             Nothing -> [(n,Unknown [])]
analyse r g = flatten $ do
 n <- [1..size]
 case pPlay n g of
  (0,_) -> []
  (m,g') -> case pCheckWinner n m g' of
             Just _ -> [(n,Win [])]
             Nothing -> [(n,opp $ analyse (r-1) g')]

