#!/usr/bin/env cabal
----
-- Beware. This is very scrappy code.
----
{- cabal:
build-depends: base, random, mtl
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Function ((&))
import Numeric
import System.Environment
import System.Random
import Text.Read (readMaybe)

sizefactor :: Int -> Float
sizefactor depth = 1 / fromIntegral (depth + 2)

len :: Int -> Float
len depth = 72 * 1.2 * sizefactor depth

tspread :: Float
tspread = 20 * pi / 180 :: Float

tbranches :: [Int]
tbranches = [-2 .. 1]

tjitter :: StdGen -> (Float, StdGen)
tjitter = uniformR (-0.8 * tspread, -0.2 * tspread)

data S = S {gen :: StdGen, idx :: Int} deriving (Show)

data Pos = Pos {x :: Float, y :: Float, theta :: Float, depth :: Int, maxdepth :: Int, decay :: Float}

main :: IO ()
main = do
  (seed, decay) <- readArgs
  putStr $ file seed decay 2
  where
    readArgs :: IO (Int, Float)
    readArgs = do
      (arg1, arg2) <-
        getArgs >>= \case
          [arg1] -> return (arg1, "0")
          [arg1, arg2] -> return (arg1, arg2)
          _ -> err
      (,) <$> readMaybe arg1 <*> readMaybe arg2 & \case
        Just args -> return args
        _ -> err
    err = ioError . userError $ "usage: provide two arguments (integer random seed and floating point decay factor), and pipe to `" <> nextcmd <> "`"

nextcmd :: String
nextcmd = "neato -n2 -Tsvg"

file :: Int -> Float -> Int -> String
file seed decay depth =
  let specs = run (node >>= tree) seed (Pos 0 0 0 0 depth decay)
   in unlines $
        [ "#!/usr/bin/env " <> nextcmd,
          "# seed " <> show seed,
          "digraph {",
          "bgcolor=transparent",
          "node[shape=point]"
        ]
          ++ specs
          ++ ["}"]

newtype App a = App (ReaderT Pos (State S) a) deriving (Functor, Applicative, Monad, MonadState S, MonadReader Pos)

run :: App a -> Int -> Pos -> a
run (App m) seed p0 = flip evalState (S (mkStdGen seed) 0) . flip runReaderT p0 $ m

getRand :: (StdGen -> (a, StdGen)) -> App a
getRand genf = do
  (x, g) <- gets (genf . (.gen))
  modify (\s -> s {gen = g})
  return x

node :: App Int
node = gets (.idx) <* modify (\s -> s {idx = s.idx + 1})

branch :: Int -> App (Int, Pos)
branch i = do
  pos <- asks id
  theta' <- jitter (fromIntegral i * tspread + pos.theta)
  n <- node
  let pos' = fwd (len pos.depth) $ pos {theta = theta', depth = pos.depth + 1}
  return (n, pos')
  where
    jitter :: Float -> App Float
    jitter x = do
      j <- getRand tjitter
      return $ x + j

tree :: Int -> App [String]
tree i = do
  bs <- traverse branch =<< decaying tbranches
  depth <- asks (.depth)
  n <- asks (fmtNode i)
  let edges = [fmtEdge depth i j | (j, _) <- bs]
  let out = n : edges
  maxdepth <- asks (.maxdepth)
  if depth >= maxdepth
    then pure $ [fmtNode j p | (j, p) <- bs] ++ out
    else (++ out) <$> concatMapM (\(j, pos) -> local (const pos) (tree j)) bs
  where
    decaying :: [a] -> App [a]
    decaying xs = do
      thr <- asks (.decay)
      let variate = mapRandom (> thr) $ uniformR (0.0, 1.0)
      filterM (const $ getRand variate) xs

mapRandom :: (a -> b) -> (g -> (a, g)) -> g -> (b, g)
mapRandom = fmap . first

concatMapM :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f xs = concat <$> traverse f xs

fmtNode :: Int -> Pos -> String
fmtNode i pos =
  let s = 0
   in show i
        <> " ["
        <> ("pos=\"" <> floatStr pos.x <> "," <> floatStr pos.y <> "\"")
        <> (" height=" <> floatStr s)
        <> (" width=" <> floatStr s)
        <> " style=invis"
        <> "]"

fmtEdge :: Int -> Int -> Int -> String
fmtEdge depth i j =
  let r = sizefactor depth * 2
   in (show i <> " -> " <> show j)
        <> " ["
        <> ("arrowsize=" <> show r)
        <> (" penwidth=" <> show r)
        <> "]"

fwd :: Float -> Pos -> Pos
fwd d pos =
  pos
    { x = pos.x + d * sin pos.theta,
      y = pos.y + d * cos pos.theta
    }

floatStr :: Float -> String
floatStr x = showFFloat (Just 4) x ""
