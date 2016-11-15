module Sampling exposing (listSample,betaSample,binomialSample, fromJust)
{-|
Sampling from beta and binomial distributions

Not efficient at all
|-}
import Random exposing (..)
import Trampoline exposing (..)
import Debug exposing (crash)

fromJust : Maybe a -> a
fromJust x = case x of
  Just y -> y
  Nothing -> Debug.crash "Found Nothing in a call to fromJust"

listSample : Seed -> Generator a -> Int -> (Seed, List a)
listSample seed gen n = Trampoline.evaluate <| lSamp seed gen n []

lSamp seed gen i acc = case i of
  0 -> Trampoline.done (seed, acc)
  _ -> let (s,newseed) = Random.step gen seed
       in Trampoline.jump (\() -> lSamp newseed gen (i-1) (s :: acc))

betaSample : Seed -> Int -> Int -> (Float,Seed)
betaSample seed alpha beta =
   let (newseed, ls) = listSample seed (Random.float 0 1) (alpha+beta+1)
   in
    (fromJust (List.head (List.drop (alpha-1) (List.sort ls))), newseed)

binomialSample : Seed -> Float -> Int -> (Int,Seed)
binomialSample seed p n = Trampoline.evaluate <| binSample seed p n 0

binSample seed p n acc = case n of
  0 -> Trampoline.done (acc,seed)
  _ -> let (s,newseed) = Random.step (Random.float 0 1) seed
           v = if s < p then 1 else 0
       in Trampoline.jump (\() -> binSample newseed p (n-1) (acc+v))
