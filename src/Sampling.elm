module Sampling exposing (listSample,betaSample,binomialSample, fromJust, weightCdf, weightedSample, pickRandom)
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

weightCdf : List (a,Float) -> List Float
weightCdf l = List.scanl (\(x,y) acc -> y+acc) 0 l |> List.drop 1

weightedSample seed xs =
  let weightedCdf = weightCdf xs
      maxWeight = fromJust <| List.maximum weightedCdf
      (rand, newseed) = Random.step (Random.float 0 maxWeight) seed
      (index,_) = fromJust <| List.head <| List.filter (\(i,y) -> y > rand) <| List.indexedMap (\i x -> (i,x)) weightedCdf
   in (index, newseed)

isJust x = case x of
  Nothing -> False
  (Just _) -> True

pickRandom : Seed -> List a -> (Seed, Maybe a)
pickRandom seed x = case x of
  [] -> (seed,Nothing)
  xs -> let n = List.length xs
            (npicked, newseed) = Random.step (Random.int 0 (n-1)) seed
            mxs = List.indexedMap (\i x -> if i==npicked then Just x else Nothing) xs
            el = List.filter isJust mxs
        in (newseed, Maybe.withDefault Nothing <| List.head el)
