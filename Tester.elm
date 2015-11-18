module Tester where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task, andThen)
import Signal exposing (Signal, Address)
import Random exposing (..)
import String exposing (toInt,left)
import Debug exposing (log)
import Trampoline exposing (..)

-- Model definitions
type alias Model = {
  weeklyImpressions : Int,
  playerAds : List Ad,
  allocationMethod : ImpressionAllocation,
  seed : Seed
  }

initialModel = { weeklyImpressions = 1000,
                 playerAds = [],
                 seed=startTimeSeed,
                 allocationMethod = Random
                 }

port startTime : Float

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime

type alias Ad = {
  trueCtr : Float,
  impressions : Int,
  clicks : Int,
  status : Status,
  adId : Int
  }

type Status = Active | Paused

type ImpressionAllocation = Random | Bandit

type Action = NoOp |
              ResetAll |
              RequestNewAd |
              PauseAd Int |
              ActivateAd Int |
              ChangeWeeklyImpressions Int |
              ChangeAllocationMethod |
              RunWeek

fromJust : Maybe a -> a
fromJust x = case x of
  Just y -> y

betaSample : Seed -> Int -> Int -> (Float,Seed)
betaSample seed alpha beta =
   let (ls, seed') = Random.generate (Random.list (alpha+beta+1) (Random.float 0 1)) seed
   in 
    (fromJust (List.head (List.drop (alpha-1) (List.sort ls))), seed')

binomialSample : Seed -> Float -> Int -> (Int,Seed)
binomialSample seed p n = trampoline <| binSample' seed p n 0

binSample' seed p n acc = case n of
  0 -> Done (acc,seed)
  _ -> let (s,seed') = Random.generate (Random.float 0 1) seed
           v = if s < p then 1 else 0
       in Continue (\() -> binSample' seed' p (n-1) (acc+v))

newAdvert : Model -> (Ad, Model)
newAdvert model =
  let (ctr,seed') = betaSample model.seed 50 1000
      n = List.length model.playerAds
  in
    ( {trueCtr = ctr, impressions=0, clicks=0, status=Active, adId=n+1},
      {model | seed <- seed'})

onSliderChange : Address Action -> Attribute
onSliderChange addr = on "input" targetValue (\val -> convertAndMessage addr val)

convertAndMessage addr i = case (toInt i) of
    Ok x -> Signal.message addr (ChangeWeeklyImpressions x)
    Err e -> Signal.message addr (ChangeWeeklyImpressions 1000)


-- view
view : Address Action -> Model -> Html
view address model =
  let totalimpressions = List.sum <| List.map .impressions model.playerAds
      totalclicks = List.sum <| List.map .clicks model.playerAds
  in
  div [] [
   div [] [
   button [onClick address ResetAll] [text "Reset"],
   button [onClick address RequestNewAd,
           if List.isEmpty model.playerAds then class "button-primary" else class "button"] [text "New Advert"],
   button [onClick address ChangeAllocationMethod] [
     case model.allocationMethod of
       Random -> text "Switch to 'Optimise for...'"
       Bandit -> text "Switch to 'Rotate'"
       ]
       ],
   div [] [    
     button [onClick address RunWeek,
            if List.isEmpty model.playerAds then class "button" else class "button-primary"] [text "Run"],
     span [] [text "Weekly impressions: "],       
     node "input" [ type' "range",
                  Html.Attributes.min (toString 1),
                  Html.Attributes.max (toString 10000),
                  Html.Attributes.value (toString model.weeklyImpressions),
                  onSliderChange address
                  ] [],
     span [] [text (toString model.weeklyImpressions)]
     ],
   table [] [thead [] [tr [] [th [] [text "Advert"],
                              th [] [text "Impressions"],
                              th [] [text "Clicks"],
                              th [] [text "Observed CTR"],
                              th [] [text "Pause/Activate"]]
                       ],
             tbody [] (List.map (viewAd address) model.playerAds),
             thead [] [tr [] [th [] [text "Totals"],
                              th [] [text <| toString totalimpressions],
                              th [] [text <| toString totalclicks],
                              th [] [text <| formatPercentage <| (toFloat totalclicks)/(toFloat totalimpressions)],
                              th [] []]
                              ]
             ]
   
   ]

formatPercentage : Float -> String
formatPercentage p = let f = p * 100 |> toString |> left 5 |> \x -> String.append x "%"
  in if isNaN p then "--" else f

viewAd : Address Action -> Ad -> Html
viewAd address ad = tr [] [
         td [] [text ("Advert "++(toString ad.adId))],
         td [] [text <| toString ad.impressions],
         td [] [text <| toString ad.clicks],
         td [] [text <| formatPercentage <| (toFloat ad.clicks) / (toFloat ad.impressions)],
         td [] [case ad.status of
                  Active ->
                    button [onClick address (PauseAd ad.adId)] [text "Pause"]
                  Paused ->
                    button [onClick address (ActivateAd ad.adId)] [text "Activate"]
               ]
          ]

pauseAd : Int -> Ad -> Ad
pauseAd i ad = if ad.adId == i then {ad | status <- Paused } else ad

activateAd i ad = if ad.adId == i then {ad | status <- Active } else ad

filterActiveAds : List Ad -> List Ad
filterActiveAds ads = List.filter (\ad -> ad.status == Active) ads

allocateImpression model ads =
  let (activeAds,inactiveAds) = List.partition (\ad -> ad.status == Active) ads
      nactive = List.length activeAds
      (impressionIndex, seed') = Random.generate (Random.int 0 (nactive-1)) model.seed
      activeAds' = List.indexedMap (\i ad -> if i == impressionIndex then {ad | newimps <- ad.newimps+1} else ad) activeAds
  in
    ({model | seed <- seed'},
     List.append activeAds' inactiveAds)

allocateImpressions n (model,ads) =
  case n of
    0 -> Done (model,ads)
    _ -> Continue (\() -> allocateImpression model ads |> allocateImpressions (n-1))   

allocateClick (model, ad) =
  let (clcks, seed') = binomialSample model.seed ad.trueCtr ad.newimps
  in
    ({model | seed <- seed'},
     {ad | impressions <- ad.impressions + ad.newimps,
           clicks <- ad.clicks + clcks})

updateNthAd n model ads =
  let new = List.indexedMap (\i ad -> if i==(n-1) then allocateClick (model,ad) else (model,ad)) <| ads
      newModel = fst <| fromJust <| List.head <| List.drop (n-1) new
      newAds = List.map snd new
  in
    (newModel, newAds)
    
  

allocateClicks n (model, ads) = case n of
  0 -> Done (model, ads)
  _ -> Continue (\() -> updateNthAd n model ads |> allocateClicks (n-1))


update : Action -> Model -> Model
update action model = case action of
  NoOp -> model
  ResetAll -> initialModel
  RequestNewAd -> let (ad,model') = newAdvert model
    in
      {model' | playerAds <- ad :: (model'.playerAds) }
  PauseAd i ->
    {model | playerAds <- List.map (pauseAd i) model.playerAds}
  ActivateAd i ->
    {model | playerAds <- List.map (activateAd i) model.playerAds}
  ChangeWeeklyImpressions i ->
    {model | weeklyImpressions <- i }
  ChangeAllocationMethod -> case model.allocationMethod of
    Random -> {model | allocationMethod <- Bandit}
    Bandit -> {model | allocationMethod <- Random}
  RunWeek -> let
               ads = List.map (\x -> {x | newimps = 0}) model.playerAds
               nads = List.length ads
               (model', ads') = trampoline <| allocateImpressions model.weeklyImpressions (model,ads)
               (model'',ads'') = trampoline <| allocateClicks nads (model',ads')
               ads''' = List.map (\x -> {x - newimps}) ads''
             in {model'' | playerAds <- List.sortBy (\x -> (-1) * x.adId) ads'''}

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


main : Signal Html
main =
  Signal.map (view actions.address) model