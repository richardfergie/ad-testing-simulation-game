module Tester exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task, andThen)
--import Signal exposing (Signal, Address)
import Random exposing (..)
import String exposing (left)
import Json.Decode exposing (int,map)
import Debug exposing (log,crash)
import Trampoline exposing (..)
import Sampling

-- Model definitions
type alias Model = {
  weeklyImpressions : Int,
  playerAds : List Ad,
  allocationMethod : ImpressionAllocation,
  seed : Seed,
  numberOfWeeks : Int,
  cheatCompetitor : Maybe Competitor
  }

type alias Flags = {
        randomSeed : Int
    }

init : Flags -> (Model, Cmd Action)
init flag = ({
    weeklyImpressions = 1000,
    playerAds = [],
    seed= Random.initialSeed flag.randomSeed,
    allocationMethod = Random,
    numberOfWeeks = 0,
    cheatCompetitor = Nothing
    }
    , Cmd.none)

type alias Ad = {
  trueCtr : Float,
  impressions : Int,
  clicks : Int,
  status : Status,
  adId : Int
  }

type Strategy = Strategy (Model -> List Ad -> (Model, List Ad))
unStrategy : Strategy -> (Model -> List Ad -> (Model, List Ad))
unStrategy (Strategy x) = x


type alias Competitor = {
        modifier : Strategy,
        competitorAds : List Ad,
        competitorAllocationMethod : ImpressionAllocation
    }

cheat : Strategy
cheat = Strategy <| \model ads -> case (List.length (filterActiveAds ads)) of
  0 -> let (ad1,newmodel) = newAdvert model
           (ad2,newnewmodel) = newAdvert newmodel
       in (newnewmodel, ad1 :: ad2 :: [])
  1 -> let (ad1,newmodel) = newAdvert model
       in (newmodel, ad1 :: ads)
  _ -> let (newadvert,newmodel) = newAdvert model
           maxCtr = Sampling.fromJust <| List.maximum <| List.map .trueCtr ads
           updatedAds = List.map (\ad -> if ad.trueCtr >= maxCtr then ad else {ad | status = Paused}) ads
       in (newmodel, newadvert :: updatedAds)

type Status = Active | Paused

type ImpressionAllocation = Random | Bandit

type Action = ResetAll |
              RequestNewAd |
              PauseAd Int |
              ActivateAd Int |
              ChangeWeeklyImpressions Int |
              ChangeAllocationMethod |
              RunWeek |
              ToggleCheatCompetitor

newAdvert : Model -> (Ad, Model)
newAdvert model =
  let (ctr,newseed) = Sampling.betaSample model.seed 50 1000
      n = List.length model.playerAds
  in
    ( {trueCtr = ctr, impressions=0, clicks=0, status=Active, adId=n+1},
      {model | seed = newseed})

onWeeklyImpressionsChange : Attribute Action
onWeeklyImpressionsChange = on "input" convertToWeeklyImpressions

convertToWeeklyImpressions : Json.Decode.Decoder Action
convertToWeeklyImpressions = Json.Decode.map ChangeWeeklyImpressions Json.Decode.int

-- views
view : Model -> Html Action
view model = div [class "row"] [
  div [class "columns", class "seven"] [viewPlayerData model],
  div [class "columns", class "five"] [viewCompetitorData model]
      ]

viewCompetitorData : Model -> Html Action
viewCompetitorData model = div [] [
    table [] [
         thead [] [tr [] [
                        th [] [text "Strategy"],
                        th [] [text "Impressions"],
                        th [] [text "Clicks"],
                        th [] [text "CTR"]
                       ]],
        tbody [] [viewSingleCompetitor "Cheater " ToggleCheatCompetitor (model.cheatCompetitor)]
        ]
  ]

viewSingleCompetitor : String -> Action -> Maybe Competitor -> Html Action
viewSingleCompetitor name action mcomp = case mcomp of
  Nothing -> tr [] [
               td [] [checkbox action name False],
               td [] [text "--"],
               td [] [text "--"],
               td [] [text "--"]
               ]
  Just comp -> let totalimpressions = List.sum <| List.map .impressions comp.competitorAds
                   totalclicks = List.sum <| List.map .clicks comp.competitorAds
               in tr [] [
                td [] [checkbox action name True],
                td [] [text <| toString totalimpressions],
                td [] [text <| toString totalclicks],
                td [] [text <| formatPercentage <| (toFloat totalclicks)/(toFloat totalimpressions)]
                ]

viewPlayerData : Model -> Html Action
viewPlayerData model =
  let totalimpressions = List.sum <| List.map .impressions model.playerAds
      totalclicks = List.sum <| List.map .clicks model.playerAds
  in
  div [] [
   div [] [text <| String.append (toString model.numberOfWeeks) <| if model.numberOfWeeks == 1 then " week" else " weeks"],
   div [] [
   button [onClick ResetAll] [text "Reset"],
   button [onClick RequestNewAd,
           if List.isEmpty model.playerAds then class "button-primary" else class "button"] [text "New Advert"],
   button [onClick ChangeAllocationMethod] [
     (case model.allocationMethod of
       Random -> text "Switch to 'Optimise for...'"
       Bandit -> text "Switch to 'Rotate'"
     )
       ]
       ],
   div [] [
     button [onClick RunWeek,
            if List.isEmpty model.playerAds then class "button" else class "button-primary"] [text "Run"],
     span [] [text "Weekly impressions: "],
     node "input" [ type_ "range",
                  Html.Attributes.min (toString 1),
                  Html.Attributes.max (toString 10000),
                  Html.Attributes.value (toString model.weeklyImpressions),
                  onWeeklyImpressionsChange
                  ] [],
     span [] [text (toString model.weeklyImpressions)]
     ],
   table [] [thead [] [tr [] [th [] [],
                              th [] [text "Impressions"],
                              th [] [text "Clicks"],
                              th [] [text "Observed CTR"],
                              th [] [text "Pause/Activate"]],
                       tr [] [th [] [text "Totals"],
                              th [] [text <| toString totalimpressions],
                              th [] [text <| toString totalclicks],
                              th [] [text <| formatPercentage <| (toFloat totalclicks)/(toFloat totalimpressions)],
                              th [] []]
                       ],
             tbody [] (List.map viewAd model.playerAds)
             ]

   ]

formatPercentage : Float -> String
formatPercentage p = let f = p * 100 |> toString |> left 5 |> \x -> String.append x "%"
  in if isNaN p then "--" else f

viewAd : Ad -> Html Action
viewAd ad = tr [] [
         td [] [text ("Advert "++(toString ad.adId))],
         td [] [text <| toString ad.impressions],
         td [] [text <| toString ad.clicks],
         td [] [text <| formatPercentage <| (toFloat ad.clicks) / (toFloat ad.impressions)],
         td [] [case ad.status of
                  Active ->
                    button [onClick (PauseAd ad.adId)] [text "Pause"]
                  Paused ->
                    button [onClick (ActivateAd ad.adId)] [text "Activate"]
               ]
          ]

checkbox : msg -> String -> Bool -> Html msg
checkbox msg name c = label []
    [ input [ type_ "checkbox", onCheck (\_ -> msg), checked c ] []
    , text name
    ]

pauseAd : Int -> Ad -> Ad
pauseAd i ad = if ad.adId == i then {ad | status = Paused } else ad

activateAd : Int -> Ad -> Ad
activateAd i ad = if ad.adId == i then {ad | status = Active } else ad

filterActiveAds : List Ad -> List Ad
filterActiveAds ads = List.filter (\ad -> ad.status == Active) ads

evenWeights : seed -> List a -> (seed,List (a,number))
evenWeights seed xs = (seed, List.map (\x -> (x,1)) xs)

wilsonUpperBound : Float -> Float -> Float
wilsonUpperBound phat n = (1/(1+(1/n)*1.96*1.96))*(phat + 1.96*1.96/(2*n) + 1.96*sqrt(phat*(1-phat)/n + 1.96*1.96/(4*n*n)))

wilsonWeightedCtr : List Ad -> List (Ad, Float)
wilsonWeightedCtr ads =
  let observedCtrs = List.map (\x -> (toFloat x.clicks)/(toFloat x.impressions)) ads
      weightings = List.map2 (\x y -> if isNaN x then 1 else wilsonUpperBound x (toFloat y.impressions)) observedCtrs ads
  in
    List.map2 (\x y -> (x,y)) ads weightings

ucb1 : a -> List Ad -> (a, List (Ad, Float))
ucb1 seed ads =
  let observedCtrs = List.map (\x -> (toFloat x.clicks)/(toFloat x.impressions)) ads
      ln = logBase e
      zip = List.map2 (\x y -> (x,y))
      t = toFloat <| List.sum <| List.map .impressions ads
      weights = List.map2 (\x ad -> x + sqrt(2*(ln t)/(toFloat ad.impressions))) observedCtrs ads
      maxWeight = Sampling.fromJust <| List.maximum weights
      weightsAll = List.map (\w -> if w > maxWeight - 0.0000000001 then 1 else 0) weights
      weightsNaN = List.map (\x -> if isNaN x then 1 else 0) observedCtrs
  in
    if (List.isEmpty <| List.filter isNaN observedCtrs) then (seed,(zip ads weightsAll)) else (seed,(zip ads weightsNaN))

epsilonGreedy : Random.Seed -> List Ad -> (Random.Seed, List (Ad,Float))
epsilonGreedy seed ads =
  let observedCtrs = List.map (\x -> (toFloat x.clicks)/(toFloat x.impressions)) ads
      (eps, newseed) = Random.step (Random.float 0 1) seed
      (index, newnewseed) = Random.step (Random.int 0 <| (List.length ads)-1) newseed
      nansreplaced = List.map (\x -> if isNaN x then 0 else x) observedCtrs
      maxCtr = Sampling.fromJust <| List.maximum nansreplaced
      exploitWeights = List.map (\w -> if w > maxCtr - 0.000001 then 1 else 0) nansreplaced
      exploreWeights = List.indexedMap (\i _ -> if i==index then 1 else 0) nansreplaced
      zip = List.map2 (\x y -> (x,y))
  in if eps < 0.2 then (newnewseed,zip ads exploreWeights) else (newnewseed,zip ads exploitWeights)

allocateImpression : Model -> ImpressionAllocation -> List Ad -> (Model, List Ad)
allocateImpression model alloc ads =
  let (activeAds,inactiveAds) = List.partition (\ad -> ad.status == Active) ads
      weightingFunc = if alloc == Random then evenWeights else epsilonGreedy
      (seed,weightedsamp) = weightingFunc model.seed activeAds
      (impressionIndex, newseed) = Sampling.weightedSample seed weightedsamp
      (p, newnewseed) = Random.step (Random.float 0 1) newseed
      newActiveAds = List.indexedMap (\i ad -> if i == impressionIndex then {ad | impressions = ad.impressions+1, clicks = if ad.trueCtr > p then ad.clicks+1 else ad.clicks } else ad) activeAds
  in
    ({model | seed = newnewseed},
     List.append newActiveAds inactiveAds)

allocateImpressions : Int -> ImpressionAllocation -> (Model, List Ad) -> Trampoline.Trampoline (Model, List Ad)
allocateImpressions n alloc (model,ads) =
  case n of
    0 -> Trampoline.done (model,ads)
    _ -> Trampoline.jump (\() -> allocateImpression model alloc ads |> allocateImpressions (n-1) alloc)

runPlayerAds : Model -> Model
runPlayerAds model = case (filterActiveAds model.playerAds) of
  [] -> model
  _ -> let ads = model.playerAds
           (newmodel, newads) = Trampoline.evaluate <| allocateImpressions model.weeklyImpressions model.allocationMethod (model,ads)
         in {newmodel | playerAds = newads}

runCompetitorAds : (Model -> Maybe Competitor) -> (Competitor -> Model -> Model) -> Model -> Model
runCompetitorAds getCompetitor setCompetitor model = case (getCompetitor model) of
  Nothing -> model
  Just comp -> let strat = unStrategy comp.modifier
                   (newmodel,newads) = strat model comp.competitorAds
                   (newnewmodel, newnewads) = Trampoline.evaluate <| allocateImpressions model.weeklyImpressions comp.competitorAllocationMethod (newmodel, newads)
                   newcomp = {comp | competitorAds = newnewads}
               in setCompetitor newcomp newnewmodel

update : Action -> Model -> (Model, Cmd Action)
update action model = case action of
  ResetAll -> ({
    weeklyImpressions = 1000,
    playerAds = [],
    seed=model.seed,
    allocationMethod = Random,
    numberOfWeeks = 0,
    cheatCompetitor = Nothing
    }
    , Cmd.none)
  RequestNewAd -> let (ad,newmodel) = newAdvert model
    in
      ({newmodel | playerAds = ad :: (newmodel.playerAds) }, Cmd.none)
  PauseAd i ->
    ({model | playerAds = List.map (pauseAd i) model.playerAds}, Cmd.none)
  ActivateAd i ->
    ({model | playerAds = List.map (activateAd i) model.playerAds}, Cmd.none)
  ChangeWeeklyImpressions i ->
    ({model | weeklyImpressions = i }, Cmd.none)
  ChangeAllocationMethod -> case model.allocationMethod of
    Random -> ({model | allocationMethod = Bandit}, Cmd.none)
    Bandit -> ({model | allocationMethod = Random}, Cmd.none)
  RunWeek -> let newmodel = runPlayerAds model
                 newnewmodel = runCompetitorAds .cheatCompetitor (\x model -> {model | cheatCompetitor = Just x}) newmodel
             in ({newnewmodel | numberOfWeeks = newnewmodel.numberOfWeeks + 1}, Cmd.none)
  ToggleCheatCompetitor -> case model.cheatCompetitor of
    Nothing -> ({model | cheatCompetitor = Just {modifier = cheat, competitorAds = [], competitorAllocationMethod = Bandit}}, Cmd.none)
    Just _ -> ({model | cheatCompetitor = Nothing}, Cmd.none)

subscriptions = \_ -> Sub.none

main =
  Html.programWithFlags { init=init, update=update, view=view, subscriptions=subscriptions}
