module Ballot
    exposing
        ( Ballot
        , Tournament
        , Round
        , School
        , Student
        , Party(..)
        , Phase(..)
        , Result(..)
        , Score
        , tournamentInformation
        , roundInformation
        , prosecutionTeam
        , prosecutionTeamNumber
        , defenseTeam
        , defenseTeamNumber
        , pointsForParty
        , pointsForStudent
        , pointsForSchool
        , resultForParty
        , resultForSchool
        )


type alias Ballot =
    { tournament : Tournament
    , round : Round
    , prosecution : School
    , defense : School
    , scores : List Score
    , attorneyRanks : List Student
    , witnessRanks : List Student
    }


type Result
    = Win Margin
    | Loss Margin
    | Tie


type Phase
    = Pretrial (Maybe Description)
    | Opening
    | Witness (Maybe Name)
    | Direct (Maybe Name)
    | Cross (Maybe Name)
    | Closing
    | ClerkBailiff
    | All


type alias Score =
    { phase : Phase
    , student : Student
    , points : Points
    }


type alias Tournament =
    { name : Name
    , year : Int
    }


type alias Round =
    { number : Int
    , scorer : Name
    }


type alias School =
    { name : Name
    , number : Int
    }


type alias Student =
    { name : Name
    , school : School
    }


type Party
    = Prosecution
    | Defense


type alias Points =
    Int


type alias Margin =
    Int


type alias Name =
    String


type alias Description =
    String


tournamentInformation : Ballot -> String
tournamentInformation ballot =
    ballot.tournament.name
        ++ " "
        ++ toString ballot.tournament.year


roundInformation : Ballot -> String
roundInformation ballot =
    "Round "
        ++ toString ballot.round.number
        ++ ": "
        ++ ballot.round.scorer


prosecutionTeam : Ballot -> String
prosecutionTeam ballot =
    ballot.prosecution.name


defenseTeam : Ballot -> String
defenseTeam ballot =
    ballot.defense.name


prosecutionTeamNumber : Ballot -> Int
prosecutionTeamNumber ballot =
    ballot.prosecution.number


defenseTeamNumber : Ballot -> Int
defenseTeamNumber ballot =
    ballot.defense.number


pointsForParty : Party -> Phase -> Ballot -> Points
pointsForParty party phase ballot =
    case phase of
        All ->
            scoresForParty party ballot
                |> sumScores

        _ ->
            scoresForParty party ballot
                |> scoresForPhase phase
                |> sumScores


pointsForSchool : School -> Phase -> Ballot -> Maybe Int
pointsForSchool school phase ballot =
    if isProsecution school ballot then
        Just (pointsForParty Prosecution phase ballot)
    else if isDefense school ballot then
        Just (pointsForParty Defense phase ballot)
    else
        Nothing


pointsForStudent : Student -> Phase -> Ballot -> Int
pointsForStudent student phase ballot =
    case phase of
        All ->
            scoresForStudent student ballot.scores
                |> sumScores

        _ ->
            scoresForStudent student ballot.scores
                |> scoresForPhase phase
                |> sumScores


sumScores : List Score -> Points
sumScores scores =
    List.map (\score -> score.points) scores
        |> List.sum


scoresForParty : Party -> Ballot -> List Score
scoresForParty party ballot =
    case party of
        Prosecution ->
            scoresForProsecution ballot

        Defense ->
            scoresForDefense ballot


scoresForProsecution : Ballot -> List Score
scoresForProsecution ballot =
    List.filter (isScoreForSchool ballot.prosecution) ballot.scores


scoresForDefense : Ballot -> List Score
scoresForDefense ballot =
    List.filter (isScoreForSchool ballot.defense) ballot.scores


isScoreForSchool : School -> Score -> Bool
isScoreForSchool school score =
    score.student.school == school


scoresForStudent : Student -> List Score -> List Score
scoresForStudent student scores =
    List.filter (isStudent student) scores


scoresForPhase : Phase -> List Score -> List Score
scoresForPhase phase scores =
    case phase of
        Pretrial (Just description) ->
            List.filter (isPhase phase) scores

        Pretrial _ ->
            List.filter (isAnyPretrial) scores

        Witness (Just name) ->
            List.filter (isPhase phase) scores

        Witness _ ->
            List.filter (isAnyWitness) scores

        Direct (Just name) ->
            List.filter (isPhase phase) scores

        Direct _ ->
            List.filter (isAnyDirect) scores

        Cross (Just name) ->
            List.filter (isPhase phase) scores

        Cross _ ->
            List.filter (isAnyCross) scores

        _ ->
            List.filter (isPhase phase) scores


resultForParty : Party -> Ballot -> Result
resultForParty party ballot =
    let
        prosPoints =
            pointsForParty Prosecution All ballot

        defPoints =
            pointsForParty Defense All ballot

        margin =
            prosPoints - defPoints
    in
        case party of
            Prosecution ->
                if margin > 0 then
                    Win margin
                else if margin < 0 then
                    Loss margin
                else
                    Tie

            Defense ->
                if margin < 0 then
                    Win -margin
                else if margin > 0 then
                    Loss -margin
                else
                    Tie


resultForSchool : School -> Ballot -> Maybe Result
resultForSchool school ballot =
    if isProsecution school ballot then
        Just (resultForParty Prosecution ballot)
    else if isDefense school ballot then
        Just (resultForParty Defense ballot)
    else
        Nothing


ranksForWitnesses ballot =
    List.indexedMap (,) ballot.witnessRanks


isAnyWitness : Score -> Bool
isAnyWitness score =
    case score.phase of
        Witness _ ->
            True

        _ ->
            False


isAnyDirect : Score -> Bool
isAnyDirect score =
    case score.phase of
        Direct _ ->
            True

        _ ->
            False


isAnyCross : Score -> Bool
isAnyCross score =
    case score.phase of
        Cross _ ->
            True

        _ ->
            False


isAnyPretrial : Score -> Bool
isAnyPretrial score =
    case score.phase of
        Pretrial _ ->
            True

        _ ->
            False


isPhase : Phase -> Score -> Bool
isPhase phase score =
    score.phase == phase


isStudent : Student -> Score -> Bool
isStudent student score =
    score.student == student


isProsecution : School -> Ballot -> Bool
isProsecution school ballot =
    ballot.prosecution == school


isDefense : School -> Ballot -> Bool
isDefense school ballot =
    ballot.defense == school



-- pointsForProsecution : Phase -> Ballot -> Points
-- pointsForProsecution phase ballot =
--     case phase of
--         All ->
--             scoresForProsecution ballot
--                 |> sumScores
--
--         _ ->
--             scoresForProsecution ballot
--                 |> scoresForPhase phase
--                 |> sumScores
--
--
-- pointsForDefense : Phase -> Ballot -> Points
-- pointsForDefense phase ballot =
--     case phase of
--         All ->
--             scoresForDefense ballot
--                 |> sumScores
--
--         _ ->
--             scoresForDefense ballot
--                 |> scoresForPhase phase
--                 |> sumScores
--
--
-- partyForSchool : School -> Ballot -> Maybe Party
-- partyForSchool school ballot =
--     if ballot.prosecution == school then
--         Just Prosecution
--     else if ballot.defense == school then
--         Just Defense
--     else
--         Nothing
--
--
