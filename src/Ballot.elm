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
        , pointsForTeam
        , pointsForStudent
        , result
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
    , party : Party
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


result : School -> Ballot -> Result
result school ballot =
    let
        prosPoints =
            pointsForTeam All Prosecution ballot

        defPoints =
            pointsForTeam All Defense ballot

        margin =
            prosPoints - defPoints
    in
        case school.party of
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


pointsForStudent : Phase -> Student -> Ballot -> Int
pointsForStudent phase student ballot =
    case phase of
        All ->
            scoresForStudent student ballot.scores
                |> pointsForScores

        _ ->
            scoresForStudent student ballot.scores
                |> scoresForPhase phase
                |> pointsForScores


pointsForTeam : Phase -> Party -> Ballot -> Int
pointsForTeam phase party ballot =
    case phase of
        All ->
            scoresForParty party ballot.scores
                |> pointsForScores

        _ ->
            scoresForParty party ballot.scores
                |> scoresForPhase phase
                |> pointsForScores


pointsForScores : List Score -> Points
pointsForScores scores =
    List.map (\score -> score.points) scores
        |> List.sum


scoresForStudent : Student -> List Score -> List Score
scoresForStudent student scores =
    List.filter (isStudent student) scores


scoresForParty : Party -> List Score -> List Score
scoresForParty party scores =
    List.filter (isParty party) scores


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


isParty : Party -> Score -> Bool
isParty party score =
    score.student.school.party == party
