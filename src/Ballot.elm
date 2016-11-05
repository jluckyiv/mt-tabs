module Ballot exposing (..)


type alias Ballot =
    { tournament : Tournament
    , round : Round
    , prosecution : School
    , defense : School
    , scores : List Score
    , prosecutionWitnessScores : List WitnessScore
    , defenseWitnessScores : List WitnessScore
    , attorneyRanks : List Student
    , witnessRanks : List Student
    }


type Phase
    = Pretrial (Maybe Description)
    | Opening
    | Closing
    | ClerkBailiff
    | ProsecutionWitness Name
    | DefenseWitness Name


type alias Score =
    { phase : Phase
    , prosecution : Student
    , prosecutionPoints : Points
    , defense : Student
    , defensePoints : Points
    }


type alias WitnessScore =
    { phase : Phase
    , prosecution : Student
    , prosecutionPoints : Points
    , defense : Student
    , defensePoints : Points
    , witness : Student
    , witnessPoints : Points
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


score : Ballot -> Phase -> Maybe Score
score ballot phase =
    List.filter (\score -> score.phase == phase) ballot.scores
        |> List.take 1
        |> List.head


totalPoints : Party -> Ballot -> Int
totalPoints party ballot =
    List.sum
        [ nonWitnessPoints party ballot
        , directExamPoints party ballot
        , witnessPoints party ballot
        , crossExamPoints party ballot
        ]


nonWitnessPoints : Party -> Ballot -> Int
nonWitnessPoints party ballot =
    case party of
        Prosecution ->
            sumPoints .prosecutionPoints ballot.scores

        Defense ->
            sumPoints .defensePoints ballot.scores


witnessPoints : Party -> Ballot -> Int
witnessPoints party ballot =
    case party of
        Prosecution ->
            sumPoints .witnessPoints ballot.prosecutionWitnessScores

        Defense ->
            sumPoints .witnessPoints ballot.defenseWitnessScores


directExamPoints : Party -> Ballot -> Int
directExamPoints party ballot =
    case party of
        Prosecution ->
            sumPoints .prosecutionPoints ballot.prosecutionWitnessScores

        Defense ->
            sumPoints .defensePoints ballot.defenseWitnessScores


crossExamPoints : Party -> Ballot -> Int
crossExamPoints party ballot =
    case party of
        Prosecution ->
            sumPoints .prosecutionPoints ballot.defenseWitnessScores

        Defense ->
            sumPoints .defensePoints ballot.prosecutionWitnessScores


sumPoints : (score -> Points) -> List score -> Points
sumPoints property scores =
    List.map (\score -> property score) scores
        |> List.sum
