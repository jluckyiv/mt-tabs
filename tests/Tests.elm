module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Ballot exposing (..)


all : Test
all =
    describe "Ballot"
        [ test "Tournament information" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        (tournamentInformation ballot)
                        "Beach Ball Classic 2016"
        , test "Round information" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        (roundInformation ballot)
                        "Round 3: Sharis Manokian"
        , test "Prosecution information" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( prosecutionTeam ballot, prosecutionTeamNumber ballot )
                        ( "Tamalpais", 20 )
        , test "Defense information" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( defenseTeam ballot, defenseTeamNumber ballot )
                        ( "King", 7 )
        , test "Prosecution scoring" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForParty Prosecution All ballot
                        , pointsForSchool ballot.prosecution All ballot
                        , resultForParty Prosecution ballot
                        , resultForSchool ballot.prosecution ballot
                        )
                        ( 150, Just 150, Win 6, Just (Win 6) )
        , test "Defense scoring" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForParty Defense All ballot
                        , pointsForSchool ballot.defense All ballot
                        , resultForParty Defense ballot
                        , resultForSchool ballot.defense ballot
                        )
                        ( 144, Just 144, Loss -6, Just (Loss -6) )
        , test "Points for pretrial" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForParty Prosecution
                            (Pretrial <| Nothing)
                            ballot
                        , pointsForParty Prosecution
                            (Pretrial <| Just "Prepared Argument")
                            ballot
                        , pointsForParty Prosecution
                            (Pretrial <| Just "Questions")
                            ballot
                        , pointsForParty Defense
                            (Pretrial <| Nothing)
                            ballot
                        , pointsForParty Defense
                            (Pretrial <| Just "Prepared Argument")
                            ballot
                        , pointsForParty Defense
                            (Pretrial <| Just "Questions")
                            ballot
                        )
                        ( 15, 7, 8, 17, 8, 9 )
        , test "Points for clerk and bailiff" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForParty Prosecution ClerkBailiff ballot
                        , pointsForParty Defense ClerkBailiff ballot
                        )
                        ( 10, 10 )
        , test "Points for examinations" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForParty Prosecution (Direct <| Nothing) ballot
                        , pointsForParty Prosecution (Cross <| Nothing) ballot
                        , pointsForParty Defense (Direct <| Nothing) ballot
                        , pointsForParty Defense (Cross <| Nothing) ballot
                        )
                        ( 34, 36, 34, 32 )
        , test "Points for individual examinations" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForParty Prosecution
                            (Direct <| Just "Lin Stark")
                            ballot
                        , pointsForParty Defense
                            (Cross <| Just "Lin Stark")
                            ballot
                        , pointsForParty Prosecution
                            (Witness <| Just "Lin Stark")
                            ballot
                        , pointsForParty Defense
                            (Direct <| Just "Devin Tyler")
                            ballot
                        , pointsForParty Prosecution
                            (Cross <| Just "Devin Tyler")
                            ballot
                        , pointsForParty Defense
                            (Witness <| Just "Devin Tyler")
                            ballot
                        )
                        ( 8, 7, 9, 9, 8, 10 )
        , test "Points for students" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForStudent torrinDiaz Opening ballot
                        , pointsForStudent torrinDiaz (Cross <| Just "Lin Stark") ballot
                        , pointsForStudent torrinDiaz (Cross <| Just "Dana Greyjoy") ballot
                        , pointsForStudent torrinDiaz (Cross <| Nothing) ballot
                        , pointsForStudent torrinDiaz (Direct <| Just "Frankie Lyman") ballot
                        , pointsForStudent torrinDiaz (Direct <| Nothing) ballot
                        , pointsForStudent torrinDiaz All ballot
                        , pointsForStudent alexPigeon (Witness <| Just "Addison Frey") ballot
                        , pointsForStudent alexPigeon All ballot
                        )
                        ( 7, 7, 8, 15, 8, 8, 30, 8, 8 )
        , test "Ranks for students" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( ballot.witnessRanks
                        , ballot.attorneyRanks
                        )
                        ( [ ericaSorenson, celesteMoore, aviPerkoff ]
                        , [ elissaAsch, georgiaPemberton, cameronLucky ]
                        )
        ]


tournament =
    Tournament "Beach Ball Classic" 2016


tamalpais =
    School "Tamalpais" 20


king =
    School "King" 7


round =
    Round 3 "Sharis Manokian"


michaelPile =
    Student "Michael Pile" tamalpais


rachelPriebe =
    Student "Rachel Priebe" king


elissaAsch =
    Student "Elissa Asch" tamalpais


torrinDiaz =
    Student "Torrin Diaz" king


georgiaPemberton =
    Student "Georgia Pemberton" tamalpais


cameronLucky =
    Student "Cameron Lucky" king


carolineHerdman =
    Student "Caroline Herdman" tamalpais


danielSosa =
    Student "Daniel Sosa" king


robertDonohue =
    Student "Robert Donohue" tamalpais


hunterFisk =
    Student "Hunter Fisk" king


aviPerkoff =
    Student "Avi Perkoff" tamalpais


celesteMoore =
    Student "Celeste Moore" tamalpais


marioMicklow =
    Student "Mario Micklow" tamalpais


sarahLunder =
    Student "Sarah Lunder" tamalpais


alexPigeon =
    Student "Alex Pigeon" king


ericaSorenson =
    Student "Erica Sorenson" king


elizabethMontoya =
    Student "Elizabeth Montoya" king


carmenFloresLopez =
    Student "Carmen Flores-Lopez" king


createManokianBallot =
    let
        scores =
            [ Score (Pretrial <| Just "Prepared Argument")
                michaelPile
                7
            , Score
                (Pretrial <| Just "Prepared Argument")
                rachelPriebe
                8
            , Score (Pretrial <| Just "Questions")
                michaelPile
                8
            , Score
                (Pretrial <| Just "Questions")
                rachelPriebe
                9
            , Score Opening
                elissaAsch
                9
            , Score
                Opening
                torrinDiaz
                7
            , Score (Direct <| Just "Lin Stark")
                georgiaPemberton
                8
            , Score
                (Cross <| Just "Lin Stark")
                torrinDiaz
                7
            , Score
                (Witness <| Just "Lin Stark")
                aviPerkoff
                9
            , Score (Direct <| Just "Hayden West")
                carolineHerdman
                8
            , Score (Cross <| Just "Hayden West")
                danielSosa
                8
            , Score (Witness <| Just "Hayden West")
                celesteMoore
                9
            , Score (Direct <| Just "Julian Blake")
                carolineHerdman
                8
            , Score (Cross <| Just "Julian Blake")
                cameronLucky
                9
            , Score (Witness <| Just "Julian Blake")
                marioMicklow
                9
            , Score (Direct <| Just "Dana Greyjoy")
                elissaAsch
                10
            , Score (Cross <| Just "Dana Greyjoy")
                torrinDiaz
                8
            , Score (Witness <| Just "Dana Greyjoy")
                marioMicklow
                9
            , Score (Cross <| Just "Addison Frey")
                elissaAsch
                9
            , Score (Direct <| Just "Addison Frey")
                danielSosa
                9
            , Score (Witness <| Just "Addison Frey")
                alexPigeon
                8
            , Score (Cross <| Just "Devin Tyler")
                carolineHerdman
                8
            , Score (Direct <| Just "Devin Tyler")
                danielSosa
                9
            , Score (Witness <| Just "Devin Tyler")
                ericaSorenson
                10
            , Score (Cross <| Just "Frankie Lyman")
                georgiaPemberton
                9
            , Score (Direct <| Just "Frankie Lyman")
                torrinDiaz
                8
            , Score (Witness <| Just "Frankie Lyman")
                elizabethMontoya
                9
            , Score (Cross <| Just "Cameron Awbrey")
                georgiaPemberton
                10
            , Score (Direct <| Just "Cameron Awbrey")
                cameronLucky
                8
            , Score (Witness <| Just "Cameron Awbrey")
                carmenFloresLopez
                7
            , Score Closing georgiaPemberton 10
            , Score Closing cameronLucky 10
            , Score ClerkBailiff robertDonohue 10
            , Score ClerkBailiff hunterFisk 10
            ]

        attorneyRanks =
            [ elissaAsch
            , georgiaPemberton
            , cameronLucky
            ]

        witnessRanks =
            [ ericaSorenson
            , celesteMoore
            , aviPerkoff
            ]
    in
        Ballot tournament
            round
            tamalpais
            king
            scores
            attorneyRanks
            witnessRanks
