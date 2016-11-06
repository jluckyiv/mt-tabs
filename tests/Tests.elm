module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Ballot exposing (..)


all : Test
all =
    describe "Ballot information"
        [ test "Basic information" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( tournamentInformation ballot
                        , roundInformation ballot
                        , prosecutionTeam ballot
                        , prosecutionTeamNumber ballot
                        , defenseTeam ballot
                        , defenseTeamNumber ballot
                        )
                        ( "Beach Ball Classic 2016"
                        , "Round 3: Sharis Manokian"
                        , "Tamalpais"
                        , 20
                        , "King"
                        , 7
                        )
        , test "Team scoring" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForTeam All Prosecution ballot
                        , pointsForTeam All Defense ballot
                        , result ballot.prosecution ballot
                        , result ballot.defense ballot
                        )
                        ( 150
                        , 144
                        , Win 6
                        , Loss -6
                        )
        , test "Points for phases" <|
            \() ->
                let
                    ballot =
                        createManokianBallot
                in
                    Expect.equal
                        ( pointsForTeam Opening Prosecution ballot
                        , pointsForTeam Closing Prosecution ballot
                        , pointsForTeam ClerkBailiff Prosecution ballot
                        , pointsForTeam (Pretrial <| Nothing) Prosecution ballot
                        , pointsForTeam (Pretrial <| Just "Prepared Argument") Prosecution ballot
                        , pointsForTeam (Direct <| Just "Lin Stark") Prosecution ballot
                        , pointsForTeam (Cross <| Just "Lin Stark") Defense ballot
                        , pointsForTeam (Witness <| Just "Lin Stark") Prosecution ballot
                        , pointsForTeam (Direct <| Nothing) Prosecution ballot
                        )
                        ( 9
                        , 10
                        , 10
                        , 15
                        , 7
                        , 8
                        , 7
                        , 9
                        , 34
                        )
        , test "Points for students" <|
            \() ->
                let
                    ballot =
                        createManokianBallot

                    king =
                        School "King" 7 Defense

                    torrinDiaz =
                        Student "Torrin Diaz" king

                    hunterFisk =
                        Student "Hunter Fisk" king

                    rachelPriebe =
                        Student "Rachel Priebe" king
                in
                    Expect.equal
                        ( pointsForStudent All torrinDiaz ballot
                        , pointsForStudent (Cross <| Nothing) torrinDiaz ballot
                        , pointsForStudent (Direct <| Nothing) torrinDiaz ballot
                        , pointsForStudent All rachelPriebe ballot
                        , pointsForStudent All hunterFisk ballot
                        )
                        ( 30
                        , 15
                        , 8
                        , 17
                        , 10
                        )
        , test "Ranks for students" <|
            \() ->
                let
                    ballot =
                        createManokianBallot

                    king =
                        School "King" 7 Defense

                    tamalpais =
                        School "Tamalpais" 20 Prosecution

                    elissaAsch =
                        Student "Elissa Asch" tamalpais

                    georgiaPemberton =
                        Student "Georgia Pemberton" tamalpais

                    cameronLucky =
                        Student "Cameron Lucky" king

                    torrinDiaz =
                        Student "Torrin Diaz" king

                    ericaSorenson =
                        Student "Erica Sorenson" king

                    celesteMoore =
                        Student "Celeste Moore" tamalpais

                    aviPerkoff =
                        Student "Avi Perkoff" tamalpais
                in
                    Expect.equal
                        ( ballot.witnessRanks
                        , ballot.attorneyRanks
                        )
                        ( [ ericaSorenson, celesteMoore, aviPerkoff ]
                        , [ elissaAsch, georgiaPemberton, cameronLucky ]
                        )
        ]


createManokianBallot =
    let
        tournament =
            Tournament "Beach Ball Classic" 2016

        tamalpais =
            School "Tamalpais" 20 Prosecution

        king =
            School "King" 7 Defense

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
