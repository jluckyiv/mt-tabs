module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Ballot
    exposing
        ( Ballot
        , Tournament
        , Round
        , School
        , Student
        , Party(..)
        , Phase(..)
        , Score
        , WitnessScore
        , tournamentInformation
        , roundInformation
        , prosecutionTeam
        , prosecutionTeamNumber
        , defenseTeam
        , defenseTeamNumber
        , totalPoints
        )


all : Test
all =
    describe "Ballot information"
        [ test "Basic information" <|
            \() ->
                let
                    ballot =
                        createTestBallot
                in
                    Expect.equal
                        ( tournamentInformation ballot
                        , roundInformation ballot
                        , prosecutionTeam ballot
                        , prosecutionTeamNumber ballot
                        , defenseTeam ballot
                        , defenseTeamNumber ballot
                        , totalPoints Prosecution ballot
                        , totalPoints Defense ballot
                        )
                        ( "Beach Ball Classic 2016"
                        , "Round 3: Sharis Manokian"
                        , "Tamalpais"
                        , 20
                        , "King"
                        , 7
                        , 150
                        , 144
                        )
          -- , test "Prosecution totals" <|
          --     \() ->
          --         let
          --             ballot =
          --                 createTestBallot
          --         in
          --             Expect.equal
          --                 ( prosecutionOpeningPoints
          --                 , prosecutionClosingPoints
          --                 , prosecutionPretrial
          --                 )
          --                 ()
        ]


createTestBallot =
    let
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

        pretrialArgument =
            Score (Pretrial <| Just "Prepared Argument")
                michaelPile
                7
                rachelPriebe
                8

        pretrialQuestions =
            Score (Pretrial <| Just "Questions")
                michaelPile
                8
                rachelPriebe
                9

        openingStatements =
            Score Opening
                elissaAsch
                9
                torrinDiaz
                7

        prosWitness1 =
            WitnessScore (ProsecutionWitness "Lin Stark")
                georgiaPemberton
                8
                torrinDiaz
                7
                aviPerkoff
                9

        prosWitness2 =
            WitnessScore (ProsecutionWitness "Hayden West")
                carolineHerdman
                8
                danielSosa
                8
                celesteMoore
                9

        prosWitness3 =
            WitnessScore (ProsecutionWitness "Julian Blake")
                carolineHerdman
                8
                cameronLucky
                9
                marioMicklow
                9

        prosWitness4 =
            WitnessScore (ProsecutionWitness "Dana Greyjoy")
                elissaAsch
                10
                torrinDiaz
                8
                marioMicklow
                9

        defWitness1 =
            WitnessScore (DefenseWitness "Addison Frey")
                elissaAsch
                9
                danielSosa
                9
                alexPigeon
                8

        defWitness2 =
            WitnessScore (DefenseWitness "Devin Tyler")
                carolineHerdman
                8
                danielSosa
                9
                ericaSorenson
                10

        defWitness3 =
            WitnessScore (DefenseWitness "Frankie Lyman")
                georgiaPemberton
                9
                torrinDiaz
                8
                elizabethMontoya
                9

        defWitness4 =
            WitnessScore (DefenseWitness "Cameron Awbrey")
                georgiaPemberton
                10
                cameronLucky
                8
                carmenFloresLopez
                7

        closingArguments =
            Score Closing
                georgiaPemberton
                10
                cameronLucky
                10

        clerkBailiff =
            Score ClerkBailiff
                robertDonohue
                10
                hunterFisk
                10

        scores =
            [ pretrialArgument
            , pretrialQuestions
            , openingStatements
            , closingArguments
            , clerkBailiff
            ]

        prosecutionWitnessScores =
            [ prosWitness1
            , prosWitness2
            , prosWitness3
            , prosWitness4
            ]

        defenseWitnessScores =
            [ defWitness1
            , defWitness2
            , defWitness3
            , defWitness4
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
            prosecutionWitnessScores
            defenseWitnessScores
            attorneyRanks
            witnessRanks
