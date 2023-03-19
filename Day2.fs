namespace AdventOfSharp

open System.IO
open Output
open Shared

module Day2 =

    exception HandMatchError of string

    type Hand =
        | Rock
        | Paper
        | Scissors

    type Outcome =
        | Win
        | Draw
        | Loss

    type Round = Hand * Hand

    type StrategizedRound = Hand * Outcome

    let getHandFromString input : Hand =
        match input with
        | "A" | "X" -> Rock
        | "B" | "Y" -> Paper
        | "C" | "Z" -> Scissors
        | _ -> raise (HandMatchError($"Could not match {input}"))

    let getOutcomeFromString input : Outcome =
        match input with
        | "X" -> Loss
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> raise (HandMatchError($"Could not match {input}"))

    let getRoundFromPair (roundstr: string) : Round =
        let splitpair: seq<string> = roundstr.Split(" ") |> filterOutEmptyStrings
        (Seq.item 0 splitpair |> getHandFromString, Seq.item 1 splitpair |> getHandFromString)

    let getStrategizedRoundFromPair (roundstr: string) : StrategizedRound =
        let splitpair: seq<string> = roundstr.Split(" ") |> filterOutEmptyStrings
        (Seq.item 0 splitpair |> getHandFromString, Seq.item 1 splitpair |> getOutcomeFromString)

    let calculateHandTypeScore (round: Round) : int =
        match snd round with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let calculateWinDrawLoss (round: Round) : int =
        match round with
        | (Rock, Rock) -> 3
        | (Rock, Paper) -> 6
        | (Rock, Scissors) -> 0
        | (Paper, Rock) -> 0
        | (Paper, Paper) -> 3
        | (Paper, Scissors) -> 6
        | (Scissors, Rock) -> 6
        | (Scissors, Paper) -> 0
        | (Scissors, Scissors) -> 3

    let calculateRoundScore (round: Round) : int =
        let outcomeScore = calculateWinDrawLoss round
        let handTypeScore = calculateHandTypeScore round
        outcomeScore + handTypeScore
    
    let adaptRound (round: StrategizedRound) : Round =
        match round with
        | (x, Draw) -> (x, x)
        | (Rock, Win) -> (fst round, Paper)
        | (Rock, Loss) -> (fst round, Scissors)
        | (Paper, Win) -> (fst round, Scissors)
        | (Paper, Loss) -> (fst round, Rock)
        | (Scissors, Win) -> (fst round, Rock)
        | (Scissors, Loss) -> (fst round, Paper)

    let part1 input : int =
        let rounds =
            splitStringByNewLineAndFilterOutEmptyStrings input
            |> Seq.map (fun pair -> getRoundFromPair pair)

        let roundScores = Seq.map calculateRoundScore rounds
        Seq.sum roundScores

    let part2 input =
        let rounds =
            splitStringByNewLineAndFilterOutEmptyStrings input
            |> Seq.map (fun pair -> getStrategizedRoundFromPair pair)

        let roundScores = Seq.map adaptRound rounds |> Seq.map calculateRoundScore
        Seq.sum roundScores

    let computeDay2 =
        printfn "Processing day 2 part 1..."

        let filenameTest = Path.Combine(__SOURCE_DIRECTORY__, $"inputs/day2test.txt")
        let day2TestInput = File.ReadAllText(filenameTest)

        let filenameActual = Path.Combine(__SOURCE_DIRECTORY__, $"inputs/day2.txt")
        let day2Input = File.ReadAllText(filenameActual)

        let expectedTestValue1 = 15
        let testValue1 = part1 day2TestInput
        let testResult1 = isSuccess testValue1 expectedTestValue1
        print "Test part 1: %A" testValue1 testResult1

        let day2part1solution = part1 day2Input
        print "Solution to day 2 part 1: %A" day2part1solution Info

        printfn "Processing day 1 part 2..."

        let expectedTestValue2 = 12
        let testValue2 = part2 day2TestInput
        let testResult2 = isSuccess testValue2 expectedTestValue2
        print "Test part 2: %A" testValue2 testResult2

        let day2part2solution = part2 day2Input
        print "Solution to day 2 part 2: %A" day2part2solution Info
