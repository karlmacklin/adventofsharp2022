namespace AdventOfSharp

open System.IO
open Output
open Shared

module Day1 =

    let parseInput (input: string) =
        input.Split("\n\n")
        |> Seq.map (fun elf -> splitStringByNewLineAndFilterOutEmptyStrings elf)
        |> Seq.map (fun x -> x |> Seq.map int)

    let part1 input =
        let elfs = input |> parseInput
        let elfsCalorieSummary = elfs |> Seq.map Seq.sum
        let maxCalories = elfsCalorieSummary |> Seq.max
        maxCalories

    let part2 input =
        let elfs = input |> parseInput
        let elfsCalorieSummary = elfs |> Seq.map Seq.sum
        Seq.sortDescending elfsCalorieSummary
        |> Seq.take 3
        |> Seq.sum

    let computeDay1 =
        printfn "Processing day 1 part 1..."

        let filenameTest = Path.Combine(__SOURCE_DIRECTORY__, $"inputs/day1test.txt")
        let day1TestInput = File.ReadAllText(filenameTest)

        let filenameActual = Path.Combine(__SOURCE_DIRECTORY__, $"inputs/day1.txt")
        let day1Input = File.ReadAllText(filenameActual)

        let expectedTestValue1 = 24000
        let testValue1 = part1 day1TestInput
        let testResult1 = isSuccess testValue1 expectedTestValue1
        print "Test part 1: %A" testValue1 testResult1

        let day1part1solution = part1 day1Input
        print "Solution to day 1 part 1: %A" day1part1solution Info

        printfn "Processing day 1 part 2..."

        let expectedTestValue2 = 45000
        let testValue2 = part2 day1TestInput
        let testResult2 = isSuccess testValue2 expectedTestValue2
        print "Test part 2: %A" testValue2 testResult2

        let day1part2solution = part2 day1Input
        print "Solution to day 1 part 2: %A" day1part2solution Info

