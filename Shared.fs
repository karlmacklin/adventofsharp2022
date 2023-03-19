namespace AdventOfSharp

module Shared =
    let isNotEmptyString str = str <> ""

    let filterOutEmptyStrings arr = arr |> Seq.filter isNotEmptyString

    let splitStringByNewLineAndFilterOutEmptyStrings (input: string) =
      input.Split("\n")
      |> filterOutEmptyStrings