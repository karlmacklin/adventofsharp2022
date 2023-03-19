namespace AdventOfSharp

module Output =

    open System

    type Result =
        | Success
        | Fail
        | Info

    let print format value result =

        let color =
            match result with
            | Success -> ConsoleColor.Green
            | Fail -> ConsoleColor.Red
            | Info -> ConsoleColor.Blue

        Console.ForegroundColor <- color
        printfn format value
        Console.ForegroundColor <- ConsoleColor.White

    let isSuccess result expected =
        if result = expected then Success else Fail
