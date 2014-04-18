module Main

open System


[<EntryPoint>]
let main args =
    printfn "Loading..."

    let wordTrie =
        Words.readLines @"..\..\..\words.txt"
        |> Seq.map (fun x -> x.ToUpper())
        |> Seq.map List.ofSeq
        |> Words.trie

    let numbers () =
        printfn "Enter the six numbers: "
        let a = Console.ReadLine() |> int32
        let b = Console.ReadLine() |> int32
        let c = Console.ReadLine() |> int32
        let d = Console.ReadLine() |> int32
        let e = Console.ReadLine() |> int32
        let f = Console.ReadLine() |> int32
        printfn "Now enter the target: "
        let target = Console.ReadLine() |> int32
        let expression,result = Numbers.compute target [a; b; c; d; e; f]
        printfn "Result: %s = %d" expression result 
        Console.ReadLine()

    let words () =
        printfn "Enter the nine letters below: "
        let letters = Console.ReadLine()
        let result = Words.compute letters wordTrie
        printfn "Results: %A" result
        Console.ReadLine()

    let rec chooseGame () =
        printfn "Choose your game! 'n' for numbers, 'w' for words, 'q' for quit"
        let cmd = Console.ReadKey().KeyChar
        match cmd with
        | 'n' ->
            numbers() |> ignore
            chooseGame()
        | 'w' ->
            words() |> ignore
            chooseGame()
        | 'q' -> ()
        | _ -> chooseGame()

    chooseGame()
    0