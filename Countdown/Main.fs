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
        let parseInt str =
            match Int32.TryParse str with
            | true, i -> Some i
            | _ -> None
    
        let rec getNumbers () =
            printfn "Enter the numbers separated by a space:"
            let numbers = Console.ReadLine().Split [|' '|] |> Seq.choose parseInt |> List.ofSeq
            match numbers with
            | [] -> printfn "Please try again"
                    getNumbers ()
            | n  -> printfn "Numbers are: %A" n
                    n

        let rec getTarget () =
            printfn "Now enter the target:"
            match Console.ReadLine () |> parseInt with
            | Some t -> printfn "Target is %A" t
                        t
            | None   -> printfn "Please try again"
                        getTarget ()

        let numbers = getNumbers ()
        let target = getTarget ()
        let result = Numbers.compute numbers target
        if Numbers.eval result = target then
            printfn "It's a match! %s" (Numbers.write result)
        else
            printfn "Not quite... %s" (Numbers.write result)

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