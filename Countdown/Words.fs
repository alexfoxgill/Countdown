module Words

open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let rec remove item = function
| x::xs when x = item -> xs
| x::xs -> x :: remove item xs
| [] -> []

let rec contains item = function
| x::xs when x = item -> true
| x::xs -> contains item xs
| [] -> false

// The first way: enumerate through each word in turn, test whether it can be made
let rec canBeMade letters = function
| [] -> true
| x::xs when letters |> contains x -> canBeMade (remove x letters) xs
| _ -> false

let findLetters letters words =
    words
    |> Seq.filter (canBeMade letters)
    |> Seq.map (fun x -> System.String (List.toArray x))

// The second way: construct a trie and test it
type Trie =
| Char of char * Trie list
| Terminal

let rec trie words =
    let firstLetter = function
    | [] -> None
    | x::_ -> Some x
    
    let toTrie = function
    | None, _ -> Terminal
    | Some x, ws -> Char (x, ws |> Seq.map List.tail |> trie)
    
    words
    |> Seq.groupBy firstLetter
    |> Seq.map toTrie
    |> List.ofSeq

let findLettersTrie letters trie =
    let rec find' letters acc trie  =
        match trie with
        | Char (c, ts) when contains c letters ->
            ts
            |> Seq.collect (find' (remove c letters) (c :: acc))
            |> List.ofSeq
        | Char (c, ts) -> []
        | Terminal -> [acc]
    
    trie
    |> Seq.collect (find' letters [])
    |> Seq.map (fun x -> String (List.rev x |> List.toArray))

let compute (letters : string) trie =
    trie
    |> findLettersTrie (letters.ToUpper() |> List.ofSeq)
    |> Seq.groupBy String.length
    |> Seq.sortBy (fst >> (-) 1)
    |> Seq.take 2

