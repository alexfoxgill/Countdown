module Main

type Expr =
| Num of int
| Add of Expr * Expr
| Sub of Expr * Expr
| Mul of Expr * Expr
| Div of Expr * Expr

let rec eval = function
| Num x     -> x
| Add (x,y) -> eval x + eval y
| Sub (x,y) -> eval x - eval y
| Mul (x,y) -> eval x * eval y
| Div (x,y) -> eval x / eval y

let rec write = function
| Num x     -> x.ToString()
| Add (x,y) -> "(" + write x + "+" + write y + ")"
| Sub (x,y) -> "(" + write x + "-" + write y + ")"
| Mul (x,y) -> "(" + write x + "*" + write y + ")"
| Div (x,y) -> "(" + write x + "/" + write y + ")"

let rec powerset = function
| [] -> [[]]
| x::xs -> List.fold (fun list xs -> (x::xs)::xs::list) [] (powerset xs);

let combinations n set =
    let rec combinations' acc size set = seq {
        match size, set with 
        | n, x::xs -> 
            if n > 0 then yield! combinations' (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations' acc n xs 
        | 0, [] -> yield acc 
        | _     -> () }
    combinations' [] n set

let toPair list =
    match list with
    | x::y::[] -> x,y
    | _ -> failwith "no"

let rec generateOperators (x,y) =
    if eval x < eval y then generateOperators (y, x)
    else seq {
        let x', y' = eval x, eval y
        yield Add (x, y)
        yield Mul (x, y)
        yield Sub (x, y)
        if y' <> 0 && x' % y' = 0 then
            yield Div (x, y)
}

let rec condense set =
    match set with
    | x::[] -> seq { yield x }
    | _ -> set
        |> combinations 2
        |> Seq.map (fun pair -> toPair pair, Set.ofList set - Set.ofList pair |> Set.toList)
        |> Seq.collect (fun (pair, list) -> generateOperators pair |> Seq.map (fun op -> op :: list))
        |> Seq.collect condense

let compute target list =
    list
    |> List.map Num
    |> powerset
    |> Seq.collect condense
    |> Seq.map (fun x -> write x, eval x)
    |> Seq.find (fun (x, e) -> e = target)

[<EntryPoint>]
let main args =
    let result = compute 745 [50; 100; 75; 7; 8; 4]
    0