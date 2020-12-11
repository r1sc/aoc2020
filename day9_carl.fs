module day9_carl

open System

let solveFor (input: string) =
    let numbers = 
        input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int64
        |> Seq.toList

    let (_, firstInvalid) = 
        numbers
        |> Seq.skip 25
        |> Seq.indexed
        |> Seq.filter (fun (i, n) -> 
            numbers
            |> Seq.skip i
            |> Seq.take 25
            |> Seq.collect (fun n1 ->
                numbers
                |> Seq.skip i
                |> Seq.take 25
                |> Seq.filter ((<>) n1)
                |> Seq.map ((+) n1)
            )
            |> Seq.contains n
            |> not
        )
        |> Seq.head

    let aggregateWhile seed func predicate =
        let rec loop result source =
            match source with
            | [] -> result
            | element::rest ->
                let nextResult = func result element
                if not (predicate nextResult) then
                    result
                else
                    loop nextResult rest
    
        loop seed

    let weakness = 
        seq {
            for i1 in [0..numbers.Length-1] do
                numbers
                |> Seq.skip i1
                |> Seq.toList
                |> aggregateWhile {| Min = Int64.MaxValue; Max = 0L; Sum = 0L |} 
                    (fun a n -> {| Min = min a.Min n; Max = max a.Max n; Sum = a.Sum + n |}) 
                    (fun a -> a.Sum <= firstInvalid)            
        }
        |> Seq.filter (fun x -> x.Sum = firstInvalid)
        |> Seq.map (fun x -> x.Min + x.Max)
        |> Seq.head

    (firstInvalid, weakness)


let part1 input =
    let (firstInvalid, _) = solveFor input
    firstInvalid

let part2 input =
    let (_, weakness) = solveFor input
    weakness