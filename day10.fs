module day10

open System.IO

let input = 
    File.ReadAllLines(@"Files\10.txt")
    |> Array.map int64
    |> Array.sort


let part1() =
    let differences =
        input
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> (a, b-a))
    
    let numDifferencesBy num =
        1 + (differences |> Seq.filter (fun (_, b) -> b = num) |> Seq.length)
        
    (numDifferencesBy 1L) * (numDifferencesBy 3L)

#nowarn "40"
open System.Collections.Generic

let part2() = 

    let setInput = input |> Set.ofArray

    let memoize f =
        let cache = new Dictionary<_, _>()
        let memoizedFunction x =
            match cache.TryGetValue(x) with
            | (true, v) -> v
            | _ -> 
                let v = f(x) 
                cache.Add(x, v)
                v
        memoizedFunction

    let maxnum = Seq.max input
    let rec countLeaves = memoize (fun n ->
        if n = maxnum then
            1L
        elif not (setInput.Contains n) then
            0L
        else
            (countLeaves (n+1L)) + (countLeaves (n+2L)) + (countLeaves (n+3L))
    )

    countLeaves 0L