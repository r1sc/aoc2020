module day9

open System.IO

let input = 
    File.ReadAllLines("Files\\9.txt")
    |> Seq.map int64

let sumAvailable list =
    Seq.allPairs list list 
    |> Seq.map (fun (a, b) -> a + b)

let validNumbers (nums: int64 list) =
    match nums |> List.rev with
    | first::rest -> 
        if sumAvailable rest |> Seq.contains first then
            None
        else
            Some first
    | _ -> None

let part1() =
    input
    |> Seq.windowed (25 + 1)
    |> Seq.map Seq.toList
    |> Seq.map validNumbers
    |> Seq.pick id

let traverse f list =
    let rec aux acc toRead =
        match toRead with
        | [] -> None
        | x::xs ->
            let newAcc = x::acc
            if f newAcc then
                Some newAcc
            else
                aux newAcc xs
    aux [] list

let findContinous num (list: int64 list) =
    seq {
        for i in [0..List.length list] do
            list.[i..]
            |> traverse (fun l -> (List.sum l) = num)
    }

let part2 numberFromPart1 =
    input
    |> Seq.toList
    |> findContinous numberFromPart1
    |> Seq.tryPick id
    |> function
        | Some lst -> 
            let largest = lst |> Seq.max
            let smallest = lst |> Seq.min
            smallest + largest
        | None -> failwith "Invalid number not found"