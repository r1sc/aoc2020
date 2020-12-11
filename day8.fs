module day8

open System.IO

let input = File.ReadAllLines("Files/8.txt")

type Instruction = { Opcode: string; Displacement: int; RunCount: int}
let instructions =
    input
    |> Seq.map (fun line -> 
        let opcodeDisp = line.Split(" ")
        { Opcode = opcodeDisp.[0]; Displacement = int opcodeDisp.[1]; RunCount = 0 }
    )
    |> Seq.indexed
    |> Map.ofSeq

type RunResult = Looped | Ended
let rec execute acc pc instructions =
    match instructions |> Map.tryFind pc with
    | None -> (Ended, acc)
    | Some instruction ->
        if instruction.RunCount > 0 then 
            (Looped, acc)
        else
            instructions
            |> Map.add pc { instruction with RunCount = instruction.RunCount+1 }
            |> match (instruction.Opcode, instruction.Displacement) with
                | ("acc", value) -> execute (acc+value) (pc+1)
                | ("nop", _) -> execute acc (pc+1)
                | ("jmp", displacement) -> execute acc (pc+displacement)
                | _ -> failwith "Invalid opcode"

let patch instruction = 
    match instruction.Opcode with
    | "nop" -> { instruction with Opcode = "jmp" }
    | "jmp" -> { instruction with Opcode = "nop" }
    | _ -> instruction

let part1() = execute 0 0 instructions
let part2() = 
    seq {
        for (pc, instruction) in Map.toSeq instructions do
            instructions
            |> Map.add pc (patch instruction)
            |> execute 0 0
    } 
    |> Seq.find (fun (x, _) -> x = Ended)