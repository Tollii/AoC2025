open System.IO

Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

type Direction =
    | Left
    | Right

let parse (line: string) =
    match line[0] with
    | 'L' -> Left, (int line[1..])
    | 'R' -> Right, (int line[1..])
    | _ -> failwith "Invalid input"

let wrap value = ((value % 100) + 100) % 100

let moveLeft amount dial =
    let newPosition = wrap (dial - amount)
    let zeroPasses =
        match dial with
        | 0 -> amount / 100
        | dial when dial > amount -> 0
        | _ -> 1 + (amount - dial) / 100

    newPosition, zeroPasses

let moveRight amount dial =
    let newPosition = wrap (dial + amount)
    let zeroPasses = ((dial + amount) / 100)
    newPosition, zeroPasses

let result =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map parse
    |> List.map (fun (direction, amount) ->
        match direction with
        | Left -> moveLeft amount
        | Right -> moveRight amount)
    |> List.scan (fun (newPosition, _) move -> move newPosition) (50, 0)

let partOneAnswer =
    result
    |> List.filter (fun (position, _) -> position = 0)
    |> List.length

let partTwoAnswer = result |> List.sumBy snd

printfn "Password 1 is: %d" partOneAnswer
printfn "Password is: %d" partTwoAnswer
