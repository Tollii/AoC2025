open System.IO

Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

let containsRepeatingSequence1 (input: uint64) =
    let str = string input
    let middleIndex = str.Length / 2
    let firstHalf = str[0 .. middleIndex - 1]
    firstHalf = str[middleIndex..]

let containsRepeatingSequence2 (input: uint64) =
    let str = string input
    let len = str.Length

    let rec check span =
        match span with
        | s when s > len / 2 -> false
        | s when len % s <> 0 -> check (span + 1)
        | _ ->
            let pattern = str[0 .. span - 1]
            String.replicate (len / span) pattern = str || check (span + 1)

    len > 1 && check 1

let input =
    File.ReadLines "input.txt"
    |> Seq.exactlyOne
    |> (fun line -> line.Split ',')
    |> Array.map (fun s -> s.Split '-')
    |> Array.map (fun s -> [ (uint64 s[0]) .. (uint64 s[1]) ])

let partOneAnswer =
    input
    |> Array.map (List.filter containsRepeatingSequence1)
    |> Array.filter (fun ids -> ids.Length > 0)
    |> Array.map List.sum
    |> Array.sum

let partTwoAnswer =
    input
    |> Array.map (List.filter containsRepeatingSequence2)
    |> Array.filter (fun ids -> ids.Length > 0)
    |> Array.map List.sum
    |> Array.sum
