(* A sudoku solver in 30 lines - optimised for terseness, not readability! *)

let tilesDistinct = List.choose id >> (fun vs -> vs |> List.length = (vs |> Set.ofList |> Set.count))

let squares = List.map (List.chunkBySize 3) >> List.chunkBySize 3 >> List.map List.transpose >> List.concat >> List.map List.concat

let isValid board =
    let rowsValid = List.forall tilesDistinct
    rowsValid board && rowsValid (List.transpose board) && rowsValid (squares board)

let rec updateItem index update =
    function
    | x::xs when index = 0 -> (update x)::xs
    | x::xs -> x::(updateItem (index - 1) update xs)
    | _ -> failwith "Out of bounds"

let fillInValid boards (i, j) =
    let fillAt row col value = updateItem row (updateItem col (fun _ -> Some value))
    boards |> Seq.collect (fun board -> [1..9] |> Seq.map (fun v -> fillAt i j v board)) |> Seq.filter isValid

let solveBoard board =
    board
    |> Seq.mapi (fun i -> Seq.mapi (fun j -> function None -> Some (i, j) | Some _ -> None))
    |> Seq.concat
    |> Seq.choose id
    |> Seq.fold fillInValid (Seq.singleton board)

(* Parsing and printing *)

let parseBoard (s : string) =
    let parseTile = function ' ' -> None | c -> Some (int c - 48)
    s.Split '\n' |> Seq.map (Seq.map parseTile >> List.ofSeq) |> List.ofSeq

let printBoard board =
    let printTile = function None -> ' ' | Some v -> char (v + 48)
    let printRow = Seq.map printTile >> Array.ofSeq >> System.String
    board |> Seq.map printRow |> String.concat "\n" |> printfn "%s"

let testBoard =
    "     6  1\n" +
    " 2  5 7  \n" +
    "5  12  6 \n" +
    "      6 3\n" +
    "  28 49  \n" +
    "6 7      \n" +
    " 5  19  8\n" +
    "  1 8  4 \n" +
    "8  2     "
    |> parseBoard

testBoard |> solveBoard |> Seq.head |> printBoard
