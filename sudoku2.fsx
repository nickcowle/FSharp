(* A sudoku solver in 30 lines - optimised for terseness, not readability! *)

let tilesDistinct = List.choose id >> (fun vs -> vs |> List.length = (vs |> Set.ofList |> Set.count))

let squares = List.map (List.chunkBySize 3) >> List.chunkBySize 3 >> List.map List.transpose >> List.concat >> List.map List.concat

let isValid board =
    let rowsValid = List.forall tilesDistinct
    rowsValid board && rowsValid (List.transpose board) && rowsValid (squares board)

let toPartialBoard (crs, cr, pr, prs) =
    let rec mapAndAppend f xs ys = match xs with [] -> ys | (x::xs) -> mapAndAppend f xs ((f x)::ys)
    mapAndAppend (List.map Some) crs ((mapAndAppend Some cr pr)::prs)

let rec solveBoard =
    function
    | crs, [], [], [] -> Seq.singleton (List.rev crs)
    | crs, [], [], pr::prs -> solveBoard (crs, [], pr, prs)
    | crs, cr, [], prs -> solveBoard ((List.rev cr)::crs, [], [], prs)
    | crs, cr, (Some i::pr), prs -> solveBoard (crs, i::cr, pr, prs)
    | crs, cr, (None::pr), prs ->
        {1..9}
        |> Seq.map (fun i -> crs, i::cr, pr, prs)
        |> Seq.filter (toPartialBoard >> isValid)
        |> Seq.collect solveBoard

(* Parsing and printing *)

let parseBoard (s : string) =
    let parseTile = function ' ' -> None | c -> Some (int c - 48)
    s.Split '\n' |> Seq.map (Seq.map parseTile >> List.ofSeq) |> List.ofSeq
    |> (fun prs ->  [], [], [], prs)

let printBoard zipper =
    let printTile = function None -> ' ' | Some v -> char (v + 48)
    let printRow = Seq.map printTile >> Array.ofSeq >> System.String
    zipper |> (List.map (List.map Some)) |> Seq.map printRow |> String.concat "\n" |> printfn "%s"

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
