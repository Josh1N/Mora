open System.Security.Cryptography.X509Certificates

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

(*let validPositions = ["A1"; "A4"; "A7"; "B2"; "B4"; "B6"; "C3"; "C4"; "C5"; "D1"; "D2"; "D3"; "D5"; "D6"; "D7"; "E3"; "E4"; "E5"; "F2"; "F4"; "F6"; "G1"; "G4"; "G7"]

let A1 = 'X'
let A4 = 'O'
let A7 = 'O'
let B2 = 'O'
let B4 = 'O'
let B6 = 'O'
let C3 = 'O'
let C4 = 'O'
let C5 = 'O'
let D1 = 'O'
let D2 = 'O'
let D3 = 'O'
let D5 = 'O'
let D6 = 'O'
let D7 = 'O'
let E3 = 'O'
let E4 = 'O'
let E5 = 'O'
let F2 = 'O'
let F4 = 'O'
let F6 = 'O'
let G1 = 'O'
let G4 = 'O'
let G7 = 'O'



let printBoard = 
    printf "  1  2  3  4  5  6  7  \n" 
    printf "A %c--------%c--------%c  \n" 'O' 'O' 'O'
    printf "  | \      |      / |  \n" 
    printf "B |  %c-----%c-----%c  |  \n" 'O' 'O' 'O'
    printf "  |  | \   |   / |  |  \n"
    printf "C |  |  %c--%c--%c  |  |  \n" 'O' 'O' 'O'
    printf "  |  |  |     |  |  |  \n"
    printf "D %c--%c--%c     %c--%c--%c  \n" 'O' 'O' 'O' 'O' 'O' 'O'
    printf "  |  |  |     |  |  |  \n"
    printf "E |  |  %c--%c--%c  |  |  \n" 'O' 'O' 'O'
    printf "  |  | /   |   \ |  |  \n"
    printf "F |  %c-----%c-----%c  |  \n" 'O' 'O' 'O'
    printf "  | /      |      \ |  \n"
    printf "G %c--------%c--------%c  \n" 'O' 'O' 'O'




let mills = [("A1","A4","A7");("B2","B4","B6");("C3","C4","C5");("D1","D2","D3");("D5","D6","D7");("E3","E4","E5");("F2","F4","F6");("G1","G4","G7");("A1","D1","G1");("B2","D2","F2");("C3","D3","E3");("A4","B4","C4");("E4","F4","G4");("C5","D5","E5");("B6","D6","F6");("A7","D7","G7");("A1","B2","C3");("A7","B6","C5");("G1","F2","E3");("G7","F6","E5")]

let updatePosition= 
     A1 ='X'
     printBoard
*)
type Cell=
| Black
| White
| Blank


type GameBoard =
| Board of (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell*Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)

type State = 
|Placing of GameBoard
|Moving of GameBoard
|Flying of GameBoard

let swapPlayer x=
    match x with 
    |Black ->White
    |White ->Black
    |Blank -> failwith "Noooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"




           
let blankBoard =
    let blankRow = Blank,Blank, Blank
    let blankRowLong= Blank,Blank, Blank,Blank,Blank, Blank


 
    Board (blankRow, blankRow, blankRow, blankRowLong, blankRow,blankRow, blankRow)
    
let printBoard (Board (rowA, rowB, rowC,rowD, rowE,rowF, rowG)) =
    System.Console.Clear ()
    let printCells cell =
        match cell with
        | B -> 'B'
        | W -> 'W'
        | Blank -> 'O'

    printf "  1  2  3  4  5  6  7  \n" 
    let (x,y,z)=rowA
    printf "A %c--------%c--------%c  \n" (printCells x) (printCells y) (printCells z)
    printf "  | \      |      / |  \n" 
    let (x,y,z)=rowB
    printf "B |  %c-----%c-----%c  |  \n" (printCells x) (printCells y) (printCells z)
    printf "  |  | \   |   / |  |  \n"
    let (x,y,z)=rowC
    printf "C |  |  %c--%c--%c  |  |  \n" (printCells x) (printCells y) (printCells z)
    printf "  |  |  |     |  |  |  \n"
    let (a,b,c,d,e,f)=rowD
    printf "D %c--%c--%c     %c--%c--%c  \n" (printCells a) (printCells b) (printCells c) (printCells d) (printCells e) (printCells f)
    printf "  |  |  |     |  |  |  \n"
    let (x,y,z)=rowE
    printf "E |  |  %c--%c--%c  |  |  \n" (printCells x) (printCells y) (printCells z)
    printf "  |  | /   |   \ |  |  \n"
    let (x,y,z)=rowF
    printf "F |  %c-----%c-----%c  |  \n" (printCells x) (printCells y) (printCells z)
    printf "  | /      |      \ |  \n"
    let (x,y,z)=rowG
    printf "G %c--------%c--------%c  \n" (printCells x) (printCells y) (printCells z)

let available board position =
    match position with
    |'A', y ->
        match y with 
        |'1' ->
            match board with
            |Board((Blank,_,_),_,_,_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with
            |Board((_,Blank,_),_,_,_,_,_,_) -> true
            |_ -> false
        |'7'  ->
            match board with
            |Board((_,_,Blank),_,_,_,_,_,_) -> true
            |_ -> false
    |'B', y ->
        match y with 
        |'2' ->
            match board with
            |Board(_,(Blank,_,_),_,_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with
            |Board(_,(_,Blank,_),_,_,_,_,_) -> true
            |_ -> false
        |'6'  ->
            match board with
            |Board(_,(_,_,Blank),_,_,_,_,_) -> true
            |_ -> false
    |'C', y ->
        match y with 
        |'3' ->
            match board with
            |Board(_,_,(Blank,_,_),_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with
            |Board(_,_,(_,Blank,_),_,_,_,_) -> true
            |_ -> false
        |'5'  ->
            match board with
            |Board(_,_,(_,_,Blank),_,_,_,_) -> true
            |_ -> false
    |'D', y ->
        match y with 
        |'1' ->
            match board with
            |Board(_,_,_,(Blank,_,_,_,_,_),_,_,_) -> true
            |_ -> false
        |'2' ->
            match board with
            |Board(_,_,_,(_,Blank,_,_,_,_),_,_,_) -> true
            |_ -> false
        |'3'  ->
            match board with
            |Board(_,_,_,(_,_,Blank,_,_,_),_,_,_) -> true
            |_ -> false
        |'5' ->
            match board with
            |Board(_,_,_,(_,_,_,Blank,_,_),_,_,_) -> true
            |_ -> false
        |'6' ->
            match board with
            |Board(_,_,_,(_,_,_,_,Blank,_),_,_,_) -> true
            |_ -> false
        |'7'  ->
            match board with
            |Board(_,_,_,(_,_,_,_,_,Blank),_,_,_) -> true
            |_ -> false
    |'E', y ->
        match y with 
        |'3' ->
            match board with
            |Board(_,_,(Blank,_,_),_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with
            |Board(_,_,(_,Blank,_),_,_,_,_) -> true
            |_ -> false
        |'5'  ->
            match board with
            |Board(_,_,(_,_,Blank),_,_,_,_) -> true
            |_ -> false
    |'F', y ->
        match y with 
        |'2' ->
            match board with
            |Board(_,(Blank,_,_),_,_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with
            |Board(_,(_,Blank,_),_,_,_,_,_) -> true
            |_ -> false
        |'6'  ->
            match board with
            |Board(_,(_,_,Blank),_,_,_,_,_) -> true
            |_ -> false
    |'G', y ->
        match y with 
        |'1' ->
            match board with
            |Board((Blank,_,_),_,_,_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with
            |Board((_,Blank,_),_,_,_,_,_,_) -> true
            |_ -> false
        |'7'  ->
            match board with
            |Board((_,_,Blank),_,_,_,_,_,_) -> true
            |_ -> false
    |_ -> false

let gameCheck board =
    Placing board

let move (player: Cell) (Board(rowA, rowB, rowC,rowD, rowE,rowF, rowG)) pos =
    let newBoard = 
        let changeCol column (a,b,c) = 
            match column with
            |1 -> player, b, c
            |2 -> a,player,c
            |3-> a,b, player
            |_ -> failwith "Invalid move"
        let changeColLong col (a,b,c,d,e,f)=
            match col with
            |1 -> player, b, c,d,e,f
            |2 -> a,player,c,d,e,f
            |3-> a,b, player,d,e,f
            |4 -> a,b,c,d,player,f
            |5 -> a,b,c,d,e, player
            |6-> a,b, c,d,e,player
            |_ -> failwith "Invalid move"
        let data =
            match pos with
            |'A',y ->
                match y with
                |'1' -> changeCol 1 rowA, rowB, rowC,rowD, rowE,rowF, rowG
                |'4' -> changeCol 2 rowA, rowB, rowC,rowD, rowE,rowF, rowG
                |'7' -> changeCol 3 rowA, rowB, rowC,rowD, rowE,rowF, rowG
            |'B',y ->
                match y with
                |'2' -> rowA, changeCol 1  rowB, rowC,rowD, rowE,rowF, rowG
                |'4' -> rowA, changeCol 2  rowB, rowC,rowD, rowE,rowF, rowG
                |'6' -> rowA, changeCol 3  rowB, rowC,rowD, rowE,rowF, rowG
            |'C',y ->
                match y with
                |'3' -> rowA, rowB, changeCol 1  rowC,rowD, rowE,rowF, rowG
                |'4' -> rowA, rowB, changeCol 2  rowC,rowD, rowE,rowF, rowG
                |'6' -> rowA, rowB, changeCol 3  rowC,rowD, rowE,rowF, rowG
            |'D',y ->
                match y with 
                |'1' -> rowA, rowB, rowC, changeColLong 1  rowD, rowE,rowF, rowG
                |'2' -> rowA, rowB, rowC, changeColLong 2  rowD, rowE,rowF, rowG
                |'3' -> rowA, rowB, rowC, changeColLong 3  rowD, rowE,rowF, rowG
                |'5' -> rowA, rowB, rowC, changeColLong 4  rowD, rowE,rowF, rowG
                |'6' -> rowA, rowB, rowC, changeColLong 5  rowD, rowE,rowF, rowG
                |'7' -> rowA, rowB, rowC, changeColLong 6  rowD, rowE,rowF, rowG
            |'E',y ->
                match y with
                |'3' -> rowA, rowB, rowC,rowD, changeCol 1   rowE,rowF, rowG
                |'4' -> rowA, rowB, rowC,rowD, changeCol 2   rowE,rowF, rowG
                |'6' -> rowA, rowB, rowC,rowD, changeCol 3   rowE,rowF, rowG
            |'F',y ->
                match y with
                |'2' -> rowA, rowB, rowC,rowD, rowE, changeCol 1 rowF, rowG
                |'4' -> rowA, rowB, rowC,rowD, rowE, changeCol 2 rowF, rowG
                |'6' -> rowA, rowB, rowC,rowD, rowE, changeCol 3 rowF, rowG
            |'G',y ->
                match y with
                |'1' ->  rowA, rowB, rowC,rowD, rowE,rowF,changeCol 1 rowG
                |'4' ->  rowA, rowB, rowC,rowD, rowE,rowF,changeCol 2 rowG
                |'7' ->  rowA, rowB, rowC,rowD, rowE,rowF,changeCol 3 rowG
        Board data
    gameCheck newBoard  
            
let rec execute (Player: Cell)board =
    System.Console.Clear ()
    printBoard board
    printf "%A' s turn to go. Enter position you want to place a cow on \n"Player 
    let x = (System.Console.ReadLine())
    let playerMove = x.[0], x.[1]
    match available board playerMove with 
    |true-> move Player board playerMove    
    |_-> execute Player board

let rec executeGame currentPlayer board=
    match execute currentPlayer board with 
    |Placing newBoard -> executeGame(swapPlayer currentPlayer) newBoard




 


        
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    executeGame Black blankBoard

    System.Console.ReadLine()
    0 // return an integer exit code
