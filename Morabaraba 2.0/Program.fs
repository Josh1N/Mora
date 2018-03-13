open System.Security.Cryptography.X509Certificates

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

(*
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

*)

(*  
    above are the remnants of our second attempt approach, 
    the following code is our next approach where we decided 
    to follow the structure of the TicTacToe example as closely
    as possible while adapting it to the Morabaraba gameplay.
*)



type Cell=  // Black and White being the opposing teams
| Black     // Black will be represented by a 'B' character on the board
| White     // White will be represented by a 'W' character on the board
| Blank     // for positions not occupied by cows on the board 

type GameBoard =    // a record with a field of tuples of tuple for all the valid positions on each row of the board
| Board of (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell*Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)

type State =    // a field for each state
|Placing of GameBoard
|Moving of GameBoard
|Flying of GameBoard
// will also need on for winning and drawing (is it possible to draw in Morabaraba?)

let swapPlayer x=   // how turns will be implemented 
    match x with 
    |Black ->White
    |White ->Black
    |Blank -> Blank //failwith "Nooooooooooooooooooooo" // maybe change this to Blank ]
                    // nope no sense made 
           
let blankBoard =    // creating a Gameboard with all blank positions 
    let blankRow = Blank,Blank, Blank
    let blankRowLong= Blank,Blank, Blank,Blank,Blank, Blank 
    Board (blankRow, blankRow, blankRow, blankRowLong, blankRow,blankRow, blankRow)
    
let printBoard (Board (rowA, rowB, rowC,rowD, rowE,rowF, rowG)) =
    System.Console.Clear ()
    let printCells cell =
        match cell with
        | Black -> 'B'  // why aren't these matching cell with Black and White? 
        | White -> 'W'
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
    printf "G %c--------%c--------%c  \n\n" (printCells x) (printCells y) (printCells z)

let rec available board position =  // uses pattern matching to check if the given position is occupied or not
                                // and if not, whether it is Black or White

    match position with
    |'A', y ->              
        match y with 
        |'1' ->
            match board with
            |Board((Blank,_,_),_,_,_,_,_,_) -> "Blank"
            |Board((Black,_,_),_,_,_,_,_,_) -> "Black"
            |Board((White,_,_),_,_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'4' ->
            match board with
            |Board((_,Blank,_),_,_,_,_,_,_) -> "Blank"
            |Board((_,Black,_),_,_,_,_,_,_) -> "Black"
            |Board((_,White,_),_,_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'7'  ->
            match board with
            |Board((_,_,Blank),_,_,_,_,_,_) -> "Blank"
            |Board((_,_,Black),_,_,_,_,_,_) -> "Black"
            |Board((_,_,White),_,_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
    |'B', y ->
        match y with 
        |'2' ->
            match board with
            |Board(_,(Blank,_,_),_,_,_,_,_) -> "Blank"
            |Board(_,(Black,_,_),_,_,_,_,_) -> "Black"
            |Board(_,(White,_,_),_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'4' ->
            match board with
            |Board(_,(_,Blank,_),_,_,_,_,_) -> "Blank"
            |Board(_,(_,Black,_),_,_,_,_,_) -> "Black"
            |Board(_,(_,White,_),_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'6'  ->
            match board with
            |Board(_,(_,_,Blank),_,_,_,_,_) -> "Blank"
            |Board(_,(_,_,Black),_,_,_,_,_) -> "Black"
            |Board(_,(_,_,White),_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
    |'C', y ->
        match y with 
        |'3' ->
            match board with
            |Board(_,_,(Blank,_,_),_,_,_,_) -> "Blank"
            |Board(_,_,(Black,_,_),_,_,_,_) -> "Black"
            |Board(_,_,(White,_,_),_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'4' ->
            match board with
            |Board(_,_,(_,Blank,_),_,_,_,_) -> "Blank"
            |Board(_,_,(_,Black,_),_,_,_,_) -> "Black"
            |Board(_,_,(_,White,_),_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'5'  ->
            match board with
            |Board(_,_,(_,_,Blank),_,_,_,_) -> "Blank"
            |Board(_,_,(_,_,Black),_,_,_,_) -> "Black"
            |Board(_,_,(_,_,White),_,_,_,_) -> "White"
            //|_ -> failwith "wut"
    |'D', y ->
        match y with 
        |'1' ->
            match board with
            |Board(_,_,_,(Blank,_,_,_,_,_),_,_,_) -> "Blank"
            |Board(_,_,_,(Black,_,_,_,_,_),_,_,_) -> "Black"
            |Board(_,_,_,(White,_,_,_,_,_),_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'2' ->
            match board with
            |Board(_,_,_,(_,Blank,_,_,_,_),_,_,_) -> "Blank"
            |Board(_,_,_,(_,Black,_,_,_,_),_,_,_) -> "Black"
            |Board(_,_,_,(_,White,_,_,_,_),_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'3'  ->
            match board with
            |Board(_,_,_,(_,_,Blank,_,_,_),_,_,_) -> "Blank"
            |Board(_,_,_,(_,_,Black,_,_,_),_,_,_) -> "Black"
            |Board(_,_,_,(_,_,White,_,_,_),_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'5' ->
            match board with
            |Board(_,_,_,(_,_,_,Blank,_,_),_,_,_) -> "Blank"
            |Board(_,_,_,(_,_,_,Black,_,_),_,_,_) -> "Black"
            |Board(_,_,_,(_,_,_,White,_,_),_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'6' ->
            match board with
            |Board(_,_,_,(_,_,_,_,Blank,_),_,_,_) -> "Blank"
            |Board(_,_,_,(_,_,_,_,Black,_),_,_,_) -> "Black"
            |Board(_,_,_,(_,_,_,_,White,_),_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'7'  ->
            match board with
            |Board(_,_,_,(_,_,_,_,_,Blank),_,_,_) -> "Blank"
            |Board(_,_,_,(_,_,_,_,_,Black),_,_,_) -> "Black"
            |Board(_,_,_,(_,_,_,_,_,White),_,_,_) -> "White"
            //|_ -> failwith "wut"
    |'E', y ->
        match y with 
        |'3' ->
            match board with
            |Board(_,_,(Blank,_,_),_,_,_,_) -> "Blank"
            |Board(_,_,(Black,_,_),_,_,_,_) -> "Black"
            |Board(_,_,(White,_,_),_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'4' ->
            match board with
            |Board(_,_,(_,Blank,_),_,_,_,_) -> "Blank"
            |Board(_,_,(_,Black,_),_,_,_,_) -> "Black"
            |Board(_,_,(_,White,_),_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'5'  ->
            match board with
            |Board(_,_,(_,_,Blank),_,_,_,_) -> "Blank"
            |Board(_,_,(_,_,Black),_,_,_,_) -> "Black"
            |Board(_,_,(_,_,White),_,_,_,_) -> "White"
            //|_ -> failwith "wut"
    |'F', y ->
        match y with 
        |'2' ->
            match board with
            |Board(_,(Blank,_,_),_,_,_,_,_) -> "Blank"
            |Board(_,(Black,_,_),_,_,_,_,_) -> "Black"
            |Board(_,(White,_,_),_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'4' ->
            match board with
            |Board(_,(_,Blank,_),_,_,_,_,_) -> "Blank"
            |Board(_,(_,Black,_),_,_,_,_,_) -> "Blank"
            |Board(_,(_,White,_),_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'6'  ->
            match board with
            |Board(_,(_,_,Blank),_,_,_,_,_) -> "Blank"
            |Board(_,(_,_,Black),_,_,_,_,_) -> "Black"
            |Board(_,(_,_,White),_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
    |'G', y ->
        match y with 
        |'1' ->
            match board with
            |Board((Blank,_,_),_,_,_,_,_,_) -> "Blank"
            |Board((Black,_,_),_,_,_,_,_,_) -> "Black"
            |Board((White,_,_),_,_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'4' ->
            match board with
            |Board((_,Blank,_),_,_,_,_,_,_) -> "Blank"
            |Board((_,Black,_),_,_,_,_,_,_) -> "Black"
            |Board((_,White,_),_,_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
        |'7'  ->
            match board with
            |Board((_,_,Blank),_,_,_,_,_,_) -> "Blank"
            |Board((_,_,Black),_,_,_,_,_,_) -> "Black"
            |Board((_,_,White),_,_,_,_,_,_) -> "White"
            //|_ -> failwith "wut"
    |_ -> //printf "not valid"
          "wut"
           
         (* printf "Enter position you want to place a cow on \n"
          let x = (System.Console.ReadLine())
          let playerMove = char(x.[0]), char(x.[1])
          available board playerMove //execute Player board*)
            

let gameCheck board =
    Placing board

let checkMills board=
    let mills = [["A1";"A4";"A7"];["B2";"B4";"B6"];["C3";"C4";"C5"];["D1";"D2";"D3"];["D5";"D6";"D7"];
                ["E3";"E4";"E5"];["F2";"F4";"F6"];["G1";"G4";"G7"];["A1";"D1";"G1"];["B2";"D2";"F2"];
                ["C3";"D3";"E3"];["A4";"B4";"C4"];["E4";"F4";"G4"];["C5";"D5";"E5"];["B6";"D6";"F6"];
                ["A7";"D7";"G7"];["A1";"B2";"C3"];["A7";"B6";"C5"];["G1";"F2";"E3"];["G7";"F6";"E5"]]
    List.choose (fun (x: string list) -> 
                               match available board (x.[0].[0], x.[0].[1]) with
                                | "Black" -> match available board (x.[1].[0], x.[1].[1]) with
                                             | "Black" -> match available board (x.[2].[0], x.[2].[1]) with
                                                          | "Black" -> Some x 
                                                          | _ -> None 
                                             | _ -> None 
                                | "White" -> match available board (x.[1].[0], x.[1].[1]) with
                                             | "White" -> match available board (x.[2].[0], x.[2].[1]) with
                                                          | "White" -> Some x
                                                          | _ -> None 
                                             | _ -> None                           
                                | _ -> None 
                         ) mills 
                   

let move (player: Cell) (Board(rowA, rowB, rowC,rowD, rowE,rowF, rowG)) pos =
    let newBoard =  // has to create a new board with all the same symbols as the previous one 
                    // except for the given position
                    // becuase things are immutable 
        let changeCol column (a,b,c) =  // changes the given position to the character
                                        // of the player whose turn it is 
            match column with            
            |1 -> player, b, c
            |2 -> a,player,c
            |3-> a,b, player
            |_ -> failwith "Invalid move"
        let changeColLong col (a,b,c,d,e,f)=    // does the same for the exception of the row with six positions   
            match col with
            |1 -> player, b, c,d,e,f
            |2 -> a,player,c,d,e,f
            |3-> a,b, player,d,e,f
            |4 -> a,b,c,d,player,f
            |5 -> a,b,c,d,e, player
            |6-> a,b, c,d,e,player
            |_ -> failwith "Invalid move"
        let data =  // uses the name of the position and the above functions to update the position in the row 
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
    gameCheck newBoard  // should probably swap players and call execute again here 
            
let rec execute (Player: Cell)board =
    System.Console.Clear () // nice fresh start 
    printBoard board
    printf "%A's turn to go. Enter position you want to place a cow on \n" Player 
    let x = (System.Console.ReadLine())
    let playerMove = char(x.[0]), char(x.[1])   // takes the input postion and seperates the position name into
                                    // X and Y co-ords
                                    // we need to implement some sort of catch for invalid data
                                    // ~ needs to be only two charaters long
    match available board playerMove with   // to make sure it's a real option and not already occupied 
    |"Blank"-> move Player board playerMove    
    |_-> printf "position taken, pls try another"
         execute Player board // repeats until valid input is given 

let rec executeGame currentPlayer board=
    match execute currentPlayer board with 
    |Placing newBoard -> executeGame(swapPlayer currentPlayer) newBoard
       
[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn " Morabaraba Game Rules \n
    There are three main phases to the game:\n
            * Placing the cows \n
            * Moving the cows \n
            * Flying the cow \n
            For placing Cows: \n
            At the beginning, each player has 12 cows (pieces); one player has Black cows, the other player has\n White cows.The board is empty to start with.
            The player with the Black cows moves first.\n Each turn consists of placing a cow onto the board. Cows can only be placed on empty locations. \n
            Choose a location by entering a CAPITAL letter and number representing the cell you want to place your cow!\n"
    printfn "Black starts the game: press Enter to begin."
   // printBoard blankBoard
    System.Console.ReadLine()

    executeGame Black blankBoard // Black player starts
                        


    System.Console.ReadLine()

    0 // return an integer exit code




