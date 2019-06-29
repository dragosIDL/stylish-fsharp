(*
    12-1 Concatenating collections
    You come across the following code, which adds some new transations to an existing collection of transactions. 
    It seems to be a bottleneck in your system.
*)

type Transaction = { Id: int }

let addTransactions oldTransactions newTransactions =
    oldTransactions @ newTransactions
(*
    Assuming that the old and new transaction collections don't have to be F# lists, 
    how could you speed up the system with minimal code changes?
*)
let addTransactionsFaster oldTransactions newTransactions =
    Array.append oldTransactions newTransactions

(*
    12-2 Speeding up filtering
    A colleague suggests that you could speed up the following code by mapping to the distance in parallel, then filtering. 
    (At the time of writing there is no Array.Parallel.filter function, which is why you'd have to map first.)
*)

type Float3 = float32 * float32 * float32

let withinRadius radius (here:Float3) coords = 
    let distance x1 y1 z1 x2 y2 z2 = 
        let dx = x1 - x2
        let dy = y1 - y2
        let dz = z1 - z2
        dx * dx + dy * dy + dz * dz |> sqrt
    
    let x1, y1, z1 = here
    
    coords
    |> Array.Parallel.map (fun (x2, y2, z2) -> distance x1 y1 z1 x2 y2 z2)
    |> Array.filter (fun d -> d <= radius)

(*
    Would you expect this to improve performance? Why/why not?
    Solution: 
        Parallel option will be slower because a new array will be generated. 
        This also depends on the complexity of the mapped method
*)

(*
    12-3 Changing the approach to csv generation
    How could you change the code below so that the entire 2D array is mapped into string representations of the numbers
    in one step, and only then converted into CSV lines?
*)

open System
open System.Text

let private buildLine data = 
    let cols =
        data 
        |> Array.Parallel.map (sprintf "%f")
    String.Join (",", cols)

let buildCsv data = 
    let sb = StringBuilder()
    for r in [0..(Array2D.length1 data) - 1] do
        let row = data.[r, *]
        let rowString = row |> buildLine
        sb.AppendLine rowString |> ignore
    sb.ToString()    

let buildCsv1 (data:float[,]) = 

    let strings = data |> Array2D.map (sprintf "%f")

    [| for r in [0..(Array2D.length1 strings) - 1] do yield strings.[r, *] |]
    |> Array.Parallel.map (fun cols -> String.Join (",", cols))
    |> Array.fold (fun (sb : StringBuilder) c -> sb.AppendLine c) (StringBuilder())
    |> fun x -> x.ToString() 

let testData = Array2D.init 1000 1000 (fun a b -> (float a + float b))


printfn "first one"
#time
let a = buildCsv testData
#time "off"

printfn "second one one"
#time
let b = buildCsv1 testData
#time "off"