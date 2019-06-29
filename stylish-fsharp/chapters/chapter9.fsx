(*
     9-1 Functions as arguments
     Define another functoin called meltiply that multiplies its arguments. Can it be used by applyAndPrint?
*)

let add a b = a + b
let applyAndPrint f a b = 
    printfn "%i" (f a b)

let multiply a b = a * b

applyAndPrint multiply 10 20

(*
What if you want to send in a function to substract its second input from its first? 
Is it possible to do this without defining a named function called something like substract?
*)
applyAndPrint (-) 10 30

(*
    9-2 Functions returning functions
    Define another function called rangeCounter that returns a function that generates numbers in a circular pattern 
    between a specified ranged, for example: 3, 4, 5, 6, 3, 4, 5, 6, 3...
*)

let rangeCounter min max = 
    let nrs = [min..max]
    let mutable index = -1

    fun () -> 
        index <- (index + 1)
        nrs.[index % nrs.Length]

let counter = rangeCounter 3 6
let values = [1..10] |> List.map (fun _ -> counter())

(*
    9-3 Partial Application
    The code below shows a fuction featureScale that "normalizes" a dataset, so that all the values fall into a 
    specified range. The scale funciton calls fuaetureScale to normalize a dataset into the range 0..1.
*)

let featureScale a b xMin xMax x = 
    a + ((x - xMin) * (b - a)) / (xMin - xMax)

let scale (data: seq<float>) =
    let minX = data |> Seq.min
    let maxX = data |> Seq.max
    
    let zeroOneScale x =
        featureScale 0. 1. minX maxX x

    data
    |> Seq.map zeroOneScale

scale [100.; 150.; 200.;]

(*
    9-4 Function composition
    You have a list of functions, each of which takes a float argument and returns another float, like this:
    let pipeline = 
        [ fun x -> x * 2.
          fun x -> x * x
          fun x -> x - 99.9 ]

    The list is non-empty but otherwise can have any length.
    How would you write a function applyAll that can take such a list of functions and apply them all, making the 
    result of the first function and feeding it into the second, taking the result of that and feeding it into the 
    third function, and so forth, until a final result is produced? Your function should be callable like this:

    let applyAll (p: (float -> float) list) 
*)

let applyAll (p: (float -> float) list) x =
    p |> List.fold (fun state current -> current state) x


let applyAll1 (p: (float -> float) list) =
    p |> List.reduce (>>)


let pipeline = 
    [ fun x -> x * 2.
      fun x -> x * x
      fun x -> x - 99.9 ]

let x = applyAll pipeline 100. //39900.1
let y = applyAll1 pipeline 100. //39900.1