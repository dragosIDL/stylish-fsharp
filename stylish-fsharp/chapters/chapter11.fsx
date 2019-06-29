(*
    11-1 Reproducing maperror
    Can you implement a passThroughRejects function, with the same behavior as the build-in mapError function?
*)

type Outcome<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

let adapt func = function
| Success x -> func x
| Failure f -> Failure f

let passThrough func = function
| Success x -> func x |> Success
| Failure x -> Failure x

let passThroughRejects func = function
| Success x -> Success x 
| Failure x -> func x |> Failure 
 