module biofs
open Bio.Parse

[<EntryPoint>]
let main argv =
    printfn "Hi"
    
    printfn "%A" argv
    0 // return an integer exit code
