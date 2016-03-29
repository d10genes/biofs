namespace Ba3

open Bio.Parse
open Bio.Type
open Bio.Utils
open FParsec

module Ba3a =
    let compositionK k = 
        kmers k >> Set.ofSeq
    let compositionKshow k = 
        compositionK k >> Set.map ds2str >> String.concat "\n"
    let pcomp = int_ws .>>. pDNAD

    let ba3a_main pathin pathout =
        let sin = System.IO.File.ReadAllText(pathin)
        match run pcomp sin with
        | Success((k, dna), _, _) ->
            let res = compositionKshow k dna
            System.IO.File.WriteAllText(pathout, res)
            printfn "Answer written to %s" pathout
        | Failure _ -> failwith "Invalid parse"


    // compositionK k dna |> Set.map ds2str |> String.concat "\n"
        
    // compositionKprob k dna  