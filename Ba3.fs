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
    let pcomp = intWs .>>. pDNAD

    let ba3a_main pathin pathout =
        let sin = System.IO.File.ReadAllText(pathin)
        match run pcomp sin with
        | Success((k, dna), _, _) ->
            let res = compositionKshow k dna
            System.IO.File.WriteAllText(pathout, res)
            printfn "Answer written to %s" pathout
        | Failure _ -> failwith "Invalid parse"

module Ba3b =
    let ppath = strandsD

    let recon xss =
        let fst = xss |> List.head
        let rst = xss |> List.tail |> List.map List.last
        fst @ rst

    let Ba3b_main () = readWrite "data/ch3/path.txt" ppath (recon >> dl2str)

module Ba3c =
    let pref = init
    let suff = List.tail
    let matchIxs xs ys =
        seq {for i, x in List.indexed xs do
                for j, y in List.indexed ys
                    do if y = x then yield i, j }
    let ff f g xs = (List.map f xs, List.map g xs)
    let mkAlst dnas =
        let prefs, suffs = (ff pref suff) dnas
        // let prefs, suffs = (pref &&& suff) dnas
        let ixs = matchIxs suffs prefs
        let dnaa = Array.ofList dnas
        let alst = [ for i, j in ixs -> dnaa.[i], dnaa.[j] ]
        alst

    let show_alst alst =
        String.concat "\n"
            [ for x, y in alst
                -> sprintf "%s -> %s" (dl2str x) (dl2str y) ]

    let Ba3c_main () = readWrite "data/ch3/rosalind_ba3c.txt"
                        strandsD (mkAlst >> show_alst)

module Ba3d =
    // Construct De Bruijn Graph from DNA string
    open Bio.deBruijn
    let pdbruijn = intWs .>>. pDNAD

    // let solveDbg  = grpDBG vals |> List.map dedupeTup |> showTups
    let solveDbg (k, dna) = kmerl k dna |> grpDBG |> List.map dedupeTup |> showTups

    let Ba3d_main () = readWrite "data/ch3/rosalind_ba3d2.txt" pdbruijn (solveDbg)


module Ba3e =
    // Construct De Bruijn Graph from kmers
    open Bio.deBruijn

    // let solveDbg  = grpDBG vals |> List.map dedupeTup |> showTups
    let solveDbg dnas = dnas |> grpDBG |> List.map dedupeTup |> showTups

    let Ba3e_main () = readWrite "data/ch3/rosalind_ba3e.txt" strandsD (solveDbg)


module Ba3f =
    open Bio.Euler

    let solveEulerCycle em =
        let path, _ = eulerCycle em
        System.String.Join("->", (List.map (fun i -> i.ToString()) path))

    let Ba3fMain () = readWrite "data/ch3/rosalind_ba3fa.txt" pEulCycMap (solveEulerCycle)

