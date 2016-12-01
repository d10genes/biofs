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
    let solveDbg dnas = dnas |> grpDBG |> List.map dedupeTup |> showTups
    let Ba3e_main () = readWrite "data/ch3/rosalind_ba3e.txt" strandsD (solveDbg)


module Ba3f =
    // solve Eulerian cycle
    open Bio.Euler

    let solveEulerCycle em =
        let path, _ = eulerCycle em
        System.String.Join("->", (List.map (fun i -> i.ToString()) path))

    let Ba3fMain () = readWrite "data/ch3/rosalind_ba3fa.txt" pEulCycMap (solveEulerCycle)
    let Ba3fMainFn fn = readWrite fn pEulCycMap (solveEulerCycle)


/// Solve Eulerian path by first converting to and then
/// from a cycle
module Ba3g =
    open Bio.Euler
    open Bio.EulerPath
    let solveEulerPath tl =
        let path = eulerPath tl
        System.String.Join("->", (List.map (fun i -> i.ToString()) (path)))


    // let Ba3gMain () = readWrite "data/ch3/rosalind_ba3g.txt" pEulCycMap (solveEulerPath)
    let Ba3gMain () = readWrite "data/ch3/rosalind_ba3g.txt" pEulCyc (solveEulerPath)
    let Ba3gMainf fn = readWrite fn pEulCyc (solveEulerPath)

/// String reconstruction problem
/// Basically, genomePath(EulerianPath(DeBruijnGraph(kmers)))
module Ba3h =
    open Bio.deBruijn
    open Bio.EulerPath
    let reconP = intWs .>>. strandsD

    let stringRecon dnas =
        dnas |> grpDBG |> List.map dedupeTup
        |> eulerPath
        |> recon

    let solveReconstruction (_, dnas:seq<Nuke list>) =
        stringRecon dnas
        |> List.map showDna |> String.concat ""

    let Ba3hMainf fn = readWrite fn reconP solveReconstruction

// K Universal string
module Ba3i =
    open Bio.kUniversal
    let kP = intWs
    let solveKUniversalString k =
        k2circ k |> List.map (fun x -> x.ToString()) |> String.concat ""
    let Ba3iMainf fn = readWrite fn kP solveKUniversalString
    // Ba3iMainf "data/ch3/universal_string.16.txt"

module Ba3j =
    open Bio.GapPatterns
    let solveGappedPatterns ((k, d), strands) =
        let fsts = List.map fst strands
        let snds = List.map snd strands
        let res = stitchZip fsts snds k d
        res |> ch2str
    let Ba3jMainf fn = readWrite fn gappedPatP solveGappedPatterns
    // TODO: check parser type error; perform on rosalind set
    // Ba3jMainf "data/ch3/dataset_6206_7.txt"


    // Note: parsing issue; must cut off trailing newline with
    // `perl -pi -e 'chomp if eof' $fn`