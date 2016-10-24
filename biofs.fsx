#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#r "packages/NUnit/lib/net40/nunit.framework.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
// open Bio.deBruijn
open Bio.Euler
// open Bio.DegreeMeasure
#load "./Ba3.fs"
open Ba3.Ba3f
open FParsec
open System.IO

open Bio.deBruijn
open Bio.EulerPath
let pdbruijn = intWs .>>. pDNAD
// let solveDbg (k, dna) = kmerl k dna |> grpDBG |> List.map dedupeTup |> showTups

open Ba3.Ba3g

// Ba3gMainf "data/ch3/dataset_203_6.txt"
// Ba3gMain ()

// Ba3gMain ()

// let inp = """4
// AAGATTCTCTAAGA"""
let inp_ = """GAGG
CAGG
GGGG
GGGA
CAGG
AGGG
GGAG"""

let inp = """4
CTTA
ACCA
TACC
GGCT
GCTT
TTAC"""

// 1. Parse
let recon = intWs .>>. strandsD
let (Success((k, dnas), _, _)) = run (recon) inp
// let (Success((res), _, _)) = run (pdbruijn) inp

// 2. construct deBruijn graph from kmers
// - convert 3-lists to 3-tuples
// dnas |> grpDBG |> List.map dedupeTup
let lTo3Tup = function
    | [x; y; z] -> x, y, z
    | other -> failwithf "Should pass len 3 list; not %A" other
let third (_, _, c) = c

let res1 = dnas
           |> grpDBG
        //    |> List.map (dedupeTup >> (lTo3Tup *** id))
           |> List.map (dedupeTup >> (lTo3Tup *** (List.map lTo3Tup)))
    // |> List.map (lTo3Tup *** (List.map lTo3Tup)) |> eulerPath

// 3. convert path to cycle map
let em', xEdge = toCycle res1
let path, _ = eulerCycle em'

let prependHead (xss: 'a list list) (x: 'a) =
    match xss with
    | h :: rst -> (x :: h) :: rst
    | _ -> failwith "Can't prependHead to empty list!"

/// Given a sequence of tuples, most should overlap with the
/// preceding and succeeding tuple. Join these together. Otherwise,
/// keep non-overlapping tuples in their own entries
let rec trySplice_ dnas prev accum =
    let _, b, c = prev
    match dnas with
    | (a', b', c') :: rst ->
        if (a', b') = (b, c) then trySplice_ rst (a', b', c') (prependHead accum c')
        else trySplice_ rst (a', b', c') ([c'; b'; a'] :: accum)
    | _ -> accum |> List.map List.rev

let trySplice (path: ('a * 'a * 'a) List) =
    let path' = path.Tail
    let h = path.Head
    trySplice_ path'.Tail h [[path'.Head |> third]] |> List.concat

trySplice path
// TODO: convert 3-tuple to n-list usage
let path' = path.Tail
let h = path.Head
h


path'.Head |> third
path.Tail
// let getIMap map =
//     map |> Map.toSeq // |> Seq.fold
// let s = getIMap em'
// Seq.head s


let collectVals (m: Map<'a, Set<'a * 'a>>) =
    let rec addEdges (elst: ('a * 'a) list) (s: Set<'a>) =
        match elst with
        | (e1, e2) :: rst -> addEdges rst s + Set.ofList [e1; e2]
        | _ -> s

    let rec collect (elst: ('a * Set<'a * 'a>) list) (s: Set<'a>) =
        match elst with
        | (k, edgeSet) :: rst -> collect rst (addEdges (Set.toList edgeSet) (s.Add(k)))
        | _ -> s
    collect (Map.toList m) Set.empty

/// Get all keys and values in a map and map them to integers
let toImap map =
    let l = map |> Map.toList |> List.length
    List.zip (collectVals map |> Set.toList) [1..l]
    |> Map.ofList

let map2int (m: Map<'a, Set<'a * 'a>>)=
    let imap = toImap m
    let rec mapEdgeSet (xsAccum: (int * int) list) (xs: ('a * 'a) list) =
        match xs with
        | (a, b) :: rst ->
            mapEdgeSet ((imap.Item(a), imap.Item(b)) :: xsAccum) rst
        | _ -> Set.ofList xsAccum

    m |> Map.toList
      |> List.map (fun (k, vset) -> (imap.Item k, vset |> Set.toList |> mapEdgeSet []))

map2int em'

// TODO: now have int representations; see what goes wrong in cycles

toImap em'
let valMap em =
    collectVals em





let addEdges (ee: Set<'a * 'a>) (s: Set<'a>) =
    Seq.fold (fun (sacc: Set<'a>) (e: Edge<'a>) -> addEdgeNodes e sacc)
        Set.empty ee


let _, ss = Seq.head s
Seq.fold (fun (sacc: Set<'a>) (e: Edge<'a>) -> addEdgeNodes e sacc) Set.empty ss

// Seq.fold (fun (sacc: Set<'a>) ((t: 'a), (ts: Set<Edge<'a>>)) ->
//             addEdgeNodes sacc).Add(t)) Set.empty s

eulerPath res1
let t1, t2 = res1.Head
t1
// Prev
// let (Success((em), _, _)) = run (pEulCycMap) inp
// let (Success((tl), _, _)) = run (pEulCyc) inp

// run pEulCycLine "14 -> 3"  // Success: (14, [3])
// run pEulCycLine "14 -> 30,23"  // Success: (14, [30; 23])
// run pEulCyc inp
