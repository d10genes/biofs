namespace Bio

open FParsec
open System.IO

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

module Type =
    type Nuke = A | C | G | T
    type DNA = Nuke list
    let readDna = function
        | 'A' -> A
        | 'C' -> C
        | 'G' -> G
        | 'T' -> T
        | _   -> failwith "This isn't a valid nucleotide"

    let showDna = function
        | A -> "A"
        | C -> "C"
        | G -> "G"
        | T -> "T"

    let dl2str dna = dna |> List.map showDna |> String.concat ""
    let ds2str dna = dna |> Seq.map showDna |> String.concat ""

module Parse =
    open Type
    let ws = spaces
    let int = pint32
    let nl = pchar '\n'

    let intWs : Parser<int32,unit> = pint32 .>> ws

    let nucleotide: (CharStream<unit> -> Reply<char>) = anyOf "ACGT"
    let nucleotideD = nucleotide |>> readDna
    let pDNA = many nucleotide
    let pDNAD = many1 nucleotideD .>> (opt nl)
    let strands = ws >>. (sepBy1 pDNA nl)
    let strandsD = ws >>. many pDNAD // .>> ('\n' |> pchar |> opt)
    // let strandsD = ws >>. (sepBy1 pDNAD nl) // .>> ('\n' |> pchar |> opt)
    let nums: Parser<int32 list,unit> = sepBy1 int (pchar ' ')


    let str s = pstring s
    let sepList : Parser<_> = between (str "[") (str "]") (sepBy pint32 (str ";"))

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


module Utils =
    let keyF f k v = f k
    let valF f k v = f v
    let curry f a b = f (a,b)
    let uncurry f (a,b) = f a b
    let (&&&) f g x = f x, g x
    let ( *** ) (f: 'a -> 'b) (g: 'c -> 'd) (x, y) = (f x, g y)

    let recon xss =
        let fst = xss |> List.head
        let rst = xss |> List.tail |> List.map List.last
        fst @ rst

    let kmers = Seq.windowed
    let kmerl k xs = Seq.windowed k xs |> Seq.map List.ofArray
    // let rec init = function
    //     | [h] -> []
    //     | h :: tl -> h :: init tl
    //     | _ -> failwith "Empty list."

    let appFname fn suff =
        let fbase = Path.GetFileNameWithoutExtension fn
        let fext = Path.GetExtension fn
        let fdir = Path.GetDirectoryName fn
        let fn2 = String.concat "" [| fbase; suff; fext |]
        Path.Combine(fdir, fn2)

    let readWrite pathin parser f =
        printfn "Hi"
        let pathout = appFname pathin ".out"
        let textin = File.ReadAllText(pathin)
        match run parser textin with
        | Success(parsed, _, _) ->
            let res = f parsed
            File.WriteAllText(pathout, res)
            printfn "Answer written to %s" pathout
        | Failure _ -> failwith "Invalid parse"

    let rec cartesian = function
        | [] -> Seq.singleton []
        | L::Ls -> cartesian Ls |> Seq.collect (fun c -> L |> Seq.map (fun x->x::c))

    let init lst =
        let rec loop revAcc = function
            | [] -> failwith "empty list"
            | hd::tl ->
                match tl with
                | [] -> List.rev revAcc
                | _ -> loop (hd::revAcc) tl
        loop [] lst

    let lastElement ls = List.reduce (fun _ i -> i) ls

    let zip l1 l2 =
        let rec zip' l1 l2 accum =
            match l1 with
            | [] -> accum
            | x1 :: l1s ->
                match l2 with
                | [] -> accum
                | x2 :: l2s -> zip' l1s l2s ((x1, x2) :: accum)
        zip' l1 l2 []


module deBruijn =
    open Type
    open Utils

    let presuf x = (init x, List.tail x)
    let grpDBG kms =
        kms |> List.ofSeq |> List.map presuf
        |> List.sort |> List.groupBy fst

    let dedupeTup x = (fst &&& (snd >> List.map snd)) x
    let showTup k = k |> (dl2str *** (List.map dl2str >> String.concat ","))
                    |> (fun (x, y) -> String.concat " -> " [|x; y|])
    let showTups ks = List.map showTup ks |> String.concat "\n"

module Euler =
    open Parse
    let (><) f a b = f b a

    type Edge<'a> = ('a * 'a)
    /// List of nodes and the nodes that they point to
    type OutList<'a> = ('a * 'a list) list
    type EdgeList<'a> = Edge<'a> list
    type EdgeSet<'a when 'a: comparison> = Set<Edge<'a>>
    type EdgeMap<'a when 'a: comparison> = Map<'a, Set<Edge<'a>>>

    let src ((s, _): Edge<'a>): 'a = s
    let snk ((_, k): Edge<'a>): 'a  = k

    // let (p: Parser<_> ) = sepBy int (pchar ',')
    let pEulCycLine : Parser<_> = int .>> pstring " -> " .>>. (sepBy int (pchar ','))
    let pEulCyc = sepEndBy1 pEulCycLine nl

    let unrollEmap (emap: EdgeMap<'a>): EdgeSet<'a> = emap |> Map.toSeq |> Seq.map snd |> Seq.reduce Set.union
    let emptyEmap (emap:EdgeMap<'a>): bool = emap |> unrollEmap |> Set.isEmpty
    let nEdges em = em |> unrollEmap |> Set.count

    let tupsL2eM (xs: ('a * 'a list) list): EdgeMap<'a> =
        let tupL2Em (h, ys) =
            (h, (Set.map (fun x -> (h, x)) (Set.ofList ys)))
        xs |> List.map tupL2Em |> Map.ofList

    let tupsL2eL (xs: ('a * 'a list) list) : EdgeList<'a> =
        let rec tupL2El = function
            | h, (y :: ys) -> (h, y) :: tupL2El (h, ys)
            | h, [] -> []
        List.collect tupL2El xs

    let pEulCycMap = pEulCyc |>> tupsL2eM


    /// Clear keys with corresponding empty sets.
    let cleanMap (em: EdgeMap<'a>): EdgeMap<'a> = Map.filter (fun _ y -> y |> Set.isEmpty |> not)  em

    /// Extract all sources from edges in map values.
    let em2Src (emap: EdgeMap<'a>): Set<'a> =
        emap |> Map.toList |> List.collect (snd >> (Set.map fst) >> Set.toList) |> Set.ofList

    let unitePairs xs = Set.union (Set.map fst xs) (Set.map snd xs)

    let chooseNodeGen f edgeSet =
        let allNodes = unitePairs edgeSet
        List.ofSeq allNodes |> f

    let chooseNode x = chooseNodeGen List.min x

    let chooseEdge (edgeCands: EdgeSet<'a>): Option<Edge<'a>> =
        match Set.toList edgeCands with
        | y :: _ -> Some y
        | [] -> None

    let remove (emap: EdgeMap<'a>) (start, fin): EdgeMap<'a> =
        let edges = emap.Item start
        Map.add start (edges.Remove (start, fin)) emap

    let addEdge (emap: EdgeMap<'a>) (start, fin): EdgeMap<'a> =
        match emap.TryFind start with
        | Some edges ->  Map.add start (edges.Add (start, fin)) emap
        | None ->  Map.add start (Set.ofList [(start, fin)]) emap

    let randCycle (start: 'a option) (emap: EdgeMap<'a>) =
        printfn "%d edges" (nEdges emap)

        let startNode = defaultArg start (unrollEmap emap |> chooseNode)
        let rec randCycleR (em: EdgeMap<'a>) path node =
            match em.TryFind node |> ((><) defaultArg) Set.empty
                |> chooseEdge with
            | Some (edge: Edge<'a>) ->
                randCycleR (remove em edge) (src edge:: path) (snk edge)
            | None -> (node :: path, em)
        let retpath, retem = randCycleR emap [] startNode
        (List.rev retpath), retem
    // let em = tupsL2eM rawvals2

    let rec splice subl = function
        | [] -> []
        | x :: xs ->
            if x = List.head subl
            then subl @ xs
            else x :: splice subl xs

    let eulerCycle (emap_: EdgeMap<'a>) =
        let rec whileCycle path emap  =
            if emptyEmap emap
            then path, emap
            else
                let path', emap' = randCycle None emap
                whileCycle (splice path' path) emap'
        let rp, re = randCycle None emap_
        whileCycle rp re

    module EulerTest =
        open NUnit.Framework
        [<Test>]
        let emTest = [
                (0, set [(0, 3)]);
                (1, set [(1, 0); (1, 5)]); (2, set [(2, 1); (2, 4)]);
                (3, set [(3, 2)]); (4, set [(4, 2)]); (5, set [(5, 1)])] |> Map.ofList
        let pTest, _ = eulerCycle emTest
        let ``this should be cycle``() =
            Assert.AreEqual(pTest, [0; 3; 2; 4; 2; 1; 5; 1; 0])

module EulerPath =
    module DegreeMeasure =
        open Euler
        open Utils

        let rec length = function
            | [] -> 0
            | _ :: rst -> 1 + length rst

        let slen xs = xs |> Set.toList |> length

        let revTup (a, b) = (b, a)
        let revEl el =
            el |> tupsL2eL
            |> List.map revTup
            |> List.sortBy fst |> List.groupBy fst
            |> List.map (fun (f, xs) -> (f, List.map snd xs))

        let revMap (tl: OutList<'a>) = tl |> revEl |> tupsL2eM

        let nEdges (n, edges) = slen edges

        /// Union of keys of 2 maps
        let allKs (m1: EdgeMap<'a>) (m2: EdgeMap<'a>) =
            let keys m = m |> Map.toSeq |> Seq.map fst |> Set.ofSeq
            keys m1 + keys m2

        let inOutK (m1: EdgeMap<'a>) (m2: EdgeMap<'a>) (k: 'a) =
            let getSize (m: EdgeMap<'a>) =
                defaultArg (m.TryFind k) Set.empty |> slen
            // (m1.TryFind k) |> slen, (m1.Item k) |> slen
            getSize m1, getSize m2

        /// Compare degrees of all nodes in 2 edgemaps
        let inOutAll (m1: EdgeMap<'a>) (m2: EdgeMap<'a>) =
            allKs m1 m2 |> Set.toList
            |> List.map (id &&& (inOutK m1 m2))
            |> Map.ofList

        /// Given an outlist, return a map with out- and in-degree
        /// of each node
        let inAndOut (tl: OutList<'a>) =
            let em1 = tupsL2eM tl
            let em2 = revMap tl
            inOutAll em1 em2

        let imbalancedNodes degMap =
            degMap
            // |> Map.map (fun _ (x, y) -> x - y)
            |> Map.map (valF (uncurry (-)))
            // |> Map.filter (fun _ x -> x <> 0)
            |> Map.filter (valF ( (<>) 0))

    open DegreeMeasure
    open Euler

    let toCycle tl =
        let em = tupsL2eM tl
        let getUnevens xs =
            xs |> inAndOut |> imbalancedNodes
            |> Map.toList |> List.sortBy snd
        let unevens =
            match (getUnevens tl) with
            | (endn, _) :: [(startn, _)] -> (endn, startn)
            | _ -> failwith "Should have 2 uneven nodes"
        addEdge em unevens, unevens

    let unsplicePath (path: 'a list) ((endN, startN): Edge<'a>) =
        let rec unsplicePathR (path_: 'a list) accumRev =
            match path_ with
            | a :: b :: rst ->
                if (a = endN && b = startN)
                then List.rev accumRev, rst
                else unsplicePathR (b :: rst) (a :: accumRev)
            | _ -> failwith "Nodes not found"
        let xs, ys = unsplicePathR path []
        startN :: ys @ xs.Tail @ [endN]

    let eulerPath tl =
        let em', xEdge = toCycle tl
        let path, _ = eulerCycle em'
        unsplicePath path xEdge

