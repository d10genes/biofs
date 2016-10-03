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
    let (&&&) f g x = f x, g x
    let ( *** ) (f: 'a -> 'b) (g: 'c -> 'd) (x, y) = (f x, g y)

    let kmers = Seq.windowed
    let kmerl k xs = Seq.windowed k xs |> Seq.map List.ofArray
    let rec init = function
        | [h] -> []
        | h :: tl -> h :: init tl
        | _ -> failwith "Empty list."

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

    let tupsL2eM (xs: ('a * 'a list) list): EdgeMap<'a> =
        let tupL2Em (h, ys) =
            (h, (Set.map (fun x -> (h, x)) (Set.ofList ys)))
        xs |> List.map tupL2Em |> Map.ofList
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

    let randCycle (start: 'a option) (emap: EdgeMap<'a>) =
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