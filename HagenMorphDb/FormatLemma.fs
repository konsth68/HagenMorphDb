namespace HagenMorphDb


open System.Linq
open System.Collections.Generic
open System


type NounString =
    {
        N : string
        G : string
        D : string
        A : string
        I : string
        P : string
    
        Npl : string
        Gpl : string
        Dpl : string
        Apl : string
        Ipl : string
        Ppl : string
    }

type AdjString =
    {
        Nm : string
        Gm : string
        Dm : string
        Am : string
        Im : string
        Pm : string
    
        Nf : string
        Gf : string
        Df : string
        Af : string
        If : string
        Pf : string

        Nn : string
        Gn : string
        Dn : string
        An : string
        In : string
        Pn : string

        Npl : string
        Gpl : string
        Dpl : string
        Apl : string
        Ipl : string
        Ppl : string
    }
    
type IpfPresentString =
    {
        P1sg : string
        P2sg : string
        P3sg : string

        P1pl : string
        P2pl : string
        P3pl : string

        //Inf : string
    }

type IpfPasseString =
    {
        P123sg : string
        P123pl : string
    }

type IpfFutureString =
    {
        P1sg : string
        P2sg : string
        P3sg : string

        P1pl : string
        P2pl : string
        P3pl : string

        //Inf : string
    }

type IpfParticiplePresentString  =  
    {
        Nm : string
        Gm : string
        Dm : string
        Am : string
        Im : string
        Pm : string
    
        Nf : string
        Gf : string
        Df : string
        Af : string
        If : string
        Pf : string

        Nn : string
        Gn : string
        Dn : string
        An : string
        In : string
        Pn : string

        Npl : string
        Gpl : string
        Dpl : string
        Apl : string
        Ipl : string
        Ppl : string
    }
    
type IpfParticiplePastString  =  
    {
        Nm : string
        Gm : string
        Dm : string
        Am : string
        Im : string
        Pm : string
    
        Nf : string
        Gf : string
        Df : string
        Af : string
        If : string
        Pf : string

        Nn : string
        Gn : string
        Dn : string
        An : string
        In : string
        Pn : string

        Npl : string
        Gpl : string
        Dpl : string
        Apl : string
        Ipl : string
        Ppl : string
    }

type VerbString =
    {
        IpfPresent : IpfPresentString
        IpfPasse : IpfPasseString
        IpfFuture : IpfFutureString
        IpfParticiplePresent : IpfParticiplePresentString
        IpfParticiplePast : IpfParticiplePastString
    }

type VerbStringArray =
    {
        IpfPresentArray : string[][]
        IpfPasseArray : string[][]
        IpfFutureArray : string[][]
        IpfParticiplePresentArray : string[][] 
        IpfParticiplePastArray : string[][]
    }

type FormatStingLemma =
    | Noun of NounString
    | Adjective of AdjString
    | Verb of VerbString
    | Over
    
type FormatStringLemmaArray =
    | NounArray of string[][]
    | AdjectiveArray of string[][]
    | VerbArray of VerbStringArray 
    | OverArray

type FormatStringDict =
    {
        Pos : PosTag
       
        NounStringDict : Dictionary<string,string[]>
        AdjStringDict : Dictionary<string,string[]>
        IpfPresentStrinDict : Dictionary<string,string[]>
        IpfPastStringDict : Dictionary<string,string[]>
        IpfFutureStringDict : Dictionary<string,string[]>
        IpfParticiplePresentStringDict : Dictionary<string,string[]>
        IpfParticiplePastStringDict : Dictionary<string,string[]>
    }

           
module FormatLemma =

    let printPosTag (p :PosTag) =    
        match p with
        |PosTag.None              ->  ""  
        |PosTag.Noun              ->  "m//f" 
        |PosTag.Adjective         ->  "adj" 
        |PosTag.Verb              ->  "ipf"  
        |PosTag.Adverb            ->  ""  
        |PosTag.Numeral           ->  "num" 
        |PosTag.Participle        ->  "" 
        |PosTag.Transgressive     ->  ""  
        |PosTag.Pronoun           ->  "pron adj"  
        |PosTag.Preposition       ->  ""  
        |PosTag.Conjunction       ->  "" 
        |PosTag.Particle          ->  "" 
        |PosTag.Interjection      ->  "" 
        |PosTag.Predicative       ->  "" 
        |PosTag.Parenthesis       ->  "" 
        | _ -> "un " + (string (int p))
    
    let printGenderTag (g :GenderTag) =    
        match g with
        |GenderTag.None              ->  ""
        |GenderTag.Masculine         ->  "m "
        |GenderTag.Feminine          ->  "f "
        |GenderTag.Neuter            ->  "n "
        |GenderTag.Common            ->  "c "
        | _ -> "un " + (string (int g))
    
    let printNumberTag (n :NumberTag) =    
        match n with
        |NumberTag.None              ->  ""
        |NumberTag.Singular          ->  "sg "
        |NumberTag.Plural            ->  "pl "
        | _ -> "un " + (string (int n))
        
    let printCaseTag (c :CaseTag) =   
        match c with
        |CaseTag.None              ->  ""
        |CaseTag.Nominative        ->  "N "
        |CaseTag.Genitive          ->  "G "
        |CaseTag.Dative            ->  "D "
        |CaseTag.Accusative        ->  "A "
        |CaseTag.Instrumental      ->  "I "
        |CaseTag.Prepositional     ->  "P "
        |CaseTag.Locative          ->  "L "
        |CaseTag.Partitive         ->  "Par "
        |CaseTag.Vocative          ->  "V "
        |CaseTag.Counting          ->  ""
        | _ -> "un " + (string (int c))
    let printTenseTag (t :TenseTag) =
        match t with
        |TenseTag.None               ->  ""
        |TenseTag.Present            ->  "Présent "
        |TenseTag.Past               ->  "Passé "
        |TenseTag.Future             ->  "Future "
        |TenseTag.Infinitive         ->  "Infinitive "
        | _ -> "un " + (string (int t))
        
    let printPersonTag (p :PersonTag) =
        match p with
        |PersonTag.None              ->  ""
        |PersonTag.First             ->  "1 "
        |PersonTag.Second            ->  "2 "
        |PersonTag.Third             ->  "3 "
        | _ -> "un " + (string (int p))
        
    let printAnimalTag (a :AnimalTag) =
        match a with
        |AnimalTag.None               ->  ""
        |AnimalTag.Animated           ->  "qn "
        |AnimalTag.Inanimate          ->  "qch "
        | _ -> "un " + (string (int a))
        
    let printMorph (m :Morphology) =
        //let pos = printPosTag m.PosTag
        let gen = printGenderTag m.GenderTag
        let num = printNumberTag m.NumberTag
        let cas = printCaseTag m.CaseTag
        let ten = printTenseTag m.TenseTag
        let per = printPersonTag m.PersonTag
        let ani = printAnimalTag m.AnimalTag
        
        let str = $"{gen}{num}{cas}{ten}{per}{ani}"
                
        str
    
    let flatWordRec (wm :WordRec) =
        let r = seq
                    {
                    yield wm.WordItem
                    for sw in wm.SubWordItems do
                        yield sw
                    }
        r    
    
    let flatLemmaRec (lm :LemmaRec) =
        let r = seq
                    {
                    yield lm.LemmaItem
                    for lw in lm.LemmaSubWords do
                        yield lw
                    for wm in lm.WordItems do
                        yield! flatWordRec wm
                    }
        r
    
    let testPosTag (pos: PosTag) (wt: WordInfo) =
        (wt.Morph.PosTag = pos || pos = PosTag.Any)
    
    let testGenderTag (gen: GenderTag) (wt: WordInfo) =
        (wt.Morph.GenderTag = gen || gen = GenderTag.Any)
    
    let testNumberTag (num: NumberTag) (wt: WordInfo) =
        (wt.Morph.NumberTag = num || num = NumberTag.Any)

    let testCaseTag (cas: CaseTag) (wt: WordInfo) =
        (wt.Morph.CaseTag = cas || cas = CaseTag.Any)
            
    let testTenseTag (ten: TenseTag) (wt: WordInfo) =
        (wt.Morph.TenseTag = ten || ten = TenseTag.Any)   
        
    let testPersonTag (per: PersonTag) (wt: WordInfo) =
        (wt.Morph.PersonTag = per || per = PersonTag.Any)
    
    let testAnimalTag (ani: AnimalTag) (wt: WordInfo) =
        (wt.Morph.AnimalTag = ani || ani = AnimalTag.Any)
        
    (* More readable version 
    let testPosTag (pos: PosTag) (wt: WordInfo) =
        let a = wt.Morph.PosTag = pos
        let b = wt.Morph.PosTag = PosTag.Any
        let r = a || b
        r

    let testGenderTag (gen: GenderTag) (wt: WordInfo) =
        let a = wt.Morph.GenderTag = gen
        let b = wt.Morph.GenderTag = GenderTag.Any
        let r = a || b
        r
        
    let testNumberTag (num: NumberTag) (wt: WordInfo) =
        let a = wt.Morph.NumberTag = num
        let b = wt.Morph.NumberTag = NumberTag.Any
        let r = a || b
        r

    let testCaseTag (cas: CaseTag) (wt: WordInfo) =
        let a = wt.Morph.CaseTag = cas
        let b = wt.Morph.CaseTag = CaseTag.Any
        let r = a || b
        r
    let testTenseTag (ten: TenseTag) (wt: WordInfo) =
        let a = wt.Morph.TenseTag = ten
        let b = wt.Morph.TenseTag = TenseTag.Any
        let r = a || b
        r
    let testPersonTag (per: PersonTag) (wt: WordInfo) =
        let a = wt.Morph.PersonTag = per
        let b = wt.Morph.PersonTag = PersonTag.Any
        let r = a || b
        r
    let testAnimalTag (ani: AnimalTag) (wt: WordInfo) =
        let a = wt.Morph.AnimalTag = ani
        let b = wt.Morph.AnimalTag = AnimalTag.Any
        let r = a || b
        r
    *)
            
    let getWordWithMorph  (pos :PosTag) (gen :GenderTag) (num :NumberTag)
                          (cas :CaseTag) (ten :TenseTag) (per :PersonTag)
                          (ani :AnimalTag) (wiq :WordInfo seq) =   
            wiq 
                |> Seq.filter (fun w -> testPosTag pos w)  
                |> Seq.filter (fun w -> testGenderTag gen w) 
                |> Seq.filter (fun w -> testNumberTag num w) 
                |> Seq.filter (fun w -> testCaseTag cas w) 
                |> Seq.filter (fun w -> testTenseTag ten w) 
                |> Seq.filter (fun w -> testPersonTag per w) 
                |> Seq.filter (fun w -> testAnimalTag ani w) 
            
    
    let formatString (morf :string) (word :string) =
        $"<| {morf} |>  <# {word} #>"    
        
    let formatTagString (wiq :WordInfo seq) = 
        match wiq.Count() with
        | x when x = 0 -> "-"
        | x when x = 1 ->
                let w = wiq.First()
                formatString (printMorph w.Morph) w.Word 
        | x when x > 1 ->
                wiq 
                |> Seq.map (fun w -> formatString (printMorph w.Morph) w.Word)
                |> String.concat ", "
                
        | _ -> "-" 
        
    let makeNounString (lm: LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        
        let ns :NounString =
            {
                N = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Singular CaseTag.Nominative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                G = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Singular CaseTag.Genitive TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                D = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Singular CaseTag.Dative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                A = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Singular CaseTag.Accusative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                I = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Singular CaseTag.Instrumental TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Singular CaseTag.Prepositional TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
                Npl = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Plural CaseTag.Nominative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
                Gpl = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Plural CaseTag.Genitive TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
                Dpl = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Plural CaseTag.Dative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Apl = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Plural CaseTag.Accusative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ipl = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Plural CaseTag.Instrumental TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ppl = getWordWithMorph PosTag.Noun GenderTag.Any NumberTag.Plural CaseTag.Prepositional TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
            }
        ns
        
    let makeNounStringArray (lm: LemmaRec)=
        let ns = makeNounString lm
        [|
           [|ns.N ; ns.Npl |];
           [|ns.G ; ns.Gpl |];
           [|ns.D ; ns.Dpl |];
           [|ns.A ; ns.Apl |];
           [|ns.I ; ns.Ipl |];
           [|ns.P ; ns.Ppl |]       
        |]                 
    
    let makeNounStringDict (lm :LemmaRec) =
        let ns = makeNounString lm
        
        let dic = Dictionary<string,string[]>()
        dic.Add("N",[|ns.N ; ns.Npl |])
        dic.Add("G",[|ns.G ; ns.Gpl |])
        dic.Add("D",[|ns.D ; ns.Dpl |])
        dic.Add("A",[|ns.A ; ns.Apl |])
        dic.Add("I",[|ns.I ; ns.Ipl |])
        dic.Add("P",[|ns.P ; ns.Ppl |])
        
        dic.Add("Type",[|"Noun"|])

        dic
    

    let makeAdjString (lm: LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        
        let adj :AdjString =
            {
                Nm = getWordWithMorph PosTag.Adjective GenderTag.Masculine NumberTag.Singular CaseTag.Nominative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gm = getWordWithMorph PosTag.Adjective GenderTag.Masculine NumberTag.Singular CaseTag.Genitive TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dm = getWordWithMorph PosTag.Adjective GenderTag.Masculine NumberTag.Singular CaseTag.Dative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Am = getWordWithMorph PosTag.Adjective GenderTag.Masculine NumberTag.Singular CaseTag.Accusative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Im = getWordWithMorph PosTag.Adjective GenderTag.Masculine NumberTag.Singular CaseTag.Instrumental TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pm = getWordWithMorph PosTag.Adjective GenderTag.Masculine NumberTag.Singular CaseTag.Prepositional TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
                Nf = getWordWithMorph PosTag.Adjective GenderTag.Feminine NumberTag.Singular CaseTag.Nominative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gf = getWordWithMorph PosTag.Adjective GenderTag.Feminine NumberTag.Singular CaseTag.Genitive TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Df = getWordWithMorph PosTag.Adjective GenderTag.Feminine NumberTag.Singular CaseTag.Dative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Af = getWordWithMorph PosTag.Adjective GenderTag.Feminine NumberTag.Singular CaseTag.Accusative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                If = getWordWithMorph PosTag.Adjective GenderTag.Feminine NumberTag.Singular CaseTag.Instrumental TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pf = getWordWithMorph PosTag.Adjective GenderTag.Feminine NumberTag.Singular CaseTag.Prepositional TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
                Nn = getWordWithMorph PosTag.Adjective GenderTag.Neuter NumberTag.Singular CaseTag.Nominative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gn = getWordWithMorph PosTag.Adjective GenderTag.Neuter NumberTag.Singular CaseTag.Genitive TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dn = getWordWithMorph PosTag.Adjective GenderTag.Neuter NumberTag.Singular CaseTag.Dative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                An = getWordWithMorph PosTag.Adjective GenderTag.Neuter NumberTag.Singular CaseTag.Accusative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                In = getWordWithMorph PosTag.Adjective GenderTag.Neuter NumberTag.Singular CaseTag.Instrumental TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pn = getWordWithMorph PosTag.Adjective GenderTag.Neuter NumberTag.Singular CaseTag.Prepositional TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
                Npl = getWordWithMorph PosTag.Adjective GenderTag.Any NumberTag.Plural CaseTag.Nominative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gpl = getWordWithMorph PosTag.Adjective GenderTag.Any NumberTag.Plural CaseTag.Genitive TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dpl = getWordWithMorph PosTag.Adjective GenderTag.Any NumberTag.Plural CaseTag.Dative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Apl = getWordWithMorph PosTag.Adjective GenderTag.Any NumberTag.Plural CaseTag.Accusative TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ipl = getWordWithMorph PosTag.Adjective GenderTag.Any NumberTag.Plural CaseTag.Instrumental TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ppl = getWordWithMorph PosTag.Adjective GenderTag.Any NumberTag.Plural CaseTag.Prepositional TenseTag.Any PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
            }                
        adj
        
    let makeAdjStringArray (lm :LemmaRec) =
        let adj = makeAdjString lm
        [|
           [| adj.Nm; adj.Nf; adj.Nn; adj.Npl |];
           [| adj.Gm; adj.Gf; adj.Gn; adj.Dpl |];
           [| adj.Dm; adj.Df; adj.Dn; adj.Dpl |];
           [| adj.Am; adj.Af; adj.An; adj.Apl |];
           [| adj.Im; adj.If; adj.In; adj.Ipl |];
           [| adj.Pm; adj.Pf; adj.Pn; adj.Ppl |]       
        |]                 
     
    let makeAdjStringDict (lm :LemmaRec) =
        let adj = makeAdjString lm
        
        let dic = Dictionary<string,string[]>()
        dic.Add("N",[| adj.Nm; adj.Nf; adj.Nn; adj.Npl |])
        dic.Add("G",[| adj.Gm; adj.Gf; adj.Gn; adj.Dpl |])
        dic.Add("D",[| adj.Dm; adj.Df; adj.Dn; adj.Dpl |])
        dic.Add("A",[| adj.Am; adj.Af; adj.An; adj.Apl |])
        dic.Add("I",[| adj.Im; adj.If; adj.In; adj.Ipl |])
        dic.Add("P",[| adj.Pm; adj.Pf; adj.Pn; adj.Ppl |])   

        dic.Add("Type",[|"Adj"|])
        
        dic
         
    let makeIpfPresentString (lm :LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        
        let ipfPr :IpfPresentString =
            {
                P1sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Present PersonTag.First AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P2sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Present PersonTag.Second AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P3sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Present PersonTag.Third AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString

                P1pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Present PersonTag.First AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P2pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Present PersonTag.Second AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P3pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Present PersonTag.Third AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString

                //Inf = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Any CaseTag.Any TenseTag.Infinitive PersonTag.Any AnimalTag.Any flatWorInfoSeq
                //                     |> formatTagString
            }                
        ipfPr
    
    let makeIpfPresentStringArray (lm :LemmaRec) =
        let ipfPr = makeIpfPresentString lm
        [|
           [| ipfPr.P1sg; ipfPr.P1pl |];
           [| ipfPr.P2sg; ipfPr.P2pl |];
           [| ipfPr.P3sg; ipfPr.P3pl |]
        |]
    
    let makeIpfPresentStrinDict (lm :LemmaRec) =
        let ipfPr = makeIpfPresentString lm
        
        let dic = Dictionary<string,string[]>()
        dic.Add("P1",[| ipfPr.P1sg; ipfPr.P1pl |])
        dic.Add("P2",[| ipfPr.P2sg; ipfPr.P2pl |])
        dic.Add("P3",[| ipfPr.P3sg; ipfPr.P3pl |])
        //dic.Add("Inf",[| ipfPr.Inf |])
        
        dic.Add("Type",[|"Ipf"; "Present"|])


        dic


    let makeIpfPasseString (lm :LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        let ipfPasse : IpfPasseString =
            {
                P123sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P123pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
            }
        ipfPasse
    let makeIpfPasseStringArray (lm :LemmaRec) =
        let ipfPasse = makeIpfPasseString lm
        [|
           [| ipfPasse.P123sg; ipfPasse.P123pl |]
        |]
    
    let makeIpfPasseStrinDict (lm :LemmaRec) =
        let ipfPasse = makeIpfPasseString lm
        
        let dic = Dictionary<string,string[]>()
        dic.Add("P123",[| ipfPasse.P123sg; ipfPasse.P123pl |])

        dic.Add("Type",[|"Ipf"; "Pass"|])
        
        dic

    let makeIpfFutureString (lm :LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        let ipfFut : IpfFutureString =
            {
                P1sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Future PersonTag.First AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P2sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Future PersonTag.Second AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P3sg = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Singular CaseTag.Any TenseTag.Future PersonTag.Third AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString

                P1pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Future PersonTag.First AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P2pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Future PersonTag.Second AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                P3pl = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Plural CaseTag.Any TenseTag.Future PersonTag.Third AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString

                //Inf = getWordWithMorph PosTag.Verb GenderTag.Any NumberTag.Any CaseTag.Any TenseTag.Infinitive PersonTag.Any AnimalTag.Any flatWorInfoSeq
                //                     |> formatTagString
            }                
        ipfFut
        
    let makeIpfFutureStringArray (lm :LemmaRec) =
        let ipfFut = makeIpfFutureString lm
        [|
           [| ipfFut.P1sg; ipfFut.P1pl |];
           [| ipfFut.P2sg; ipfFut.P2pl |];        
           [| ipfFut.P3sg; ipfFut.P3pl |]
        |]
    
    let makeIpfFutureStrinDict (lm :LemmaRec) =
        let ipfFut = makeIpfFutureString lm
        
        let dic = Dictionary<string,string[]>()
        dic.Add("P1",[| ipfFut.P1sg; ipfFut.P1pl |])
        dic.Add("P2",[| ipfFut.P2sg; ipfFut.P2pl |])
        dic.Add("P3",[| ipfFut.P3sg; ipfFut.P3pl |])
        
        dic.Add("Type",[|"Ipf"; "Fut"|])

        dic
    
    let makeIpfParticiplePresentString (lm :LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        let ipfPartPr : IpfParticiplePresentString =
            {
                Nm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Nominative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Genitive TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Dative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Am = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Accusative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Im = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Instrumental TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Prepositional TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq                                     
                                     |> formatTagString
                                     
                Nf = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Nominative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gf = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Genitive TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Df = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Dative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Af = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Accusative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                If = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Instrumental TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pf = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Prepositional TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                                     
                Nn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Nominative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Genitive TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Dative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                An = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Accusative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                In = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Instrumental TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Prepositional TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                                     
                Npl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Nominative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gpl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Genitive TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dpl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Dative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Apl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Accusative TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ipl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Instrumental TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ppl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Prepositional TenseTag.Present PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
            }                
        ipfPartPr
        
    let makeIpfParticiplePresentStringArray (lm :LemmaRec) =
        let ipfPartPr = makeIpfParticiplePresentString lm
        [|
           [| ipfPartPr.Nm; ipfPartPr.Nf; ipfPartPr.Nn; ipfPartPr.Npl |];
           [| ipfPartPr.Gm; ipfPartPr.Gf; ipfPartPr.Gn; ipfPartPr.Gpl |]; 
           [| ipfPartPr.Dm; ipfPartPr.Df; ipfPartPr.Dn; ipfPartPr.Dpl |];
           [| ipfPartPr.Am; ipfPartPr.Af; ipfPartPr.An; ipfPartPr.Apl |];
           [| ipfPartPr.Im; ipfPartPr.If; ipfPartPr.In; ipfPartPr.Ipl |];
           [| ipfPartPr.Pm; ipfPartPr.Pf; ipfPartPr.Pn; ipfPartPr.Ppl |]
        |]
    
    let makeIpfParticiplePresentStrinDict (lm :LemmaRec) =
        let ipfPartPr = makeIpfParticiplePresentString lm

        let dic = Dictionary<string,string[]>()
        dic.Add("N",[| ipfPartPr.Nm; ipfPartPr.Nf; ipfPartPr.Nn; ipfPartPr.Npl |])
        dic.Add("G",[| ipfPartPr.Gm; ipfPartPr.Gf; ipfPartPr.Gn; ipfPartPr.Gpl |])
        dic.Add("D",[| ipfPartPr.Dm; ipfPartPr.Df; ipfPartPr.Dn; ipfPartPr.Dpl |])
        dic.Add("A",[| ipfPartPr.Am; ipfPartPr.Af; ipfPartPr.An; ipfPartPr.Apl |])
        dic.Add("I",[| ipfPartPr.Im; ipfPartPr.If; ipfPartPr.In; ipfPartPr.Ipl |])
        dic.Add("P",[| ipfPartPr.Pm; ipfPartPr.Pf; ipfPartPr.Pn; ipfPartPr.Ppl |])

        dic.Add("Type",[|"Partic"; "Present"|])

        dic
        

    let makeIpfParticiplePastString (lm :LemmaRec) =
        let flatWorInfoSeq = flatLemmaRec lm
        let ipfPartPast : IpfParticiplePastString =
            {
                Nm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Nominative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Genitive TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Dative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Am = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Accusative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Im = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Instrumental TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pm = getWordWithMorph PosTag.Participle GenderTag.Masculine NumberTag.Singular CaseTag.Prepositional TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq                                     
                                     |> formatTagString
                
                Nf = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Nominative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gf = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Genitive TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Df = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Dative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Af = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Accusative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                If = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Instrumental TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pf = getWordWithMorph PosTag.Participle GenderTag.Feminine NumberTag.Singular CaseTag.Prepositional TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                
                Nn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Nominative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Genitive TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Dative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                An = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Accusative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                In = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Instrumental TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Pn = getWordWithMorph PosTag.Participle GenderTag.Neuter NumberTag.Singular CaseTag.Prepositional TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                
                Npl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Nominative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Gpl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Genitive TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Dpl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Dative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Apl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Accusative TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ipl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Instrumental TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString
                Ppl = getWordWithMorph PosTag.Participle GenderTag.Any NumberTag.Plural CaseTag.Prepositional TenseTag.Past PersonTag.Any AnimalTag.Any flatWorInfoSeq
                                     |> formatTagString    
            }                
        ipfPartPast
        
    let makeIpfParticiplePastStringArray (lm :LemmaRec) =
        let ipfPartPast = makeIpfParticiplePastString lm
        [|
           [| ipfPartPast.Nm; ipfPartPast.Nf; ipfPartPast.Nn; ipfPartPast.Npl |];
           [| ipfPartPast.Gm; ipfPartPast.Gf; ipfPartPast.Gn; ipfPartPast.Gpl |]; 
           [| ipfPartPast.Dm; ipfPartPast.Df; ipfPartPast.Dn; ipfPartPast.Dpl |];
           [| ipfPartPast.Am; ipfPartPast.Af; ipfPartPast.An; ipfPartPast.Apl |];
           [| ipfPartPast.Im; ipfPartPast.If; ipfPartPast.In; ipfPartPast.Ipl |];
           [| ipfPartPast.Pm; ipfPartPast.Pf; ipfPartPast.Pn; ipfPartPast.Ppl |]
        |]
    
    let makeIpfParticiplePastStrinDict (lm :LemmaRec) =
        let ipfPartPast = makeIpfParticiplePastString lm
        
        let dic = Dictionary<string,string[]>()
        dic.Add("N",[| ipfPartPast.Nm; ipfPartPast.Nf; ipfPartPast.Nn; ipfPartPast.Npl |])
        dic.Add("G",[| ipfPartPast.Gm; ipfPartPast.Gf; ipfPartPast.Gn; ipfPartPast.Gpl |])
        dic.Add("D",[| ipfPartPast.Dm; ipfPartPast.Df; ipfPartPast.Dn; ipfPartPast.Dpl |])
        dic.Add("A",[| ipfPartPast.Am; ipfPartPast.Af; ipfPartPast.An; ipfPartPast.Apl |])
        dic.Add("I",[| ipfPartPast.Im; ipfPartPast.If; ipfPartPast.In; ipfPartPast.Ipl |])
        dic.Add("P",[| ipfPartPast.Pm; ipfPartPast.Pf; ipfPartPast.Pn; ipfPartPast.Ppl |])

        dic.Add("Type",[|"Partic"; "Past"|])
        
        dic
 

    let makeFormatString (lms :LemmaRec seq) =
        lms
        |> Seq.map (fun lm ->
                            match lm.LemmaItem.Morph.PosTag with
                            | PosTag.Noun -> makeNounString lm |> Noun
                            | PosTag.Adjective -> makeAdjString lm |> Adjective
                            | PosTag.Verb ->    
                                                let verbStr : VerbString =
                                                            {
                                                                IpfPresent = makeIpfPresentString lm  
                                                                IpfPasse = makeIpfPasseString lm
                                                                IpfFuture = makeIpfFutureString lm 
                                                                IpfParticiplePresent = makeIpfParticiplePresentString lm
                                                                IpfParticiplePast = makeIpfParticiplePastString lm
                                                            }
                                                Verb verbStr
                            | _ -> Over
                            )
        |> Seq.filter (fun x -> x <> Over)
    
    let makeFormatStringArray (lms :LemmaRec seq) =
        lms
        |> Seq.map (fun lm ->
                            match lm.LemmaItem.Morph.PosTag with
                            | PosTag.Noun -> makeNounStringArray lm |> NounArray
                            | PosTag.Adjective -> makeAdjStringArray lm |> AdjectiveArray
                            | PosTag.Verb ->    
                                                let verbStrArray : VerbStringArray =
                                                            {
                                                                IpfPresentArray = makeIpfPresentStringArray lm  
                                                                IpfPasseArray = makeIpfPasseStringArray lm
                                                                IpfFutureArray = makeIpfFutureStringArray lm 
                                                                IpfParticiplePresentArray = makeIpfParticiplePresentStringArray lm
                                                                IpfParticiplePastArray = makeIpfParticiplePastStringArray lm
                                                            }
                                                VerbArray verbStrArray
                            | _ -> OverArray
                            )
        |> Seq.filter (fun x -> x <> OverArray)
    
     

    let makeLemmaFormatStringDict (lm :LemmaRec) =
        match lm.LemmaItem.Morph.PosTag with
        | PosTag.Noun -> 
                            let nounDict:FormatStringDict = 
                                {
                                    Pos = PosTag.Noun

                                    NounStringDict = makeNounStringDict lm
                                    AdjStringDict = null
                                    IpfPresentStrinDict  = null
                                    IpfPastStringDict = null
                                    IpfFutureStringDict  = null
                                    IpfParticiplePresentStringDict = null 
                                    IpfParticiplePastStringDict  = null
                                }
                            nounDict
        | PosTag.Adjective -> 
                            let adjDict:FormatStringDict = 
                                {
                                    Pos = PosTag.Adjective
                                    NounStringDict = null
                                    AdjStringDict = makeAdjStringDict lm
                                    IpfPresentStrinDict  = null
                                    IpfPastStringDict = null
                                    IpfFutureStringDict  = null
                                    IpfParticiplePresentStringDict = null 
                                    IpfParticiplePastStringDict  = null
                                }
                            adjDict
        | PosTag.Verb ->
                            let verbDict:FormatStringDict = 
                                {
                                    Pos = PosTag.Verb
                                    NounStringDict = null
                                    AdjStringDict = null
                                    IpfPresentStrinDict  = makeIpfPresentStrinDict lm
                                    IpfPastStringDict = makeIpfPasseStrinDict lm
                                    IpfFutureStringDict  = makeIpfFutureStrinDict lm
                                    IpfParticiplePresentStringDict = makeIpfParticiplePresentStrinDict lm 
                                    IpfParticiplePastStringDict  = makeIpfParticiplePastStrinDict lm
                                }
                            verbDict
        | _ -> 
                            let overDict:FormatStringDict = 
                                {
                                    Pos = PosTag.None
                                    NounStringDict = null
                                    AdjStringDict = null
                                    IpfPresentStrinDict  = null
                                    IpfPastStringDict = null 
                                    IpfFutureStringDict  = null
                                    IpfParticiplePresentStringDict = null
                                    IpfParticiplePastStringDict  = null
                                }
                            overDict

    let makeFormatStringDict (lms :LemmaRec seq) =
        lms
        |> Seq.map (fun lm -> makeLemmaFormatStringDict lm )
        |> Seq.filter (fun x -> x.Pos <> PosTag.None)