namespace HagenMorphDb

open System
open DapperDb
open System.Collections.Generic

[<CLIMutable>]
type HagenRawRead =
    {
        HagenId : int64
        LemmaId : int64
        UpMorphId : int64
        Type : int
        NotUsed : int
        Word : string        
        AccentPosMain : int
        PosTag : int
        GenderTag : int
        NumberTag : int
        CaseTag : int
        TenseTag : int
        PersonTag : int
        AnimalTag :int
        OverTag : int64
    }

type TypeRec =
    | Error             = 0
    | Lemma             = 1
    | Word              = 2
    | SubWord           = 3

type PosTag =      
    |None              =  0  
    |Noun              =  1  
    |Adjective         =  2  
    |Verb              =  3  
    |Adverb            =  4  
    |Numeral           =  5  
    |Participle        =  6  
    |Transgressive     =  7  
    |Pronoun           =  8  
    |Preposition       =  9  
    |Conjunction       =  10 
    |Particle          =  11 
    |Interjection      =  12 
    |Predicative       =  13 
    |Parenthesis       =  14 
    |Any               =  32768

type GenderTag =
    |None              =  0
    |Masculine         =  1
    |Feminine          =  2
    |Neuter            =  3
    |Common            =  4
    |Any               =  32768

type NumberTag =
    |None              =  0
    |Singular          =  1
    |Plural            =  2
    |Any               =  32768

type CaseTag =
    |None              =  0
    |Nominative        =  1
    |Genitive          =  2
    |Dative            =  3
    |Accusative        =  4
    |Instrumental      =  5
    |Prepositional     =  6
    |Locative          =  7
    |Partitive         =  8
    |Vocative          =  9
    |Counting          = 10
    |Any               =  32768

type TenseTag =
    |None              =  0
    |Past              =  1
    |Present           =  2
    |Future            =  3
    |Infinitive        =  4
    |Any               =  32768

type PersonTag =
    |None              =  0
    |First             =  1
    |Second            =  2
    |Third             =  3
    |Any               =  32768

type AnimalTag =
    |None              =  0
    |Animated          =  1
    |Inanimate         =  2
    |Any               =  32768

[<Flags>]
type OverTag =
    |None               = 0L
    |Superlative        = 1L         
    |Comparative        = 2L         
    |Short              = 4L         
    |Immutable          = 8L         
    |Perfect            = 16L        
    |Imperfect          = 32L        
    |PastSimple         = 64L        
    |Intransitive       = 128L       
    |Transitive         = 256L       
    |Reflexive          = 512L       
    |PerNot             = 1024L      
    |Impersonal         = 2048L      
    |Imperative         = 4096L      
    |Circumstantial     = 8192L      
    |Definitive         = 16384L     
    |Time               = 32768L     
    |Places             = 65536L     
    |Goals              = 131072L    
    |Reasons            = 262144L    
    |Qualitative        = 524288L    
    |Power              = 1048576L   
    |Interrogative      = 2097152L   
    |Direction          = 4194304L   
    |OfManner           = 8388608L   
    |Quantitative       = 16777216L  
    |Ordinal            = 33554432L  
    |Indefinite         = 67108864L  
    |Collective         = 134217728L 
    |Passive            = 268435456L 
    |Adverb             = 536870912L 
    |Adjective          = 1073741824L
    |Noun               = 2147483648L
    |Any                = 4_611_686_018_427_387_904L

type MorphIndent =
    {
        HagenId : int64
        LemmaId : int64
        WordId : int64
    }

type Morphology =
    {
        PosTag : PosTag
        GenderTag : GenderTag
        NumberTag : NumberTag
        CaseTag : CaseTag
        TenseTag : TenseTag
        PersonTag : PersonTag
        AnimalTag : AnimalTag
        OverTag : OverTag
    }

type WordInfo =
    {
        Id : MorphIndent        
        Type : TypeRec
        Morph : Morphology
        Word : string
        AccentPos :int
    }

type WordRec =
    {
        WordItem :WordInfo
        SubWordItems :ResizeArray<WordInfo>
    }

type LemmaRec =
    {
        LemmaItem :WordInfo
        LemmaSubWords :ResizeArray<WordInfo>
        WordItems :ResizeArray<WordRec>
    }

module DataMorph =

    let accented     = [|"а́"; "я́"; "у́"; "ю́"; "о́"; "е́"; "э́"; "и́"; "ы́"; "А́"; "Я́"; "У́"; "Ю́"; "О́"; "Е́"; "Э́"; "И́"|]
    let no_accented  = [|"а"; "я"; "у"; "ю"; "о"; "е"; "э"; "и"; "ы"; "А"; "Я"; "У"; "Ю"; "О"; "Е"; "Э"; "И"|]

    let mutable accentedDict :Dictionary<string,string> = null
    let mutable noaccentedDict :Dictionary<string,string> = null 

    let createAccentedDicts =
        let accdict = Dictionary<string,string>()
        let noaccdict = Dictionary<string,string>()

        for i in [0..16] do
            accdict.Add(accented[i],no_accented[i])
            noaccdict.Add(no_accented[i],accented[i])

        accentedDict <- accdict
        noaccentedDict <- noaccdict 

    let HagenRawReadColumn = "HagenId,LemmaId,UpMorphId,Type,NotUsed,Word,AccentPosMain,PosTag,GenderTag,NumberTag,CaseTag,TenseTag,PersonTag,AnimalTag,OverTag"
        
    
    let mutable Db = null //DapperDbObj ($"Data Source={currDir}\\Db\\HagenMorph.db ;Version=3", false)    

    let initDataMorphDb (db :DapperDbObj) =
        createAccentedDicts
        Db <- db    
        ()
    
    let replaceAt (i :int) (c :char) (s :string) =
        let arr = s.ToCharArray()
        arr.[i] <- c
        string arr

    let replaceAt2 (i :int) (x:string) (s: string) = 
        let s1 = s.Substring(0,i) + x + s.Substring(i + 1) 
        s1

    let setAccentInWord (inRec :HagenRawRead) =
        let acc = inRec.AccentPosMain
        let ch = string inRec.Word[acc]
        let aword = replaceAt2 acc noaccentedDict[ch] inRec.Word
        aword

    let convertHagenRawReadToWordInfo (inRec:HagenRawRead) : WordInfo =
        
        let idRec = 
            {
                HagenId = inRec.HagenId
                LemmaId = inRec.LemmaId
                WordId = inRec.UpMorphId
            }
        
        let morph = 
            {
                PosTag = enum<PosTag>(inRec.PosTag)
                GenderTag = enum<GenderTag>(inRec.GenderTag)
                NumberTag = enum<NumberTag>(inRec.NumberTag)
                CaseTag = enum<CaseTag>(inRec.CaseTag)
                TenseTag = enum<TenseTag>(inRec.TenseTag)
                PersonTag = enum<PersonTag>(inRec.PersonTag)
                AnimalTag = enum<AnimalTag>(inRec.AnimalTag)
                OverTag = LanguagePrimitives.EnumOfValue inRec.OverTag
            }
        
        let WordInfoRec  =  
            {
                Id = idRec
                Type = enum<TypeRec>(inRec.Type)
                Morph = morph
                Word = setAccentInWord inRec
                AccentPos = inRec.AccentPosMain
            }
        
        WordInfoRec        
                            
    let GetSubWords  (lId :int64) (wId :int64) (wdInSeq :WordInfo seq) =
        wdInSeq
        |> Seq.filter (fun wt -> wt.Id.LemmaId = lId && wt.Id.WordId = wId && wt.Type = TypeRec.SubWord )


    let fillWordRec (word :WordInfo) (lemmaId :int64) (wdInSeq :WordInfo seq) =
        let WordSubWord = GetSubWords lemmaId word.Id.HagenId wdInSeq
        
        let wRec :WordRec =
            {
                WordItem = word
                SubWordItems = ResizeArray(WordSubWord)
            }
        
        wRec
            
    let fillLemmaRec (lemma :WordInfo) (wsInSeq :WordInfo seq) =
        let lemmaSubWord = GetSubWords lemma.Id.HagenId lemma.Id.HagenId wsInSeq               
        
        let wordSeq = wsInSeq |> Seq.filter (fun x -> x.Id.LemmaId = lemma.Id.HagenId && x.Type = TypeRec.Word)
        
        let words = wordSeq |> Seq.map (fun x -> fillWordRec x lemma.Id.HagenId wsInSeq)
        
        let lem :LemmaRec =
                {
                    LemmaItem = lemma
                    LemmaSubWords = ResizeArray(lemmaSubWord)
                    WordItems = ResizeArray(words)
                }
                 
        lem       

    let sqlQueryPartWord (partword :string) =
        let sql = $"SELECT {HagenRawReadColumn} FROM HagenRaw WHERE Word like '{partword}%%'"
        sql
    
    let sqlQueryWord (word :string) =
        let sql = $"SELECT "+ 
                  $" {HagenRawReadColumn} FROM HagenRaw "+ 
                  " WHERE (LemmaId = ( "+
                  $" SELECT HagenId FROM HagenRaw WHERE (Word = \'{word}\') and (Type = 1) ) ) "+ 
                  " OR "+
                  " (HagenId = ( "+ 
                  $" SELECT HagenId FROM HagenRaw WHERE (Word = \'{word}\') and (Type = 1 ) ) ) " 
        sql
    
        
    let getMorphFromHagenRaw  (sql :string) : HagenRawRead seq option =        
        let res = Db.QueryManyDapper<HagenRawRead> sql
        res

                    
    let convertHagenDbToLemmas (hdb :HagenRawRead seq option) =
        let wi = match hdb with
                    | Some x -> x |> Seq.map (fun m -> convertHagenRawReadToWordInfo m) 
                    | None -> failwith "Do not get morph from db"
        
        let lemmasSeq = wi |> Seq.filter (fun x -> x.Type = TypeRec.Lemma)
        
        let lemmas = lemmasSeq |> Seq.map (fun x -> fillLemmaRec x wi)
        
        lemmas     
    
    let getLemmaWord (word :string) =
        let mr = getMorphFromHagenRaw (sqlQueryWord word)
        let lm = convertHagenDbToLemmas mr
        lm
        
    let getLemmaPartWord (partword :string) =
        let mr = getMorphFromHagenRaw (sqlQueryPartWord partword)
        let lm = convertHagenDbToLemmas mr
        lm 