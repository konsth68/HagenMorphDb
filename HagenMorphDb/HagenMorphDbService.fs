namespace HagenMorphDb

open KTrie
open HagenMorphDb.TrieOpt
open HagenMorphDb.DataMorph
open HagenMorphDb.FormatLemma
open System.IO
open Serilog

type IHagenMorphService =
    abstract GetLemmaForPartialWord  : string -> ResizeArray<LemmaRec>
    abstract GetLemmaWord  : string -> ResizeArray<LemmaRec>
    abstract GetTrieLemmaWord  : string -> ResizeArray<LemmaRec>
    abstract GetFormatLemmaWord  : string -> ResizeArray<FormatStringDict>
    abstract GetTrieLemmaWordPos  :  string * PosTag-> ResizeArray<LemmaRec>
    abstract GetFormatLemmaWordPos  : string * PosTag -> ResizeArray<FormatStringDict>

type HagenMorphService(DbString :string,LogDb :bool)  =    
    let Db = DapperDb.DapperDbObj (DbString, LogDb)
    let LemmasTrie = TrieDictionary<LemmaTrieData>()
    
    do
        initTrieDb (Db, LemmasTrie)
        initLemmaTrie() 
        initDataMorphDb Db
    
    interface IHagenMorphService with
        member this.GetLemmaForPartialWord (partWord :string) =  
            let r = getLemmaPartWord partWord 
            let res = ResizeArray<LemmaRec> r
            res
    
        member this.GetLemmaWord (word :string) =  
            let r = getLemmaWord word 
            let res = ResizeArray<LemmaRec> r
            res
    
        member this.GetTrieLemmaWord (word :string) = 
            let r = getMorphWord word 
            ResizeArray<LemmaRec> r
         
    
        member this.GetFormatLemmaWord (word :string) =
            let r = getLemmaWord word         
            let fr = makeFormatStringDict r
            ResizeArray<FormatStringDict> fr
           
        member this.GetTrieLemmaWordPos (word :string, posTag: PosTag) = 
            let r = getMorphWordPos word posTag
            ResizeArray<LemmaRec> r
        
    
        member this.GetFormatLemmaWordPos (word :string, posTag: PosTag) =
            let r = getMorphWordPos word posTag         
            let fr = makeFormatStringDict r
            ResizeArray<FormatStringDict> fr
        
    new () =
        //let curDir = Directory.GetCurrentDirectory()
        let initString = $"Data Source=c:\\users\\konsth\\AppData\\Local\\FranDict\\HagenMorph.db"
        
        //HmdLog.Logger.Information $"INITSTRING = {initString}"
        HagenMorphService(initString,false)
