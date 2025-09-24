namespace HagenMorphDb

open KTrie
open HagenMorphDb.TrieOpt
open HagenMorphDb.DataMorph
open HagenMorphDb.FormatLemma

type HagenMorphService(DbString :string,LogDb :bool)  =    
    let Db = DapperDb.DapperDbObj (DbString, LogDb)
    let LemmasTrie = TrieDictionary<LemmaTrieData>()
    
    do
        initTrieDb (Db, LemmasTrie)
        initLemmaTrie() 
        initDataMorphDb Db
    
    member this.GetLemmaForPartialWord (partWord :string) =  
        let r = getLemmaPartWord partWord 
        let res = ResizeArray r
        res
    
    member this.GetLemmaWord (word :string) =  
        let r = getLemmaWord word 
        let res = ResizeArray r
        res
    
    member this.GetTrieLemmaWord (word :string) = 
        let r = getMorphWord word 
        ResizeArray r
         
    
    member this.GetFormatLemmaWord (word :string) =
        let r = getLemmaWord word         
        let fr = makeFormatStringDict r
        ResizeArray fr
           
    member this.GetTrieLemmaWordPos (word :string, posTag: PosTag) = 
        let r = getMorphWordPos word posTag
        ResizeArray r
        
    
    member this.GetFormatLemmaWordPos (word :string, posTag: PosTag) =
        let r = getMorphWordPos word posTag         
        let fr = makeFormatStringDict r
        ResizeArray fr
        
               