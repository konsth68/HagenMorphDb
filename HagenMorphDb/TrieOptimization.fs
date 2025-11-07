namespace HagenMorphDb

open KTrie
open HagenMorphDb.DapperDb
open HagenMorphDb.DataMorph

[<CLIMutable>]
type LemmaData =
    {
        
        HagenId :int64
        Type :int
        Word :string
        PosTag :int
    }

type LemmaTrieData =
    |Normal of LemmaData
    |Duplicate of LemmaData seq



module TrieOpt =
    
    let currDir = System.IO.Directory.GetCurrentDirectory()
    let mutable Db = null //DapperDbObj ($"Data Source={currDir}\\Db\\HagenMorph.db ;Version=3", false)    
    let mutable LemmaTrie = null   //= TrieDictionary<LemmaTrieData>() 

    let initTrieDb (db :DapperDbObj,trie :TrieDictionary<LemmaTrieData>) =
        Db <- db    
        LemmaTrie <- trie 
        ()
    
    //----------------------------------------------------------------------------------------------------------------
        
    let getDbAllLemma () =
        let sql = $"SELECT HagenId,Type,Word,PosTag FROM HagenRaw WHERE Type = 1 and NotUsed = 0 and "+ 
                                                    "(PosTag IN ( 1,2,3,5,8,6,7)) ORDER BY Word"
        let res = Db.QueryManyDapper<LemmaData> sql
        checkSeqOption res
           
    let parseLemmaGroup (key:string, group: LemmaData seq) =
        let lst = Seq.toList group
        match lst with
        | [] -> None
        | [x] -> Some (key, Normal x)
        | _ -> Some (key, Duplicate lst)
    
    let convertToLemmaTrieData (dbData: LemmaData seq ) =     
        dbData
        |> Seq.groupBy (fun x -> x.Word) 
        |> Seq.map (fun x -> parseLemmaGroup x ) 
    
    let fillLemmaTrie (data :(string * LemmaTrieData) option seq)  =
        let r = data |> Seq.choose id
        for k,v in r do
            LemmaTrie.Add(k,v)
        ()
                            
    let initLemmaTrie () =
        let r = getDbAllLemma ()
        let rr = convertToLemmaTrieData r
        fillLemmaTrie rr
        
    //----------------------------------------------------------------------------------------------------------------
        
    let getHagenRawRead (lemmaId :int64) =
        let sql = $"SELECT {HagenRawReadColumn} FROM HagenRaw WHERE HagenId = {lemmaId} OR lemmaId = {lemmaId} " 
        let res = Db.QueryManyDapper<HagenRawRead> sql
        checkSeqOption res
    
    let getHagenRawReadMany (lemmaIds :int64 seq) =
        let ids = lemmaIds |> Seq.map string |> String.concat ","
        let sql = $"SELECT {HagenRawReadColumn} FROM HagenRaw WHERE HagenId IN ({ids}) OR lemmaId IN ({ids}) " 
        let res = Db.QueryManyDapper<HagenRawRead> sql
        checkSeqOption res
                    
    let getLemmaTrieData (word :string) =
        let r = LemmaTrie.TryGetValue(word)
        match r with
        | x,y when x = true  -> Some y
        | x,_ when x = false -> None
        | _ -> None
    
    let getOneLemma (ld :LemmaData) =
        getHagenRawRead ld.HagenId

    let getListHagenId (data :LemmaData seq) = 
        data |> Seq.map (fun x -> x.HagenId)    
    
    let getManyLemma (ldq :LemmaData seq) =
        let idq = getListHagenId ldq
        getHagenRawReadMany idq
    
    let convertHagenRawReadToLemmas (hdb :HagenRawRead seq) =
        let wi = hdb |> Seq.map (fun m -> convertHagenRawReadToWordInfo m)
        
        let lemmasSeq = wi |> Seq.filter (fun x -> x.Type = TypeRec.Lemma)
        
        let lemmas = lemmasSeq |> Seq.map (fun x -> fillLemmaRec x wi)
        
        lemmas     
    
    let getMorphWord (word :string)  =
        let tl = getLemmaTrieData word
        match tl with
        |Some x -> 
            match x with
            |Normal ld -> let hr = getOneLemma ld
                          convertHagenRawReadToLemmas hr
                                      
            |Duplicate lds -> let hrr = getManyLemma lds
                              convertHagenRawReadToLemmas hrr
                          
        |None -> Seq.empty

    let getMorphWordPos (word :string) (pos :PosTag)  =
        let tl = getLemmaTrieData word
        let r = match tl with
                |Some x -> 
                    match x with
                    |Normal ld -> let hr = getOneLemma ld
                                  convertHagenRawReadToLemmas hr
                                      
                    |Duplicate lds -> let hrr = getManyLemma lds
                                      convertHagenRawReadToLemmas hrr
                          
                |None -> Seq.empty
        r
        |> Seq.filter (fun l -> l.LemmaItem.Morph.PosTag = pos)

    let getMorphWordByLemmaId (lemmaId :int64)  =
        let hr = getHagenRawRead lemmaId
        let lemmas = convertHagenRawReadToLemmas hr
        lemmas