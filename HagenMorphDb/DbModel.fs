namespace HagenMorphDb

open DapperDb

[<CLIMutable>]
type HagenRaw =
    {
        Id : int64
        HagenId : int64
        LemmaId : int64
        UpMorphId : int64
        Type :int
        NotUsed :int
        Word : string        
        DispWord : string
        GrammarStr : string
        Stem : string
        AccentPosMain : int
        AccentPosSecond : int
        PosTag : int
        GenderTag : int
        NumberTag : int
        CaseTag : int
        TenseTag : int
        PersonTag : int
        AnimalTag :int
        OverTag : int64
        Frequency : string
        SignOfOnce : string
        Semantic : string
    }


[<CLIMutable>]
type MainTagDict =
    {
        Id : int64
        Tag : string
        Type : string
        Cod : int
        Russian : string
    }


[<CLIMutable>]
type OverTagDict =
    {
        Id : int64
        Bit : int
        IntVal : int64
        Tag : string
        Type : string
        Russian : string
    }


module DbModel =
    
    let Db = DapperDbObj ("",false)
    
    //Clear Table
    let ClearTable (table :string) =
        let sql = $"DELETE FROM {table}"
        Db.ExecuteDapper sql
        
    //HagenRaw
    let allHagenRawColumn = "Id,HagenId,LemmaId,UpMorphId,Type,NotUsed,Word,DispWord,GrammarStr,Stem,AccentPosMain,AccentPosSecond,PosTag,GenderTag,NumberTag,CaseTag,TenseTag,PersonTag,AnimalTag,OverTag,Frequency,SignOfOne,Semantic"  
    
    //sql string
            
    let hagenRawInsertString (data :HagenRaw) =         
        let sql = $"INSERT INTO HagenRaw " +
                   "(HagenId," +
                   "LemmaId," +
                   "UpMorphId," +
                   "Type," +
                   "NotUsed," +                  
                   "Word," +
                   "DispWord," +
                   "GrammarStr," +
                   "Stem," +
                   "AccentPosMain," +
                   "AccentPosSecond," +
                   "PosTag," +
                   "GenderTag," +
                   "NumberTag," +
                   "CaseTag," +
                   "TenseTag," +
                   "PersonTag," +
                   "AnimalTag," +
                   "OverTag," +
                   "Frequency," +
                   "SignOfOne," +
                   "Semantic" +
                   " ) VALUES ( " +
                   $"{data.HagenId}," +
                   $"{data.LemmaId}," +
                   $"{data.UpMorphId}," +
                   $"{data.Type}," +
                   $"{data.NotUsed}," +
                   $"\'{data.Word}\'," +                   
                   $"\'{data.GrammarStr}\'," +
                   $"\'{data.Stem}\'," +
                   $"{data.AccentPosMain}," +
                   $"{data.AccentPosSecond}," +
                   $"{data.PosTag}," +
                   $"{data.GenderTag}," +
                   $"{data.NumberTag}," +
                   $"{data.CaseTag}," +
                   $"{data.TenseTag}," +
                   $"{data.PersonTag}," +
                   $"{data.AnimalTag}," +                  
                   $"{data.OverTag}," +
                   $"\'{data.Frequency}\'," +
                   $"\'{data.SignOfOnce}\'," +
                   $"\'{data.Semantic}\' ) "                                                
        sql
              
    let hagenRawUpdateString (data :HagenRaw) :string =
        let sql = $"UPDATE HagenRaw SET " +
                      $" HagenId = {data.HagenId}, " + 
                      $" LemmaId = {data.LemmaId}, " +
                      $" UpMorphId = {data.UpMorphId}, " +
                      $" Type = {data.Type}, " +
                      $" NotUsed = {data.NotUsed}, " +                      
                      $" Word = \'{data.Word}\', " +
                      $"DispWord = \'{data.DispWord}\'," +
                      $" GrammarStr = \'{data.GrammarStr}\', " +
                      $" Stem = \'{data.Stem}\', " +
                      $" AccentPosMain = {data.AccentPosMain}, " +
                      $" AccentPosSecond = {data.AccentPosSecond}, " +
                      $" PosTag = {data.PosTag}, " +
                      $" GenderTag = {data.GenderTag}, " +                      
                      $" NumberTag = {data.NumberTag}, " +
                      $" CaseTag = {data.CaseTag}, " +
                      $" TenseTag = {data.TenseTag}, " +
                      $" PersonTag = {data.PersonTag}, " +
                      $" AnimalTag = {data.AnimalTag}, " +
                      $" OverTag = {data.OverTag}, "+
                      $" Frequency = \'{data.Frequency}\', " +
                      $" SignOfOne = \'{data.SignOfOnce}\', " +
                      $" Semantic = \'{data.Semantic}\' " +
                      $" WHERE Id = {data.Id}"
        sql
        
    let hagenRawDeleteString (data :HagenRaw) :string =
        let sql = $"DELETE FROM HagenRaw WHERE Id = {data.Id}"
        sql
    
    let hagenRawGetAllString () :string =
        let sql = $"SELECT {allHagenRawColumn} FROM HagenRaw"
        sql

    let hagenRawGetIdString (id :int64) :string =
        let sql = $"SELECT {allHagenRawColumn} FROM HagenRaw WHERE Id = {id}"
        sql
    
    let hagenRawGetFilterString (filter :string) :string =
        let sql = $"SELECT {allHagenRawColumn} FROM HagenRaw WHERE {filter}"
        sql

    //Db function
    
    let insertHagenRaw (data :HagenRaw) =
        let sql = hagenRawInsertString data
        Db.ExecuteDapper sql
    
    let updateHagenRaw (data :HagenRaw) =
        let sql = hagenRawUpdateString data
        Db.ExecuteDapper sql
    
    let deleteHagenRaw (data :HagenRaw)=
        let sql = hagenRawDeleteString data
        Db.ExecuteDapper sql
             
    let getAllHagenRaw () =
        let sql = hagenRawGetAllString()
        Db.QueryManyDapper<HagenRaw> sql
    
    let getIdHagenRaw (id :int64) =
        let sql = hagenRawGetIdString id
        Db.QueryOneDapper<HagenRaw> sql
    
    let getFilterHagenRaw (filter :string) =
        let sql = hagenRawGetFilterString filter
        Db.QueryManyDapper<HagenRaw> sql 

    //Main Tag
    
    let allMainTagColumn = "Id,Tag,Type,Cod,Russian"
    
    // sql string
    
    let mainTagDictInsertString (table :string) (data :MainTagDict) =
        let sql = $"INSERT INTO {table} (Tag,Type,Cod,Russian) VALUES (\'{data.Tag}\',\'{data.Type}\',{data.Cod},\'{data.Russian}\')"
        sql
    
    let mainTagDictUpdateString (table :string) (data :MainTagDict) =
        let sql = $"UPDATE {table} SET Tag = \'{data.Tag}\', Type = \'{data.Type}\', Cod = {data.Cod}, Russian = \'{data.Russian}\' WHERE Id = {data.Id}"
        sql
    
    let mainTagDictDeleteString (table :string) (data :MainTagDict) =
        let sql = $"DELETE FROM {table} WHERE Id = {data.Id}"
        sql
    
    let mainTagDictGetAllString (table :string) :string =
        let sql = $"SELECT {allMainTagColumn} FROM {table}"
        sql

    let mainTagDictGetIdString (table :string) (id :int64) :string =
        let sql = $"SELECT {allMainTagColumn} FROM {table} WHERE Id = {id}"
        sql
    
    let mainTagGetDictFilterString (table :string) (filter :string) :string =
        let sql = $"SELECT {allMainTagColumn} FROM {table} WHERE {filter}"
        sql

    // Db function
    
    let insertMainTagDict (table :string) (data :MainTagDict) =
        let sql = mainTagDictInsertString table data
        Db.ExecuteDapper sql
    
    let updateMainTagDict (table :string) (data :MainTagDict) =
        let sql = mainTagDictUpdateString table data
        Db.ExecuteDapper sql
    
    let deleteMainTagDict (table :string) (data :MainTagDict) =
        let sql = mainTagDictDeleteString  table data
        Db.ExecuteDapper sql
             
    let getAllMainTagDict (table :string) () =
        let sql = mainTagDictGetAllString table 
        Db.QueryManyDapper<MainTagDict> sql
    
    let getIdMainTagDict (table :string) (id :int64) =
        let sql = mainTagDictGetIdString table id
        Db.QueryOneDapper<MainTagDict> sql
    
    let getFilterMainTagDict (table :string) (filter :string) =
        let sql = mainTagGetDictFilterString table filter
        Db.QueryManyDapper<MainTagDict> sql 


    // Over Tag
    
    let allOverTagColumn = "Id,Bit,IntVal,Tag,Type,Russian"
    
    // Sql String
    
    let OverTagDictInsertString (data :OverTagDict) :string =
        let sql = $"INSERT INTO OverTagDict (Bit,IntVal,Tag,Type,Russian) VALUES ({data.Bit},{data.IntVal},\'{data.Tag}\',\'{data.Type}\',\'{data.Russian}\') "
        sql

    let OverTagDictUpdateString (data :OverTagDict) :string =
        let sql = $"UPDATE OverTagDict SET Bit = {data.Bit},IntVal = {data.IntVal},Tag = \'{data.Tag}\',Type = \'{data.Type}\',Russian = \'{data.Russian}\' WHERE Id = {data.Id} "
        sql

        
    let OverTagDictDeleteString (data :OverTagDict) :string =
        let sql = $"DELETE FROM OverTagDict WHERE Id = {data.Id}"
        sql

    let OverTagDictGetAllString () :string =
        let sql = $"SELECT {allOverTagColumn} FROM OverTagDict"
        sql

    let OverTagDictGetIdString (id :int64) :string =
        let sql = $"SELECT {allOverTagColumn} FROM OverTagDict WHERE Id = {id}"
        sql
    
    let OverTagDictGetFilterString (filter :string) :string =
        let sql = $"SELECT {allOverTagColumn} FROM OverTagDict WHERE {filter}"
        sql

    // Db function
    
    let insertOverTagDict (data :OverTagDict) =
        let sql = OverTagDictInsertString data
        Db.ExecuteDapper sql
    
    let updateOverTagDict (data :OverTagDict) =
        let sql = OverTagDictUpdateString data
        Db.ExecuteDapper sql
    
    let deleteOvertTag (data :OverTagDict)=
        let sql = OverTagDictDeleteString data
        Db.ExecuteDapper sql
             
    let getAllOverTagDict () =
        let sql = OverTagDictGetAllString()
        Db.QueryManyDapper<OverTagDict> sql
    
    let getIdOverTagDict (id :int64) =
        let sql = OverTagDictGetIdString id
        Db.QueryOneDapper<OverTagDict> sql
    
    let getFilterOverTagDict (filter :string) =
        let sql = OverTagDictGetFilterString filter
        Db.QueryManyDapper<OverTagDict> sql 


    //Specification table for MainTag
    
    //Gender
    let insertGenderTagDict (data :MainTagDict) =
        insertMainTagDict "GenderTagDict" data
        
    let updateGenderTagDict (data :MainTagDict) =
        updateMainTagDict "GenderTagDict" data
    
    let deleteGenderTagDict (data :MainTagDict) =
        deleteMainTagDict "GenderTagDict" data
                     
    let getAllGenderTagDict () =
        getAllMainTagDict "GenderTagDict"
    
    let getIdGenderTagDict (id :int64) =
        getIdMainTagDict "GenderTagDict" id
    
    let getFilterGenderTagDict (filter :string) =
        getFilterMainTagDict "GenderTagDict" filter
    
    
    //Case    
    let insertCaseTagDict (data :MainTagDict) =
        insertMainTagDict "CaseTagDict" data
        
    let updateCaseTagDict (data :MainTagDict) =
        updateMainTagDict "CaseTagDict" data
    
    let deleteCaseTagDict (data :MainTagDict) =
        deleteMainTagDict "CaseTagDict" data
                     
    let getAllCaseTagDict () =
        getAllMainTagDict "CaseTagDict"
    
    let getIdCaseTagDict (id :int64) =
        getIdMainTagDict "CaseTagDict" id
    
    let getFilterCaseTagDict (filter :string) =
        getFilterMainTagDict "CaseTagDict" filter
        
    //Number

    let insertNumberTagDict (data :MainTagDict) =
        insertMainTagDict "NumberTagDict" data
        
    let updateNumberTagDict (data :MainTagDict) =
        updateMainTagDict "NumberTagDict" data
    
    let deleteNumberTagDict (data :MainTagDict) =
        deleteMainTagDict "NumberTagDict" data
                     
    let getAllNumberTagDict () =
        getAllMainTagDict "NumberTagDict"
    
    let getIdNumberTagDict (id :int64) =
        getIdMainTagDict "NumberTagDict" id
    
    let getFilterNumberTagDict (filter :string) =
        getFilterMainTagDict "NumberTagDict" filter
    
    //Person
    
    let insertPersonTagDict (data :MainTagDict) =
        insertMainTagDict "PersonTagDict" data
        
    let updatePersonTagDict (data :MainTagDict) =
        updateMainTagDict "PersonTagDict" data
    
    let deletePersonTagDict (data :MainTagDict) =
        deleteMainTagDict "PersonTagDict" data
                     
    let getAllPersonTagDict () =
        getAllMainTagDict "PersonTagDict"
    
    let getIdPersonTagDict (id :int64) =
        getIdMainTagDict "PersonTagDict" id
    
    let getFilterPersonTagDict (filter :string) =
        getFilterMainTagDict "PersonTagDict" filter
    
    //Pos

    let insertPosTagDict (data :MainTagDict) =
        insertMainTagDict "PosTagDict" data
        
    let updatePosTagDict (data :MainTagDict) =
        updateMainTagDict "PosTagDict" data
    
    let deletePosTagDict (data :MainTagDict) =
        deleteMainTagDict "PosTagDict" data
                     
    let getAllPosTagDict () =
        getAllMainTagDict "PosTagDict"
    
    let getIdPosTagDict (id :int64) =
        getIdMainTagDict "PosTagDict" id
    
    let getFilterPosTagDict (filter :string) =
        getFilterMainTagDict "PosTagDict" filter
    
    //Tense
    
    let insertTenseTagDict (data :MainTagDict) =
        insertMainTagDict "TenseTagDict" data
        
    let updateTenseTagDict (data :MainTagDict) =
        updateMainTagDict "TenseTagDict" data
    
    let deleteTenseTagDict (data :MainTagDict) =
        deleteMainTagDict "TenseTagDict" data
                     
    let getAllTenseTagDict () =
        getAllMainTagDict "TenseTagDict"
    
    let getIdTenseTagDict (id :int64) =
        getIdMainTagDict "TenseTagDict" id
    
    let getFilterTenseTagDict (filter :string) =
        getFilterMainTagDict "TenseTagDict" filter
    
    //Animal
    
        
    let insertAnimalTagDict (data :MainTagDict) =
        insertMainTagDict "AnimalTagDict" data
        
    let updateAnimalTagDict (data :MainTagDict) =
        updateMainTagDict "AnimalTagDict" data
    
    let deleteAnimalTagDict (data :MainTagDict) =
        deleteMainTagDict "AnimalTagDict" data
                     
    let getAllAnimalTagDict () =
        getAllMainTagDict "AnimalTagDict"
    
    let getIdAnimalTagDict (id :int64) =
        getIdMainTagDict "AnimalTagDict" id
    
    let getFilterAnimalTagDict (filter :string) =
        getFilterMainTagDict "AnimalTagDict" filter
    
