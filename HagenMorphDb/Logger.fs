namespace HagenMorphDb

open Serilog

[<RequireQualifiedAccess>]
module HmdLog =

    let loggerConfig =
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .WriteTo.File(".\ParseFranDict.log")
            .CreateLogger()

    
    let Logger = loggerConfig
