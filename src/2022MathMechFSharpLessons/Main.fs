namespace _2022MathMechFSharpLessons

open System.Reflection

module AssemblyInfo =

    let metaDataValue (mda: AssemblyMetadataAttribute) = mda.Value

    let getMetaDataAttribute (assembly: Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.find (fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getGitHash assembly =
        "GitHash"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getVersion assembly =
        "AssemblyVersion"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let assembly = lazy (Assembly.GetEntryAssembly())

    let printVersion () =
        let version = assembly.Force().GetName().Version
        printfn "%A" version

    let printInfo () =
        let assembly = assembly.Force()
        let name = assembly.GetName()
        let version = assembly.GetName().Version
        let releaseDate = getReleaseDate assembly
        let githash = getGitHash assembly
        printfn "%s - %A - %s - %s" name.Name version releaseDate githash

module Main =
    open Argu

    type CLIArguments =
        | Info
        | Version
        | Favorite_Color of string // Look in App.config
        | Dispersion of int list // Argu does not support int []
        | OddBetween of int * int
        | NaivePower of int * int
        | Power of int * int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Info -> "More detailed information"
                | Version -> "Version of application"
                | Favorite_Color _ -> "Favorite color"
                | Dispersion _ -> "Difference between max and min of an array"
                | OddBetween _ -> "Create array of odd numbers from first to second"
                | NaivePower _ -> "Get first in power of second naive way"
                | Power _ -> "Get first in power of second"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser =
            ArgumentParser.Create<CLIArguments>(programName = "_2022MathMechFSharpLessons")

        let results = parser.Parse(argv)

        if results.Contains Version then
            AssemblyInfo.printVersion ()
        elif results.Contains Info then
            AssemblyInfo.printInfo ()
        elif results.Contains Dispersion then
            match results.TryGetResult Dispersion with
            | Some (a) ->
                Task1.Dispersion(a)
                |> printfn "Difference between max&min in this array is %d"
            | None -> parser.PrintUsage() |> printfn "%s"
        elif results.Contains OddBetween then
            match results.TryGetResult OddBetween with
            | Some (a, b) ->
                printfn "Odd numbers between %d and %d:" a b

                for i in Task1.OddBetween(a, b) do
                    printfn "%d" i
            | None -> parser.PrintUsage() |> printfn "%s"
        elif results.Contains NaivePower then
            match results.TryGetResult NaivePower with
            | Some (a, b) ->
                Task1.NaivePower(a, b)
                |> printfn "%d in power of %d is %d (evaluated naively)" a b
            | None -> parser.PrintUsage() |> printfn "%s"
        elif results.Contains Power then
            match results.TryGetResult Power with
            | Some (a, b) ->
                Task1.Power(a, b)
                |> printfn "%d in power of %d is %d" a b
            | None -> parser.PrintUsage() |> printfn "%s"
        else
            parser.PrintUsage() |> printfn "%s"

        0
