namespace MathMechFSharpLessons.Tests

open Expecto
open FsCheck
open MathMechFSharpLessons

module OOPListGenerator =
    open Task2
    open OOPList

    // https://fscheck.github.io/FsCheck//TestData.html
    let listGen<'elementType> : Gen<IList<'elementType>> =
        let rec listGen' mSize =
            match mSize with
            | 0 -> Gen.constant (EmptyList<'elementType>() :> IList<'elementType>)
            | n when n > 0 ->
                let subtree = listGen' (n - 1)

                Gen.oneof [ Gen.constant (EmptyList<'elementType>() :> IList<'elementType>)
                            Gen.map2
                                (fun hd tl -> NonEmptyList<'elementType>(hd, tl))
                                Arb.generate<'elementType>
                                subtree ]
            | _ -> invalidArg "mSize" "Only positive arguments are allowed"

        Gen.sized listGen'

    type OOPListGenerator =
        static member IList() = Arb.fromGen listGen

        static member Register() =
            Arb.register<OOPListGenerator> () |> ignore

    let OOPListGenConfig =
        { FsCheckConfig.defaultConfig with arbitrary = [ typeof<OOPListGenerator> ] }
