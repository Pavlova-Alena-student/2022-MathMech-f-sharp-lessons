namespace MathMechFSharpLessons.Tests

//open Expecto
open FsCheck
open Random
open MathMechFSharpLessons

// sized generation needed, so I don't want to use Expecto methods for generation of matrix (which should be possible)
module MatrixGenerator =
    open Task4
    open LinAlg

    let matrixGen<'elementType when 'elementType: equality> (height: int) (width: int) : 'elementType list list =
        let lst =
            (Gen.listOfLength height (Gen.listOfLength width (Arb.generate<'elementType>)))
                .Sample(1, 1).[0]

        lst
