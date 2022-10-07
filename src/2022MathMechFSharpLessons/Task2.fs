namespace _2022MathMechFSharpLessons

module Task2 =
    let rec getFuncListFromOOPList<'value> (lst: OOPList.IList<'value>) =
        match lst with
        | :? OOPList.EmptyList<'value> -> FuncList.Empty
        | :? OOPList.NonEmptyList<'value> as lst -> FuncList.Cons(lst.Head, getFuncListFromOOPList<'value> lst.Tail)
        | _ -> raise <| new OOPList.UnknownListTypeException()

    let rec getOOPListFromFuncList<'value> lst =
        match lst with
        | FuncList.Empty -> OOPList.EmptyList<'value>() :> OOPList.IList<'value>
        | FuncList.Cons (h, tl) -> OOPList.NonEmptyList<'value>(h, getOOPListFromFuncList<'value> tl)
