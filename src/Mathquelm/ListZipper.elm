module Mathquelm.ListZipper
    exposing
        ( ListZipper
        , empty
        , fromList
        , fromListEnd
        , getAfter
        , getAllAfter
        , getAllBefore
        , getBefore
        , goLeft
        , goRight
        , goToEnd
        , goToStart
        , insertAfter
        , insertBefore
        , isAtEnd
        , isAtStart
        , map
        , nest
        , removeAfter
        , removeBefore
        , toList
        )


type ListZipper a
    = ListZipper (List a) (List a)


empty : ListZipper a
empty =
    ListZipper [] []


left : ListZipper a -> List a
left zipper =
    case zipper of
        ListZipper left _ ->
            left


right : ListZipper a -> List a
right zipper =
    case zipper of
        ListZipper _ right ->
            right


setLeft : List a -> ListZipper a -> ListZipper a
setLeft newLeft zipper =
    ListZipper newLeft (right zipper)


setRight : List a -> ListZipper a -> ListZipper a
setRight newRight zipper =
    ListZipper (left zipper) newRight


toList : ListZipper a -> List a
toList zipper =
    List.reverse (left zipper) ++ right zipper


fromList : List a -> ListZipper a
fromList list =
    empty
        |> insertAfter list


fromListEnd : List a -> ListZipper a
fromListEnd list =
    empty
        |> insertBefore list


insertBefore : List a -> ListZipper a -> ListZipper a
insertBefore toInsert zipper =
    setLeft (List.reverse toInsert ++ left zipper) zipper


insertAfter : List a -> ListZipper a -> ListZipper a
insertAfter toInsert zipper =
    setRight (toInsert ++ right zipper) zipper


goLeft : ListZipper a -> ListZipper a
goLeft zipper =
    case left zipper of
        x :: xs ->
            ListZipper xs (x :: right zipper)

        [] ->
            zipper


goRight : ListZipper a -> ListZipper a
goRight zipper =
    case right zipper of
        x :: xs ->
            ListZipper (x :: left zipper) xs

        [] ->
            zipper


goToStart : ListZipper a -> ListZipper a
goToStart zipper =
    toList zipper
        |> fromList


goToEnd : ListZipper a -> ListZipper a
goToEnd zipper =
    toList zipper
        |> fromListEnd


isAtStart : ListZipper a -> Bool
isAtStart zipper =
    List.isEmpty (left zipper)


isAtEnd : ListZipper a -> Bool
isAtEnd zipper =
    List.isEmpty (right zipper)


getAllBefore : ListZipper a -> List a
getAllBefore =
    left >> List.reverse


getAllAfter : ListZipper a -> List a
getAllAfter =
    right


getBefore : ListZipper a -> Maybe a
getBefore zipper =
    List.head (left zipper)


getAfter : ListZipper a -> Maybe a
getAfter zipper =
    List.head (right zipper)


removeBefore : ListZipper a -> ListZipper a
removeBefore zipper =
    setLeft
        (List.tail (left zipper)
            |> Maybe.withDefault []
        )
        zipper


removeAfter : ListZipper a -> ListZipper a
removeAfter zipper =
    setRight
        (List.tail (right zipper)
            |> Maybe.withDefault []
        )
        zipper


nest : ListZipper a -> ListZipper a -> ListZipper a
nest inner outer =
    outer
        |> insertBefore (left inner)
        |> insertAfter (right inner)


map : (a -> b) -> ListZipper a -> ListZipper b
map fn zipper =
    ListZipper
        (List.map fn (left zipper))
        (List.map fn (right zipper))
