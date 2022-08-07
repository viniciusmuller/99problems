module FSharp99Problems.OneToTen

type NestedList<'a> =
    | Elem of 'a
    | NList of NestedList<'a> list

module Solutions =
    // Problem 1 - Find the last element of a list
    let rec last =
        function
        | [] -> None
        | [ x ] -> Some x
        | _ :: xs -> last xs

    // Problem 2 - Find the last but one element of a list
    let rec lastButOne =
        function
        | [] -> None
        | [ _ ] -> None
        | (x :: [ _ ]) -> Some x
        | _ :: xs -> lastButOne xs

    // Problem 3 - Find the K'th element of a list. The first element in the
    // list is number 1
    let rec elementAt lst n =
        match (lst, n) with
        | ((x :: _), 1) -> Some x
        | ([], n) -> None
        | ((x :: tail), n) -> elementAt tail (n - 1)

    // Problem 4 - Find the number of elements of a list
    let length lst = List.fold (fun acc _ -> acc + 1) 0 lst

    // Problem 5 - Reverse a list
    let reverse lst =
        List.fold (fun acc actual -> actual :: acc) List.empty lst

    // Problem 6 -  Find out whether a list is a palindrome. A palindrome
    // can be read forward or backward; e.g. (x a m a x).
    let isPalindrome lst = (reverse lst) = lst

    // Problem 7 - Flatten a nested list structure.
    let rec flatten =
        function
        | Elem x -> [ x ]
        | NList (x :: xs) -> (flatten x) @ (flatten (NList xs))
        | NList [] -> []

    // Problem 8 - Eliminate consecutive duplicates of list elements.
    let compress lst =
        lst
        |> List.fold
            (fun acc item ->
                match acc with
                | x :: xs when item = x -> item :: xs
                | _ -> item :: acc)
            List.empty
        |> List.rev

    // Problem 9 - Pack consecutive duplicates of list elements into sublists.
    // Icompresselements they should be placed in separate
    // sublists.
    let pack lst =
        lst
        |> List.fold
            (fun acc item ->
                match acc with
                | (x :: xs), res when item = x -> item :: x :: xs, res
                | (x :: xs), res -> [ item ], (x :: xs) :: res
                | [], res -> [ item ], res)
            (List.empty, List.empty) // Current list, result list
        |> fun (curr, res) -> curr :: res
        |> List.rev

    //  Problem 10 - Run-length encoding of a list. Use the result of problem
    //  P09 to implement the so-called run-length encoding data compression
    //  method. Consecutive duplicates of elements are encoded as lists (N E)
    //  where N is the number of duplicates of the element E.
    let private encode entry = List.head entry, length entry

    let runLengthEncoding lst = lst |> pack |> List.map encode
