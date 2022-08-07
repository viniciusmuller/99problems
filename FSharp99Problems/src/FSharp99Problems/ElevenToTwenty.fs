module FSharp99Problems.ElevenToTwenty

type EncodeResult<'a> =
    | Single of 'a
    | Multiple of int * 'a

module Solutions =
    // Problem 11 - Modified run-length encoding.
    // Modify the result of problem 10 in such a way that if an element has no
    // duplicates it is simply copied into the result list. Only elements with
    // duplicates are transferred as (N E) lists.
    let encodeModified lst =
        lst
        |> OneToTen.Solutions.runLengthEncoding
        |> List.map (function
            | (n, 1) -> Single n
            | (m, n) -> Multiple(n, m))

    // Problem 12 - Decode a run-length encoded list.
    //Given a run-length code list generated as specified in problem 11.
    //Construct its uncompressed version.
    let decodeModified lst =
        lst
        |> List.map (function
            | Single m -> [ m ]
            | Multiple (n, m) -> List.replicate n m)
        |> List.concat

    // Problem 13 - Run-length encoding of a list (direct solution).
    // Implement the so-called run-length encoding data compression method
    // directly. I.e. don't explicitly create the sublists containing the
    // duplicates, as in problem 9, but only count them. As in problem P11,
    // simplify the result list by replacing the singleton lists (1 X) by X.
    let private encode n m =
        match (n, m) with
        | 1, m -> Single m
        | n, m -> Multiple(n, m)

    let encodeDirect lst =
        lst
        |> List.fold
            (fun acc item ->
                match (acc, item) with
                | (None, 0, res), item -> (Some item, 1, res)
                | (Some curr, count, res), item when item = curr -> (Some item, count + 1, res)
                | (Some curr, count, res), item -> (Some item, 1, encode count curr :: res)
                | (res, _) -> res)
            (None, 0, List.empty)
        |> (fun (curr, count, res) ->
            match curr with
            | Some item -> encode count item :: res
            | None -> failwith "should never happen")
        |> List.rev

    // Problem 14 - Duplicate the elements of a list.
    let dup lst =
        lst
        |> List.fold (fun acc item -> item :: item :: acc) List.empty
        |> List.rev

    // Problem 15 - Replicate the elements of a list a given number of times.
    // TODO: Split this into prepend and prependFold functions
    let rec private prepend n acc item =
        match (acc, item) with
        | (lst, 0), _ -> (lst, n)
        | (lst, cnt), item -> prepend n ((item :: lst), (cnt - 1)) item

    let replicate lst n =
        lst
        |> List.fold (prepend n) (List.empty, n)
        |> fst
        |> List.rev

    // Problem 16 - Drop every N'th element from a list
    let dropEvery lst n =
        lst
        |> List.fold
            (fun acc item ->
                match (acc, item) with
                | (lst, 1), item -> (lst, n)
                | (lst, cnt), item -> (item :: lst, cnt - 1))
            (List.empty, n)
        |> fst
        |> List.rev

    // Problem 17 - Split a list into two parts
    // The length of the first part is given.
    let split lst n =
        lst
        |> List.fold
            (fun acc item ->
                match acc with
                | (ls, rs, 0) -> (ls, item :: rs, 0)
                | (ls, rs, n) -> (item :: ls, rs, n - 1))
            (List.empty, List.empty, n)
        |> (fun (l, r, _) -> List.rev l, List.rev r)


    // Problem 18 - Extract a slice from a list.
    // Given two indices, i and k, the slice is the list containing the
    // elements between the i'th and k'th element of the original list (both
    // limits included). Start counting the elements with 1.

    // TODO: Improve the indexing of items (avoid using the +1 inside the
    // function below)
    let private indexed2 xs =
        List.zip (List.init (List.length xs) id) xs

    let slice lst n m =
        lst
        |> indexed2
        |> List.fold
            (fun acc entry ->
                match (acc, entry) with
                | acc, (idx, item) when idx + 1 >= n && idx + 1 <= m -> item :: acc
                | acc, _ -> acc)
            List.empty
        |> List.rev

    // Problem 19 - Rotate a list N places to the left.
    // Hint: Use the predefined functions length and (++).
    // TODO: let rotate lst n =

    // Problem 20 - Remove the K'th element from a list.
    let removeAt lst n =
        lst
        |> List.fold
            (fun acc item ->
                match acc, item with
                | (xs, n, _), item when n = 1 -> xs, n - 1, Some item
                | (xs, n, Some popped), item -> item :: xs, n - 1, Some popped
                | (xs, n, _), item -> item :: xs, n - 1, None)
            (List.empty, n, None)
        |> function
            | (xs, _, Some item) -> (Some item, List.rev xs)
            | (xs, _, None) -> (None, List.rev xs)
