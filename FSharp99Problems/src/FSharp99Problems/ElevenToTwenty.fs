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
