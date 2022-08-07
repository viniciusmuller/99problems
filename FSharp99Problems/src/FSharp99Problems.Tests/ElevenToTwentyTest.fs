module FSharp99Problems.ElevenToTwenty.Test

open NUnit.Framework
open FSharp99Problems.ElevenToTwenty
open FSharp99Problems.ElevenToTwenty.Solutions

let duplicates = [ 10; 10; 21; 21; 32; 432; 149; 30; 30 ]

let duplicatesEncoded =
    [ Multiple(2, 10)
      Multiple(2, 21)
      Single 32
      Single 432
      Single 149
      Multiple(2, 30) ]

[<SetUp>]
let Setup () = ()

[<Test>]
let ``"encodeModified" function`` () =
    Assert.AreEqual(duplicatesEncoded, encodeModified duplicates)

[<Test>]
let ``"decodeModified" function`` () =
    Assert.AreEqual(duplicates, decodeModified duplicatesEncoded)

[<Test>]
let ``"encodeDirect" function`` () =
    Assert.AreEqual(duplicatesEncoded, encodeDirect duplicates)

[<Test>]
let ``"dup" function`` () =
    let expected = [ 1; 1; 2; 2; 3; 3; 4; 4; 4; 4 ]
    Assert.AreEqual(expected, dup [ 1; 2; 3; 4; 4 ])

[<Test>]
let ``"replicate" function`` () =
    let expected =
        [ 1
          1
          1
          2
          2
          2
          3
          3
          3
          4
          4
          4
          4
          4
          4 ]

    Assert.AreEqual(expected, replicate [ 1; 2; 3; 4; 4 ] 3)

[<Test>]
let ``"dropEvery" function`` () =
  let expected = [ 'a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k' ]
  let letters = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k' ]
  Assert.AreEqual(expected, dropEvery letters 3)
