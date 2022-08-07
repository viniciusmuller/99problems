module FSharp99Problems.OneToTen.Test

open NUnit.Framework
open FSharp99Problems.OneToTen
open FSharp99Problems.OneToTen.Solutions

let lst = [ 1; 2; 3; 4; 5; 6; 7; 8 ]
let palindrome = [ 1, 2, 4, 8, 16, 8, 4, 2, 1 ]
let duplicates = [ 10; 10; 21; 21; 32; 432; 149; 30; 30 ]

let nested =
    NList(
        [ Elem 10
          NList [ Elem 19 ]
          NList [ NList [ Elem 13 ] ]
          Elem 20 ]
    )

[<SetUp>]
let Setup () = ()

[<Test>]
let ``"last" function`` () = Assert.AreEqual(last lst, Some 8)

[<Test>]
let ``"lastButOne" function`` () = Assert.AreEqual(lastButOne lst, Some 7)

[<Test>]
let ``"elementAt" function`` () =
    Assert.AreEqual(elementAt lst 3, Some 3)

[<Test>]
let ``"length" function`` () =
    Assert.AreEqual(length lst, 8)
    Assert.AreEqual(length [], 0)

[<Test>]
let ``"reverse" function`` () =
    let reversed = reverse lst
    Assert.AreEqual(reversed, List.rev lst)
    Assert.AreEqual(length reversed, length lst)

[<Test>]
let ``"isPalindrome" function`` () =
    Assert.AreEqual(isPalindrome palindrome, true)
    Assert.AreEqual(isPalindrome [ 'o'; 'v'; 'o' ], true)
    Assert.AreEqual(isPalindrome [ 'v'; 'o'; 'v'; 'o' ], false)

[<Test>]
let ``"flatten" function`` () =
    Assert.AreEqual(flatten nested, [ 10; 19; 13; 20 ])

[<Test>]
let ``"compress" function`` () =
    let expected = [ 10; 21; 32; 432; 149; 30 ]
    Assert.AreEqual(compress duplicates, expected)

[<Test>]
let ``"pack" function`` () =
    let expected =
        [ [ 10; 10 ]
          [ 21; 21 ]
          [ 32 ]
          [ 432 ]
          [ 149 ]
          [ 30; 30 ] ]

    Assert.AreEqual(pack duplicates, expected)

[<Test>]
let ``"runLengthEncoding" function`` () =
    let expected =
        [ (10, 2)
          (21, 2)
          (32, 1)
          (432, 1)
          (149, 1)
          (30, 2) ]

    Assert.AreEqual(runLengthEncoding duplicates, expected)
