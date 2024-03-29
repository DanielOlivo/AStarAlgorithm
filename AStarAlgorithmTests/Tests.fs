module Tests

open System
open Xunit
open AStarAlgorithm


[<Fact>]
let ``Corridor`` () = 
    let lines = ["...."]
    let (start,stop) = (0,0),(0,3)
    let obstacleFn = fun (y,x) -> lines.[y].[x] <> 'x'
    let path =
        AStar.init 10 (1,4)
        |> AStar.getPath obstacleFn start stop
    let expected = [0,0;0,1;0,2;0,3]
    let result = path = expected
    Assert.True(result)


[<Fact>] 
let ``OnSquare`` () = 
    let lines = [
        "...."
        "...."
        "...."
        "...."
    ]
    let (start,stop) = (3,0),(0,3)
    let obstacleFn = fun (y,x) -> lines.[y].[x] <> 'x'
    let path = 
        AStar.init 10 (4,4)
        |> AStar.getPath obstacleFn start stop
    let expected1 = [3,0;2,0;1,0;0,0;0,1;0,2;0,3]
    let expected2 = [3,0;3,1;3,2;3,3;2,3;1,3;0,3]
    let result = path = expected1 || path = expected2
    Assert.True(result)


[<Fact>] 
let ``Real Thing`` () = 
    let lines = [
        ".x..."
        ".x.x."
        ".x.x."
        "...x."
    ]
    let (start,stop) = (0,0),(3,4)
    let obstacleFn = fun (y,x) -> lines.[y].[x] <> 'x'
    let path = 
        AStar.init 10 (4,5)
        |> AStar.getPath obstacleFn start stop
    let expected = [0,0;1,0;2,0;3,0;3,1;3,2;2,2;1,2;0,2;0,3;0,4;1,4;2,4;3,4]
    let result = path = expected
    Assert.True(result)