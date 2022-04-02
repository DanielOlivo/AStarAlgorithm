namespace AStarAlgorithm

module Runner =
    let private andThen f g = 
        let innerFn x = if f x then g x else false
        innerFn
    let private (<!>) f g = andThen f g

    let private iterateOver height width = seq{for y in [0..height - 1] do for x in [0..width - 1] -> y,x}

    let private findOnGrid height width sym (grid : string list) =
        iterateOver height width |> Seq.tryFind (fun (y,x) -> grid.[y].[x] = sym) 

    let private nearby = [-1,0; 1,0; 0,1; 0,-1]

    let private get (arr : 'a[,]) (y,x) = arr.[y,x]
    let private set (arr : 'a[,]) (y,x) v = arr.[y,x] <- v

    let private add (x0,y0) (x1,y1) = x0 + x1 , y0 + y1
    let private distance d (y0,x0) (y1,x1) = 
        let dx = (x0 - x1) |> abs
        let dy = (y0 - y1) |> abs
        d * (dx + dy)

    let path (lines : string list) start stop = 
        let (height,width) = List.length lines , Seq.length lines.[0]
        //let (Some start,Some stop) = findOnGrid height width 'a' lines, findOnGrid height width 'b' lines
        let idxs = iterateOver height width
        let f = Array2D.init height width (fun _ _ -> 0)
        let g = Array2D.init height width (fun _ _ -> 0)
        let h = Array2D.init height width (fun _ _ -> 0)
        let walkable = Array2D.init height width (fun y x -> lines.[y].[x] <> 'x')
        let visited = Array2D.init height width (fun _ _ -> false)

        let makeFirstStep p = 
            set g p 0
            distance 10 p stop |> set h p
            set f p (get g p + get h p)
            set visited p true
        
        let isWithin (y,x) = x >= 0 && x < width && y >= 0 && y < height
        let isVisited p = get visited p
        let isWalkable p = get walkable p
        let isNotVisited p = p |> isVisited |> not

        let visit (p,n) = 
            let gCost = get g n
            set g p (gCost + 10)
            distance 10 p stop |> set h p
            set f p (get g p + get h p)
            set visited p true

        let calcF (candidateP,visitedP) = get g visitedP + distance 10 candidateP stop
        let isCandidate p = nearby |> List.map (add p) |> List.filter(isWithin <!> isWalkable <!> isVisited) |> fun r -> if List.isEmpty r then None else List.map (fun n -> p,n) r |> List.minBy calcF |> Some
        let getNextPos() = idxs |> Seq.filter (isWalkable <!> isNotVisited) |> Seq.choose isCandidate |> Seq.minBy calcF // here something wrong
        let rec runner ()(*steps*) = 
            //if steps = 0 then ()
            (*else*) 
            if get visited stop then () //printfn "completed; %d" steps
            else 
                let nextPos = getNextPos()
                visit nextPos
                runner() //(steps - 1)
        makeFirstStep start
        runner() //100

        let getNext current = nearby |> List.map (add current) |> List.filter (isWithin <!> isVisited) |> List.minBy (get g)

        let rec getPath path current count =
            if count > 1000 then []
            else if current = start then (current::path)
            else getPath (current::path) (getNext current) (count + 1)

        getPath [] stop 0