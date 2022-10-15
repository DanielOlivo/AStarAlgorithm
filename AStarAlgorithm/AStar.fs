namespace AStarAlgorithm


module AStar = 
    type TraversingArea = {
        Coords : Coord seq
        GetNeighbours : Coord -> Coord list
        IsOpen : OpenCheck
        IsVisited : VisitCheck
        IsCandidate : Coord -> bool
        GetCost : Coord -> Cost
        Visit : Coord -> Stop -> unit
        Reset : (Coord -> bool) -> unit
    }
    and Radius = int
    and CoordsInRadiusGen = Radius -> Coord -> Coord seq
    and VisitCheck = Coord -> bool
    and OpenCheck = Coord -> bool

    type private Adding = Coord -> Coord -> Coord
    type private CheckWithin = Size -> Coord -> bool
    type private CoordsGen = Size -> Coord seq
    type private NeighbourCoordsGen = Coord -> Coord list
    type private HCostGen = int -> Start -> Stop -> HCost
    type private Runner = Stop -> TraversingArea -> (Coord * TraversingArea) option

    let private andThen f g x = if f x then g x else false
    let (<!>) = andThen
    let private add : Adding = fun (y0,x0) (y1,x1) -> y0+y1, x0+x1
    let private isWithin : CheckWithin = fun (height,width) (y,x) -> not (y < 0 || y >= height || x < 0 || x >= width)
    let private getCoords : CoordsGen = fun (height,width) -> 
        seq{for row in {0..height-1} do for col in {0..width-1} -> row,col}
    let private getCoordsInRadius : CoordsInRadiusGen = fun radius (y,x) ->
        seq{for row in {y-radius..y+radius} do for col in {x-radius..x+radius} -> row,col}
    let private getHCost : HCostGen = fun  d (y0,x0) (y1,x1) ->
        let dx = (x0 - x1) |> abs
        let dy = (y0 - y1) |> abs
        d * (dx + dy)
    let private getNeighbourCoords : NeighbourCoordsGen = fun coord -> 
        [0,1;0,-1;1,0;-1,0] 
        |> List.map (add coord)

    let private unfold : Runner = fun stop area -> 
        if area.IsVisited stop then None
        else 
            area.Coords
            |> Seq.toList
            |> List.choose(fun coord ->
                if area.IsCandidate coord then Some (coord,area.GetCost coord) else None)
            |> fun candidates -> 
                match candidates with 
                | [] -> None
                | _ -> 
                    candidates
                    |> List.minBy(fun (_,(_,_,fCost)) -> fCost)
                    |> fun (newCoord,_) -> 
                        area.Visit newCoord stop
                        Some (newCoord,area)

    let getPath obstacleFn start stop area = 
        area.Reset obstacleFn
        area.Visit start stop
        let searching = List.unfold (unfold stop) area
        let runnerFn (coord,isFound) = 
            if isFound then None 
            elif coord = start then Some (coord,(coord,true))
            else
                coord 
                |> area.GetNeighbours
                |> List.choose(fun c -> if area.IsVisited c then Some (c, c |> area.GetCost |> fun (gCost,_,_) -> gCost) else None)
                |> List.minBy(fun (_,gCost) -> gCost)
                |> fun (c,_) -> Some (coord,(c,false))
        List.unfold runnerFn (stop,false)
        |> List.rev
        

    let init = fun d (height,width) -> 
        //let (height,width) = Array2D.length1 obstacles, Array2D.length2 obstacles
        let size = (height,width)

        let _obstacles = Map2D.init size (fun (y,x) -> false)//obstacles.[y,x])
        let gMap = Map2D.init size (fun _ -> 0)
        let hMap = Map2D.init size (fun _ -> 0)
        let fMap = Map2D.init size (fun _ -> 0)
        let visited = Map2D.init size (fun _ -> false)
        let candidates = Map2D.init size (fun _ -> false)

        let isOpen coord = _obstacles.Get coord
        let getCost = fun coord -> gMap.Get coord, hMap.Get coord,fMap.Get coord
        let coords = getCoords size
        
        {
            Coords = coords
            GetNeighbours = fun coord ->
                coord 
                |> getNeighbourCoords
                |> List.filter((isWithin size) <!> _obstacles.Get)
            IsOpen =  _obstacles.Get
            IsVisited = visited.Get
            IsCandidate = candidates.Get
            GetCost = getCost
            Visit = fun coord stop -> 
                visited.Set coord true 
                candidates.Set coord false
                // modify costs around
                coord
                |> getNeighbourCoords 
                |> List.filter((isWithin size) <!> _obstacles.Get <!> (visited.Get >> not))
                |> List.iter (fun nearCoord -> 
                    coord |> gMap.Get |> ((+) d) |> gMap.Set nearCoord
                    hMap.Set nearCoord (getHCost d nearCoord stop)
                    fMap.Set nearCoord ((gMap.Get nearCoord) + (hMap.Get nearCoord))
                    candidates.Set nearCoord true)
            Reset = fun obstaclesFn -> 
                coords 
                |> Seq.toList
                |> List.iter(fun coord ->
                    coord |> obstaclesFn |> _obstacles.Set coord
                    gMap.Set coord 0
                    hMap.Set coord 0
                    fMap.Set coord 0
                    visited.Set coord false
                    candidates.Set coord false)
        }
    let reset obstacleFn area = 
        area.Reset obstacleFn
        area