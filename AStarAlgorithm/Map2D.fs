namespace AStarAlgorithm

module Map2D = 
    type Map2D<'a> = {
        Get : Coord -> 'a
        Set : Coord -> 'a -> unit
        Count : ('a -> bool) -> int
        Reset : 'a -> unit
        IsWithin : Coord -> bool
        Size : int * int
    }

    let private coords (height,width) = [for row in [0..height-1] do for col in [0..width-1] -> row,col]

    let init (height,width) initFn = 
        let arr = Array2D.init height width (fun y x -> initFn (y,x))
        //let coords = 
        //    [for row in [0..height-1] do for col in [0..width-1] -> row,col]
        {
            Get = fun (y,x) -> arr.[y,x]
            Set = fun (y,x) value -> arr.[y,x] <- value
            Count = fun predicate ->
                (height,width)
                |> coords 
                |> List.map(fun (y,x) -> predicate arr.[y,x])
                |> List.map(fun x -> if x then 1 else 0)
                |> List.sum
            Reset = fun defaultValue ->
                (height,width) |> coords |> List.iter(fun (y,x) -> arr.[y,x] <- defaultValue)
            IsWithin = fun (y,x) -> not (y < 0 || y>= height || x < 0 || x >= width)
            Size = (height,width)
        }

    let change changeFn map2D = 
        map2D.Size
        |> coords
        |> List.iter (fun coord -> 
            coord 
            |> map2D.Get 
            |> changeFn 
            |> fun newValue -> map2D.Set coord newValue)
        map2D

    let changei changeFn map2D = 
        map2D.Size 
        |> coords 
        |> List.iter (fun coord -> 
            let currentValue = map2D.Get coord
            map2D.Set coord (changeFn coord currentValue))
        map2D