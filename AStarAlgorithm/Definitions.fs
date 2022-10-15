namespace AStarAlgorithm

type Coord = int*int
type Size = int*int // height * width
type Cost = GCost * HCost * FCost
and FCost = int
and HCost = int
and GCost = int
type Start = Coord
type Stop = Coord
type Near = Coord

