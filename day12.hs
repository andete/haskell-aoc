import Aoc
import Maze
import Data.List (nub, sort)
import Debug.Trace (trace)
import qualified Located
import qualified Data.HashSet as H
import Data.Hashable (Hashable)
import Location (Location)
import Direction4 (Direction4)
import Data.Foldable (find)

type Crop = Char
type Region = [Located Crop]
data PlotEdge = PlotEdge Crop Location Direction4 deriving (Show, Eq)
data Edge = Edge Crop [Location] Direction4 deriving (Show, Eq)

part1_example1 = do
    part1 140 "day12/example1.txt" day12part1

part1_example1a = do
    part1 772 "day12/example1a.txt" day12part1

part1_example2 = do
    part1 1930 "day12/example2.txt" day12part1

part1_input = do
    part1 1473620 "day12/input.txt" day12part1

part2_example1a = do part2 436 "day12/example1a.txt" day12part2
part2_example1e = do part2 236 "day12/example1e.txt" day12part2
part2_example1ab = do part2 368 "day12/example1ab.txt" day12part2
part2_example2 = do part2 1206 "day12/example2.txt" day12part2
part2_input = do part2 902620 "day12/input.txt" day12part2


-- recognize regions of the same crop by a painting algorithm
paint :: Maze Crop -> [Region]
paint maze = fst $ foldl (\(acc, v) plot ->
    if H.member plot v
        then (acc,v)
        else let xn = paintOne maze plot
                 v' = foldl (flip H.insert) v xn in
            (xn: acc, v')) ([], H.empty) (Maze.items maze)

-- why is the nub needed ?
paintOne :: Maze Crop -> Located Crop -> Region
paintOne maze plot = nub $ H.toList $ paintOne' (H.fromList [plot]) maze plot

paintOne' :: H.HashSet (Located Crop) -> Maze Crop -> Located Crop -> H.HashSet (Located Crop)
paintOne' visited maze plot =
    foldl (\v n -> paintOne' (H.insert n v) maze n) visited neighbours
    where crop = Located.value plot
          loc = Located.location plot
          neighbours = filter (\l -> Located.value l == crop && not (H.member l visited)) (Maze.neighbours maze loc)

area :: Region -> Int
area = length

-- amount of different crop neighbours for each part of the plot
perimeter :: Maze Crop -> Region -> Int
perimeter maze l = length n
    where crop = Located.value (head l)
          n = concatMap (filter (\(Located _ c) -> c /= Just crop) . Maze.neighbours' maze . Located.location) l

price :: Maze Crop -> Region -> Int
price maze a = ar * p
    where ar = area a
          p = perimeter maze a

-- return the edge plots of a region
edgePlots :: Maze Crop -> Region -> [Located Crop]
edgePlots maze region = filter (any (\plot -> Located.value plot /= Just crop) . Maze.neighbours' maze . Located.location) region
    where crop = Located.value (head region)

plotEdges :: Maze Crop -> Region -> [PlotEdge]
plotEdges maze region = concatMap (plotEdgeOne maze) (edgePlots maze region)

plotEdgeOne :: Maze Crop -> Located Crop -> [PlotEdge]
plotEdgeOne maze plot = map (PlotEdge crop location) edgeLocations
    where crop = Located.value plot
          location = Located.location plot
          edgeLocations = map snd $ filter (\(p, _) -> Located.value p /= Just crop) (neighbours'dir maze (Located.location plot))


-- edges are PlotEdges, but grouped by being neighbours
edges :: Maze Crop -> Region -> [Edge]
edges maze region = foldl (edgesOne maze) [] (plotEdges maze region)

edgesOne :: Maze Crop -> [Edge]-> PlotEdge -> [Edge]
edgesOne maze edges plotEdge = case existing of
    Just (Edge crop' locations dir') -> map (\edge -> if edge == Edge crop' locations dir' then Edge crop' (location:locations) dir' else edge) edges
    Nothing -> Edge crop [location] direction:edges
    where (PlotEdge crop location direction) = plotEdge
          n = map Located.location $ Maze.neighbours' maze location
          existing = find (\(Edge crop' locations dir') -> crop' == crop && dir' == direction && any (`elem` n) locations) edges

sides :: Maze Crop -> Region -> Int
sides maze region = length $ edges maze region

discounted :: Maze Crop -> Region -> Int
discounted maze region = trace (show [ar, s]) $ ar * s
    where ar = area region
          s = sides maze region

day12part1 :: [String] -> Int
day12part1 field = sum $ map (price maze) regions
    where maze = Maze.parse id field
          regions = paint maze

day12part2 :: [String] -> Int
day12part2 field = sum $ map (discounted maze) regions
    where maze = Maze.parse id field
          regions = paint maze

main = do part1_input