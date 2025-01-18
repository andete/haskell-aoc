import Util.Aoc
import qualified Util.Maze as Maze
import Data.List (nub, sort)
import Debug.Trace (trace)
import qualified Util.Located as Located
import qualified Data.HashSet as H
import Data.Hashable (Hashable)
import Util.Location (Location)
import Util.Direction4 (Direction4, sideDirs)
import Data.Foldable (find)
import Data.Maybe (isJust, fromJust)
import qualified Util.Maze as Maze

type Located = Located.Located
type Maze = Maze.Maze

type Crop = Char
type Region = [Located Crop]
-- todo: use Region2 isof Region as it is more strict, for Region it is implied that all locateds are the same crop
data Region2 = Region2 Crop [Location]
data PlotEdge = PlotEdge Crop Location Direction4 deriving (Show, Eq, Ord)
data Edge = Edge Crop [Location] Direction4 deriving (Show, Eq, Ord)

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

part2_example1test0 = do part2 4 "day12/example1.txt" (day12part2test 0)
part2_example1test1 = do part2 4 "day12/example1.txt" (day12part2test 1)

part2_example1test2 = do part2 8 "day12/example1.txt" (day12part2test 2)
part2_example1test3 = do part2 4 "day12/example1.txt" (day12part2test 3)
part2_example1test4 = do part2 4 "day12/example1.txt" (day12part2test 4)

part2_example1atest0 = do part2 4 "day12/example1a.txt" (day12part2test 0)
part2_example1atest1 = do part2 4 "day12/example1a.txt" (day12part2test 1)
part2_example1atest2 = do part2 4 "day12/example1a.txt" (day12part2test 2)
part2_example1atest3 = do part2 4 "day12/example1a.txt" (day12part2test 3)
part2_example1atest4 = do part2 20 "day12/example1a.txt" (day12part2test 4)

region :: [Located Crop] -> Region2
region plots = Region2 crop (map Located.location plots)
    where crop = Located.value (head plots)

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
          n = concatMap (filter (\(Located.Located _ c) -> c /= Just crop) . Maze.neighbours' maze . Located.location) l

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
          edgeLocations = map snd $ filter (\(p, _) -> Located.value p /= Just crop) (Maze.neighbours'dir maze (Located.location plot))


-- edges are PlotEdges, but grouped by being neighbours
edges :: Maze Crop -> Region -> [Edge]
edges maze region = foldl (edgesOne maze) [] (sort $ plotEdges maze region)

edgesOne :: Maze Crop -> [Edge]-> PlotEdge -> [Edge]
edgesOne maze edges plotEdge = case existingEdge of
    Just (Edge crop' locations dir') -> map (\edge -> if edge == Edge crop' locations dir' then Edge crop' (location:locations) dir' else edge) edges
    Nothing -> Edge crop [location] direction:edges
    where (PlotEdge crop location direction) = plotEdge
          n = map Located.location $ Maze.neighbours maze location
          existingEdge = find (\(Edge crop' locations dir') -> crop' == crop && dir' == direction && any (`elem` locations) n) edges

sides :: Maze Crop -> Region -> Int
sides maze region = length $ edges maze region

discounted :: Maze Crop -> Region -> Int
discounted maze region = ar * s
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

day12part2test :: Int -> [String] -> Int
day12part2test regionIndex field = trace (show edgePs) $ trace (Maze.showMaze (:[]) maze (map Located.location region)) $
    sides maze region
    where maze = Maze.parse id field
          regions = paint maze
          region = regions !! regionIndex
          edgePs = edges maze region

main = do part2_input