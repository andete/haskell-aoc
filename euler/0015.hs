import qualified Data.HashMap.Strict as H

routes :: (Int, Int) -> (Int, Int) -> Int
routes (a,b) (c,d)
  | a == c && b == d = 1
  | a < c && b < d = 
    routes (a+1,b) (c,d) +routes (a,b+1) (c,d)
  | a < c = routes (a+1,b) (c,d)
  | b < d = routes (a,b+1) (c,d)
  | otherwise = 0

routesCached :: H.HashMap (Int, Int) Int -> (Int, Int) -> (Int, Int) -> (H.HashMap (Int, Int) Int, Int)
routesCached cache (a,b) (c,d)
  | (a,b) `H.member` cache = (cache, cache H.! (a,b))
  | a == c && b == d = (cache, 1)
  | a < c && b < d = 
    let (cache', r1) = routesCached cache (a+1,b) (c,d)
        (cache'', r2) = routesCached cache' (a,b+1) (c,d)
        cache''' = H.insert (a+1,b) r1 cache''
        cache'''' = H.insert (a,b+1) r2 cache'''
    in (cache'''', r1 + r2)
  | a < c = routesCached cache (a+1,b) (c,d)
  | b < d = routesCached cache (a,b+1) (c,d)
  | otherwise = (cache, 0)

example = routes (0,0) (2,2)

result = snd $ routesCached H.empty (0,0) (20,20)

expected = result == 137846528820