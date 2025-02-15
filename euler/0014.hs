import Data.Foldable (maximumBy)
import qualified Data.HashMap.Strict as H
import Data.Function.Memoize
import Control.Monad.State.Lazy

colantz n = if even n then n `div` 2 else 3 * n + 1

colantzSeq n = n : if n == 1 then [] else colantzSeq (colantz n)

colantzSeqLength n = if n == 1 then 1 else 1 + colantzSeqLength (colantz n)

colantzSeqLengthCached :: H.HashMap Int Int -> Int -> (H.HashMap Int Int, Int)
colantzSeqLengthCached cache n = case H.lookup n cache of
    Just l -> (cache, l)
    Nothing -> if n == 1 then (cache, 1)
               else let (cache'', c) = colantzSeqLengthCached cache (colantz n) in (H.insert n (c+1) cache'', c + 1)

result = maximumBy (\x y -> compare (snd x) (snd y)) $ map (\i -> (i, colantzSeqLength i)) [1..99999]

snd3 (_,x,_) = x

result2 = snd3 $ foldl (\(acc,j,cache) i -> let (cache',l) = colantzSeqLengthCached cache i in if l > acc then (l,i,cache') else (acc,j,cache')) (0,0,H.empty) [1..999999]

-- experiment using State monad...
-- it is about 1/3th slower then result2... why?
-- compiled however it is ever so slightly faster then result2

type CacheState keyType valueType = State (H.HashMap keyType valueType) valueType
 
colantzSeqLengthCachedM :: Int -> CacheState Int Int
colantzSeqLengthCachedM n = do
    cache <- get
    case H.lookup n cache of
        Just l -> return l
        Nothing -> if n == 1 then return 1
                   else do
                       c <- colantzSeqLengthCachedM (colantz n)
                       let l = c + 1
                       put $ H.insert n l cache
                       return l

result3 = snd $ evalState (foldM (\(acc,j) i -> do
    l <- colantzSeqLengthCachedM i
    return $ if l > acc then (l,i) else (acc,j)) (0,0) [1..999999]) H.empty

expected = result2 == 837799