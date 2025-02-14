import Data.Matrix (Matrix (nrows, ncols), detLU, mapCol, (!), fromList)

cramersRule :: (Ord a, Fractional a) => Matrix a -> Matrix a -> Matrix a
cramersRule aMatrix bVector =
    if nr /= nc || nr /= nrows bVector || ncols bVector /= 1
        then error "Matrix must be square and have the same number of rows as the vector"
        else
    fromList nr 1 $ map ((/ dA) . detLU . replaceCol aMatrix bVector) [1..nr]
    where dA = detLU aMatrix
          nr = nrows aMatrix
          nc = ncols aMatrix

replaceCol :: (Ord a, Fractional a) => Matrix a -> Matrix a -> Int ->  Matrix a
replaceCol a b i = mapCol (\j x -> b ! (j,1)) i a

prettyPrint2D :: (Show a) => Matrix a -> Matrix a -> String
prettyPrint2D a b = show (a ! (1,1)) ++ "𝑥 + " ++ show (a ! (1,2)) ++ "𝑦 = " ++ show (b ! (1,1)) ++ "\n" ++
                    show (a ! (2,1)) ++ "𝑥 + " ++ show (a ! (2,2)) ++ "𝑦 = " ++ show (b ! (2,1)) ++ "\n"

testCramersRule = do
    let a = fromList 2 2 [12,3,2,-3]
    let b = fromList 2 1 [15,13]
    putStr (prettyPrint2D a b)
    print a
    print b
    let c = cramersRule a b
    print c

testReplaceCol = do
    let a = fromList 2 2 [1,2,3,4]
    print a
    let b = fromList 2 1 [5,6]
    print b
    let c = replaceCol a b 1
    print c