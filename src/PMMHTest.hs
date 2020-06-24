module PMMHTest where

import Control.Monad.Bayes.Class



s :: MonadSample m => m Double
s = gamma 1 1



random_walk' :: MonadInfer m => [Double] -> Double -> m [Double]
random_walk' ys s = do
    let obs x y = score ( normalPdf x 1 y)
    let expand xs [] = return xs
        expand (x : xs ) (y : ys ) = do
            x' <- normal x s
            obs x' y
            expand (x' : x : xs ) ys
    xs <- expand [0] ys
    return (reverse xs)

gen_random_walk :: MonadSample m => Int -> Double -> m [Double]
gen_random_walk n s = do
    let expand 0 xs ys = return ys
        expand n (x : xs ) ys = do
            x' <- normal x s
            y  <- normal x' 1
            expand (n-1) (x' : x : xs ) (y : ys)
    ys <- expand n [0] []
    return (reverse ys)

random_walk :: MonadInfer m => [Double] -> Double -> m Double
random_walk ys s = do
    let obs x y = score ( normalPdf x 1 y)
    let expand xs [] = return xs
        expand (x : xs ) (y : ys ) = do
            x' <- normal x s
            obs x' y
            expand (x' : x : xs ) ys
    xs <- expand [0] ys
    return s

