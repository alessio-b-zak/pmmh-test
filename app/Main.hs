module Main where

import Lib
import Charting
import System.Environment
import PMC
import PMMHTest
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Inference.PMMH

nsteps :: Int
nsteps = 14
--
--nparticles :: Int
--nparticles = 10

initParams :: Double
initParams = 2 


generateAndInfer mhs ns np params rprior = do
    dat <- gen_random_walk ns params
    posterior <- prior $ pmc mhs ns np rprior (random_walk' dat)
    post <- prior $ pmmh mhs ns np rprior (random_walk dat)
    let (posterior', _) = unzip $ head <$> post
    return (posterior, posterior')


main :: IO ()
main = do
    args  <- getArgs
    (posterior, posterior') <- sampleIO $ generateAndInfer (read $ args !! 0) nsteps (read $ args !! 1) initParams s 
    print $ length posterior
    print $ length posterior'
    toHtmlFile "posterior.html" $ plotDensity posterior
    toHtmlFile "tPosterior.html" $ plotTrace posterior
    toHtmlFile "posteriorp.html" $ plotDensity posterior'
    toHtmlFile "tPosteriorp.html" $ plotTrace posterior'

    -- run inference with generated data
    -- plot densities of params

