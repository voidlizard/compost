#!/usr/bin/env stack
{- stack --resolver lts-7.15 --install-ghc runghc
         --package interpolatedstring-perl6
         --package random
         --package text
-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, OverloadedStrings #-}

import System.Random
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Text.InterpolatedString.Perl6 (q)
import System.Environment
import Text.Read
import Data.Maybe
import Control.Monad

-- letters :: [(Char, Char, Text)]
-- letters = undefined $ T.words
--   where
greek' :: Text
greek' = [q|Α α	alpha
            Β β	beta
            Γ γ	gamma
            Δ δ	delta
            Ε ε	epsilon
            Ζ ζ	zeta
            Η η	eta
            Θ θ	theta
            Ι ι	iotau
            Κ κ	kappa
            Λ λ	lambda
            Μ μ	mu
            Ν ν	nu
            Ξ ξ	xi
            Ο ο	omicron
            Π π	pi
            Ρ ρ	rho
            Σ σ sigma
            Τ τ	tau
            Υ υ	upsilon
            Φ φ	phi
            Χ χ	chi
            Ψ ψ	psi
            Ω ω	omega|]

greek :: [(Char, Char, Text)]
greek = foldMap tr $ fmap T.words $ T.lines greek'
  where tr [l1, l2, n] = [(T.head l1, T.head l2, n)]
        tr _ = []

main = do
  (n':m':_) <- getArgs
  let n = fromMaybe 3 (readMaybe n') :: Int
  let m = fromMaybe 10 (readMaybe m') :: Int
  let gl = length greek
  let un (a1, a2, a3) = a3
  let gIdx n = n `mod` gl
  replicateM_ m $ do
    lls <- replicateM n (randomIO >>= \i -> return $ un $ greek !! (gIdx i))
    IO.putStrLn (T.intercalate " " lls)
