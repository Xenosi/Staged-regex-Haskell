{-# LANGUAGE TemplateHaskell #-}

module Main where

import Criterion.Main ( defaultMain, bench, bgroup, nf, whnf )
import TH
import Regex ( constructDFA, matchDFA, test1 )
import Language.Haskell.TH.Syntax ()
import Data.List ( elemIndex )
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.PCRE as PCRE


predicate :: String -> [String] -> Bool
predicate s l = case elemIndex s l of
                    Nothing -> error "string not found"
                    Just i -> mod i 11 == 10


main :: IO ()
main = do
    file <- readFile "gentest1.txt"
    let list = lines file
    let current = drop 114 $ take 223 list
    let filtered = filter (\x -> not $ predicate x current) current
    defaultMain [
        bgroup "naive" [
            bench x (nf (matchDFA dfa) x)
            | x <- filtered
        ]
        bgroup "pcre" [
            bench x (nf (PCRE.matchAll regex) x)
            | x <- filtered
        ]
        bgroup "staged" [
            bench x (whnf $(match') x)
            | x <- filtered]
        ]
    where dfa = constructDFA test1
          regex = PCRE.makeRegex "[r]+f[U]*b[h]+j[t]+" :: PCRE.Regex
