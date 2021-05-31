{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module TH where

import Language.Haskell.TH.Syntax
    ( returnQ,
      mkName,
      Exp(LetE, VarE, AppE, LamCaseE, ConE),
      Match(Match),
      Clause(Clause),
      Q,
      Pat(VarP, LitP, InfixP, WildP),
      Dec(FunD),
      Name,
      Body(NormalB),
      Lit(StringL, CharL) )
import Regex ( State, delta1, s01, stateList1, acceptMap, alphabet, alphabetMap )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict
import qualified Data.Map.Strict as Map


match' :: Q Exp
match' = returnQ (LetE decs (VarE (nameMap Map.! s01)))
    where decs =
            [FunD
                (nameMap Map.! state)
                [Clause
                    []
                    (NormalB
                        (LamCaseE
                            (Match
                                (LitP (StringL []))
                                (NormalB (accMap Map.! state))
                                []
                            :
                            [Match
                                (InfixP (LitP (CharL x)) '(:) (VarP cs))
                                (NormalB (AppE (VarE (nameMap Map.! (delta1 Map.! (state, x)))) (VarE cs)))
                                []
                                | x <- alphabetMap Map.! state
                            ] ++ 
                            [Match 
                                WildP
                                (NormalB (ConE 'False)) 
                                []
                            ])
                        )
                    )
                    []
                ]
                | state <- stateList1]
          nameMap = genNames stateList1 Map.empty
          accMap = acceptMap stateList1 Map.empty
          cs = mkName "cs"


genNames :: [State] -> Map State Name -> Map State Name
genNames cs m = Prelude.foldl (\ m c -> Map.insert c (mkName ("s" ++ show (Map.size m))) m) m cs

-- genBools :: [State] -> Map State Exp -> Map State Exp
-- genBools []     m = m
-- genBools (c:cs) m = genBools cs (Map.insert c $(lift (Set.member c acc1)) m)