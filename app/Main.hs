#!/usr/bin/env stack

module Main where

import Gaussian 
import Control.Concurrent
import GHC.Conc ( numCapabilities, pseq )
import System.Environment (getArgs)
import GHC.Conc.Sync (par)
import Text.Printf (printf)

main :: IO ()
main = do
  let size = 3    

  
  let mat1 = [[1, 1, 1], [2, -1, 1], [-3, 4, 1]] 
  let vec1 = [6, 3, 7]
      
  print mat1
  print vec1

  let ans = gaussian mat1 vec1 
  print ans
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print y

