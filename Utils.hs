{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( todo,
    chunksOf,
  )
where

import Language.Haskell.TH
    ( Exp, Q, location, Loc(loc_filename, loc_start) )

todo :: String -> Q Exp
todo msg = do
  loc <- location
  let pos = formatLoc loc
  [| error ("TODO at " ++ pos ++ " - " ++ msg) |]
  where
  formatLoc loc =
    let (line, col) = loc_start loc
        file = loc_filename loc
    in file ++ ":" ++ show line ++ ":" ++ show (col + 1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size xs = (take size xs):chunksOf size (drop size xs)
