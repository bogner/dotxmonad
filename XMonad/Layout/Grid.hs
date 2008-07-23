{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Grid
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A simple layout that attempts to put all windows in a square grid.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Grid (
    -- * Usage
    -- $usage
        Grid(..)
) where

import XMonad
import XMonad.Layout
import XMonad.StackSet

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Grid
--
-- Then edit your @layoutHook@ by adding the Grid layout:
--
-- > myLayouts = Grid ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data Grid a = Grid deriving (Read, Show)

instance LayoutClass Grid a where
    pureLayout Grid r s = zip ws rs
      where ws = integrate s
            rs = tile r (length ws)
    description _ = "Grid"

tile :: Rectangle -> Int -> [Rectangle]
tile r n = (splitCols x) (init rows)
           ++ (splitCols $ if (p==0) then x else p) [last rows]
    where x = ceiling . (sqrt :: Double -> Double) . fromIntegral $ n
          splitCols = concatMap . splitVertically
          rows = splitHorizontally (q+if (p/=0) then 1 else 0) r
          (q,p) = divMod n x
