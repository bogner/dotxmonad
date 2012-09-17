{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.FixedMultiColumn
-- Copyright   :  (c) 2011 Justin Bogner <mail@justinbogner.com>
-- License     :  BSD3-style (as xmonad)
--
-- Maintainer  :  Justin Bogner <mail@justinbogner.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that favors window widths that are a multiple of a
-- window's minimum resize amount. It will keep adding columns until
-- doing so would make windows that wouldn't be allowed to be wide
-- enough.
--
-----------------------------------------------------------------------------

module XMonad.Layout.FixedMultiColumn (
    -- * Usage
    -- $usage
        FixedMultiColumn(..)
) where

import Control.Monad (msum)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.X11 (Rectangle(..))
import Graphics.X11.Xlib (Window, rect_width)
import Graphics.X11.Xlib.Extras ( getWMNormalHints
                                , getWindowAttributes
                                , sh_base_size
                                , sh_resize_inc
                                , wa_border_width )

import XMonad.Core (X, LayoutClass(..), fromMessage, io, withDisplay)
import XMonad.Layout ( Resize(..), IncMasterN(..)
                     , splitHorizontallyBy, splitVertically, tile )
import XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.FixedMultiColumn
--
-- Then edit your @layoutHook@ by adding the FixedMultiColumn layout:
--
-- > myLayout = FixedMultiColumn 1 20 80 10 ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | A tiling mode based on preserving a nice fixed width
--   window. Supports 'Shrink', 'Expand' and 'IncMasterN'.
data FixedMultiColumn a =
    FixedMultiColumn !Int -- Number of windows in the master pane
                     !Int -- Number to increment by when resizing
                     !Int -- Default width of master pane
                     !Int -- Column width for normal windows
    deriving (Read, Show)

instance LayoutClass FixedMultiColumn Window where
    doLayout (FixedMultiColumn nmaster _ ncol fb) r s = do
            fws <- mapM (widthCols fb ncol) ws
            let rs   = getRects nmaster r fws
            return $ (zip ws rs, Nothing)
        where ws     = W.integrate s

    pureMessage (FixedMultiColumn nmaster delta ncol fb) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]
        where resize Shrink
                  = FixedMultiColumn nmaster delta (max 0 $ ncol - delta) fb
              resize Expand
                  = FixedMultiColumn nmaster delta (ncol + delta) fb
              incmastern (IncMasterN d)
                  = FixedMultiColumn (max 1 (nmaster+d)) delta ncol fb

    description _ = "FixedMultiColumn"

getRects :: Int -> Rectangle -> [Int] -> [Rectangle]
getRects nmaster r widths
    | nwidths <= nmaster = splitVertically nwidths r
    | otherwise          = splitVertically nmaster r1 ++ getGrid r2 widthsLeft
    where nwidths    = length widths
          widthsLeft = drop nmaster widths
          (r1,r2)    = splitByWidest (take nmaster widths) r

splitByWidest :: [Int] -> Rectangle -> (Rectangle, Rectangle)
splitByWidest ws r  = splitHorizontallyBy (maxWidth / rectWidth) r
    where maxWidth  = fromIntegral $ maximum ws   :: Rational
          rectWidth = fromIntegral $ rect_width r :: Rational

getGrid :: Rectangle -> [Int] -> [Rectangle]
getGrid r widths = splitIntoPanes nEach widths r
    where nEach  = fromMaybe max $ find fits $ [1..max]
          max    = length widths
          fits x = sum (nwiseMax x widths) <= rwidth
          rwidth = fromIntegral . rect_width $ r

splitIntoPanes :: Int -> [Int] -> Rectangle -> [Rectangle]
splitIntoPanes nEach widths r
    | length widths <= nEach = splitVertically howMany r
    | otherwise              = splitVertically howMany left ++ rest
    where nWidths      = length widths
          rest         = splitIntoPanes nEach more right
          (these,more) = splitAt howMany widths
          (left,right) = splitByWidest these r
          howMany      = fromNonzero (mod (length widths) nEach) nEach
          fromNonzero x y | x /= 0    = x
                          | otherwise = y

nwiseMax :: Int -> [Int] -> [Int]
nwiseMax _ [] = []
nwiseMax n xs = maximum nxs : nwiseMax n rest
    where (nxs, rest) = splitAt n xs

-- | Determine the width of @w@ given that we would like it to be @n@
--   columns wide, using @inc@ as a resize increment for windows that
--   don't have one
widthCols :: Int -> Int -> Window -> X Int
widthCols inc n w = withDisplay $ \d -> io $ do
    sh <- getWMNormalHints d w
    bw <- fmap (fromIntegral . wa_border_width) $ getWindowAttributes d w
    let widthHint f = f sh >>= return . fromIntegral . fst
        oneCol      = fromMaybe inc $ widthHint sh_resize_inc
        base        = fromMaybe 0 $ widthHint sh_base_size
    return $ 2 * bw + base + n * oneCol
