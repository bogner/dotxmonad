{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.FixedColumn
-- Copyright   :  (c) Justin Bogner
-- License     :  BSD3-style (as xmonad)
--
-- Maintainer  :  <mail@justinbogner.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- TODO: what is it?
--
-----------------------------------------------------------------------------

module XMonad.Layout.FixedColumn (
    -- * Usage
    -- $usage
        FixedColumn(..)
) where

import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import Graphics.X11.Xlib (Window(..), rect_width)
import Graphics.X11.Xlib.Extras ( getWMNormalHints
                                , getWindowAttributes
                                , sh_resize_inc
                                , wa_border_width)

import XMonad.Core (X, LayoutClass(..), fromMessage, io, withDisplay)
import XMonad.Layout (Resize(..), IncMasterN(..), tile)
import XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.FixedColumn
--
-- Then edit your @layoutHook@ by adding the FixedColumn layout:
--
-- > myLayouts = FixedColumn ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data FixedColumn a = FixedColumn !Int !Int !Int deriving (Read, Show)

instance LayoutClass FixedColumn Window where
    doLayout (FixedColumn nmaster _ ncol) r s = do
            fws <- mapM (widthCols ncol) ws
            let frac = maximum (take nmaster fws) // rect_width r
                rs   = tile frac r nmaster (length ws)
            return $ (zip ws rs, Nothing)
        where ws     = W.integrate s
              x // y = fromIntegral x / fromIntegral y

    pureMessage (FixedColumn nmaster delta ncol) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]
        where resize Shrink = FixedColumn nmaster delta (max 0 $ ncol - delta)
              resize Expand = FixedColumn nmaster delta (min 1 $ ncol + delta)
              incmastern (IncMasterN d) =
                  FixedColumn (max 0 (nmaster+d)) delta ncol

    description _ = "FixedColumn"

widthCols :: Int -> Window -> X Int
widthCols n w = withDisplay $ \d -> io $ do
    sh <- getWMNormalHints d w
    bw <- fmap (fromIntegral . wa_border_width) $ getWindowAttributes d w
    let oneCol = fromMaybe 5 (sh_resize_inc sh
                              >>= \(x, y) -> return $ fromIntegral y)
    io . putStrLn $ show n ++ " " ++ show oneCol
    return $ 2 * bw + n * oneCol
