{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PerRow
-- Copyright   :  (c) 2008 Justin Bogner
-- License     :  BSD3
--
-- Maintainer  :  mail@justinbogner.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout which gives each independent application it's own row of
-- windows
--
-----------------------------------------------------------------------------

module XMonad.Layout.PerRow
    ( -- * Usage:
      -- $usage
      PerRow (..)
    ) where

import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.PerRow
--
-- Then edit your @layoutHook@ by adding the PerRow layout:
--
-- > myLayouts = PerRow ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- $comments
-- This only groups consecutive windows of an application, so that
-- changing the order of windows always has an effect. It might be
-- better to group them no matter what order they come in, but I
-- haven't worked out a way to do that that doesn't result in very
-- awkward behaviour.

data PerRow a = PerRow deriving (Read, Show)

instance LayoutClass PerRow Window where
    doLayout PerRow r s = groupByM sameClass (W.integrate s) >>=
                          \ws -> return ((arrange r ws),Nothing)
    description _ = "PerRow"

-- span and groupBy lifted to monads, thanks to Cale on #haskell
spanM             :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
spanM p []         = return ([],[])
spanM p xs@(x:xs') = do v <- p x
                        if v then do (ys,zs) <- spanM p xs'
                                     return (x:ys,zs)
                             else return ([], xs)

groupByM           :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM eq []     = return []
groupByM eq (x:xs) = do (ys,zs) <- spanM (eq x) xs
                        gs <- groupByM eq zs
                        return ((x:ys) : gs)

sameClass w1 w2 = flip runQuery w1 $ getClass w1 >>= \q -> getClass w2 =? q
getClass w = liftX $ withDisplay $ \d -> fmap resClass $ io $ getClassHint d w

arrange      :: Eq a => Rectangle -> [[a]] -> [(a, Rectangle)]
arrange r ws = concat $ zipWith place (splitVertically (length ws) r) ws

place      :: Rectangle -> [a] -> [(a, Rectangle)]
place r ws = zip ws $ splitHorizontally (length ws) r
