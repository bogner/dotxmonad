-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.EwmhFewerDesktops
-- Copyright    : (c) 2008 Justin Bogner <mail@justinbogner.com>
-- License      : BSD3-style (as xmonad)
--
-- Maintainer   : Justin Bogner <mail@justinbogner.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Reports used workspaces to EWMH, but doesn't report workspaces that
-- don't have windows on them. This allows your pager to show your
-- desktops, but without taking up too much space.
--
-----------------------------------------------------------------------------
module XMonad.Hooks.EwmhFewerDesktops (
    -- * Usage
    -- $usage
    ewmhFewerDesktopsLogHook
    ) where

import Data.List
import Data.Maybe

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W

import XMonad.Util.WorkspaceCompare

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops
-- > import XMonad.Hooks.EwmhFewerDesktops
-- >
-- > myLogHook :: X ()
-- > myLogHook = ewmhDesktopsLogHook >> ewmhFewerDesktopsLogHook
-- >
-- > main = xmonad defaultConfig { logHook = myLogHook }
--
-- `EwmhDesktops` and `ewmhDesktopsLogHook >>` aren't strictly
-- necessary, but the above is a likely setup.
--
-- For more detailed instructions on editing the logHook see:
--
-- "XMonad.Doc.Extending#The_log_hook_and_external_status_bars"

-- This is redundant code, we actually just want to replace
-- getSortByIndex in ewmhDesktopsLogHook:
-- | Sets the _NET_NUMBER_OF_DESKTOPS X atom to @n@
setNumberOfDesktops :: (Integral a) => a -> X ()
setNumberOfDesktops n = withDisplay $ \dpy -> do
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    c <- getAtom "CARDINAL"
    r <- asks theRoot
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral n]


ewmhFewerDesktopsLogHook :: X ()
ewmhFewerDesktopsLogHook = withWindowSet $ \s -> do
    sort' <- getSortByIndex'
    let ws = sort' $ W.workspaces s
    setNumberOfDesktops (length ws)

-- | Sort by index, dropping unused workspaces from the end of the
--   list
getSortByIndex' :: X WorkspaceSort
getSortByIndex' = withWindowSet $ \s ->
    getSortByIndex >>= \f -> return $ dropBoring s . f

-- | Drops empty and inactive workspaces from the end of the list
dropBoring :: Eq i
              => W.StackSet i l a s sd -> [W.Workspace i l a]
                                       -> [W.Workspace i l a]
dropBoring s = reverse . dropWhile boring . reverse
    where boring (W.Workspace _ _ (Just _)) = False
          boring (W.Workspace i _ Nothing)  = i /= W.currentTag s
