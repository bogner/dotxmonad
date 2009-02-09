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
-- Like EwmhDesktops, but only reports workspaces that have windows on
-- them. This allows your pager to show your desktops, but without
-- taking up too much space.
--
-----------------------------------------------------------------------------
module XMonad.Hooks.EwmhFewerDesktops (
    -- * Usage
    -- $usage
    ewmhFewerDesktopsLogHook
    ) where

import Data.Maybe

import XMonad
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHookCustom)
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhFewerDesktops
-- >
-- > myLogHook :: X ()
-- > myLogHook = ewmhFewerDesktopsLogHook
-- >
-- > main = xmonad defaultConfig { logHook = myLogHook }
--
-- This logHook replaces the logHook from XMonad.Hooks.EwmhDesktops,
-- so that one should be removed from your configuration if using this
-- one.
--
-- For more detailed instructions on editing the logHook see:
--
-- "XMonad.Doc.Extending#The_log_hook_and_external_status_bars"

ewmhFewerDesktopsLogHook = withWindowSet $ \s ->
    ewmhDesktopsLogHookCustom $ dropBoring s

-- | Drops empty and inactive workspaces from the end of the list
dropBoring :: Eq i
              => W.StackSet i l a s sd -> [W.Workspace i l a]
                                       -> [W.Workspace i l a]
dropBoring s = reverse . dropWhile boring . reverse
    where boring (W.Workspace _ _ (Just _)) = False
          boring (W.Workspace i _ Nothing)  = i /= W.currentTag s
