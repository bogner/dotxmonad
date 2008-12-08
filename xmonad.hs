{-# LANGUAGE FlexibleInstances #-}
import XMonad hiding (keys,layoutHook,logHook,manageHook,modMask,workspaces)
import qualified XMonad (keys,layoutHook,logHook,manageHook,modMask,workspaces)
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLayout,ewmhDesktopsLogHook)
import XMonad.Hooks.EwmhFewerDesktops (ewmhFewerDesktopsLogHook)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks,ToggleStruts(..))
import XMonad.Layout.FixedColumn (FixedColumn(..))
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise

import Prelude hiding (catch)

import Control.Applicative ((<$>))
import Control.Monad.State (when)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import qualified Data.Map as M
import Text.Regex.Posix ((=~))

main :: IO ()
main = xmonad bogConfig

bogConfig = defaultConfig
            { XMonad.focusedBorderColor = "#5c888b"
            , XMonad.keys               = addKeys keys
            , XMonad.layoutHook         = layoutHook
            , XMonad.logHook            = logHook
            , XMonad.manageHook         = manageHook
            , XMonad.modMask            = modMask
            , XMonad.mouseBindings      = mouse
            , XMonad.normalBorderColor  = "#dcdccc"
            , XMonad.terminal           = "dterm.sh"
            , XMonad.workspaces         = workspaces
            }

modMask :: KeyMask
modMask = mod4Mask -- Super

addKeys :: M.Map (ButtonMask, KeySym) (X ())
           -> XConfig Layout
           -> M.Map (ButtonMask, KeySym) (X ())
addKeys k c = M.union k $ XMonad.keys defaultConfig c

keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
       [ ((super,           xK_p),     runOrRaisePrompt xpConfig)
       , ((super .|. shift, xK_p),     shellPrompt xpConfig)
       , ((super,           xK_b),     sendMessage ToggleStruts)
       -- alt-tab, for when others use my computer
       , ((alt,             xK_Tab),   windows W.focusDown)
       -- since we have alt-tab, super-tab might as well cycle backwards
       , ((super,           xK_Tab),   windows W.focusUp)
       , ((super,           xK_grave), windows viewPrev)
       , ((super,           xK_w),     kill)
       , ((super .|. shift, xK_slash), spawn "todo-notify.sh")
       ]
    where super = modMask
          alt   = mod1Mask
          shift = shiftMask

viewPrev :: W.StackSet i l a s sd -> W.StackSet i l a s sd
--viewHidden = W.view . head . W.hidden
viewPrev s = s { W.current = (W.current s) { W.workspace = head (W.hidden s) }
               , W.hidden = W.workspace (W.current s) : tail (W.hidden s) }

mouse :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
mouse _ = M.fromList $
    [ ((super .|. shift, button1), (\w -> focus w >> float w))
    , ((super,           button1), mouseAction mouseMoveWindow)
    , ((super .|. ctrl,  button1), mouseAction mouseResizeWindow)
    ]
    where super = modMask
          ctrl  = controlMask
          shift = shiftMask
          mouseAction f = \w -> whenFloat w (focus w >> f w
                                                     >> windows W.shiftMaster)

whenFloat :: Window -> X () -> X ()
whenFloat w f = isFloat w >>= \b -> when b f

isFloat :: Window -> X Bool
isFloat w = gets windowset >>= \ws -> return (M.member w $ W.floating ws)

xpConfig :: XPConfig
xpConfig = defaultXPConfig
           { font        = "xft:Bitstream Vera Sans Mono:pixelsize=10"
           , bgColor     = "#3f3f3f"
           , fgColor     = "#dcdccc"
           , fgHLight    = "#3f3f3f"
           , bgHLight    = "#dcdccc"
           , borderColor = "#dcdccc"
           , position    = Top
           }

manageHook :: ManageHook
manageHook = composeAll
             [ resource  =? "Navigator"      --> doF (shiftView "web")
             -- floats should always appear at the very top
             , floating                      --> doF W.shiftMaster
             -- Some windows should always come first...
             , className =? "Emacs"          --> doF W.shiftMaster
             , className =? "XDvi"           --> doF W.shiftMaster
             , className =? "GV"             --> doF W.shiftMaster
             -- apps that are bad at tiling
             , className =? "feh"            --> doFloat
             , className =? "Gitk"           --> doFloat
             , className =? "Meld"           --> doFloat
             , className =? "Kompare"        --> doFloat
             , className =? "Mbrowse"        --> doFloat
             -- bitkeeper's dialogs have stupid names
             , className ~? "(Diff|Rev)tool" --> doFloat
             , className ~? "Toplevel"       --> doFloat
             --
             , manageDocks
             ] <+> doF W.swapDown
    where
      floating = (ask >>= liftX . willFloat)

-- This is logic copied from XMonad.Operations.manage, since
-- manageHook is called before windows are floated
-- | Determine if @w@ will be a floating window
willFloat :: Window -> X Bool
willFloat w = withDisplay $ \d -> do
                sh <- io $ getWMNormalHints d w
                let isFixedSize = sh_min_size sh /= Nothing
                                  && sh_min_size sh == sh_max_size sh
                isTransient <- isJust <$> io (getTransientForHint d w)
                f <- isFloat w
                return (isFixedSize || isTransient || f)

shiftView w = W.greedyView w . W.shift w

-- | @q ~? x@. if the result of @q@ matches the Regex @x@, return 'True'.
q ~? x = fmap (=~ x) q

workspaces :: [[Char]]
workspaces = ["web", "dev", "com"] ++ map show [4..9]

layoutHook =
    smartBorders $
    layoutHints $
    ewmhDesktopsLayout $ avoidStruts $
    FixedColumn 1 20 80 10 ||| Full

logHook :: X ()
logHook = ewmhDesktopsLogHook
          >> ewmhFewerDesktopsLogHook
          >> fadeInactiveLogHook 0xe0000000
