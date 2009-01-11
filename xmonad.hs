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
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.State (when)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import qualified Data.Map as M
import Text.Regex.Posix ((=~))

main :: IO ()
main = xmonad bogConfig

------------------------------------------------------------
-- Configurations
------------------------------------------------------------

bogConfig = defaultConfig
            { XMonad.focusedBorderColor = "#5c888b"
            , XMonad.keys               = addKeys keys
            , XMonad.layoutHook         = layoutHook
            , XMonad.logHook            = logHook
            , XMonad.manageHook         = manageHook
            , XMonad.modMask            = super
            , XMonad.mouseBindings      = mouse
            , XMonad.normalBorderColor  = fgColor
            , XMonad.terminal           = "urxvtcd"
            , XMonad.workspaces         = workspaces
            }

xpConfig :: Prompt.XPConfig
xpConfig = Prompt.defaultXPConfig
           { Prompt.font        = "xft:Bitstream Vera Sans Mono:pixelsize=10"
           , Prompt.bgColor     = bgColor
           , Prompt.fgColor     = fgColor
           , Prompt.fgHLight    = bgColor
           , Prompt.bgHLight    = fgColor
           , Prompt.borderColor = fgColor
           , Prompt.position    = Prompt.Top
           }

------------------------------------------------------------
-- Configuration definitions
------------------------------------------------------------
super :: KeyMask
super = mod4Mask

bgColor, fgColor :: [Char]
bgColor = "#3f3f3f"
fgColor = "#dcdccc"

keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
       [ ((super,           xK_p),     runOrRaisePrompt xpConfig)
       , ((super .|. shift, xK_p),     shellPrompt xpConfig)
       , ((super,           xK_b),     sendMessage ToggleStruts)
       -- alt-tab, for when others use my computer
       , ((super,           xK_Tab),   windows W.focusDown)
       , ((super,           xK_grave), windows viewPrev)
       , ((super,           xK_w),     kill)
       , ((super .|. shift, xK_slash), spawn "todo-notify.sh")
       ]
    where shift = shiftMask

mouse :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
mouse _ = M.fromList $
    [ ((super .|. shift, button1), (\w -> focus w >> float w))
    , ((super,           button1), mouseAction mouseMoveWindow)
    , ((super .|. ctrl,  button1), mouseAction mouseResizeWindow)
    ]
    where ctrl  = controlMask
          shift = shiftMask
          mouseAction f = \w -> whenFloat w (focus w >> f w
                                                     >> windows W.shiftMaster)

layoutHook =
    smartBorders $
    layoutHints $
    ewmhDesktopsLayout $ avoidStruts $
    FixedColumn 1 20 80 10 ||| Full

logHook :: X ()
logHook = ewmhDesktopsLogHook
          >> ewmhFewerDesktopsLogHook
          >> fadeInactiveLogHook 0xe0000000

manageHook :: ManageHook
manageHook = composeAll
             [ resource  =? "Navigator"      --> doF (shiftView "web")
             -- floats should always appear at the very top
             , floating                      --> doF W.shiftMaster
             -- Some windows should always come first...
             , className <? [ "Emacs"
                            , "GV"
                            , "XDvi"
                            , "Xpdf"
                            ]                --> doF W.shiftMaster
             -- apps that are bad at tiling
             , className <? [ "feh"
                            , "Gitk"
                            , "Meld"
                            , "Kompare"
                            , "Mbrowse"
                            , "Zenity"
                            ]                --> doFloat
             -- bitkeeper's dialogs have stupid names
             , className ~? "(Diff|Rev)tool" --> doFloat
             , className ~? "Toplevel"       --> doFloat
             --
             , manageDocks
             ] <+> doF W.swapDown
    where
      floating = (ask >>= liftX . willFloat)
                 -- gnome panel applets make everything shift around
                 -- when shifted to master.
                 <&&> (liftM not $ resource <? [ "gnome-panel"
                                               , "mixer_applet2"
                                               ])

workspaces :: [[Char]]
workspaces = ["web", "dev", "com"] ++ map show [4..9]

------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------

type KeyMap = M.Map (ButtonMask, KeySym) (X ())

-- | Add the given keymap @k@ to the default XMonad keymap, choosing
--   elements from @k@ in case of conflicting bindings.
addKeys :: KeyMap -> XConfig Layout -> KeyMap
addKeys k c = M.union k $ XMonad.keys defaultConfig c

-- | View the most recently viewed workspace
viewPrev :: W.StackSet i l a s sd -> W.StackSet i l a s sd
viewPrev s = s { W.current = (W.current s) { W.workspace = head (W.hidden s) }
               , W.hidden = W.workspace (W.current s) : tail (W.hidden s) }

-- | Perform the X action defined by @f@ when @w@ is a floating
--   window, and do nothing otherwise.
whenFloat :: Window -> X () -> X ()
whenFloat w f = isFloat w >>= \b -> when b f

-- | Determine whether or not @w@ is a floating window
isFloat :: Window -> X Bool
isFloat w = gets windowset >>= \ws -> return (M.member w $ W.floating ws)

-- This is logic copied from XMonad.Operations.manage, since
-- manageHook is called before windows are floated
-- | Determine if @w@ will be floated when it becomes managed.
willFloat :: Window -> X Bool
willFloat w = withDisplay $ \d -> do
                sh <- io $ getWMNormalHints d w
                let isFixedSize = sh_min_size sh /= Nothing
                                  && sh_min_size sh == sh_max_size sh
                isTransient <- isJust <$> io (getTransientForHint d w)
                f <- isFloat w
                return (isFixedSize || isTransient || f)

-- | shift the focused window to workspace @w@, and follow it there
shiftView :: (Ord a, Eq i, Eq s)
             => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
shiftView w = W.greedyView w . W.shift w

-- | @q ~? x@. if the result of @q@ matches the Regex @x@, return 'True'.
q ~? x = fmap (=~ x) q

-- | Return 'True' if @q@ is an element of @xs@
q <? xs = fmap (flip elem xs) q
