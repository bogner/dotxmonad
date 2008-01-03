import XMonad hiding
    (workspaces,manageHook,numlockMask,keys,logHook,borderWidth,mouseBindings
    ,defaultGaps,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor)
import qualified XMonad
    (workspaces,manageHook,numlockMask,keys,logHook,borderWidth,mouseBindings
    ,defaultGaps,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor)
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
--import XMonad.Layout.PerWorkspace
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import qualified Data.Map as M
import Data.Bits ((.|.))

main = xmonad bogConfig

bogConfig = defaultConfig
            { XMonad.defaultGaps   = [(24,0,0,0)]
            , XMonad.keys          = \c -> keys `M.union` XMonad.keys defaultConfig c
            , XMonad.mouseBindings = mouse
            , XMonad.logHook       = ewmhDesktopsLogHook
            , XMonad.manageHook    = manageHook
            , XMonad.modMask       = modMask
            , XMonad.terminal      = "urxvtcd"
            , XMonad.workspaces    = workspaces
            , XMonad.layoutHook    = layoutHook
            }

modMask = mod4Mask

keys = M.fromList $
       [ ((modMask,               xK_p), shellPrompt xpConfig)
       , ((modMask .|. shiftMask, xK_p), windowPromptGoto xpConfig)
       ]

mouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, button1), (\w -> focus w >> float w))
    , ((modMask, button1), (\w -> focus w >> doWithWS (mouseMove w)))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask .|. controlMask, button1), (\w -> focus w >> doWithWS (mouseResize w)))
    ]

isFloat w ws = M.member w $ W.floating ws

mouseMove w ws | isFloat w ws = mouseMoveWindow w
               | otherwise    = return ()
-- if we can determine what window is at a given x y coord, then
{-
moveTiled w = whenX (isClient w) $ withDisplay $ \d -> do
                io $ raiseWindow d w -- we probably don't need this
                (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
                let ox = fromIntegral ox'
                    oy = fromIntegral oy'
                mouseDrag (\ex ey -> io $ swapWindows (windowAt ex ey) (windowAt ox oy))
-}

mouseResize w ws | isFloat w ws = mouseResizeWindow w
                 | otherwise    = return ()

doWithWS f = gets windowset >>= \s -> f s

xpConfig =
    XPC { font              = "xft:Bitstream Vera Sans Mono:pixelsize=10"
        , bgColor           = "#3f3f3f"
        , fgColor           = "#dcdccc"
        , fgHLight          = "#3f3f3f"
        , bgHLight          = "#dcdccc"
        , borderColor       = "#dcdccc"
        , promptBorderWidth = 1
        , position          = Top
        , height            = 18
        , historySize       = 256
        }

manageHook = composeAll
             [ className =? "Emacs"           --> doF (W.shift "dev")
             , className =? "Firefox-bin"     --> doF (W.shift "web")
             , className =? "Gimp"            --> doFloat
             , className =? "Pidgin"          --> doF (W.shift "com")
             , className =? "Thunderbird-bin" --> doF (W.shift "com")
             , resource  =? "desktop_window"  --> doIgnore
             , resource  =? "kdesktop"        --> doIgnore
             , resource  =? "gnome-panel"     --> doIgnore
             , resource  =? "kicker"          --> doIgnore
             ] <+> doF W.swapDown

workspaces = ["web", "dev", "com" ] ++ map show [4..9]

layoutHook =
--    onWorkspace "1" (bigTiled ||| Full) $
    tiled ||| bigTiled ||| Mirror tiled ||| Grid ||| noBorders Full
        where
          tiled    = Tall nmaster delta (1/2)
          bigTiled = Tall nmaster delta (11/16)
          nmaster = 1
          delta   = 3/100
