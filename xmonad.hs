import XMonad hiding
    (workspaces,manageHook,numlockMask,keys,logHook,borderWidth,mouseBindings
    ,defaultGaps,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor)
import qualified XMonad
    (workspaces,manageHook,numlockMask,keys,logHook,borderWidth,mouseBindings
    ,defaultGaps,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor)
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import qualified Data.Map as M
import Data.Bits ((.|.))

main = xmonad bogConfig

bogConfig = defaultConfig
            { XMonad.keys          = \c -> keys `M.union` XMonad.keys defaultConfig c
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
       , ((modMask,               xK_b), sendMessage ToggleStruts)
       ]

mouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, button1), (\w -> focus w >> float w))
    , ((modMask, button1),
       (\w -> focus w >> withFloat mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask .|. controlMask, button1),
       (\w -> focus w >> withFloat mouseResizeWindow w))
    ]

withFloat f w = gets windowset >>= \ws -> if (isFloat w ws)
                                          then f w
                                          else return ()
    where isFloat w ws = M.member w $ W.floating ws

-- TODO: if we can determine what window is at a given x y coord, then
{-
moveTiled w = whenX (isClient w) $ withDisplay $ \d -> do
                io $ raiseWindow d w -- we probably don't need this
                (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
                let ox = fromIntegral ox'
                    oy = fromIntegral oy'
                mouseDrag (\ex ey -> io $ swapWins (windowAt ex ey) (windowAt ox oy))
-}

xpConfig = defaultXPConfig
           { font        = "xft:Bitstream Vera Sans Mono:pixelsize=10"
           , bgColor     = "#3f3f3f"
           , fgColor     = "#dcdccc"
           , fgHLight    = "#94bff3"
--           , bgHLight    = "#dcdccc"
           , borderColor = "#dcdccc"
           , position    = Top
           }

manageHook = composeAll
             [ className =? "Emacs"           --> doF (W.shift "dev")
             , className =? "Firefox-bin"     --> doF (W.shift "web" . W.swapUp)
             , className =? "Pidgin"          --> doF (W.shift "com")
             , className =? "Thunderbird-bin" --> doF (W.shift "com")
             , resource  =? "xdvi"            --> doF W.swapUp
             , resource  =? "gv"            --> doF W.swapUp
             , manageDocks
             ] <+> doF W.swapDown

workspaces = ["web", "dev", "com" ] ++ map show [4..9]

layoutHook =
    avoidStruts $
    tiled ||| bigTiled ||| Mirror tiled
              ||| Mirror Grid ||| noBorders Full
        where
          tiled    = Tall nmaster delta (1/2)
          bigTiled = Tall nmaster delta (11/16)
          nmaster  = 1
          delta    = 3/100
