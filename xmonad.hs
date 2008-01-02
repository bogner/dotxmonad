import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
--import XMonad.Layout.PerWorkspace
import XMonad.StackSet (floating)

import qualified Data.Map as M
import Data.Bits ((.|.))

main = xmonad bogConfig

bogConfig = defaultConfig
            { defaultGaps = [(24,0,0,0)]
            , keys        = \c -> keys' `M.union` keys defaultConfig c
            , mouseBindings = mouse'
            , logHook     = logHook'
            , manageHook  = manageHook'
            , modMask     = modMask'
            , terminal    = "urxvtcd"
            , workspaces  = workspaces'
            , layoutHook  = layoutHook'
            }

modMask' = mod4Mask

keys' = M.fromList $
        [ ((modMask',               xK_p), shellPrompt xpConfig)
        , ((modMask' .|. shiftMask, xK_p), windowPromptGoto xpConfig)
        ]

mouse' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouse' (XConfig {XMonad.modMask = modMask'}) = M.fromList $
    [ ((modMask' .|. shiftMask, button1), (\w -> focus w >> float w))
    , ((modMask', button1), (\w -> focus w >>
                                   ifFloat w mouseMoveWindow))
    -- button 2 is middle click
    , ((modMask', button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask', button3), (\w -> focus w >> 
                                   ifFloat w mouseResizeWindow))
    ]

ifFloat w f = withWindowSet $ \s ->
              if (M.member w (floating s)) then f w else return ()

moveIfFloating w = withWindowSet $ \s -> do
                     focus w
                     if (M.member w (floating s)) then
                          mouseMoveWindow w else
                          windows $ W.sink w

xpConfig =
    XPC { font              = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
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

logHook' = do ewmhDesktopsLogHook
              return ()

manageHook' = composeAll
              [ className =? "Gimp"           --> doFloat
              , className =? "Firefox-bin"    --> doF (W.shift "web")
              , resource  =? "desktop_window" --> doIgnore
              , resource  =? "kdesktop"       --> doIgnore
              , resource  =? "gnome-panel"    --> doIgnore
              , resource  =? "kicker"         --> doIgnore
              ] <+> doF W.swapDown

workspaces' = ["web", "dev", "com" ] ++ map show [4..9]

layoutHook' =
--    onWorkspace "1" (bigTiled ||| Full) $
    tiled ||| bigTiled ||| Mirror tiled ||| Full
  where
     tiled    = Tall nmaster delta (1/2)
     bigTiled = Tall nmaster delta (11/16)
     nmaster = 1
     delta   = 3/100
