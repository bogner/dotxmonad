import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
--import XMonad.Layout.PerWorkspace

import qualified Data.Map as M

main = xmonad bogConfig

bogConfig = defaultConfig
            { keys        = \c -> keys' `M.union`
                            keys defaultConfig c
            , layoutHook  = layoutHook'
            , logHook     = logHook'
            , manageHook  = manageHook'
            , modMask     = mod4Mask
            , terminal    = "urxvtcd"
            }

logHook' = do ewmhDesktopsLogHook
              return ()

manageHook' = composeAll
              [ className =? "Gimp"           --> doFloat
              , resource  =? "desktop_window" --> doIgnore
              , resource  =? "kdesktop"       --> doIgnore
              ] <+> manageDocks <+> doF W.swapDown

-- workspaces' = ["web", "dev", "chat" ] ++ map show [4..9]

layoutHook' =
--    onWorkspace "1" (bigTiled ||| Full) $
    avoidStruts (tiled ||| bigTiled ||| Mirror tiled ||| Full)
  where
     tiled    = Tall nmaster delta (1/2)
     bigTiled = Tall nmaster delta (11/16)
     nmaster = 1
     delta   = 3/100

keys' = M.fromList $
    [ ((mod4Mask, xK_b), sendMessage ToggleStruts)
    ]
