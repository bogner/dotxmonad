import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
--import XMonad.Layout.PerWorkspace

main = xmonad bogConfig

bogConfig = defaultConfig
            { defaultGaps = [(24,0,0,0)]
            , logHook     = logHook'
            , manageHook  = manageHook'
            , modMask     = mod4Mask
            , terminal    = "urxvtcd"
            , layoutHook  = layoutHook'
            }

logHook' = do ewmhDesktopsLogHook
              return ()

manageHook' = composeAll
              [ className =? "Gimp"           --> doFloat
              , resource  =? "desktop_window" --> doIgnore
              , resource  =? "kdesktop"       --> doIgnore
              , resource  =? "gnome-panel"    --> doIgnore
              , resource  =? "kicker"         --> doIgnore
              ] <+> doF W.swapDown

-- workspaces' = ["web", "dev", "chat" ] ++ map show [4..9]

layoutHook' =
--    onWorkspace "1" (bigTiled ||| Full) $
    tiled ||| bigTiled ||| Mirror tiled ||| Full
  where
     tiled    = Tall nmaster delta (1/2)
     bigTiled = Tall nmaster delta (11/16)
     nmaster = 1
     delta   = 3/100
