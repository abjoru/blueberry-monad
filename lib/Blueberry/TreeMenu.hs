module Blueberry.TreeMenu where

import Blueberry.Variables
import Blueberry.Utils

import Data.Tree
import System.Exit (exitSuccess)
import qualified Data.Map as M

import XMonad
import qualified XMonad.Actions.TreeSelect as TS

------------------------------------------------------------------------
-- TREESELECT
------------------------------------------------------------------------
-- TreeSelect displays your workspaces or actions in a Tree-like format.
-- You can select desired workspace/action with the cursor or hjkl keys.
-- My TreeSelect menu is rather large and includes a mixture of applications,
-- web bookmarks, configuration files and xmonad actions.

nodeAccessories :: Tree (TS.TSNode (X ()))
nodeAccessories = Node (TS.TSNode "+ Accessories" "Accessory applications" (return ()))
  [ Node (TS.TSNode "Archive Manager" "Tool for archived packages" (spawn "file-roller")) []
  , Node (TS.TSNode "Calculator" "GUI version of qalc" (spawn "qalculate-gtk")) []
  , Node (TS.TSNode "Calibre" "Manages books on my ereader" (spawn "calibre")) []
  , Node (TS.TSNode "Castero" "Terminal podcast client" (spawn (myTerminal ++ " -e castero"))) []
  , Node (TS.TSNode "Picom Toggle on/off" "Compositor for window managers" (spawn "killall picom; picom")) []
  , Node (TS.TSNode "VirtualBox" "Oracle's virtualization program" (spawn "virtualbox")) []
  ]

-- TODO: Make this read from filesystem (i.e. games folder)
nodeGames :: Tree (TS.TSNode (X ()))
nodeGames = Node (TS.TSNode "+ Games" "fun and games" (return ()))
  [ Node (TS.TSNode "Steam" "The Steam gaming platform" (spawn "steam")) []
  , Node (TS.TSNode "Holiday Island" "RenPy Game" (spawn "~/Games/HolidayIsland-0.2.1.0-pc/HolidayIsland.sh")) []
  , Node (TS.TSNode "Broken Dreamers" "RenPy Game" (spawn "~/Games/BrokenDreamers-0.7.0-pc/BrokenDreamers.sh")) []
  , Node (TS.TSNode "Bad Memories" "RenPy Game" (spawn "~/Games/Bad-Memories-0.5.2-pc/Bad-Memories.sh")) []
  , Node (TS.TSNode "Treasure of Nardia" "RenPy Game" (spawn "~/Games/Treasure-of-Nardia-v42072/launcher.sh")) []
  , Node (TS.TSNode "False Hero" "RenPy Game" (spawn "~/Games/FalseHero-False_Hero_v0.25_RAW_BETA-pc/FalseHero.sh")) []
  , Node (TS.TSNode "Time for You" "RenPy Game" (spawn "~/Games/TIME_FOR_YOU(0.10.0)-0.10.0-pc/TIME_FOR_YOU(0.10.0).sh")) []
  , Node (TS.TSNode "What a Legend" "RenPy Game" (spawn "~/Games/What-a-Legend-0.2-pc/What-a-Legend.sh")) []
  ]

nodeGraphics :: Tree (TS.TSNode (X ()))
nodeGraphics = Node (TS.TSNode "+ Graphics" "graphics programs" (return ()))
  [ Node (TS.TSNode "Gimp" "GNU image manipulation program" (spawn "gimp")) []
  , Node (TS.TSNode "Inkscape" "An SVG editing program" (spawn "inkscape")) []
  , Node (TS.TSNode "LibreOffice Draw" "LibreOffice drawing program" (spawn "lodraw")) []
  , Node (TS.TSNode "Shotwell" "Photo management program" (spawn "shotwell")) []
  , Node (TS.TSNode "Simple Scan" "A simple scanning program" (spawn "simple-scan")) []
  ]

nodeInternet :: Tree (TS.TSNode (X ()))
nodeInternet = Node (TS.TSNode "+ Internet" "internet and web programs" (return ())) 
  [ Node (TS.TSNode "QuteBrowser" "Minimal web browser" (spawn "qutebrowser")) []
  , Node (TS.TSNode "Brave" "The privacy browser" (spawn "brave")) []
  , Node (TS.TSNode "Discord" "Chat and video chat platform" (spawn "discord")) []
  , Node (TS.TSNode "FileZilla" "An FTP client" (spawn "filezilla")) []
  , Node (TS.TSNode "Firefox" "Open source web browser" (spawn "firefox")) []
  , Node (TS.TSNode "Geary" "Email client with a nice GUI" (spawn "geary")) []
  ]

nodeMultimedia :: Tree (TS.TSNode (X ()))
nodeMultimedia = Node (TS.TSNode "+ Multimedia" "sound and video applications" (return ()))
  [ Node (TS.TSNode "Alsa Mixer" "Alsa volume control utility" (spawn (myTerminal ++ " -e alsamixer"))) []
  , Node (TS.TSNode "Plex" "Plex Media Player" (spawn "plexmediaplayer")) []
  , Node (TS.TSNode "Audacity" "Graphical audio editing program" (spawn "audacity")) []
  , Node (TS.TSNode "Deadbeef" "Lightweight music player" (spawn "deadbeef")) []
  , Node (TS.TSNode "EMMS" "Emacs multimedia player" (spawn "xxx")) []
  , Node (TS.TSNode "Kdenlive" "Open source non-linear video editor" (spawn "kdenlive")) []
  , Node (TS.TSNode "OBS Studio" "Open Broadcaster Software" (spawn "obs")) []
  , Node (TS.TSNode "Pianobar" "A terminal Pandora client" (spawn (myTerminal ++ " -e pianobar"))) []
  , Node (TS.TSNode "VLC" "Multimedia player and server" (spawn "vlc")) []
  ]

nodeOffice :: Tree (TS.TSNode (X ()))
nodeOffice = Node (TS.TSNode "+ Office" "office applications" (return ()))
  [ Node (TS.TSNode "LibreOffice" "Open source office suite" (spawn "libreoffice")) []
  , Node (TS.TSNode "LibreOffice Base" "Desktop database front end" (spawn "lobase")) []
  , Node (TS.TSNode "LibreOffice Calc" "Spreadsheet program" (spawn "localc")) []
  , Node (TS.TSNode "LibreOffice Draw" "Diagrams and sketches" (spawn "lodraw")) []
  , Node (TS.TSNode "LibreOffice Impress" "Presentation program" (spawn "loimpress")) []
  , Node (TS.TSNode "LibreOffice Math" "Formula editor" (spawn "lomath")) []
  , Node (TS.TSNode "LibreOffice Writer" "Word processor" (spawn "lowriter")) []
  , Node (TS.TSNode "Zathura" "PDF Viewer" (spawn "zathura")) []
  ]

nodeProgramming :: Tree (TS.TSNode (X ()))
nodeProgramming = Node (TS.TSNode "+ Programming" "programming and scripting tools" (return ()))
  [ Node (TS.TSNode "Python" "Python interactive prompt" (spawn (myTerminal ++ " -e python"))) []
  , Node (TS.TSNode "Haskell" "Haskell interactive promp" (spawn (myTerminal ++ " -e ghci"))) []
  ]

nodeSystem :: Tree (TS.TSNode (X ()))
nodeSystem = Node (TS.TSNode "+ System" "system tools and utilities" (return ()))
  [ Node (TS.TSNode "Alacritty" "GPU accelerated terminal" (spawn "alacritty")) []
  , Node (TS.TSNode "Etcher" "USB stick writer" (spawn "xxx")) []
  , Node (TS.TSNode "Glances" "Terminal system monitor" (spawn (myTerminal ++ " -e glances"))) []
  , Node (TS.TSNode "Gufw" "GUI uncomplicated firewall" (spawn "gufw")) []
  , Node (TS.TSNode "Htop" "Terminal process viewer" (spawn (myTerminal ++ " -e htop"))) []
  , Node (TS.TSNode "LXAppearance" "Customize look and feel" (spawn "lxappearance")) []
  , Node (TS.TSNode "Nitrogen" "Wallpaper viewer and setter" (spawn "nitrogen")) []
  , Node (TS.TSNode "Nmon" "Network monitor" (spawn (myTerminal ++ " -e nmon"))) []
  , Node (TS.TSNode "PCManFM" "Lightweight graphical file manager" (spawn "pcmanfm")) []
  , Node (TS.TSNode "Simple Terminal" "Suckless simple terminal" (spawn "st")) []
  , Node (TS.TSNode "Stress Terminal UI" "Stress your system" (spawn (myTerminal ++ " -e vifm"))) []
  , Node (TS.TSNode "Vifm" "Vim-like file manager" (spawn (myTerminal ++ " -e vifm"))) []
  ]

nodeBookmarks :: Tree (TS.TSNode (X ()))
nodeBookmarks = Node (TS.TSNode "+ Bookmarks" "a list of web bookmarks" (return ()))
  [ Node (TS.TSNode "+ Linux" "a list of web bookmarks" (return ()))
    [ Node (TS.TSNode "+ Arch Linux" "btw, i use arch!" (return ()))
      [ Node (TS.TSNode "Arch Linux" "Arch Linux homepage" (spawn (myBrowser ++ "https://www.archlinux.org/"))) []
      , Node (TS.TSNode "Arch Wiki" "The best Linux wiki" (spawn (myBrowser ++ "https://wiki.archlinux.org/"))) []
      , Node (TS.TSNode "AUR" "Arch User Repository" (spawn (myBrowser ++ "https://aur.archlinux.org/"))) []
      , Node (TS.TSNode "Arch Forums" "Arch Linux web forum" (spawn (myBrowser ++ "https://bbs.archlinux.org/"))) []
      ]
    , Node (TS.TSNode "+ Linux News" "linux news and blogs" (return ()))
      [ Node (TS.TSNode "DistroWatch" "Linux distro release announcments" (spawn (myBrowser ++ "https://distrowatch.com/"))) []
      , Node (TS.TSNode "LXer" "LXer linux news aggregation" (spawn (myBrowser ++ "http://lxer.com"))) []
      , Node (TS.TSNode "OMG Ubuntu" "Ubuntu news, apps and reviews" (spawn (myBrowser ++ "https://www.omgubuntu.co.uk"))) []
      ]
    , Node (TS.TSNode "+ Window Managers" "window manager documentation" (return ()))
      [ Node (TS.TSNode "Awesome" "awesomewm documentation" (return ()))
        [ Node (TS.TSNode "Awesome" "Homepage for awesome wm" (spawn (myBrowser ++ "https://awesomewm.org/"))) []
        , Node (TS.TSNode "Awesome GitHub" "The GutHub page for awesome" (spawn (myBrowser ++ "https://github.com/awesomeWM/awesome"))) []
        , Node (TS.TSNode "r/awesome" "Subreddit for awesome" (spawn (myBrowser ++ "https://www.reddit.com/r/awesomewm/"))) []
        ]
      , Node (TS.TSNode "+ Dwm" "dwm documentation" (return ()))
        [ Node (TS.TSNode "Dwm" "Dynamic window manager" (spawn (myBrowser ++ "https://dwm.suckless.org/"))) []
        , Node (TS.TSNode "Dwmblocks" "Modular status bar for dwm" (spawn (myBrowser ++ "https://github.com/torrinfail/dwmblocks"))) []
        , Node (TS.TSNode "r/suckless" "Subreddit for suckless software" (spawn (myBrowser ++ "https://www.reddit.com/r/suckless//"))) []
        ]
      , Node (TS.TSNode "+ Qtile" "qtile documentation" (return ()))
        [ Node (TS.TSNode "Qtile" "Tiling window manager in Python" (spawn (myBrowser ++ "http://www.qtile.org"))) []
        , Node (TS.TSNode "Qtile GitHub" "The GitHub page for qtile" (spawn (myBrowser ++ "https://github.com/qtile/qtile"))) []
        , Node (TS.TSNode "r/qtile" "Subreddit for qtile" (spawn (myBrowser ++ "https://www.reddit.com/r/qtile/"))) []
        ]
      , Node (TS.TSNode "+ XMonad" "xmonad documentation" (return ()))
        [ Node (TS.TSNode "XMonad" "Homepage for XMonad" (spawn (myBrowser ++ "http://xmonad.org"))) []
        , Node (TS.TSNode "XMonad GitHub" "The GitHub page for XMonad" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad"))) []
        , Node (TS.TSNode "xmonad-contrib" "Third party extensions for XMonad" (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmonad-contrib"))) []
        , Node (TS.TSNode "xmonad-ontrib GitHub" "The GitHub page for xmonad-contrib" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad-contrib"))) []
        , Node (TS.TSNode "Xmobar" "Minimal text-based status bar"  (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmobar"))) []
        ]
      ]
    ]
  , Node (TS.TSNode "+ Search and Reference" "Search engines, indices and wikis" (return ()))
    [ Node (TS.TSNode "DuckDuckGo" "Privacy-oriented search engine" (spawn (myBrowser ++ "https://duckduckgo.com/"))) []
    , Node (TS.TSNode "Google" "The evil search engine" (spawn (myBrowser ++ "http://www.google.com"))) []
    , Node (TS.TSNode "Thesaurus" "Lookup synonyms and antonyms" (spawn (myBrowser ++ "https://www.thesaurus.com/"))) []
    , Node (TS.TSNode "Wikipedia" "The free encyclopedia" (spawn (myBrowser ++ "https://www.wikipedia.org/"))) []
    ]
  , Node (TS.TSNode "+ Programming" "programming and scripting" (return ()))
    [ Node (TS.TSNode "Bash and Shell Scripting" "shell scripting documentation" (return ()))
      [ Node (TS.TSNode "GNU Bash" "Documentation for bash" (spawn (myBrowser ++ "https://www.gnu.org/software/bash/manual/"))) []
      , Node (TS.TSNode "r/bash" "Subreddit for bash" (spawn (myBrowser ++ "https://www.reddit.com/r/bash/"))) []
      , Node (TS.TSNode "r/commandline" "Subreddit for the command line" (spawn (myBrowser ++ "https://www.reddit.com/r/commandline/"))) []
      , Node (TS.TSNode "Learn Shell" "Interactive shell tutorial" (spawn (myBrowser ++ "https://www.learnshell.org/"))) []
      ]
    , Node (TS.TSNode "+ Elisp" "emacs lisp documentation" (return ()))
      [ Node (TS.TSNode "Emacs Lisp" "Reference manual for elisp" (spawn (myBrowser ++ "https://www.gnu.org/software/emacs/manual/html_node/elisp/"))) []
      , Node (TS.TSNode "Learn Elisp in Y Minutes" "Single webpage for elisp basics" (spawn (myBrowser ++ "https://learnxinyminutes.com/docs/elisp/"))) []
      , Node (TS.TSNode "r/Lisp" "Subreddit for lisp languages" (spawn (myBrowser ++ "https://www.reddit.com/r/lisp/"))) []
      ]
    , Node (TS.TSNode "+ Haskell" "haskell documentation" (return ()))
      [ Node (TS.TSNode "Haskell.org" "Homepage for haskell" (spawn (myBrowser ++ "http://www.haskell.org"))) []
      , Node (TS.TSNode "Hoogle" "Haskell API search engine" (spawn "https://hoogle.haskell.org/")) []
      , Node (TS.TSNode "r/haskell" "Subreddit for haskell" (spawn (myBrowser ++ "https://www.reddit.com/r/Python/"))) []
      , Node (TS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/haskell"))) []
      ]
    , Node (TS.TSNode "+ Python" "python documentation" (return ()))
      [ Node (TS.TSNode "Python.org" "Homepage for python" (spawn (myBrowser ++ "https://www.python.org/"))) []
      , Node (TS.TSNode "r/Python" "Subreddit for python" (spawn (myBrowser ++ "https://www.reddit.com/r/Python/"))) []
      , Node (TS.TSNode "Python on StackExchange" "Newest python topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/python"))) []
      ]
    ]
  , Node (TS.TSNode "+ Vim" "vim and neovim documentation" (return ()))
    [ Node (TS.TSNode "Vim.org" "Vim, the ubiquitous text editor" (spawn (myBrowser ++ "https://www.vim.org/"))) []
    , Node (TS.TSNode "r/Vim" "Subreddit for vim" (spawn (myBrowser ++ "https://www.reddit.com/r/vim/"))) []
    , Node (TS.TSNode "Vi/m StackExchange" "Vi/m related questions" (spawn (myBrowser ++ "https://vi.stackexchange.com/"))) []
    ]
  ]

nodeConfigs :: Tree (TS.TSNode (X ()))
nodeConfigs = Node (TS.TSNode "+ Config Files" "config files that edit often" (return ()))
  [ Node (TS.TSNode "+ xmobar configs" "My xmobar config files" (return ()))
    [ Node (TS.TSNode "xmobar mon1" "status bar on monitor 1" (spawn (myEditor ++ "~/.config/xmobar/xmobarrc0"))) []
    , Node (TS.TSNode "xmobar mon2" "status bar on monitor 2" (spawn (myEditor ++ "~/.config/xmobar/xmobarrc1"))) []
    , Node (TS.TSNode "xmobar mon3" "status bar on monitor 3" (spawn (myEditor ++ "~/.config/xmobar/xmobarrc2"))) []
    , Node (TS.TSNode "xmobar mon4" "status bar on monitor 4" (spawn (myEditor ++ "~/.config/xmobar/xmobarrc3"))) []
    ]
  , Node (TS.TSNode "+ xmonad configs" "My xmonad config files" (return ()))
    [ Node (TS.TSNode "xmonad.hs" "My XMonad Main" (spawn (myEditor ++ "~/.config/xmonad/xmonad.hs"))) []
    , Node (TS.TSNode "GridSelect.hs" "My XMonad GridSelect menu" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/GridSelect.hs"))) []
    , Node (TS.TSNode "KeyBindings.hs" "My XMonad keybindings" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/KeyBindings.hs"))) []
    , Node (TS.TSNode "Layouts.hs" "My XMonad layouts" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/Layouts.hs"))) []
    , Node (TS.TSNode "Prompts.hs" "My XMonad prompts" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/Prompts.hs"))) []
    , Node (TS.TSNode "Scratchpads.hs" "My XMonad named scratchpads" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/Scratchpads.hs"))) []
    , Node (TS.TSNode "TreeMenu.hs" "My XMonad TreeSelect menu" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/TreeMenu.hs"))) []
    , Node (TS.TSNode "Variables.hs" "My XMonad variables" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/Variables.hs"))) []
    , Node (TS.TSNode "Palette.hs" "My XMonad palette" (spawn (myEditor ++ "~/.config/xmonad/lib/Blueberry/Palette.hs"))) []
    ]
  , Node (TS.TSNode "+ zshrc" "Config for the z shell" (return ())) 
    [ Node (TS.TSNode "zshrc" "Main ZSH config" (spawn (myEditor ++ "~/.config/zsh/.zshrc"))) []
    , Node (TS.TSNode "aliases" "ZSH aliases" (spawn (myEditor ++ "~/.config/zsh/aliases.zsh"))) []
    , Node (TS.TSNode "completion" "ZSH completion" (spawn (myEditor ++ "~/.config/zsh/completion.zsh"))) []
    , Node (TS.TSNode "config" "ZSH config" (spawn (myEditor ++ "~/.config/zsh/config.zsh"))) []
    , Node (TS.TSNode "exports" "ZSH exports" (spawn (myEditor ++ "~/.config/zsh/exports.zsh"))) []
    , Node (TS.TSNode "extensions" "ZSH extensions" (spawn (myEditor ++ "~/.config/zsh/extensions.zsh"))) []
    , Node (TS.TSNode "fpath" "ZSH fpath" (spawn (myEditor ++ "~/.config/zsh/fpath.zsh"))) []
    , Node (TS.TSNode "prompt" "ZSH prompt" (spawn (myEditor ++ "~/.config/zsh/prompt.zsh"))) []
    , Node (TS.TSNode "pkg" "ZSH pkg" (spawn (myEditor ++ "~/.config/zsh/zsh.pkg"))) []
    ]
  , Node (TS.TSNode "+ neovim" "neovim text editor" (return ())) 
    [ Node (TS.TSNode "init" "nvim init config" (spawn (myEditor ++ "~/.config/nvim/init.vim"))) []
    , Node (TS.TSNode "airline" "nvim airline" (spawn (myEditor ++ "~/.config/nvim/airline.vim"))) []
    , Node (TS.TSNode "colorizer" "nvim colorizer" (spawn (myEditor ++ "~/.config/nvim/colorizer.vim"))) []
    , Node (TS.TSNode "dvisuals" "nvim dvisuals" (spawn (myEditor ++ "~/.config/nvim/dvisuals.vim"))) []
    , Node (TS.TSNode "mergetool" "nvim mergetool" (spawn (myEditor ++ "~/.config/nvim/mergetool.vim"))) []
    , Node (TS.TSNode "nerdtree" "nvim nerdtree" (spawn (myEditor ++ "~/.config/nvim/nerdtree.vim"))) []
    , Node (TS.TSNode "vim-notes" "nvim vim-notes" (spawn (myEditor ++ "~/.config/nvim/vim-notes.vim"))) []
    , Node (TS.TSNode "pkg" "nvim pkg" (spawn (myEditor ++ "~/.config/nvim/nvim.pkg"))) []
    , Node (TS.TSNode "preinstall-apt" "nvim preinstall for apt" (spawn (myEditor ++ "~/.config/nvim/preinstall-apt.sh"))) []
    , Node (TS.TSNode "postinstall" "nvim postinstall" (spawn (myEditor ++ "~/.config/nvim/postinstall.sh"))) []
    ]
  , Node (TS.TSNode "polybar" "easy-to-use status bar" (spawn (myEditor ++ "~/.config/polybar/config"))) []
  , Node (TS.TSNode "alacritty" "alacritty terminal emulator" (spawn (myEditor ++ "~/.config/alacritty/alacritty.yml"))) []
  , Node (TS.TSNode "bashrc" "the bourne again shell" (spawn (myEditor ++ "~/.bashrc"))) []
  , Node (TS.TSNode "qutebrowser config.py" "qutebrowser web browser" (spawn (myEditor ++ "~/.config/qutebrowser/config.py"))) []
  , Node (TS.TSNode "xresources" "xresources file" (spawn (myEditor ++ "~/.Xresources"))) []
  ]

nodeScreenshot :: Tree (TS.TSNode (X ()))
nodeScreenshot = Node (TS.TSNode "+ Screenshots" "take a screenshot" (return ()))
  [ Node (TS.TSNode "Quick fullscreen" "take screenshot immediately" (spawn "scrot -d 1 ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
  , Node (TS.TSNode "Delayed fullscreen" "take screenshot in 5 secs" (spawn "scrot -d 5 ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
  , Node (TS.TSNode "Section screenshot" "take screenshot of section" (spawn "scrot -s ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
  ]

nodeXmonad :: Tree (TS.TSNode (X ()))
nodeXmonad = Node (TS.TSNode "+ XMonad" "window manager commands" (return ()))
  [ Node (TS.TSNode "+ View Workspaces" "View a specific workspace" (return ()))
    [ Node (TS.TSNode "View 1" "View workspace 1" (spawn "~/.config/xmonad/xmonadctl 1")) []
    , Node (TS.TSNode "View 2" "View workspace 2" (spawn "~/.config/xmonad/xmonadctl 3")) []
    , Node (TS.TSNode "View 3" "View workspace 3" (spawn "~/.config/xmonad/xmonadctl 5")) []
    , Node (TS.TSNode "View 4" "View workspace 4" (spawn "~/.config/xmonad/xmonadctl 7")) []
    , Node (TS.TSNode "View 5" "View workspace 5" (spawn "~/.config/xmonad/xmonadctl 9")) []
    , Node (TS.TSNode "View 6" "View workspace 6" (spawn "~/.config/xmonad/xmonadctl 11")) []
    , Node (TS.TSNode "View 7" "View workspace 7" (spawn "~/.config/xmonad/xmonadctl 13")) []
    , Node (TS.TSNode "View 8" "View workspace 8" (spawn "~/.config/xmonad/xmonadctl 15")) []
    , Node (TS.TSNode "View 9" "View workspace 9" (spawn "~/.config/xmonad/xmonadctl 17")) []
    ]
  , Node (TS.TSNode "+ Shift Workspaces" "Send focused window to specific workspace" (return ()))
    [ Node (TS.TSNode "View 1" "View workspace 1" (spawn "~/.config/xmonad/xmonadctl 2")) []
    , Node (TS.TSNode "View 2" "View workspace 2" (spawn "~/.config/xmonad/xmonadctl 4")) []
    , Node (TS.TSNode "View 3" "View workspace 3" (spawn "~/.config/xmonad/xmonadctl 6")) []
    , Node (TS.TSNode "View 4" "View workspace 4" (spawn "~/.config/xmonad/xmonadctl 8")) []
    , Node (TS.TSNode "View 5" "View workspace 5" (spawn "~/.config/xmonad/xmonadctl 10")) []
    , Node (TS.TSNode "View 6" "View workspace 6" (spawn "~/.config/xmonad/xmonadctl 12")) []
    , Node (TS.TSNode "View 7" "View workspace 7" (spawn "~/.config/xmonad/xmonadctl 14")) []
    , Node (TS.TSNode "View 8" "View workspace 8" (spawn "~/.config/xmonad/xmonadctl 16")) []
    , Node (TS.TSNode "View 9" "View workspace 9" (spawn "~/.config/xmonad/xmonadctl 18")) []
    ]
  , Node (TS.TSNode "Next layout" "Switch to next layout" (spawn "~/.config/xmonad/xmonadctl next-layout")) []
  , Node (TS.TSNode "Recompile" "Recompile XMonad" (spawn "xmonad --recompile")) []
  , Node (TS.TSNode "Restart" "Restart XMonad" (spawn "xmonad --restart")) []
  , Node (TS.TSNode "Quit" "Restart XMonad" (io exitSuccess)) []
  ]

nodeSeparator :: Tree (TS.TSNode (X ()))
nodeSeparator = Node (TS.TSNode "------------------------" "" (spawn "xdotool key Escape")) []

ioNodeGames :: IO (Tree (TS.TSNode (X ())))
ioNodeGames = do
  games <- myGamesFolder >>= listGames
  let ch = map (\(a,b) -> Node (TS.TSNode a "" (spawn b)) []) games
  return $ Node (TS.TSNode "+ Games" "Local Games" (return ())) ch

treeSelectAction :: TS.TSConfig (X ()) -> X ()
treeSelectAction a = do
  nodeGames <- io ioNodeGames
  TS.treeselectAction a [ nodeAccessories
                        , nodeGames
                        , nodeGraphics
                        , nodeInternet
                        , nodeMultimedia
                        , nodeOffice
                        , nodeProgramming
                        , nodeSystem
                        , nodeSeparator
                        , nodeBookmarks
                        , nodeConfigs
                        , nodeScreenshot
                        , nodeSeparator
                        , nodeXmonad
                        ]

-- Configuration options for treeSelect
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd292d3e
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffd0d0d0, 0xff202331)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

-- Keybindings for treeSelect menus. Use h-j-k-l to navigate.
-- Use 'o' and 'i' to move forward/back in the workspace history.
-- Single KEY's are for top-level nodes. SUPER+KEY are for the
-- second-level nodes. SUPER+ALT+KEY are third-level nodes.
myTreeNavigation = M.fromList
  [ ((0, xK_Escape),    TS.cancel)
  , ((0, xK_Return),    TS.select)
  , ((0, xK_space),     TS.select)
  , ((0, xK_Up),        TS.movePrev)
  , ((0, xK_Down),      TS.moveNext)
  , ((0, xK_Left),      TS.moveParent)
  , ((0, xK_Right),     TS.moveChild)
  , ((0, xK_k),         TS.movePrev)
  , ((0, xK_j),         TS.moveNext)
  , ((0, xK_h),         TS.moveParent)
  , ((0, xK_l),         TS.moveChild)
  , ((0, xK_o),         TS.moveHistBack)
  , ((0, xK_i),         TS.moveHistForward)
  ]
