# blueberry-monad

`blueberry-monad` is a customized [XMonad](https://xmonad.org/) configuration
designed to provide a polished tiling window manager experience complete with a
multi-monitor status bar, curated keybindings, and quality-of-life helpers such
as application grids and cryptocurrency widgets. The project is organised as a
standard Haskell package so it can be tested and evolved like a regular
codebase.

## Repository layout

```
app/                  -- Entry point that wires everything into XMonad
lib/BMonad/           -- Library code that implements the configuration pieces
  Bar/                -- Xmobar integration (widgets, icons, helpers)
  Config/             -- Types, YAML parsers, and defaults for user config
  Crypto/             -- External data fetchers (CoinGecko, Fear & Greed index)
  Bar.hs, Config.hs…  -- Public surface that is re-used from the entry point
test/                 -- Hspec test suite covering parsers and helper modules
```

The `blueberry-monad.cabal` and `package.yaml` files describe the build, while
`stack.yaml` allows the project to be built with Stack if preferred.

## Runtime flow

1. **Startup** – `app/Main.hs` fetches the user configuration from the XDG
   config directory, spins up the logger, launches one or more Xmobar status
   bars, and finally hands control over to XMonad via `launch`.
2. **Layout & management** – `BMonad.bmonadLayout` composes a set of tiling
   layouts (tall, float, grid, tabs) with helpers such as window navigation,
   tabbed sublayouts, and smart borders; the companion `bmonadManageHook`
   defines rules for floating and workspace shifting based on window metadata.
3. **Bars** – `BMonad.Bar` spawns one Xmobar per monitor and exposes a
   `bmobarPP` pretty-printer so the main process can feed workspace and window
   information into them. Monitor-specific widget sets (coins, memory, network,
   etc.) are driven by `cfgMobarSettings` from the loaded configuration.
4. **Keybindings** – `BMonad.KeyBindings` groups related key combinations into
   sections such as essentials, GridSelect menus, workspace navigation, and
   multimedia controls. Each binding is declared with human-readable
   descriptions so they can be displayed through the `showKeys` helper.
5. **Configuration** – `BMonad.Config` and `BMonad.Config.Types` translate YAML
   files (`bmonad.yaml`, `themes.yaml`, `applications.yaml`) into strongly typed
   records. Defaults are provided for colours, fonts, and widgets so the window
   manager can start even if the user configuration is incomplete.
6. **Utilities & integrations** – helper modules provide quality-of-life
   features such as logging (`BMonad.Log`), screen counting and colour parsing
   (`BMonad.Utils`), and status widgets that pull external data like coin prices
   and the Fear & Greed index (`BMonad.Crypto`, `BMonad.Bar.Plugin.*`).

## Configuration files

At runtime the configuration loader looks for files in the user's
`$XDG_CONFIG_HOME/xmonad` directory (typically `~/.config/xmonad/`):

* `bmonad.yaml` – high-level settings such as workspaces, preferred terminal,
  game directory, and widget selection.
* `themes.yaml` – colour palettes referenced by name in `bmonad.yaml`.
* `applications.yaml` – lists used to populate GridSelect menus for launching
  favourite programs.

If these files are missing or invalid the project falls back to sensible
defaults defined in `BMonad.Config.Types` so XMonad still launches.

## Testing

The `test/` directory contains Hspec suites for the configuration and helper
modules. Running `stack test` (or `cabal test`) validates that the YAML parsers
handle expected shapes and that utility functions behave correctly. This is
particularly useful when evolving the configuration schema or adding new
widgets.

## Where to go next

* **Customise the look & feel** – tweak `themes.yaml` or extend `Scheme` with
  new colour slots, then update `BMonad.Bar.Utils` to expose new helpers.
* **Add widgets** – use the existing Xmobar plugins under
  `lib/BMonad/Bar/Plugin` as templates for integrating additional system or web
  data sources, and wire them into `cfgMobarSettings`.
* **Extend keybindings** – `BMonad.KeyBindings` is structured in labelled
  groups so new shortcuts can be slotted in without losing readability.
* **Improve testing** – add regression coverage around new configuration
  options to keep the user-facing YAML format stable.

Armed with this overview you should be able to navigate the modules, adjust the
configuration to your needs, and confidently add new behaviour.
