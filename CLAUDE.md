# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build with Stack (preferred)
stack build

# Run tests
stack test

# Run a single test file
stack test --ta '-m "<test description pattern>"'

# Build with Cabal
cabal build

# Run tests with Cabal
cabal test
```

## Architecture Overview

blueberry-monad is a custom XMonad window manager configuration packaged as a Haskell library. It provides a complete tiling WM setup with multi-monitor status bars (Xmobar or Polybar), keybindings, and cryptocurrency widgets.

### Module Structure

**Core Entry Point**: `app/Main.hs` reads CLI options, loads configuration, sets up logging, launches the status bar, and calls `XMonad.launch`.

**Main Library** (`lib/BMonad.hs`): Re-exports all public modules and defines:
- `bmonadLayout`: Composed layout (tall, cols, floats, tabs, grid) with smart borders, tabbed sublayouts, and window navigation
- `bmonadManageHook`: Window placement rules based on title/className/resource

**Configuration** (`lib/BMonad/Config/`):
- `Types.hs`: Core data types (`Config`, `Theme`, `Scheme`, `MobarSettings`, etc.) with YAML `FromJSON` instances and defaults
- `Parsers.hs`: YAML file parsing from `$XDG_CONFIG_HOME/xmonad/`
- Runtime config files: `bmonad.yaml`, `themes.yaml`, `applications.yaml`

**Status Bar** (`lib/BMonad/Bar/`):
- `Bar.hs`: Spawns Xmobar per monitor, provides `bmobarPP` pretty-printer
- `Widgets.hs`: Widget rendering helpers
- `Plugin/`: Custom Xmobar plugins (CoinPriceWidget, FGIWidget, UpstreamStatusWidget)

**Keybindings** (`lib/BMonad/KeyBindings.hs`): Organized into named sections (essentials, GridSelect, workspaces, windows, layouts, monitors, multimedia). Press `Mod+F1` to display bindings.

**Crypto Integration** (`lib/BMonad/Crypto/`): Fetches coin prices from CoinGecko and Fear & Greed index data.

### Two Bar Alternatives

The system supports two status bar backends selected via CLI:
- **Xmobar** (default): Custom Xmobar with Haskell plugins (`bar/BMobar.hs`)
- **Polybar**: Alternative bar system (`lib/Polybar.hs`)

### Configuration Flow

1. `bmonadConfig` in `Config.hs` loads YAML files from XDG config directory
2. Missing files or keys fall back to defaults in `Config/Types.hs`
3. Theme color schemes are defined in `themes.yaml` and referenced by name
4. Widget selection per monitor is configured in `bmonad.yaml` under `mobar` section

## GHC Warnings

The project uses strict warning flags including `-Wall`, `-Wincomplete-record-updates`, and `-Wmissing-export-lists`. All exports must be explicit.

## Testing

Tests use Hspec with auto-discovery (`test/Spec.hs`). Test files cover:
- `ConfigurationSpec.hs`: YAML parsing
- `ApplicationsSpec.hs`: Application config parsing
- `CryptoSpec.hs`: API response parsing
- `UtilsSpec.hs`: Utility functions
