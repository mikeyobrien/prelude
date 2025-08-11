# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an Emacs Prelude configuration - a curated Emacs distribution with extensive personal customizations. The configuration uses a modular architecture with core functionality separated from optional modules and personal overlays.

## Architecture

### Key Directories
- `core/` - Essential Prelude functionality (package management, UI, editor settings)
- `modules/` - Optional language and tool modules (loaded on demand)
- `personal/` - User customizations that override defaults
- `vendor/` - Third-party packages not in repositories
- `straight/` - Straight.el package manager cache (user addition)
- `elpa/` - Traditional Emacs package storage

### Loading Order
1. `init.el` bootstraps the configuration
2. Core modules load in sequence (packages → custom → ui → core → editor → keybindings → platform)
3. Optional modules from `modules/` (specified in `personal/prelude-modules.el`)
4. Personal configurations from `personal/` directory
5. `personal/init.el` applies final customizations

## Package Management

The configuration uses hybrid package management:
- **Primary**: Built-in package.el with MELPA, GNU ELPA, and Org ELPA repositories
- **Secondary**: Straight.el (in `personal/preload/`) for GitHub packages and version locking
- Packages auto-install when needed (demand-based loading for language modes)

## Key Customizations

### Evil Mode (Vim Emulation)
- Comprehensive Evil mode setup with evil-collection
- Spacemacs-style leader key system (SPC prefix)
- Custom keybindings in `personal/init.el`
- Integration with vterm, which-key, and general.el

### Personal Configuration Files
- `personal/init.el` - Main user customizations (Evil setup, keybindings, UI tweaks)
- `personal/prelude-modules.el` - Enabled modules selection
- `personal/termux-config.el` - Mobile/Termux optimizations
- `personal/preload/straight.el` - Straight.el bootstrap

## Commands

### Testing Configuration
```bash
# Test if configuration loads without errors
emacs --batch --load init.el

# Start Emacs with this configuration
emacs
```

### Package Management
```elisp
;; Update all packages
M-x package-list-packages RET U x

;; Install a specific package
M-x package-install RET package-name RET

;; For straight.el packages
M-x straight-pull-all
```

### Common Development Tasks
- **Reload configuration**: `M-x eval-buffer` in init.el
- **Byte-compile**: `M-x byte-recompile-directory` on `.emacs.d`
- **Check for errors**: `M-x check-parens` in Elisp files

## Important Patterns

### Adding New Modules
1. Create module file in `modules/` following `prelude-LANGUAGE.el` naming
2. Add to `personal/prelude-modules.el` to enable
3. Use `prelude-require-packages` for dependencies

### Customization Best Practices
- Keep personal changes in `personal/` directory
- Use `personal/preload/` for early-loading configurations
- Follow existing keybinding patterns (Evil leader keys under SPC)
- Respect demand-based loading for language modes

### Working with Evil Mode
- Leader key is SPC in normal/visual modes
- Use general.el for defining new keybindings
- Evil-collection provides consistent bindings across packages
- Terminal (vterm) has special Evil integration

## CI/Testing
- GitHub Actions workflow in `.github/workflows/ci.yml`
- Tests configuration loading on Ubuntu
- Markdown linting for documentation