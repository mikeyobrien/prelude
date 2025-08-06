# Emacs Navigation Guide for Prelude Configuration

## Basic Emacs Navigation (Default bindings - Evil mode overrides many)

### Movement
- `C-f` / `C-b` - Forward/backward one character
- `C-n` / `C-p` - Next/previous line  
- `C-a` / `C-e` - Beginning/end of line
- `M-f` / `M-b` - Forward/backward one word
- `M-<` / `M->` - Beginning/end of buffer
- `C-v` / `M-v` - Page down/up

### With Evil Mode (you have this enabled)
- Normal Vim movements: `h j k l`, `w b e`, `0 $`, `gg G`
- Switch modes: `ESC` (normal), `i` (insert), `v` (visual)

## File & Directory Operations

### Opening Files
- `C-x C-f` - Find file (with Vertico completion)
- `C-x C-r` - Recent files (if recentf is enabled)
- `C-x d` - Dired (directory editor)

### Buffers
- `C-x b` - Switch buffer
- `C-x C-b` - List buffers
- `C-x k` - Kill buffer
- `C-x C-s` - Save current buffer
- `C-x s` - Save all modified buffers

### Windows
- `C-x 2` - Split horizontally
- `C-x 3` - Split vertically  
- `C-x 1` - Delete other windows
- `C-x 0` - Delete current window
- `C-x o` - Other window

## Prelude-Specific Features

### Super Key Shortcuts (s = Cmd on Mac)
- `s-p` - Switch project (projectile)
- `s-f` - Find file in project
- `s-g` - Grep in project
- `s-l` - Go to line
- `s-/` - Comment/uncomment region

### Projectile (C-c p prefix)
- `C-c p f` - Find file in project
- `C-c p p` - Switch project
- `C-c p s g` - Grep in project
- `C-c p k` - Kill project buffers

### Help
- `C-h k` - Describe key
- `C-h f` - Describe function
- `C-h v` - Describe variable
- `C-h m` - Describe current modes

## Dired (Directory Navigation)

When in dired (`C-x d`):
- `RET` - Open file/directory
- `^` - Go up directory
- `+` - Create directory
- `d` - Mark for deletion, `x` to execute
- `R` - Rename/move
- `C` - Copy
- `g` - Refresh

## Note on Evil Mode

Since you have Evil mode enabled, you get the best of both worlds - use Vim navigation when comfortable, but Emacs bindings for file operations and window management!

## Quality of Life Improvements Added

### Enhanced Search & Navigation
- `C-s` - Consult line search with preview
- `M-g i` - Jump to function/class with imenu
- `M-s r` - Ripgrep search in project
- `M-o` - Ace-window for quick window switching

### Git Integration
- `C-c g t` - Git time machine (browse file history)
- Git gutter shows changes in the fringe
- Magit is already configured via Prelude

### Editing Power-ups
- `C-=` - Expand region (semantic selection)
- `C->` / `C-<` - Multiple cursors
- `M-↑` / `M-↓` - Move lines up/down
- Rainbow delimiters for matching parens

### UI Enhancements
- Which-key shows available keybindings (wait 0.5s)
- Beacon highlights cursor after jumps
- Line numbers in programming modes
- Current line highlighting

### Window Management
- `C-c ←` / `C-c →` - Undo/redo window layouts
- `C-c M-p` prefix - Perspective workspaces

### File Management
- `C-x C-r` - Recent files
- Better Dired with icons and grouping
- Auto-save and backups in dedicated directories