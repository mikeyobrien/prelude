(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Fix PATH on macOS for GUI Emacs to find external tools like rg
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Adjust line length limit for whitespace-mode
;; Change 80 to your preferred line length (e.g., 100, 120, or nil to disable)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq whitespace-line-column 140)

;; Font configuration with Retina variant and fallback
(defvar my-font-variants
  '("FiraCode Nerd Font Retina"  ; Retina variant (preferred)
    "FiraCode Nerd Font"       ; Regular variant (fallback)
    "Fira Code Nerd Font")     ; Alternative naming
  "List of font variants to try in order of preference.")

(defvar my-font-size 16 "Font size to use in points (for example, 10).")

;; Install doom-themes
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


(defun font-exists-p (font)
  "Check if the FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(defun find-available-font (font-list size)
  "Find the first available font from FONT-LIST at SIZE."
  (catch 'found
    (dolist (font-name font-list)
      (let ((font-spec (format "%s-%d" font-name size)))
        (when (font-exists-p font-spec)
          (throw 'found font-spec))))
    nil))

;; Try to find and set an available font
(let ((available-font (find-available-font my-font-variants my-font-size)))
  (when available-font
    (add-to-list 'default-frame-alist `(font . ,available-font))
    (add-to-list 'default-frame-alist '(width . 170))
    (add-to-list 'initial-frame-alist `(font . ,available-font))
    (add-to-list 'initial-frame-alist '(width . 170))
    (message "Using font: %s" available-font)))

(use-package vterm
  :commands vterm
  :bind ((:map vterm-mode-map
               ("C-y" . vterm-yank)
               ("M-y" . vterm-yank-pop)
               ("C-q" . vterm-send-next-key)
               ("C-c C-e" . vterm-send-escape)))  ; Add explicit escape binding
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  :config
  ;; Ensure cursor is visible in vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Let vterm handle its own cursor, but ensure it's not hidden
              (setq-local cursor-type 'box)
              ;; Ensure cursor is visible by setting appropriate face
              (set-face-background 'cursor nil t)
              ;; Set EDITOR and GIT_EDITOR to use emacsclient in vterm
              (vterm-send-string "export EDITOR='emacsclient -n'\n")
              (vterm-send-string "export GIT_EDITOR='emacsclient -n'\n")
              (vterm-clear))))

;; Fix for shell-command with fish shell
;; This ensures fish knows it's running in a non-interactive context
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

;; Minimal modeline
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Fix evil-mode undo canary error
(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/OMMIT_EDITMSG$" "/git-rebase-todo$"))
  (undo-fu-session-global-mode))

;; Evil Collection for better evil integration with Emacs modes
;; Note: evil-want-keybinding is set to nil in personal/preload/evil-config.el
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(help org dired magit vterm vertico company
                                     which-key compile ibuffer grep
                                     ediff flycheck eglot xref
                                     occur package-menu term eshell imenu-list
                                     calendar markdown-mode))  ; Comprehensive Evil integration
  (evil-collection-init))


(defun my/q-commit()
  "Create a conventional commit for staged changes using q chat."
  (interactive)
  (async-shell-command "q chat -a --no-interactive \"Create a conventional commit for the staged commits\""))


;; Configure Magit to use Emacs for commit messages
(use-package magit
  :ensure t
  :config
  ;; Force Magit to use Emacs for editing commit messages
  (setq with-editor-emacsclient-executable "emacsclient")
  ;; Ensure we use the current Emacs instance
  (setq magit-commit-editor-mode t)
  ;; Don't use external editor
  (setenv "GIT_EDITOR" "emacsclient")
  (setenv "EDITOR" "emacsclient")

  ;; Add custom q-commit command to Magit
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-commit "c"
      '("q" "Q commit (AI)" my/q-commit))))

;; Auto-revert files when they change on disk
(global-auto-revert-mode 1)
;; Also auto-revert dired buffers
(setq global-auto-revert-non-file-buffers t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setq claude-code-ide-window-side 'bottom)
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

;; Spacemacs/Doom-style leader key configuration
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; Create a definer for the leader key
  (general-create-definer my-leader-def
    :keymaps '(normal visual emacs motion)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Create a definer for the local leader
  (general-create-definer my-local-leader-def
    :keymaps '(normal visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m")

  ;; Main menu - similar to Spacemacs/Doom
  (my-leader-def
    ""     '(nil :which-key "leader")
    "SPC"  '(execute-extended-command :which-key "M-x")
    "."    '(find-file :which-key "find file")
    ","    '(switch-to-buffer :which-key "switch buffer")
    "'"    '(vterm :which-key "terminal")
    "/"    '(projectile-ripgrep :which-key "ripgrep")
    "?"    '(which-key-show-top-level :which-key "top keybindings")
    "u"    '(universal-argument :which-key "universal arg")
    "x"    '(execute-extended-command :which-key "M-x")

    ;; Applications
    "a"    '(:ignore t :which-key "applications")
    "ad"   '(dired :which-key "dired")
    "at"   '(vterm :which-key "terminal")

    ;; Buffers
    "b"    '(:ignore t :which-key "buffer")
    "bb"   '(switch-to-buffer :which-key "switch buffer")
    "bd"   '(kill-current-buffer :which-key "kill buffer")
    "bk"   '(kill-current-buffer :which-key "kill buffer")
    "bn"   '(next-buffer :which-key "next buffer")
    "bp"   '(previous-buffer :which-key "prev buffer")
    "br"   '(revert-buffer :which-key "revert buffer")
    "bs"   '(scratch-buffer :which-key "scratch buffer")
    "bS"   '(save-some-buffers :which-key "save all buffers")
    "bm"   '(bookmark-set :which-key "set bookmark")
    "bM"   '(bookmark-jump :which-key "jump bookmark")

    ;; Files
    "f"    '(:ignore t :which-key "file")
    "ff"   '(find-file :which-key "find file")
    "fr"   '(recentf-open-files :which-key "recent files")
    "fs"   '(save-buffer :which-key "save file")
    "fS"   '(write-file :which-key "save as...")
    "ft"   '(projectile-find-file :which-key "project files")
    "fR"   '(rename-file :which-key "rename file")
    "fD"   '(delete-file :which-key "delete file")

    ;; Git/Magit
    "g"    '(:ignore t :which-key "git")
    "gg"   '(magit-status :which-key "magit status")
    "gb"   '(magit-blame :which-key "magit blame")
    "gc"   '(magit-commit :which-key "magit commit")
    "gd"   '(magit-diff :which-key "magit diff")
    "gl"   '(magit-log :which-key "magit log")
    "gp"   '(magit-push :which-key "magit push")
    "gP"   '(magit-pull :which-key "magit pull")

    ;; Help
    "h"    '(:ignore t :which-key "help")
    "hf"   '(describe-function :which-key "describe function")
    "hv"   '(describe-variable :which-key "describe variable")
    "hk"   '(describe-key :which-key "describe key")
    "hm"   '(describe-mode :which-key "describe mode")
    "hp"   '(describe-package :which-key "describe package")
    "ht"   '(load-theme :which-key "load theme")

    ;; Jump/Join
    "j"    '(:ignore t :which-key "jump")
    "jj"   '(avy-goto-char-timer :which-key "jump to char")
    "jl"   '(avy-goto-line :which-key "jump to line")
    "jw"   '(avy-goto-word-1 :which-key "jump to word")

    ;; Org mode
    "o"    '(:ignore t :which-key "org")
    "oa"   '(org-agenda :which-key "org agenda")
    "oc"   '(org-capture :which-key "org capture")
    "ol"   '(org-store-link :which-key "store link")
    "ob"   '(org-switchb :which-key "switch org buffer")

    ;; Project
    "p"    '(:ignore t :which-key "project")
    "pf"   '(projectile-find-file :which-key "find file")
    "pp"   '(projectile-switch-project :which-key "switch project")
    "pb"   '(projectile-switch-to-buffer :which-key "switch buffer")
    "pd"   '(projectile-find-dir :which-key "find dir")
    "pg"   '(projectile-grep :which-key "grep")
    "pr"   '(projectile-replace :which-key "replace")
    "pk"   '(projectile-kill-buffers :which-key "kill buffers")

    ;; Quit
    "q"    '(:ignore t :which-key "quit")
    "qq"   '(save-buffers-kill-emacs :which-key "quit emacs")
    "qr"   '(restart-emacs :which-key "restart emacs")

    ;; Search
    "s"    '(:ignore t :which-key "search")
    "ss"   '(swiper :which-key "swiper")
    "sg"   '(projectile-grep :which-key "grep")
    "sr"   '(projectile-ripgrep :which-key "ripgrep")
    "sp"   '(projectile-ag :which-key "ag in project")

    ;; Toggle
    "t"    '(:ignore t :which-key "toggle")
    "tl"   '(display-line-numbers-mode :which-key "line numbers")
    "tw"   '(whitespace-mode :which-key "whitespace")
    "tt"   '(toggle-truncate-lines :which-key "truncate lines")
    "tf"   '(toggle-frame-fullscreen :which-key "fullscreen")

    ;; Window
    "w"    '(:ignore t :which-key "window")
    "ww"   '(ace-window :which-key "ace window")
    "wd"   '(delete-window :which-key "delete window")
    "wD"   '(delete-other-windows :which-key "delete other windows")
    "ws"   '(split-window-below :which-key "split below")
    "wv"   '(split-window-right :which-key "split right")
    "wh"   '(evil-window-left :which-key "window left")
    "wj"   '(evil-window-down :which-key "window down")
    "wk"   '(evil-window-up :which-key "window up")
    "wl"   '(evil-window-right :which-key "window right")
    "w="   '(balance-windows :which-key "balance windows")
    "wm"   '(delete-other-windows :which-key "maximize window")
    "wo"   '(delete-other-windows :which-key "only window")
    "wu"   '(winner-undo :which-key "undo window change")
    "wr"   '(winner-redo :which-key "redo window change")

    ;; Quick buffer switching
    "TAB"  '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "1"    '(winum-select-window-1 :which-key "window 1")
    "2"    '(winum-select-window-2 :which-key "window 2")
    "3"    '(winum-select-window-3 :which-key "window 3")
    "4"    '(winum-select-window-4 :which-key "window 4")
    ))

;; Configure which-key to show keybindings
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

;; Override the conflicting SPC binding from prelude-evil
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC") nil))

;; Optional: Add some additional convenience bindings
(with-eval-after-load 'evil
  ;; Quick file/buffer access with - and =
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "=") 'switch-to-buffer))

;; Terminal mouse/touch support for Termux and other terminals
(when (not (display-graphic-p))
  ;; Enable xterm mouse mode for terminal
  (xterm-mouse-mode 1)

  ;; Enable mouse wheel scrolling with better touch support
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3)))

  ;; Make scrolling smoother for touch
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)

  ;; Enable mouse in Evil mode
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map [down-mouse-1] 'mouse-set-point)
    (define-key evil-visual-state-map [down-mouse-1] 'mouse-set-point)
    (define-key evil-insert-state-map [down-mouse-1] 'mouse-set-point)))

;; Window management for small screens
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-background nil))

;; Winner mode for window layout undo/redo
(winner-mode 1)

;; Popup buffer management for small screens
(setq display-buffer-alist
      '(("\\*Help\\*" display-buffer-same-window)
        ("\\*Completions\\*" display-buffer-same-window)
        ("\\*Messages\\*" display-buffer-same-window)
        ("\\*compilation\\*" display-buffer-same-window)))

;; Simplified buffer switching for small screens
(defun my/switch-to-previous-buffer ()
  "Switch to previously visited buffer. Repeated calls toggle buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(with-eval-after-load 'evil
  ;; Quick buffer toggle with backquote
  (define-key evil-normal-state-map (kbd "`") 'my/switch-to-previous-buffer))

;; Configure scratch buffer to use org-mode
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "# Scratch Buffer\n\n")

;; Command to convert org-mode buffer to markdown and copy to clipboard
(defun org-to-markdown-clipboard ()
  "Convert current org-mode buffer to markdown and copy to clipboard."
  (interactive)
  (let ((markdown-content
         (org-export-string-as (buffer-string) 'md t)))
    (kill-new markdown-content)
    (message "Org buffer exported to markdown and copied to clipboard!")))

;; Add keybinding for org to markdown conversion
(with-eval-after-load 'general
  (my-leader-def
    "om" '(org-to-markdown-clipboard :which-key "org to markdown clipboard")))

;; amzn
(use-package amz-workspace
  :after amz-common
  :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                   :files ("emacs-amazon-libs/amz-workspace.el"
                           "emacs-amazon-libs/amz-coral.el"
                           "emacs-amazon-libs/amz-bmds.el"
                           "emacs-amazon-libs/amz-brazil-cache.el"
                           "emacs-amazon-libs/amz-brazil-config.el"
                           "emacs-amazon-libs/amz-brazil-config-parser.el"
                           "emacs-amazon-libs/amz-shell.el"
                           "emacs-amazon-libs/brazil-path-cache-artifacts"))
  :custom (amz-workspace-default-root-directory "~/workspace"))


;; Java
(use-package eglot-java
  :ensure t
  :config
  ;; If using Lombok
  (add-to-list 'eglot-java-eclipse-jdt-args
               (format "-javaagent:%s" (expand-file-name "/Users/mobrienv/workplace/lombok.jar"))
               t))

;; git-backup
(use-package git-backup)

(defvar my/backup-dir (expand-file-name "~/.git-backup"))

(defun my/git-backup-versioning ()
  "Save a version of the current file."
  (unless (featurep 'git-backup)
    (require 'git-backup))
  (git-backup-version-file (executable-find "git") my/backup-dir '() (buffer-file-name)))

(defun my/git-backup-run-action (command commit-hash)
  "Execute COMMAND with COMMIT-HASH using another defaults arguments."
  (apply command `(,(executable-find "git") ,my/backup-dir ,commit-hash ,(buffer-file-name))))

(defun my/git-backup ()
  "Navigate in versions of the current file."
  (interactive)
  (unless (featurep 'git-backup)
    (require 'git-backup))
  ;; for some reason an extra space after `%h|' is required to avoid an error when
  ;; the shell command is executed
  (let* ((candidates (git-backup-list-file-change-time (executable-find "git") my/backup-dir "%cI|%h| %ar" (buffer-file-name)))
         (selection (completing-read "Pick revision: " candidates))
         (commit-hash (nth 1 (string-split selection "|")))
         (action (completing-read "Choose action: " '("diff" "new buffer" "replace current buffer"))))
    (cond ((string-equal action "diff") (my/git-backup-run-action 'git-backup-create-ediff commit-hash))
          ((string-equal action "new buffer") (my/git-backup-run-action 'git-backup-open-in-new-buffer commit-hash))
          ((string-equal action "replace current buffer") (my/git-backup-run-action 'git-backup-replace-current-buffer commit-hash))
          (t (message "Not valid option")))))

;; Consult
;; Example configuration for Consult
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)
