(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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


;; Font configuration with Retina variant and fallback
(defvar my-font-variants
  '("FiraCode Nerd Font Ret"  ; Retina variant (preferred)
    "FiraCode Nerd Font"       ; Regular variant (fallback)
    "Fira Code Nerd Font")     ; Alternative naming
  "List of font variants to try in order of preference.")

(defvar my-font-size 24 "Font size to use in points (for example, 10).")

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

;; Global text scaling for HiDPI displays
;; Increase this value to make everything larger (2.4 = 240% scale)
(setq my-emacs-scale 2.4)

;; Apply scaling to all faces
(defun my-set-face-scale ()
  "Scale all faces by my-emacs-scale factor."
  (set-face-attribute 'default nil :height (round (* 100 my-emacs-scale)))
  ;; Scale other important faces proportionally
  (dolist (face '(header-line
                  tooltip))
    (when (facep face)
      (set-face-attribute face nil :height my-emacs-scale))))

;; Apply scaling on startup
(add-hook 'after-init-hook 'my-set-face-scale)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my-set-face-scale)))

;; Interactive functions to adjust scaling on the fly
(defun my-increase-emacs-scale ()
  "Increase Emacs rendering scale by 10%."
  (interactive)
  (setq my-emacs-scale (+ my-emacs-scale 0.1))
  (my-set-face-scale)
  (message "Emacs scale: %.1f" my-emacs-scale))

(defun my-decrease-emacs-scale ()
  "Decrease Emacs rendering scale by 10%."
  (interactive)
  (setq my-emacs-scale (max 0.5 (- my-emacs-scale 0.1)))
  (my-set-face-scale)
  (message "Emacs scale: %.1f" my-emacs-scale))

(defun my-reset-emacs-scale ()
  "Reset Emacs rendering scale to 1.0."
  (interactive)
  (setq my-emacs-scale 1.0)
  (my-set-face-scale)
  (message "Emacs scale reset to 1.0"))

;; Bind scaling commands to convenient keys
(global-set-key (kbd "C-+") 'my-increase-emacs-scale)
(global-set-key (kbd "C-=") 'my-increase-emacs-scale)  ; For keyboards without numpad
(global-set-key (kbd "C--") 'my-decrease-emacs-scale)
(global-set-key (kbd "C-0") 'my-reset-emacs-scale)

(use-package vterm
  :commands vterm
  :bind ((:map vterm-mode-map
               ("C-y" . vterm-yank)
               ("M-y" . vterm-yank-pop)
               ("C-q" . vterm-send-next-key)
               ("C-z" . nil)
               ("M-:" . nil)
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
              (set-face-background 'cursor nil t)))

  ;; Better Evil integration for vterm
  (with-eval-after-load 'evil
    ;; Use emacs state in vterm by default
    (evil-set-initial-state 'vterm-mode 'emacs)

    ;; Allow ESC to switch to Evil normal mode
    (define-key vterm-mode-map (kbd "<escape>") 'evil-normal-state)

    ;; In normal mode, 'i' returns to emacs state (not insert)
    (evil-define-key 'normal vterm-mode-map (kbd "i") 'evil-emacs-state)
    (evil-define-key 'normal vterm-mode-map (kbd "a") 'evil-emacs-state)))

;; Fix for shell-command with fish shell
;; This ensures fish knows it's running in a non-interactive context
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

;; Minimal modeline
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Fix evil-mode undo canary error
(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/OMMIT_EDITMSG$" "/git-rebase-todo$"))
  (undo-fu-session-global-mode))

;; Configure evil to use undo-fu
(with-eval-after-load 'evil
  (setq evil-undo-system 'undo-fu)
  ;; Clear any corrupted undo history
  (setq-default buffer-undo-list nil))

;; Evil Collection for better evil integration with Emacs modes
;; Note: evil-want-keybinding is set to nil in personal/preload/evil-config.el
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(vterm help dired magit))  ; Start with a few modes
  (evil-collection-init))

;; Auto-revert files when they change on disk
(global-auto-revert-mode 1)
;; Also auto-revert dired buffers
(setq global-auto-revert-non-file-buffers t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
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
    "bs"   '(save-buffer :which-key "save buffer")
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
