(setq inhibit-startup-message t)

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


(defvar my-font-name "Fira Code Nerd Font" "Retina")
(defvar my-font-size 10 "Font size to use in points (for example, 10).")
(defvar my-font (format "%s-%f" my-font-name my-font-size))

(defun font-exists-p (font)
   "Check if the FONT exists." (and (display-graphic-p) (not (null (x-list-fonts font)))))

(cond ((font-exists-p my-font)
       (add-to-list 'default-frame-alist (font . ,my-font))
       (add-to-list 'default-frame-alist (width . 170))
       (add-to-list 'initial-frame-alist(font . ,my-font))
       (add-to-list 'initial-frame-alist(width  . 170))))

(use-package vterm
  :commands vterm
  :bind ((:map vterm-mode-map
               ("C-y" . vterm-yank)
               ("M-y" . vterm-yank-pop)
               ("C-q" . vterm-send-next-key)
               ("C-z" . nil)
               ("M-:" . nil)))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  :config
  ;; Make vterm work nicely with evil-mode
  (with-eval-after-load 'evil
    ;; Use emacs state in vterm by default for proper terminal interaction
    (evil-set-initial-state 'vterm-mode 'emacs)
    ;; Allow switching to normal state with C-z for navigation
    (evil-define-key 'emacs vterm-mode-map (kbd "C-z") 'evil-normal-state)
    (evil-define-key 'normal vterm-mode-map (kbd "C-z") 'evil-emacs-state)
    (evil-define-key 'normal vterm-mode-map (kbd "i") 'evil-emacs-state)
    (evil-define-key 'normal vterm-mode-map (kbd "a") 'evil-emacs-state)
    (evil-define-key 'normal vterm-mode-map (kbd "p") 'vterm-yank)
    (evil-define-key 'normal vterm-mode-map (kbd "P") 'vterm-yank)))

;; Fix evil-mode undo canary error
(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG$" "/git-rebase-todo$"))
  (undo-fu-session-global-mode))

;; Configure evil to use undo-fu
(with-eval-after-load 'evil
  (setq evil-undo-system 'undo-fu)
  ;; Clear any corrupted undo history
  (setq-default buffer-undo-list nil))

;; Evil Collection for better evil integration with Emacs modes
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(help dired magit))  ; Start with a few modes
  (evil-collection-init))

;; Auto-revert files when they change on disk
(global-auto-revert-mode 1)
;; Also auto-revert dired buffers
(setq global-auto-revert-non-file-buffers t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-side 'bottom
        claude-code-ide-window-height 30)) ; Optionally enable Emacs MCP tools

;; Test
