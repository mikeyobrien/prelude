;; ABOUTME: Minimal additions to Prelude that aren't already included
;; ABOUTME: Only features that Prelude doesn't provide out of the box

;;; Git Enhancements (Prelude has Magit but not these)

;; Git gutter - Show git changes in fringe
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.5))

;; Git time machine - Browse file history
(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine))

;;; Editing Features Not in Prelude

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Hungry delete - Delete all whitespace at once
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;;; Visual Enhancements

;; Beacon - Highlight cursor after jumps
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "#5e8d87"))

;;; Workspace Management

;; Perspective - Project workspaces
(use-package perspective
  :ensure t
  :bind (("C-x C-b" . persp-list-buffers)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

;;; Performance

;; Garbage collection optimization
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

;;; Settings that complement Prelude

;; Better scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Show matching parens instantly
(setq show-paren-delay 0)

;; Line numbers in prog-mode (Prelude doesn't enable by default)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(provide 'minimal-additions)
;;; minimal-additions.el ends here