;;; termux-config.el --- Configuration for Termux touchscreen support

;; Enable mouse support in terminal mode
(when (not (display-graphic-p))
  ;; Enable xterm mouse mode
  (xterm-mouse-mode 1)
  
  ;; Enable mouse wheel scrolling
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3)))
  
  ;; Make scrolling smoother
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  
  ;; Enable mouse in Evil mode if using Evil
  (with-eval-after-load 'evil
    (setq evil-want-fine-undo t)
    (define-key evil-normal-state-map [down-mouse-1] 'mouse-set-point)
    (define-key evil-visual-state-map [down-mouse-1] 'mouse-set-point)
    (define-key evil-insert-state-map [down-mouse-1] 'mouse-set-point))
  
  ;; Better touch scrolling behavior
  (setq mouse-wheel-follow-mouse t)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil))

;; Handle touch events as mouse events
(global-set-key [touchscreen-begin] 'mouse-set-point)
(global-set-key [touchscreen-end] 'mouse-set-point)

(provide 'termux-config)
;;; termux-config.el ends here