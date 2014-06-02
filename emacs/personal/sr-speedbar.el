;;; sr-speedbar.el --- setup sr-speedbar quick and visible access to files and folders
;;; Commentary:

; The most painful part of Emacs fo far seems to be the lack of decent
; project tree navigation. The following features are needed:
; * Stays with/inside the current pane (emacs-nav seem to do, but does no tree)
; * Stays the same length
; * Shows current file
; * Integrates with Projectile (so the root can be pinned)
;
; Currently sr-speedbar seems to be the best option.
; Other options are: speedbar, neotree, dirtree and emacs-nav.

;;; Code:

(prelude-require-package 'sr-speedbar)
(require 'sr-speedbar)

(defun my:sr-speedbar-toggle()
  (interactive)
  (sr-speedbar-toggle)
  (with-current-buffer sr-speedbar-buffer-name (setq window-size-fixed 'width)))

(global-set-key (kbd "C-x t") 'my:sr-speedbar-toggle)
(global-set-key (kbd "C-x T") (lambda()
  (interactive)
  (sr-speedbar-select-window)
  (select-frame-set-input-focus (window-frame (selected-window)))))

(define-key speedbar-file-key-map "+" 'speedbar-create-directory)

(setq sr-speedbar-skip-other-window-p 1)
(setq sr-speedbar-width 40)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-max-width 40)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-console 40)
(setq sr-speedbar-width-x 40)

(setq speedbar-verbosity-level 2)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      speedbar-directory-unshown-regexp "^\\(\\.\\|\\.\\.\\)$"
      speedbar-file-unshown-regexp "flycheck-.*")
(setq speedbar-frame-parameters
  '((minibuffer)
    (width . 40)
    (border-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)))

;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

;; Highlight the current line
(add-hook 'speedbar-mode-hook (lambda () (setq truncate-lines t)))
