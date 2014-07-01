;;; sr-speedbar.el --- setup sr-speedbar quick and visible access to files and folders
;;; Commentary:

; The most painful part of Emacs fo far seems to be the lack of decent
; project tree navigation.  The following features are needed:
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
(define-key speedbar-file-key-map "m" 'speedbar-options-menu)

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

;; Speedbar and projectile joining forces
(defun find-current-project-root()
  (setq current-dir (file-truename buffer-file-name))
  (while (not (file-exists-p (concat current-dir ".git")))
    (setq current-dir (file-name-directory (substring current-dir 0 -1))))
  (concat current-dir ""))

(defun speedbar-refresh-with-project (root-dir)
  "Refreshes speedbar with the ROOT-DIR."
  (when (and (not (equal root-dir sr-speedbar-last-refresh-dictionary))
             (not (sr-speedbar-window-p)))
    (setq sr-speedbar-last-refresh-dictionary root-dir))
  (setq default-directory root-dir)
  (speedbar-refresh))

(defun open-current-project-speedbar (root-dir)
  "Opens the current project in speedbar with ROOT-DIR."
  (if (not (sr-speedbar-exist-p))
    (sr-speedbar-toggle))
  (speedbar-refresh-with-project root-dir))

(defun speedbar-expand-line-list (&optional arg)
  (when arg
    (message arg)
    (re-search-forward (concat " " (car arg) "$"))
    (speedbar-expand-line (car arg))
    (speedbar-next 1)
    (speedbar-expand-line-list (cdr arg))))

(defun speedbar-open-current-buffer()
  (interactive)
  (let* ((root-dir (find-current-project-root))
         (original-buffer-file-directory (file-name-directory (buffer-file-name)))
         (relative-buffer-path (car (cdr (split-string original-buffer-file-directory root-dir))))
         (parents (butlast (split-string original-buffer-file-directory root-dir)))
         (original-window (get-buffer-window)))
    (save-excursion
      (open-current-project-speedbar root-dir)
      (select-window (get-buffer-window speedbar-buffer))
      (beginning-of-buffer)
      (speedbar-expand-line-list parents)
      (if (not (eq original-window (get-buffer-window speedbar-buffer)))
          (select-window original-window)
        (other-window 1)))))

(defun speedbar-options-menu (option)
  "Options menu for speedbar.

   OPTION is the argument supplied by interactive"
  (interactive "sMenu options [a(dd), r(emove)]:"))

;(defun speedbar-open-in-split
;  "Open file in split."
;  (setq new-window (split-window-horizontally (select-window next-window))
;        (message new-window))

;(speedbar-open-in-split)

;;; sr-speedbar.el ends here
