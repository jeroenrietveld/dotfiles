;;; Package --- bla

;;; Commentary:


;;; Code:


;; Install and setup evil-nerd-commenter
(prelude-require-package 'evil-nerd-commenter)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
(define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map ",cl" 'evilnc-comment-or-uncomment-to-the-line)
(define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
(define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
(define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)


;; Some settings to make evil more natual for a VI refugee
(setq evil-motion-state-modes (append evil-emacs-state-modes
                                      evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

(defun my-move-key (keymap-from keymap-to key)
  "Move a key binding from KEYMAP-FROM to KEYMAP-TO, deleting KEY from the old location."
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")


;; Allow the cursor to cross lines
(setq evil-cross-lines t)


;; Either close the current elscreen, or if only one screen, use the ":q" Evil
;; command; this simulates the ":q" behavior of Vim when used with tabs.
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows; otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-elscreen (elscreen-one-screen-p))
        (one-window (one-window-p))
        )
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     ; if there are multiple elscreens (tabs), close the current elscreen
     ((not one-elscreen)
      (elscreen-kill)
      nil)
     ; if there is only one elscreen, just try to quit (calling elscreen-kill
     ; will not work, because elscreen-kill fails if there is only one
     ; elscreen)
     (one-elscreen
      (evil-quit)
      nil)
     )))
(evil-ex-define-cmd "q[uit]" 'vimlike-quit)
(evil-ex-define-cmd "Q[uit]" 'vimlike-quit)

;;; Setup ace-jump mode

;; key combos for ace-jump
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "C-c c") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "C-c w") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "C-c l") 'ace-jump-line-mode)
(define-key evil-operator-state-map (kbd "C-c c") 'ace-jump-char-mode)
(define-key evil-operator-state-map (kbd "C-c w") 'ace-jump-word-mode)
(define-key evil-operator-state-map (kbd "C-c l") 'ace-jump-line-mode)
(define-key evil-visual-state-map (kbd "C-c c") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "C-c w") 'ace-jump-word-mode)
(define-key evil-visual-state-map (kbd "C-c l") 'ace-jump-line-mode)

;; limit ace-jump to the window I’m working in
(setq ace-jump-mode-scope 'window)


;; Switch to minibuffer no matter where the focus is at
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(define-key evil-normal-state-map (kbd "C-c o") 'switch-to-minibuffer)
(define-key evil-insert-state-map (kbd "C-c o") 'switch-to-minibuffer)
(define-key evil-operator-state-map (kbd "C-c o") 'switch-to-minibuffer)
(define-key evil-visual-state-map (kbd "C-c o") 'switch-to-minibuffer)
(global-set-key (kbd "C-c o") 'switch-to-minibuffer)



;;; The following come from http://www.emacswiki.org/emacs/Evil

;; neat `ESC' key trick from Matt Briggs
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; change mode-line color by evil state (using some Zenburn colors)
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                         ((evil-normal-state-p) '("#558830" . "#ffffff"))
                         ((evil-visual-state-p) '("#8C5300" . "#ffffff"))
                         ((evil-insert-state-p) '("#8C5353" . "#ffffff"))
                         ((evil-emacs-state-p)  '("#6F6F6F" . "#ffffff"))
                         ((buffer-modified-p)   '("#366066" . "#ffffff"))
                         (t default-color))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))))
