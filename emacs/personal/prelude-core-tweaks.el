;;; Tweaks on Prelude's core functionality


;; Prelude's whitespace mode is too whiny, so turn it off
(setq prelude-whitespace nil)
;; Yet saving whitespace was a nice feature in that mode, so setup something similar
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Wrap on word boudries
(global-visual-line-mode 1)

;; I dont like FlySpell on my comments
(setq prelude-flyspell nil)
;; TODO: get some function to do FlyCheck of comments on demand

;; Set a scroll margin
(setq scroll-margin 8)

;; Overwrite the fringe size back to the default 8 pixels
(set-fringe-mode nil)

;; Avoid potential tramp problem
;(add-hook 'desktop-save-hook 'tramp-cleanup-all-buffers)
