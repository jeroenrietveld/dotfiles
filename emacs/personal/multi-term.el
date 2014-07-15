;;; multi-term.el --- for managing multiple terminal buffers in Emacs
;;; Commentary:

; The default Emacs term seems to have some trouble with zshell
; Multi-term solves this

;;; Code:

(require 'multi-term)

(setq multi-term-program "bin/bash")

;;; multi-term.el ends here
