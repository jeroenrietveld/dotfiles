;; Great for quickly switching buffers while viewing them (Alt-TAB style)

(prelude-require-package 'iflipb)

(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
  (global-set-key
   (if (featurep 'xemacs) (kbd "<C-iso-left-tab>") (kbd "<C-S-iso-lefttab>"))
   'iflipb-previous-buffer)
