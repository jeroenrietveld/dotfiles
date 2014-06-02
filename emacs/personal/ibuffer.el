;;; ibuffer.el --- Set up ibuffer

(eval-after-load "ibuf-ext"
  '(progn

     (define-ibuffer-filter unsaved-file-buffers
         "Only show unsaved buffers backed by a real file."
       (:description "unsaved file buffers")
       (and (buffer-local-value 'buffer-file-name buf)
            (buffer-modified-p buf)))

     (define-ibuffer-filter file-buffers
         "Only show buffers backed by a real file."
       (:description "file buffers")
       (buffer-local-value 'buffer-file-name buf))

     (define-ibuffer-filter tramp-buffers
         "Only show buffers associated with a tramp connection."
       (:description "tramp buffers")
       (ibuffer-tramp-connection buf))

     (define-ibuffer-filter filename
         "Toggle current view to buffers with file or directory name matching QUALIFIER."
       (:description "filename"
                     :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
       (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                          (buffer-local-value 'dired-directory buf))
         (or (string-match qualifier (expand-file-name it))
             (string-match (expand-file-name qualifier) (expand-file-name it)))))))

(eval-after-load "ibuffer"
  '(progn
     (setq ibuffer-expert t
           ibuffer-show-empty-filter-groups nil
           ibuffer-display-summary nil)

     (setq ibuffer-saved-filter-groups
           (list
            (cons "default"
                  (append
                   (if (fboundp 'my/define-projectile-filter-groups)
                       (my/define-projectile-filter-groups))
                   '(("Notes" (or (mode . org-mode)
                                  (mode . diary-mode)
                                  (mode . org-agenda-mode)
                                  (name . "\\temp")))
                     ("Logs" (or (filename . "\.log$")
                                 (filename . "[_-][lL]og$"))))
                   (if (fboundp 'my/define-tramp-filter-groups)
                       (my/define-tramp-filter-groups))
                   '(("Dired" (mode . dired-mode))
                     ("Files" (filename . ".*"))
                     ("Help" (or (mode . Info-mode)
                                 (mode . apropos-mode)
                                 (mode . help-mode)
                                 (mode . Man-mode)))
                     ("Special" (name . "\\*.*\\*")))))))

     (defun my/ibuffer-mode-hook ()
       (hl-line-mode)
       (ibuffer-auto-mode 1)
       (define-key ibuffer-mode-map (kbd "C-x C-f") 'my/ibuffer-ido-find-file)
       (define-key ibuffer-mode-map (kbd "/ *") 'ibuffer-filter-by-unsaved-file-buffers)
       (define-key ibuffer-mode-map (kbd "/ .") 'ibuffer-filter-by-file-buffers)
       (define-key ibuffer-mode-map (kbd "/ T") 'ibuffer-filter-by-tramp-buffers)
       (ibuffer-switch-to-saved-filter-groups "default"))
     (add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook)

     (defun my/ibuffer-ido-find-file ()
       "Like `ido-find-file', but default to the directory of the buffer at point."
       (interactive)
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (with-current-buffer buf
                                        default-directory)
                                    default-directory))))
         (ido-find-file)))

     (defun my/define-projectile-filter-groups ()
       (when (boundp 'projectile-known-projects)
         (setq my/project-filter-groups
           (--map (list
               (concat "Project: " (file-name-nondirectory (directory-file-name it)))
               `(filename . ,it))
             projectile-known-projects))))

  )
)



(defadvice ibuffer (around ibuffer-toggle activate compile) ()
  "Show or bury ibuffer."
  (if (eq major-mode 'ibuffer-mode)
      (bury-buffer)
    ad-do-it))


;;; ibuffer.el ends here
