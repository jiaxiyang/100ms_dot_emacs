(load "/home/xiyang/.emacs.d/init.elc")

(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider));; eglot不高亮

(global-undo-tree-mode 1)

;; key-bind
(global-set-key "\M-n" 'next-line)
(global-set-key "\M-p" 'previous-line)
(global-unset-key [C-down-mouse-1])

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("c++" (mode . c++-mode))
               ("python" (mode . python-mode))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
                                        ;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)
