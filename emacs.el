(load "/home/xiyang/.emacs.d/init.elc")

(setq lsp-ui-doc-enable nil) ;; lsp关闭弹窗
(setq lsp-enable-symbol-highlighting nil) ;; lsp 不高亮
(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider));; eglot不高亮

(global-undo-tree-mode 1)

;; key-bind
(global-set-key "\M-n" 'next-line)
(global-set-key "\M-p" 'previous-line)
(global-unset-key [C-down-mouse-1])

(defun insert-comma()
  (interactive)
  (insert ","))
(global-set-key (kbd "C-c ,") 'insert-comma)
(global-set-key (kbd "C-x ,") 'insert-comma)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Rust" (or (mode . rust-mode) (mode . conf-toml-mode)))
               ("C++" (or(mode . c++-mode) (mode . c-mode)))
               ("Python" (mode . python-mode))
               ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("Org" (name . "^.*org$"))
               ("Magit" (mode . magit-mode))
               ("Emacs" (or
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
