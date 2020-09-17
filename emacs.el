(load "/home/xiyang/.emacs.d/init.elc")

(setq lsp-ui-doc-enable nil) ;; lsp关闭弹窗
;;(setq lsp-ui-sideline-mode nil) ;; lsp 关闭右侧错误提示
(setq lsp-enable-symbol-highlighting nil) ;; lsp 不高亮

(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider));; eglot不高亮

(global-undo-tree-mode 1)
(save-place-mode 1)

;; key-bind
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)
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
               ("Rust" (or (mode . rust-mode) (mode . conf-toml-mode)))
               ("C++" (or(mode . c++-mode) (mode . c-mode)))
               ("Dired" (mode . dired-mode))
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



;; ;; (transient-mark-mode t)
;; (defadvice kill-ring-save (before slick-copy activate compile)
;;  "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (message"Copied line")
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))
;; (defadvice kill-region (before slick-cut activate compile)
;;  "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))

(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "C-M-j") 'newline-at-end-of-line)
(global-set-key (kbd "M-RET") 'newline-at-end-of-line)

(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-r") 'symbol-overlay-remove-all)
(global-set-key (kbd "ESC <right>") 'symbol-overlay-switch-forward)
(global-set-key (kbd "ESC <left>") 'symbol-overlay-switch-backward)

(defun f-switch-to-buffer (dir)
  (unless (minibufferp)
    (let ((bn (buffer-name))
	  (name (if (> dir 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
          (active-buffers (mapcar 'window-buffer (window-list)))
          buffer p)
      (funcall name)
      (while (not p)
        (setq buffer (current-buffer))
        (if (or buffer-file-name
                (not buffer-read-only)
                (get-buffer-process buffer)
                (cl-position buffer active-buffers)
                (string= bn (buffer-name)))
            (setq p t)
          (kill-buffer)
          (funcall name))))))

(defun c-switch-to-next-buffer ()
  (interactive)
  (f-switch-to-buffer 1))

(defun c-switch-to-prev-buffer ()
  (interactive)
  (f-switch-to-buffer 0))
;; (global-set-key (kbd "M-j") 'c-switch-to-prev-buffer)
(global-set-key (kbd "M-k") 'c-switch-to-next-buffer)
(global-set-key (kbd "M-h") 'awesome-tab-backward-tab)
;; (global-set-key (kbd "M-j") 'awesome-tab-forward-group)
(global-set-key (kbd "M-k") 'awesome-tab-backward-group)
(global-set-key (kbd "M-l") 'awesome-tab-forward-tab)


