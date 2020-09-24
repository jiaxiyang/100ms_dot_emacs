(load "~/.emacs.d/init.elc")

;; var config
(setq gc-cons-threshold most-positive-fixnum)
(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq scroll-step 1)
(setq set-mark-command-repeat-pop t)

;; mode config
(global-undo-tree-mode 1)
(save-place-mode 1)
(menu-bar-mode -1)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; (display-time-mode 1)

;; key-bind
(global-set-key "\M-;" 'set-mark-command)
(global-set-key "\M-m" 'pop-to-mark-command)
;; (global-set-key "\M-m" 'pop-global-mark)

(global-set-key "\M-n" 'next-line)
(global-set-key "\M-p" 'previous-line)
(global-set-key (kbd "M-SPC") 'scroll-up-command)
(global-unset-key [C-down-mouse-1])

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
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

;; newline
(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun newline-at-begin-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-beginning-of-line 1)
  (previous-line 1)
  (newline-at-end-of-line))

(global-set-key (kbd "C-M-j") 'newline-at-end-of-line)
(global-set-key (kbd "M-RET") 'newline-at-end-of-line)
(global-set-key (kbd "C-o") 'newline-at-begin-of-line)

;; switch buffer
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
;; (global-set-key (kbd "M-k") 'c-switch-to-next-buffer)

;; hide minor-mode in modeline
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (t elem)))
              mode-line-modes))

;; zenburn theme
;; put `export TERM=xterm-256color` in .bashrc or .zshrc
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/straight/build/zenburn-theme")
(load-theme 'zenburn t)

;; solarized
;;(add-to-list 'custom-theme-load-path
;;             "~/.emacs.d/straight/build/solarized-theme")
;;(load-theme 'solarized-light t)


;; set putty color theme zenburn and set cursor color
;; "Colour5"="220,220,220", red: "Colour8"="204,147,147" in
;; putty zenburn theme
;; color set references: zenburn-theme.el
;; comand: list-face-display and list-colors-display
;; use list-face-display set in .emacs and copy to emacs.el
(global-hl-line-mode 1)
;; (setq cursor-type 'bar)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; `(ace-jump-face-foreground
     ;; ((t (:foreground ,"#cd0000" :background , "#2F2F2F" :inverse-video nil))))
 '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
 '(hl-line ((t (:background "#3e4446"))))
 '(region ((t (:background "#4F6F4F")))))

;; useful functions
(defun reload-emacs()
  "Reload emacs configuration"
  (interactive)
  (load-file "~/.emacs"))
