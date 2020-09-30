(load "~/.emacs.d/init.elc")

;; basic confi
(set-frame-parameter nil 'alpha 0.955)
(set-default-font "Consolas 14")
;; (set-frame-font "Consolas 14") ;; for 27.1xs
(setq gc-cons-threshold most-positive-fixnum)
(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq make-backup-files nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq scroll-step 1)
(setq set-mark-command-repeat-pop t)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)

;; mode config
(global-undo-tree-mode 1)
(save-place-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; (display-time-mode 1)

;; key-bind
(global-set-key (kbd "C-M-_") 'undo-tree-redo)
(global-set-key "\M-;" 'set-mark-command)
(global-set-key "\M-m" 'pop-to-mark-command)
;; (global-set-key "\M-m" 'pop-global-mark)

(global-set-key "\M-n" 'next-line)
(global-set-key "\M-p" 'previous-line)
(global-set-key (kbd "M-SPC") 'scroll-up-command)
(global-unset-key [C-down-mouse-1])
(global-unset-key [S-down-mouse-1])
(global-set-key (kbd "S-<mouse-3>") 'clipboard-yank)
(global-set-key (kbd "S-<mouse-2>") 'clipboard-yank)

(define-key ctl-x-map "\C-j" 'dired-jump)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'dired-jump-other-window)
(global-set-key (kbd "M-7") 'scroll-other-window)
(global-set-key (kbd "M-8") 'scroll-other-window-down)
(global-set-key (kbd "<f9>") 'scroll-other-window)
(global-set-key (kbd "<f10>") 'scroll-other-window-down)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; reference https://orgmode.org/worg/org-faq.html
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-h") #'awesome-tab-backward-tab)
            (local-set-key (kbd "C-M-j") #'org-meta-return)))
(setq org-startup-folded nil)

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
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)
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
 '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
 '(hl-line ((t (:background "#3e4446"))))
 '(region ((t (:background "#4F6F4F"))))
 '(yascroll:thumb-text-area ((t (:background "#6c6c6c")))))

;; useful functions
(defun reload-emacs()
  "Reload emacs configuration"
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "<f6>") 'reload-emacs)
