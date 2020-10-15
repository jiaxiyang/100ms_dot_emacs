(load "~/.emacs.d/init.elc")

;; basic confi
(set-frame-parameter nil 'alpha 0.955)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-hook 'after-init-hook #'(lambda ()
			       ;; (toggle-frame-fullscreen)))
(when (version< emacs-version "27.0")
  (set-default-font "Consolas 14"))
(when (version< "26.9" emacs-version)
  (set-frame-font "Consolas 14"))
;; (set-frame-font "Source Code Pro 14")
(when (eq window-system 'w32)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "等距更纱黑体 SC"))))
                      ;; (font-spec :family "微软雅黑"))))

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
(setq calendar-holidays nil)
(setq inhibit-compacting-font-caches t)

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
(global-set-key (kbd "<f9>") 'org-clock-jump-to-current-clock)
(global-set-key (kbd "<f12>") 'org-agenda-list)

;; org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(defun org-table-aligned()
  "org table aligned"
  (interactive)
  ;; (set-face-attribute 'org-table nil :family "等距更纱黑体 SC")
  (set-face-attribute 'org-table nil
                      :family "等距更纱黑体 SC"
                      :fontset (create-fontset-from-fontset-spec
                                (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-orgtable"
                                        ",han:等距更纱黑体 SC:size=24"
                                        ",latin:Consolas"))))

;; reference https://orgmode.org/worg/org-faq.html
(add-hook 'org-mode-hook
          (lambda ()
            (when (eq window-system 'w32)
              (org-table-aligned))
            (local-set-key (kbd "M-h") #'awesome-tab-backward-tab)
            (local-set-key (kbd "C-M-j") #'org-meta-return)))
(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-startup-folded t
      org-agenda-span 'day
      org-agenda-window-setup 'current-window
      org-agenda-start-on-weekday nil)
(setq org-agenda-inhibit-startup t) ;; ~50x speedup
;;(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
(setq org-tags-column 80)
(add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook
                                 'org-agenda-align-tags nil t)))
;; (setq org-agenda-files (list "~/org"
                             ;; "~/org/business_manage/national_exam.org"))
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files '("~/org/gtd.org"
			 "~/org/daily.org"
			 "~/org/weekly.org"
			 "~/org/ideas.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("w" "Work review" entry (file+datetree "~/org/review.org")
         "* Work review %U

|-------+------+-------+------+-------+--------------+---------|
| item  | work | learn | rest | state | relationship | average |
|-------+------+-------+------+-------+--------------+---------|
| score |      |       |      |       |              |         |
|-------+------+-------+------+-------+--------------+---------|
#+TBLFM: $7=round(vsum($2..$-1)/5)

#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 3 :block today
%?
#+END:

** work [/]
- [ ] ...
** learn [/]
- [ ] C++
- [ ] AI
- [ ] Emacs
** relationship [/]
- [ ] ...
"
         :empty-lines 1)

         ("l" "Life review" entry (file+datetree "~/org/review.org")
         "* Life review %U

|-------+-------+---------+------+-------+--------------+--------------+--------------+---------|
| item  | learn | healthy | rest | state | relationship | life average | work average | average |
|-------+-------+---------+------+-------+--------------+--------------+--------------+---------|
| score |       |         |      |       |              |              |              |         |
|-------+-------+---------+------+-------+--------------+--------------+--------------+---------|
#+TBLFM: $7=round(vsum($2..$6)/5)::$9=round(($7+$8)/2)

#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 3 :block today
%?
#+END:

** learn [/]
- [ ] talk
- [ ] blog
- [ ] EQ
- [ ] career plan
** healthy [/]
- [ ] run
- [ ] walk
- [ ] trval
- [ ] climb
** relationship [/]
- [ ] ...
"
         :empty-lines 1)))

(setq org-highest-priority ?A)
(setq org-lowest-priority  ?D)
(setq org-default-priority ?D)
(setq org-priority-faces
      '((?A . (:background "red" :foreground "white" :weight bold))
        (?B . (:background "DarkOrange" :foreground "white" :weight bold))
        (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
        (?D . (:background "DodgerBlue" :foreground "black" :weight bold))
        ))
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c@/!)")))
        ;; (sequence "|" "CANCELED(c)")))
(setq org-todo-keyword-faces '(
                               ;; ("TODO" . org-warning)
                               ;; ("DONE" . "green")
                               ("CANCELED" . "grey")
                               ))
(setq org-agenda-prefix-format
      (quote
       ((agenda . " %-10:c%?-12t% s"))))

(defun get-pic-from-url (url filename)
  (shell-command (concat "wget -O " filename " " url)))
(defun org-pomodoro-get-pictures ()
  "Get pomodoro notification pictures"
  (interactive)
  ;; (get-pic-from-url "https://pic3.zhimg.com/e76fc9f4120b9bd6ca2a7ae03a447f19_1440w.jpg?source=172ae18b" "~/begin_short_break.jpg")
  (get-pic-from-url
  "https://previews.123rf.com/images/vaselena/vaselena1507/vaselena150700001/42287443-white-alarm-clock-with-red-clock-face-with-inscription-coffee-time-without-figures-on-white-backgrou.jpg"
  "~/coffe_time.jpg")
  (get-pic-from-url
  "https://previews.123rf.com/images/vaselena/vaselena1510/vaselena151000045/47728782-white-alarm-clock-with-red-clock-face-with-inscription-work-time-without-figures-on-white-background.jpg"
  "~/work_time.jpg"))

(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (w32-shell-execute "open" "~/coffe_time.jpg")))
(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (w32-shell-execute "open" "~/work_time.jpg")))
(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (w32-shell-execute "open" "~/work_time.jpg")))

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


;; useful functions
(defun reload-emacs()
  "Reload emacs configuration"
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "M-<f5>") 'reload-emacs)
(global-set-key (kbd "<ESC> <f5>") 'reload-emacs)

(defun calfw-org-mode()
  "Open calfw org"
  (interactive)
  (load-file "~/.emacs.d/straight/build/calfw-org/calfw-org.el")
  (cfw:open-org-calendar))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-habit-show-habits-only-for-today nil)
 '(org-clocktable-defaults
   (quote
    (:maxlevel 3 :lang "en" :scope agenda :block nil :wstart 1 :mstart
               1 :tstart nil :tend nil :step nil :stepskip0 nil
               :fileskip0 t :tags nil :match nil :emphasize nil :link
               nil :narrow 40! :indent t :formula nil :timestamp nil
               :level nil :tcolumns nil :formatter nil)))
 '(org-log-into-drawer t)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info
             ol-irc ol-mhe ol-rmail ol-w3m)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
 '(hl-line ((t (:background "#3e4446"))))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(region ((t (:background "#4F6F4F"))))
 '(yascroll:thumb-text-area ((t (:background "#6c6c6c")))))
