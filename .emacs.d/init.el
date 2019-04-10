;;;;;;;;;;;;;;;
;; dot emacs

(setq user-full-name "Kirsi Rinnesalo"
      user-mail-address "kirsi.rinnesalo@gmail.com"
      default-directory "~/")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (unless (package-installed-p 'monokai-theme)
    (package-refresh-contents)
    (package-install 'monokai-theme))
  (require 'monokai-theme)
  (unless (package-installed-p 'markdown-mode)
    (package-refresh-contents)
    (package-install 'markdown-mode))
  (require 'markdown-mode))


(let ((default-directory (concat user-emacs-directory "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Split window right when window width is more than 100 cols
(setq split-height-threshold nil)
(setq split-width-threshold 100)

;;;;;;;;;;;;;;
;; theme

(if (display-graphic-p)
    (load-theme 'monokai t)
  (load-theme 'tangotango t))

(load-library "myline")

;;;;;;;;;;;;;;;
;; backups
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://www.emacswiki.org/emacs/BackupDirectory

(setq backups (concat user-emacs-directory "backups"))
(if (not (file-exists-p backups))
    (make-directory backups t))

(setq make-backup-files t
      backup-by-copying t
      backup-directory-alist `((".*" . ,backups))
      version-control t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;;;;;;;;;;;;;;;
;; ui

(when (display-graphic-p)
  (set-frame-height (selected-frame) 43)
  (set-frame-width (selected-frame) 140)
  (set-frame-position (selected-frame) 140 50))

(set-input-mode (car (current-input-mode))
		(nth 1 (current-input-mode))
		'accept-8bit-input)

(setq inhibit-startup-message t
      completion-auto-help t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq require-final-newline t)

;;displaying date and time is handled in myline
;;(setq display-time-24hr-format t
;;      display-time-day-and-date t)
;;(display-time)

(global-linum-mode t);(setq line-number-mode t)
;;in terminal add little space after linum
(when (not (display-graphic-p))
  (defun linum-format-func (line)
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format "%%%dd " w) line) 'face 'linum)))
  (setq linum-format 'linum-format-func))
    
(setq column-number-mode t)

(setq scroll-step 1)
(setq scroll-error-top-bottom t)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(turn-on-font-lock)

(show-paren-mode t)

(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 120)

;;;;;;;;;;;;;;;
;; keys

(delete-selection-mode)

(when (display-graphic-p)
  (global-set-key "\C-xh" nil)
  (global-set-key "\C-a" 'mark-whole-buffer)
  (global-set-key "\C-x\M-x" 'save-buffers-kill-emacs)
  (global-set-key "\C-x\C-c" 'dont-kill-emacs)
  )

(cond ((eq system-type 'windows-nt)
       (global-set-key (kbd "M-<left>")  'windmove-left)
       (global-set-key (kbd "M-<right>") 'windmove-right)
       (global-set-key (kbd "M-<up>")    'windmove-up)
       (global-set-key (kbd "M-<down>")  'windmove-down))
      ((eq system-type 'gnu/linux)
       (global-set-key (kbd "M-c <left>")  'windmove-left)
       (global-set-key (kbd "M-c <right>") 'windmove-right)
       (global-set-key (kbd "M-c <up>")    'windmove-up)
       (global-set-key (kbd "M-c <down>")  'windmove-down)))

(global-set-key (kbd "C-x <up>")    'enlarge-window)
(global-set-key (kbd "C-x <down>")  'shrink-window)
(global-set-key (kbd "C-x <left>")  'enlarge-window-horizontally)
(global-set-key (kbd "C-x <right>") 'shrink-window-horizontally)

(global-set-key "\M-r"  'eval-buffer)
(global-set-key "\C-xc" 'clean-buffer-list)
(global-set-key "\C-xw" 'kill-current-buffer)
(global-set-key "\C-xp" 'previous-buffer)
(global-set-key "\C-xn" 'next-buffer)

(global-set-key (kbd "C-c e") 'find-user-init-file)

;;;;;;;;;;;;;;;
;; functions

(defun find-user-init-file ()
  "Edit the user-init-file"
  (interactive)
  (find-file user-init-file))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(setq preserved-buffers '("*scratch*" "*Messages*"))

(setq to-be-removed-buffers-regexp '("\\*.*\\*"))

(defun clean-buffer-list ()
  "Kill buffers by regexp to-be-removed-buffers-regexp except ones listed in preserved-buffers."
  (interactive)
  (dolist (buf (delq (current-buffer) (buffer-list)))
    (when (buffer-live-p buf)
      (setq bn (buffer-name buf))
      (unless (cl-find bn preserved-buffers
                       :test #'string-equal)
        (if (cl-find bn to-be-removed-buffers-regexp
                     :test (lambda (bn re)
                             (if (functionp re)
                                 (funcall re bn)
                               (string-match re bn))))
            (progn
              (message "Killing buffer %s" bn)
              (kill-buffer buf))))))
  (delete-other-windows))

(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: C-x M-x")))

;;;;;;;;;;;;;;;
;; modes

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;;;;;
;; hooks

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

