;; Mode line config
;; Mainly copied from Amit http://amitp.blogspot.fi/2011/08/emacs-custom-mode-line.html
;;
;; Format:
;;   column status directory/filename [filesize] [modes] date time (uptime)
;; There's no need for line number since I have global-linum-mode enabled.
(setq-default
 mode-line-format
 '(
   (:eval (propertize "%5c" 'face
                      (if (>= (current-column) 120)
                          'myline-120col-warning-face
                        'myline-position-face)))
   "  "
   (:eval
    (cond ((buffer-modified-p)
           (propertize " ** " 'face 'myline-modified-face))
          (buffer-read-only
           (propertize " RO " 'face 'myline-read-only-face))
          (t "    ")))
   " "
   (:propertize (:eval (shortdir default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b" face myline-filename-face)
   " ["
   (:propertize "%I" 'face 'font-lock-constact-face)
   "]"
   " %n "
   (vc-mode vc-mode)
   "["
   (:propertize mode-name face myline-mode-face)
   "] ["
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'myline-minor-mode-face))
   " ] %n "
   (:eval (propertize (format-time-string "%a %b %e %H:%M")))
   " %n "
   (:eval (emacs-uptime "(%h:%m)"))
   ))

(defun shortdir (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Faces
(make-face 'myline-position-face)
(make-face 'myline-120col-warning-face)
(make-face 'myline-modified-face)
(make-face 'myline-read-only-face)
(make-face 'myline-folder-face)
(make-face 'myline-filename-face)
(make-face 'myline-mode-face)
(make-face 'myline-minor-mode-face)
(make-face 'myline-process-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20"
    :inverse-video nil
    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :box '(:line-width 6 :color "gray40" :style nil))

(set-face-attribute 'myline-position-face nil
    :inherit 'mode-line-face
    :height 100)
(set-face-attribute 'myline-120col-warning-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
(set-face-attribute 'myline-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'myline-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'myline-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'myline-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'myline-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'myline-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'myline-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
