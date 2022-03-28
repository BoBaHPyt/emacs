(defun replace-in-buffer ()
    (interactive)
    (save-excursion
      (if (equal mark-active nil) (mark-word))
      (setq curr-word (buffer-substring-no-properties (mark) (point)))
      (setq old-string (read-string "OLD string:\n" curr-word))
      (setq new-string (read-string "NEW string:\n" old-string))
      (query-replace old-string new-string nil (point-min) (point-max))
    )
)

(global-set-key (kbd "C-c z") 'replace-in-buffer)

;(require 'evil)
;  (evil-mode 1)

; replace in place
(require 'iedit)
(global-set-key (kbd "C-c C-i") 'iedit-mode)

;; set grep exclude shit
(setq fcommand (concat "find " (concat (getenv "PWD") " -type f -path '*build*' -prune -o -path '*idea*' -prune -o -path '*dist*' -prune -o -path '*.git*' -prune -o -path '*egg*' -prune -o -print0 | xargs -0 grep -s -nH -e ")))
;(setq grep-find-command "find (getenv 'PWD') -type f -path '*build*' -prune -o -print0 | xargs -0 grep -s -nH -e ")
(setq grep-find-command fcommand)


;;define function for beautify code
(defun tidy-python ()
  "Tidies the Python content in the buffer using `black'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters (pacman -S python-black before)
   "black -"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Tidy Error Buffer*"
   ;; show error buffer?
   t))

;; go to definition (performs better with silversearcher -> pacman -S the_silver_searcher)
;;(require 'dumb-jump)
(global-set-key (kbd "C-c g") 'xref-find-references)
