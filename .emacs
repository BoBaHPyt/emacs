;(defun swap-meta-and-super ()
;  "Swap the mapping of meta and super. Very useful for people using their Mac
;with a Windows external keyboard from time to time."
;  (interactive)
;  (if (eq mac-command-modifier 'super)
;    (progn
;      (setq mac-command-modifier 'meta)
;      (setq mac-option-modifier 'super)
;      (message "Command is now bound to META and Option is bound to SUPER."))
;    (progn
;      (setq mac-command-modifier 'super)
;      (setq mac-option-modifier 'meta)
;      (message "Command is now bound to SUPER and Option is bound to META."))))

;(global-set-key (kbd "C-c w") 'swap-meta-and-super)

;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------
(require 'package)
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    elpy
    magit
    use-package
    py-autopep8
    free-keys
    neotree
    rainbow-delimiters
    material-theme
    iedit))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme

;; init.el ends here

;; Marco configuration
(elpy-enable)
(ido-mode t)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Keys with C-c means for coding
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-branch-and-checkout)
(global-set-key (kbd "C-c f") 'elpy-find-file)
(global-set-key (kbd "C-c x") 'elpy-rgrep-symbol)
(global-set-key (kbd "C-c t") 'neotree-toggle)
(global-set-key (kbd "C-c g") 'neotree-find)
(global-set-key (kbd "C-c d") 'elpy-goto-definition)
;; pop-tag-mark allow us go back when we went to definition
(global-set-key (kbd "C-c r") 'pop-tag-mark)
(global-set-key (kbd "C-t") 'term)
(global-set-key (kbd "C-c C-]") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-_") 'shrink-window-horizontally)
;; next window
(global-set-key (kbd "C-c n") 'ace-window)

;; movement
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Ctrl-Shift-(up/down)
(global-set-key (kbd "C-S-<up>")  'move-line-up)
(global-set-key (kbd "C-S-<down>")  'move-line-down)

;; neo-tree config
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-width 40)

;; Open in browser
;; API_URL should be with https:// or http://
(setq api_url (getenv "API_URL"))

(setq browse-url-generic-program
      (cond
       ((eq system-type 'darwin) "open")
       (linux (executable-find "firefox"))
       ))

(defun w3mext-open-link-or-image-or-url (param1)
  "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
  (interactive)
  (let (url)
    (if (string= major-mode "w3m-mode")
        (setq url (or (w3m-anchor) (w3m-image) w3m-current-url)))
    (if (> (length param1) 0)
	(setq url param1))
    (browse-url-generic (if url url (car (browse-url-interactive-arg "URL: "))))
    ))
(global-set-key (kbd "C-c o") 'w3mext-open-link-or-image-or-url)
(global-set-key (kbd "C-c p") (lambda () (interactive) (w3mext-open-link-or-image-or-url api_url)))

;; load external
(if (file-exists-p "~/.custom.el") (load-file "~/.custom.el"))

;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))

;; Load yasnippet
(yas-global-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rjsx-mode iedit use-package treemacs-projectile treemacs-evil swiper rainbow-delimiters py-autopep8 neotree material-theme magit free-keys elpy dired-sidebar better-defaults all-the-icons-dired))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
