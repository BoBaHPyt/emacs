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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
    dumb-jump
    iedit
    request
    tblui
    markdown-mode
    spinner
    lv
    ht
    doom-modeline
    ace-window))

;; JAVA MODE
;; Install it directly from M-x list-packages -> jdee

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)
(add-to-list 'load-path "~/.emacs.d/custompackages/")
(require 'openai)
(require 'codegpt)
(require 'chatgpt)

(setq openai-base-url "http://192.210.243.31:1337/v1")
(setq openai-key "api-key")
(setq codegpt-tunnel 'chat)
(setq codegpt-model "gpt-4o")
(setq chatgpt-model "gpt-4o")
(setq chatgpt-animate-fps 30)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(defun close-all-buff ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
)
(defun run-python-ide ()
  (interactive)
  (if (file-exists-p "./venv") (pyvenv-activate "./venv"))
  ;(mapc 'kill-buffer (buffer-list))
  (neotree-show)
)

(setq inhibit-startup-message t) ;; hide the startup message
;; init.el ends here

;; Marco configuration
(elpy-enable)
(doom-modeline-mode)
(ido-mode t)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'python-mode-hook (lambda () (company-mode 1)))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

(ac-config-default)

;; Keys with C-c means for coding
(global-set-key (kbd "C-c i") 'run-python-ide)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-branch-and-checkout)
(global-set-key (kbd "C-c f") 'elpy-find-file)
(global-set-key (kbd "C-c x") 'elpy-rgrep-symbol)
;;(global-set-key (kbd "C-c t") 'neotree-toggle)
;;(global-set-key (kbd "C-c g") 'neotree-find)
(global-set-key (kbd "C-c C-g") 'grep-find)
(global-set-key (kbd "C-c d") 'elpy-goto-definition)
;; pop-tag-mark allow us go back when we went to definition
(global-set-key (kbd "C-c r") 'pop-tag-mark)
(global-set-key (kbd "C-t") 'term)
(global-set-key (kbd "C-c C-]") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-_") 'shrink-window-horizontally)
;; next window
(global-set-key (kbd "C-c n") 'ace-window)
(global-set-key (kbd "C-c <right>") 'ace-window)
;; sidebar (if emacs oppened with filename, no project-root is set, just run "emacs ." where . is the current directory)
(global-set-key (kbd "C-c t") 'dired-sidebar-toggle-with-current-directory)


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
(setq neo-window-width 30)

;; Open in browser
;; API_URL should be with https:// or http://
(setq api_url (getenv "API_URL"))

(setq browse-url-generic-program
      (cond
       ((eq system-type 'darwin) "open")
       (t "xdg-open")
       )
)

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
(if (file-exists-p "~/emacs/.custom.el") (load-file "~/emacs/.custom.el"))

;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))

;; Load yasnippet
(yas-global-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "350fef8767e45b0f81dd54c986ee6854857f27067bac88d2b1c2a6fa7fecb522" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(find-grep-options "-q")
 '(grep-find-ignored-directories
   '("build" "dist" ".idea" "SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
 '(package-selected-packages
   '(doom-modeline doom-themes chatgpt-shell realgud rjsx-mode iedit use-package treemacs-projectile treemacs-evil swiper rainbow-delimiters py-autopep8 neotree magit free-keys elpy dired-sidebar better-defaults all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'doom-Iosvkem)
(when (not window-system)
  (set-face-background 'default "unspecified-bg")
  )
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode 1)

(put 'downcase-region 'disabled nil)

(setq neo-hidden-regexp-list '("^\\.(?!gitignore).*" "^#.*#$" "~$" "__pycache__"))
