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
    use-package
    free-keys
    rainbow-delimiters
    dumb-jump
    magit
    elpy
    py-autopep8
    iedit
    request
    tblui
    markdown-mode
    spinner
    lv
    ht
    doom-modeline
    doom-themes
    ace-window
    neotree
    flycheck
    flycheck-mypy
    company-jedi
    python-mode
    web-mode
    vdiff-magit))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)
(add-to-list 'load-path "~/.emacs.d/custompackages/")

;; Requires Emacs 29 and git
(unless (package-installed-p 'pg)
   (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
   (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))
(require 'pgmacs)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(defun close-all-buff ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
)
(defun run-python-ide ()
  (interactive)
  (if (file-exists-p "./venv") (pyvenv-activate "./venv"))
  (elpy-enable)
  (neotree)

  ;; Настройка company-jedi
  (require 'company)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 1)
  ;; Настройка elpy для использования jedi
  (with-eval-after-load 'elpy-modules
    (add-to-list 'elpy-modules 'elpy-module-yasnippet)
    (add-to-list 'elpy-modules 'elpy-module-sane-defaults)
    (add-to-list 'elpy-modules 'elpy-module-highlight-indentation)
    (add-to-list 'elpy-modules 'elpy-module-flymake)
    (add-to-list 'elpy-modules 'elpy-module-company)
    (add-to-list 'elpy-modules 'elpy-module-eldoc))
  
  ;; Flycheck setup
  ;; (add-hook 'python-mode-hook 'flycheck-mode)
  (setq flycheck-python-pylint-executable "pylint")  ; Убедитесь, что pylint установлен
  (setq flycheck-flake8-executable "flake8")        ; Убедитесь, что flake8 установлен
  ;; Настройка flycheck-mypy
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-mypy-setup))
  (setq flycheck-mypy-executable "mypy")  ; Убедитесь, что mypy установлен
  (setq flycheck-mypy-follow-imports "silent")  ; По желанию: настройка обработки импортов
  (setq elpy-formatter 'autopep8)

  (add-hook 'python-mode-hook 'elpy-mode)
)

(setq inhibit-startup-message t) ;; hide the startup message
;; init.el ends here

;; Marco configuration
(require 'py-autopep8)
(require 'web-mode)
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(doom-modeline-mode)
(ido-mode t)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; Keys with C-c means for coding
(global-set-key (kbd "C-c i") 'run-python-ide)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-branch-and-checkout)
(global-set-key (kbd "C-c f") 'elpy-find-file)
(global-set-key (kbd "C-c x") 'elpy-rgrep-symbol)
(global-set-key (kbd "C-c C-g") 'grep-find)
(global-set-key (kbd "M-RET") 'elpy-goto-definition)
;; next window
(global-set-key (kbd "C-c n") 'ace-window)
(global-set-key (kbd "C-c <right>") 'ace-window)

(require 'vdiff-magit)
(define-key magit-mode-map "e" 'vdiff-magit-dwim)
(define-key magit-mode-map "E" 'vdiff-magit)
(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

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

(if (file-exists-p "~/emacs/.custom.el") (load-file "~/emacs/.custom.el"))

(setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))

(load-theme 'doom-Iosvkem :no-confirm)
(when (not window-system)
  (set-face-background 'default "unspecified-bg")
  )
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode 1)

(put 'downcase-region 'disabled nil)


;; Load yasnippet
(yas-global-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "350fef8767e45b0f81dd54c986ee6854857f27067bac88d2b1c2a6fa7fecb522"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     default))
 '(find-grep-options "-q")
 '(grep-find-ignored-directories
   '("build" "dist" ".idea" "SCCS" "RCS" "CVS" "MCVS" ".src" ".svn"
     ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (pg :vc-backend Git :url "https://github.com/emarsden/pg-el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
