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
    material-theme))

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
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-t") 'term)
(global-set-key (kbd "C-c C-f") 'elpy-find-file)

;; Marco dired sidebar
(require 'use-package)
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-subtree-line-prefix " .")
  (cond
   ((eq system-type 'darwin)
    (if (display-graphic-p)
        (setq dired-sidebar-theme 'icons)
      (setq dired-sidebar-theme 'nerd))
    (setq dired-sidebar-face '(:family "Helvetica" :height 140)))
   ((eq system-type 'windows-nt)
    (setq dired-sidebar-theme 'nerd)
    (setq dired-sidebar-face '(:family "Lucida Sans Unicode" :height 110)))
   (:default
    (setq dired-sidebar-theme 'nerd)
    (setq dired-sidebar-face '(:family "Arial" :height 140))))

  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)

  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

;; init.el ends heree
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-projectile projectile all-the-icons-dired material-theme elpy better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
