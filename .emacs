;;; init.el — Modern Emacs for Python Development (2025)

;; ==============================
;; 1. НАСТРОЙКА ПАКЕТОВ — ОБЯЗАТЕЛЬНО В НАЧАЛЕ!
;; ==============================
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Отключаем проверку подписей — если есть SSL-проблемы
(setq package-check-signature nil)

;; Убираем дублирование package-initialize — оставляем ТОЛЬКО ОДИН раз!
;; Если ты видишь предупреждение — значит, он вызывается где-то ещё.
;; Проверь: M-x find-file ~/.emacs.d/custom.el и удали там (package-initialize)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq redisplay-dont-pause t)
(setq idle-update-delay 0.016)

(add-to-list 'exec-path (expand-file-name "~/.emacs.d/pytools/bin"))
;; ==============================
;; 2. ТЕМА, ШРИФТ, КЛАВИШИ
;; ==============================

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono-9.5")
(setq default-frame-alist '((font . "FiraCode Nerd Font Mono-9.5")))

;; Установка иконок (обязательно для doom-modeline)
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; Установка doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-hud t)  ; подсказки при наведении
  :config
  (when (display-graphic-p)
    (setq doom-modeline-icon t)))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t))

;; =============================
;; magit + pgmacs
;; =============================

(use-package magit
  :ensure t)

(unless (package-installed-p 'pg)
   (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
   (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

(require 'pgmacs)
;; ==============================
;; 3. PYTHON + EGLot (СОВРЕМЕННЫЙ СПОСОБ!)
;; ==============================

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . (lambda ()
                         (setq python-indent-offset 4))))

;; --- Твоя функция поиска venv ---
(defun my/find-project-venv-dir ()
  "Find the full path to 'venv' or '.venv' directory in parent directories."
  (let ((venv (locate-dominating-file default-directory "venv"))
        (.venv (locate-dominating-file default-directory ".venv")))
    (cond
     (venv (expand-file-name "venv" venv))
     (.venv (expand-file-name ".venv" .venv))
     (t nil))))

(defun my/set-python-venv ()
  "Set python-shell-interpreter to project venv if found."
  (let ((venv-full-path (my/find-project-venv-dir)))
    (if venv-full-path
        (progn
          (setq-local python-shell-interpreter (expand-file-name "bin/python3" venv-full-path))
          (message "✅ Python interpreter set to: %s" python-shell-interpreter))
      (message "⚠️ No Python venv or .venv found in project."))))

(add-hook 'python-mode-hook 'my/set-python-venv)

;; --- Eglot: настройка сервера ---
;; Путь к общей venv с pyright и debugpy
(defvar my/python-tools-venv (expand-file-name "~/.emacs.d/pytools")
  "Shared virtual environment for LSP and debug tools.")

;; Убедись, что pyright установлен в этой venv:
;; ~/.emacs.d/pytools/bin/pip install pyright

(defun my/eglot-python-config (_server)
    "Return workspace config with PROJECT'S venv."
    (let ((project-venv (my/find-project-venv-dir)))
      (when project-venv
        `(:python (:pythonPath ,(expand-file-name "bin/python" project-venv)
                  :venvPath ,(file-name-directory project-venv)
                  :venv ,(file-name-nondirectory project-venv))))))

(use-package eglot
  :ensure t  ; не обязательно в Emacs 29+, но безопасно
  :hook (python-mode . my/setup-eglot-with-project-venv)
  :config
  ;; Регистрируем pyright из общей venv как сервер для Python
  (add-to-list 'eglot-server-programs
               `(python-mode .
			     ("pyright-langserver" "--stdio")))

  (setq-default eglot-workspace-configuration #'my/eglot-python-config)

  ;; Общие настройки eglot
  (setq eglot-events-buffer-size 0) ; меньше логов

  ;; Форматирование при сохранении
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format nil t)))

  ;; Горячая клавиша переименования
  (define-key eglot-mode-map (kbd "C-;") 'eglot-rename))

(defun my/setup-eglot-with-project-venv ()
  "Start eglot with project venv configured for Pyright."
  (interactive)
  ;; Находим проектный venv (venv или .venv)
  (let ((project-venv (my/find-project-venv-dir)))
    (when project-venv
      ;; Передаём Pyright путь к интерпретатору проекта
      (setq-local eglot-workspace-configuration
                  `(:python (:pythonPath ,(expand-file-name "bin/python" project-venv)
					:venvPath ,(file-name-directory project-venv)
					:venv ,(file-name-nondirectory project-venv)))))
    ;; Запускаем eglot
    (eglot-ensure)))



;; --- UI: подсказки, sideline и т.д. ---
;; Corfu — современная замена company
(use-package corfu
  :ensure t
  :hook (eglot-managed-mode . corfu-mode)  ; включать только при управлении LSP
  :init
  ;; Глобально можно не включать — лучше по контексту
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :custom
  (
  (corfu-auto t)                ; автоматическое всплывание
  (corfu-separator ?\s)         ; разделитель в списке
  (corfu-quit-at-boundary t)    ; выход при достижении границы
  (corfu-preselect-first t)     ; выделять первый вариант
  (corfu-cycle t)               ; зацикливать список
  (corfu-icons-enabled t)
  (corfu-popupinfo-delay 0.3)
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-max-height 20)
  ))


(use-package vertico
  :ensure t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Eglot использует встроенные механизмы Emacs: `eldoc`, `xref`, `imenu`
;; Для всплывающих подсказок можно использовать `eglot-ui` или `lsp-ui` (но lsp-ui не нужен!)

;; Простая замена lsp-ui-doc:
(use-package eldoc
  :hook (eglot-managed-mode . eldoc-mode)
  :config
  (setq eldoc-echo-area-use-multiline-p t))

;; Для sideline-диагностики (как в lsp-ui):
(use-package flymake
  :hook (python-mode . flymake-mode)
  :config
  ;; Eglot автоматически интегрируется с flymake
  (setq flymake-wrap-around nil))

;; --- DAP (отладка) — остаётся без изменений! ---
(use-package dap-mode
  :ensure t
  :commands (dap-debug dap-stop dap-continue dap-step-in dap-step-out dap-next)
  :config
  (dap-mode 1))

(require 'dap-python)
(setq dap-python-debugger 'debugpy)

;; Убедись, что debugpy установлен в той же venv:
;; ~/.emacs.d/pytools/bin/pip install debugpy

(dap-register-debug-template
 "My App"
 (list :type "python"
       :request "launch"
       :args "-i"
       :cwd "${workspaceFolder}"
       :env '(("DEBUG" . "1") (PYTHONPATH . "${workspaceFolder}"))
       :program "${file}"
       :console "integratedTerminal"
       :name "Python File"))

;; Горячие клавиши отладки — без изменений
(global-set-key (kbd "C-c d b") 'dap-breakpoint-toggle)
(global-set-key (kbd "C-c d r") 'dap-debug)
(global-set-key (kbd "C-c d s") 'dap-next)
(global-set-key (kbd "C-c d i") 'dap-step-in)
(global-set-key (kbd "C-c d o") 'dap-step-out)
(global-set-key (kbd "C-c d c") 'dap-continue)
(global-set-key (kbd "C-c d q") 'dap-disconnect)
(global-set-key (kbd "C-c d l") 'dap-breakpoints-list)

;; --- Навигация: переход к определению ---
;; Eglot интегрируется с `xref` — стандартным механизмом Emacs

(defun eglot-goto-definition ()
  "Перейти к определению символа под курсором."
  (interactive)
  (xref-find-definitions (thing-at-point 'symbol)))

(defun eglot-go-back ()
  "Вернуться назад после перехода."
  (interactive)
  (xref-pop-marker-stack))

(global-set-key (kbd "M-g d") 'eglot-goto-definition)
(global-set-key (kbd "M-g r") 'eglot-go-back)

;; ==============================
;; 6. PROJECTILE + IVY — ВЫБОР ПРОЕКТА
;; ==============================

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file))
  :config
  (setq projectile-project-search-path '("~/projects"))
  (setq projectile-indexing-method 'native)
  (setq projectile-completion-system 'ivy)

  ;; 🚫 Скрываем временные файлы и папки в projectile
  (setq projectile-globally-ignored-files
	'("#$" "~$" "\\.pyc$" "\\.swp$" "\\.DS_Store$" "TAGS$" "tags$"))

  (setq projectile-globally-ignored-directories
	'(".git" ".svn" ".hg" "CVS" "__pycache__" "venv" ".venv" "node_modules" "dist" "build" ".mypy_cache" ".pytest_cache"))

  (setq projectile-enable-caching 'persistent)
  (setq projectile-cache-file ".projectile.cache"))

(use-package ivy
  :ensure t
  :init (ivy-mode +1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)

  ;; RET на папке = зайти внутрь, не открывать Dired
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  ;; C-j = создать файл/папку
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package avy
  :ensure t
  :bind (("M-g g" . avy-goto-line)
	 ("M-g c" . avy-goto-char)))

;; Фильтр для скрытия файлов
(defun my/ignore-files (str)
  "Ignore venv, tmp, backup files."
  (not (string-match-p "\\(^[.#].*\\|~$\\|__pycache__$\\|\\.py[co]$\\|\\.swp$\\)" str)))

;; Применяем фильтр к find-file
(add-to-list 'ivy--display-transformers-alist 'my/ignore-files)

;; ==============================
;; 8. WEB / HTML / JS / CSS / JINJA2
;; ==============================

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode)
  :hook (web-mode . (lambda ()
                      (setq web-mode-engines-alist '(("django" . "\\.html$")))
                      (setq web-mode-markup-indent-offset 2)
                      (setq web-mode-css-indent-offset 2)
                      (setq web-mode-code-indent-offset 2)))
  :config
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode)))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . (lambda () (setq js2-basic-offset 2))))

(use-package css-mode
  :ensure t
  :mode ("\\.css\\'" . css-mode))

;; ==============================
;; 9. ЗАПУСК PYTHON / UVICORN
;; ==============================

(defun run-python-script ()
  "Запустить текущий Python-файл."
  (interactive)
  (if (buffer-modified-p) (save-buffer))
  (let ((cmd (format "python3 %s" (buffer-file-name))))
    (compile cmd)))

(global-set-key (kbd "C-c r") 'run-python-script)

(defun run-uvicorn ()
  "Запустить uvicorn на текущем файле (FastAPI)."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Сохраните файл перед запуском!")
      (let ((dir (file-name-directory file))
            (name (file-name-nondirectory file)))
        (compile (format "cd %s && uvicorn %s:app --reload" dir name))))))

(global-set-key (kbd "C-c u") 'run-uvicorn)


;; ==============================
;; ai
;; =============================

(use-package emigo
  :straight (:host github :repo "MatthewZMD/emigo" :files (:defaults "*.py" "*.el"))
  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  ;; Encourage using OpenRouter with Deepseek
  (emigo-model "openrouter/x-ai/grok-4-fast:free")
  (emigo-base-url "https://openrouter.ai/api/v1")
  (emigo-api-key "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"))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; ==============================
;; 10. ДРУГИЕ НАСТРОЙКИ
;; ==============================

(setq inhibit-startup-message t)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq default-tab-width 4)
(setq auto-save-default t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq make-backup-files nil)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Перемещение строк
(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Удаление лишних пробелов
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons dap-mode doom-modeline doom-themes emigo
		   js2-mode lsp-pyright lsp-ui magit neotree pg pgmacs
		   projectile swiper web-mode))
 '(package-vc-selected-packages
   '((emigo :url "https://github.com/MatthewZMD/emigo.git" :branch "main")
     (pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (pg :vc-backend Git :url "https://github.com/emarsden/pg-el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
