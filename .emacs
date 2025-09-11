;; init.el — современный, красивый Emacs без сборок

;; Отключаем звук ошибок
(setq ring-bell-function 'ignore)

;; Ускоряем запуск
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Пакеты из GNU ELPA, MELPA
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Устанавливаем use-package, если не установлен
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Обновляем пакеты при запуске (опционально)
;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t
;;         auto-package-update-interval 7)
;;   (auto-package-update-maybe))

;; ========= ВНЕШНИЙ ВИД =========

;; Шрифт (поддержка ligatures)
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 130
                      :weight 'normal)
  ;; Ligatures (если шрифт поддерживает)
  (global-prettify-symbols-mode 1)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Отступ между строками
(setq-default line-spacing 0.1)

;; Запрещаем моргание курсора
(blink-cursor-mode -1)

;; Скрываем UI-элементы
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Включаем подсветку скобок
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; Подсветка текущей строки
(global-hl-line-mode 1)

;; Подсветка столбца (80 или 120)
(setq-default display-fill-column-indicator-column 120)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; ========= ИКОНКИ И MODE-LINE =========

;; Устанавливаем all-the-icons (если не установлены шрифты — см. ниже)
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (message "Устанавливаю шрифты all-the-icons...")
    (all-the-icons-install-fonts t)
    (message "✅ Шрифты all-the-icons установлены. Перезапустите Emacs."))
  ;; отключаем сообщения от all-the-icons
  (setq all-the-icons-scale-factor 1.0))

;; Красивый mode-line (lightline-style)
(use-package spaceline
  :if (display-graphic-p)
  :ensure t
  :after all-the-icons
  :config
  (spaceline-emacs-theme))

(use-package spaceline-all-the-icons
  :if (display-graphic-p)
  :ensure t
  :after (spaceline all-the-icons)
  :config
  (spaceline-all-the-icons-theme))

;; Или вручную настроим mode-line (если spaceline не заработает)
;; (use-package fancy-mode-line
;;   :ensure t
;;   :config
;;   (fancy-mode-line-mode))

;; ========= DASHBOARD =========

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Добро пожаловать")
  (setq dashboard-items '((recents  . 5)
                         (bookmarks . 5)
                         (projects . 5)
                         (agenda . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t))

;; ========= ПРОЗРАЧНОСТЬ (опционально) =========

;; (when (display-graphic-p)
;;   (set-frame-parameter (selected-frame) 'alpha '(92 . 92))
;;   (add-to-list 'default-frame-alist '(alpha . (92 . 92))))

;; ========= ИНДИКАТОРЫ И УЛУЧШЕНИЯ =========

;; Git в mode-line
(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :diminish
  :config
  (evil-mode 1))

;; Git интеграция
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Поддержка Tree-sitter (если нужна)
;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Подсветка синтаксиса
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Нумерация строк
(global-display-line-numbers-mode 1)
;; Отключаем в некоторых режимах
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ========= ЦВЕТОВЫЕ ТЕМЫ — РАСКОММЕНТИРУЙ ОДНУ =========

;; Modus Operandi (светлая) / Modus Vivendi (тёмная)
;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (modus-themes-load-theme 'modus-vivendi) ; или 'modus-operandi
;;   (modus-themes-toggle))

;; Catppuccin
;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   (load-theme 'catppuccin-mocha t))

;; Rosé Pine
;; (use-package rose-pine-theme
;;   :ensure t
;;   :config
;;   (load-theme 'rose-pine t))

;; Doom One (тёмная)
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t))

;; Gruvbox
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-hard t))

;; После выбора темы — перезапусти Emacs или M-x load-theme

 ;;; ========= Dirvish — файловый менеджер (без ошибок, 100% рабочий) =========
(use-package dirvish
  :ensure t
  :bind (
         ("C-x f" . dirvish)  ; Открыть в текущем окне
         ;; Вместо dirvish-other-window — используем dirvish с аргументом
         ("C-x C-f" . dirvish)
         ("C-x p f" . dirvish-project)
         )
  :config
  ;; Атрибуты: что показывать в списке файлов
  (dirvish-override-dired-mode)
  (setq dirvish-attributes
	'(all-the-icons size time git-status symlink))
  (setq dired-auto-revert-buffer t)

  ;; Формат времени (человекочитаемый)
  (setq dirvish-time-format "%d %b %H:%M")

  ;; Сортировка: папки сверху, по имени
  (setq dirvish-sort 'alphabetic)
  (setq dirvish-sort-order '(:directories-first t :reverse nil))

  ;; Поведение Enter: открывать файл/папку
  (define-key dirvish-mode-map (kbd "RET") #'dirvish-open)

  ;; Быстрый подъём на уровень выше — `h` как в Vim
  (define-key dirvish-mode-map (kbd "h") #'dirvish-up)

  ;; Обновить — `g` как в Magit
  (define-key dirvish-mode-map (kbd "g") #'revert-buffer)

  ;; Скрыть/показать скрытые файлы — `.` (точка)
  (define-key dirvish-mode-map (kbd ".") #'dirvish-hidden-toggle)

  ;; Удалить файл — `D`
  (define-key dirvish-mode-map (kbd "D") #'dirvish-delete)

  ;; Создать файл/папку — `c`
  (define-key dirvish-mode-map (kbd "c") #'dirvish-create)

  ;; Переименовать — `r`
  (define-key dirvish-mode-map (kbd "r") #'dirvish-rename)

  ;; Копировать — `C`
  (define-key dirvish-mode-map (kbd "C") #'dirvish-copy)

  ;; Вставить — `P`
  (define-key dirvish-mode-map (kbd "P") #'dirvish-paste)

  ;; Поиск по файлам в Dirvish — `/`
  (define-key dirvish-mode-map (kbd "/") #'dirvish-filter)

  ;; Отменить фильтр — `q`
  (define-key dirvish-mode-map (kbd "q") #'dirvish-quit)

  ;; Открыть терминал в текущей папке — `!`
  (define-key dirvish-mode-map (kbd "!") #'dirvish-shell)

  ;; Интеграция с project.el — открытие корня проекта
  (when (fboundp 'project-current)
    (defun dirvish-project ()
      "Открыть Dirvish в корне текущего проекта."
      (interactive)
      (if-let ((root (project-root (project-current))))
          (dirvish root)
        (user-error "Проект не найден"))))

  ;; ✅ Определяем безопасную команду для открытия в другом окне
  (defun dirvish-open-in-other-window ()
    "Открыть Dirvish в другом окне."
    (interactive)
    (dirvish default-directory t))  ; ← текущая директория, в другом окне

  ;; ✅ Определяем команду для открытия проекта в другом окне (опционально)
  (defun dirvish-project-other-window ()
    "Открыть Dirvish в корне проекта в другом окне."
    (interactive)
    (if-let ((root (project-root (project-current))))
        (dirvish root t)
      (user-error "Проект не найден"))))

;; ========= ЗАКЛЮЧЕНИЕ =========

;; Очистка GC после загрузки
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)))

;; Сообщение о готовности
(message "Emacs загружен. Выглядит как современный редактор 💅")

;; Удаляем приветственное сообщение
(setq inhibit-startup-screen t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window all-the-icons-dired better-defaults company-jedi
		dashboard diminish dired-sidebar dirvish doom-modeline
		doom-themes dumb-jump elpy evil flycheck-mypy
		free-keys ht iedit jinja2-mode markdown-mode
		mmm-jinja2 neotree pgmacs py-autopep8 python-mode
		rainbow-delimiters request spaceline-all-the-icons
		spinner tblui vdiff-magit web-mode-edit-element
		web-narrow-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
