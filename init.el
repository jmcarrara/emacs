;;; init.el --- Arquivo de inicialização pessoal.
;;; Commentary:
;; Autor: Mauricio Carrara.

;;; Code:

;;###############
;;Basic config
;;###############

;;Configura fonte
;(set-frame-font "FiraCode-10")
(set-face-attribute 'default nil :height 150)

;;Maximized startup
(toggle-frame-maximized)

;;Disable toolbar
(tool-bar-mode -1)

;;Disable scrollbar
(scroll-bar-mode -1)

;;Disable menubar
;(menu-bar-mode -1)

;;Change yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;Show cursor highlighted in prog mode
;(add-hook 'prog-mode-hook 'hl-line-mode)

;;Line numbers when in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;Mostra número da coluna na mode line.
(column-number-mode 1)

;;Human readable units
(setq-default dired-listing-switches "-lha")

;;Auto refresh buffers
(global-auto-revert-mode 1)

;;Also auto refresh dired, but be quiet about it
(defvar global-auto-revert-non-file-buffers t)
(defvar auto-revert-verbose nil)

;;Set home directory to open files
(setq default-directory "~/")

;;Disable ring bell
(setq ring-bell-function 'ignore)

;;Disable auto save
(setq auto-save-default nil)

;;Disable auto backup
(setq make-backup-files nil)

;;Enable Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;;Auto close
(add-hook 'prog-mode-hook 'electric-pair-mode)

;;ERC
(defvar erc-server "irc.libera.chat")
(defvar erc-nick "mauricioc")
;;(defvar erc-user-full-name "Name here")
(defvar erc-track-shorten-start 10) ;;Enlarge channel name space in mode line
(defvar erc-autojoin-channels-alist '(("libera.chat"
				       "#emacs"
				       "#systemcrafters"
				       ;;"#openbsd"
				       ;;"#linux"
				       )))
(defvar erc-kill-bufer-on-part t)
(defvar erc-auto-query 'bury) ;;ERC buffer do not pop on direct messages.)
(defvar erc-fill-column 100) ;; Largura da coluna das mensagens)
(defvar erc-fill-function 'erc-fill-static) ;;Define coluna de nicks de tamanho estático)
(defvar erc-fill-static-center 20) ;;Define tamanho da coluna de nicks)
;;(defvar erc-track-exclude '(#emacs"));;Define canais e usuários que não serão rastreados na modeline.)
(defvar erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")) ;Exclue do rastreamento esses tipos de mesagens do modeline.
(defvar erc-track-exclude-server-buffer t) ;;Exclue o rastreamento do buffer de mensagens do servidor conectado do modeline.)
(defvar erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")) ;;Esconde esses tipos de mensagem no buffer.)

;;Scrolling conservatively no ERC
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively)
                 8192)))


;;###############
;;Funções personalizadas.
;;###############

(defun split-and-follow-horizontaly ()
  "O Cursor segue a janela dividida na horizontal."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontaly)

(defun split-and-follow-verticaly ()
  "O Cursor segue a janela dividida na vertical."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-verticaly)


;;###############
;;Melpa - packages
;;###############
(defvar package-enable-at-startup- nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;###############
;;Packages
;;###############

;;Commmands description
(use-package which-key
    :ensure t
    :config
    (which-key-mode 1))

;;Never lose your cursor again
(use-package beacon
    :ensure t
    :config
    (beacon-mode 1))

;;Expand region
(use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))

;;Nicks coloridos no ERC
(use-package erc-hl-nicks
    :ensure t
    :after erc
    :config
    (add-to-list 'erc-modules 'hl-nicks))

;;Emojis em todo lugar
(use-package emojify
    :ensure t
    :hook (after-init . global-emojify-mode))

;;Doom themes
;(use-package doom-themes
;    :ensure t
;    :config
;    ;; Global settings (defaults)
;    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
;    (load-theme 'doom-one t))

;;catppuccin-theme
;(use-package catppuccin-theme
;    :ensure t)
;    (load-theme 'catppuccin :no-confirm)
;    (setq catppuccin-flavor 'frappe) ;; or 'latte, 'macchiato, or 'mocha
;    (catppuccin-reload)

(load-theme 'modus-operandi)

;;Rainbow delimiters
(use-package rainbow-delimiters
    :ensure t
    :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;HTML - Config
(defun html-setup ()
  "Usar eletric pair mode."
  (defvar sgml-electric-tag-pair-mode))

(use-package mhtml-mode
    :hook (mhtml-mode . html-setup)
    :config
    ;; (diminish 'sgml-electric-tag-pair-mode)
    (setq-default sgml-basic-offset 2))

;;CSS - Config
(use-package css-mode
    :mode ("\\.css\\'"))

;;Rss - feeds
(use-package elfeed
    :ensure t)

(setq elfeed-feeds
      '("https://lukesmith.xyz/index.xml"
	))

;;Tag snippet
(use-package emmet-mode
    :ensure t
    :config
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (setq emmet-move-cursor-between-quotes t)) ;; default nil

;;Instalação do SLIME
(use-package slime
    :ensure t)

;;----------------------------------------------------------;;
;; Configuração básica do SLIME
;;----------------------------------------------------------;;

;; Configuração do SLIME para 1 implementação Lisp:
(setq inferior-lisp-program "sbcl")

;; Configuração do SLIME para mais de 1 implementação Lisp:
;(setq slime-lisp-implementations
;      '((cmucl ("/opt/cmucl/bin/cmucl"))
;        (sbcl ("/opt/sbcl/bin/sbcl"))))
;(setq slime-default-lisp 'sbcl)

;; Não altere aqui (só se souber o que está fazendo):
(require 'slime)
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy slime-asdf slime-sprof slime-mdot-fu
                       slime-compiler-notes-tree slime-hyperdoc
                       slime-indentation slime-repl inferior-slime slime-autodoc))
;;(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)
(setq slime-auto-select-connection 'always)
(setq slime-kill-without-query-p t)
(setq slime-description-autofocus t)
(setq slime-fuzzy-explanation "")
(setq slime-asdf-collect-notes t)
(setq slime-inhibit-pipelining nil)
(setq slime-load-failed-fasl 'always)
(setq slime-when-complete-filename-expand t)
(setq slime-repl-history-remove-duplicates t)
(setq slime-repl-history-trim-whitespaces t)
(setq slime-export-symbol-representation-auto t)
(setq slime-highlight-edits-mode t)
(setq lisp-indent-function 'common-lisp-indent-function)
(setq lisp-loop-indent-subclauses nil)
(setq lisp-loop-indent-forms-like-keywords t)
(setq lisp-lambda-list-keyword-parameter-alignment t)

;; Inicia SLIME ao abrir um arquivo .lisp:
;(add-hook 'slime-mode-hook
;          (lambda ()
;            (unless (slime-connected-p)
;              (save-excursion (slime)))))

(use-package company-quickhelp
    :ensure t)

;;----------------------------------------------------------;;
;; Configuração básica do company
;;----------------------------------------------------------;;

(use-package slime-company
    :ensure t)

;; Não altere aqui (só se souber o que está fazendo):
(require 'company)

(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0.7
      company-tooltip-align-annotations t
      company-idle-delay 0
      company-minimum-prefix-length 1)

(global-company-mode)
(push 'slime-company slime-contribs)

(define-key company-active-map (kbd "<up>") 'company-select-previous)
(define-key company-active-map (kbd "<down>") 'company-select-next)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;;Auto complete HTML
(use-package company-web
    :ensure t
    :config
    (global-set-key (kbd "C-'") 'company-web-html)
    (setq company-tooltip-align-annotations 't))

;;Apaga caracteres em branco até o próximo caractere.
(use-package hungry-delete
    :ensure t
    :config (global-hungry-delete-mode))

;;Verificação de sintaxe
(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

;;Icones no dired.
(use-package all-the-icons
    :ensure t
    :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-dired
    :ensure t)

;;Icones no Ibuffer
(use-package all-the-icons-ibuffer
    :ensure t
    :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
    :config (setq  all-the-icons-ibuffer-human-readable-size t))

;;Pesquisas no google
(use-package google-this
    :ensure t
    :config (google-this-mode 1))
;;    (setq google-translate-backend-method 'wget))

(use-package google-translate
    :ensure t)

;;Sytem crafters org-mode improviments

(defun dw/org-mode-setup ()
  "Org-mode setup."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (olivetti-mode 1))
  ;;(setq evil-auto-indent nil))

(use-package org
    :ensure t
    :hook (org-mode . dw/org-mode-setup)
    :config
    (setq org-ellipsis " ▾"
	  org-hide-emphasis-markers t))

(use-package org-bullets
    :ensure t
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(with-eval-after-load 'org-faces
  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch))

;;Selecionar usando shift no org-mode
(setq org-support-shift-select t)

(use-package dired-hide-dotfiles
    :ensure t
    :after dired
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :bind ("C-." . dired-hide-dotfiles-mode))

;;Org export twitter bootstrap
(use-package ox-twbs
    :ensure t)

(setq org-html-validation-link nil)

;;prettify checkbox list org-mode
(add-hook 'org-mode-hook (lambda ()
			   "Beautify Org Checkbox Symbol"
			   (push '("[ ]" .  "☐") prettify-symbols-alist)
			   (push '("[X]" . "☑" ) prettify-symbols-alist)
			   (push '("[-]" . "❍" ) prettify-symbols-alist)
			   (prettify-symbols-mode)))

;;Strike through on checkbox list
(defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked 'org-mode' checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

;;Configurar font fira code
(use-package fira-code-mode
    :ensure t
    :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
    :hook prog-mode)

;;Transmission
(use-package transmission
    :ensure t)

;;Trocar de janela de forma mais fácil.
(use-package switch-window
    :ensure t
    :config
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-minibuffer-shortcut ?z))

(global-set-key (kbd "C-x o") 'switch-window)

;;Olivetti mode
(use-package olivetti
    :ensure t)

;;Configurando corretor ortográfico
(setq ispell-local-dictionary-alist
      '(("brazilian"
	 "[A-ZÁÉÍÓÚÀÈÌÒÙÃÕÇÜÂÊÔa-záéíóúàèìòùãõçüâêôö]"
         "[^A-ZÁÉÍÓÚÀÈÌÒÙÃÕÇÜÂÊÔa-záéíóúàèìòùãõçüâêôö]"
         "['-]"
         nil
         ("-d" "brazilian")
         nil
         utf-8)))

(set-default 'ispell-local-dictionary "brazilian")

;;PHP mode
(use-package php-mode
    :ensure t)

;;Inserção de caracteres em latim
;;(global-set-key (kbd "C-x 8 a") (lambda () (interactive) (insert "α")))
;;Minúsculas Breve
;;ă
(global-set-key (kbd "M-s-a") (lambda()
			      (interactive) (insert-char #x103)))
;;ĕ
(global-set-key (kbd "M-s-e") (lambda()
			      (interactive) (insert-char #x115)))
;;ĭ
(global-set-key (kbd "M-s-i") (lambda()
			      (interactive) (insert-char #x12D)))
;;ŏ
(global-set-key (kbd "M-s-o") (lambda()
			      (interactive) (insert-char #x14F)))
;;ŭ
(global-set-key (kbd "M-s-u") (lambda()
			      (interactive) (insert-char #x16D)))
;;Minúsculas Macron
;;ā
(global-set-key (kbd "C-s-a") (lambda()
			      (interactive) (insert-char #x101)))
;;ē
(global-set-key (kbd "C-s-e") (lambda()
			      (interactive) (insert-char #x113)))
;;ī
(global-set-key (kbd "C-s-i") (lambda()
			      (interactive) (insert-char #x12B)))
;;ō
(global-set-key (kbd "C-s-o") (lambda()
			      (interactive) (insert-char #x14D)))
;;ū
(global-set-key (kbd "C-s-u") (lambda()
				(interactive) (insert-char #x16B)))


;;Maiúsculas Breve
;;Ă
(global-set-key (kbd "M-s-A") (lambda()
				(interactive) (insert-char #x102)))
;;Ĕ
(global-set-key (kbd "M-s-E") (lambda()
				(interactive) (insert-char #x114)))
;;
(global-set-key (kbd "M-s-I") (lambda()
				(interactive) (insert-char #x12C)))
;;Ŏ
(global-set-key (kbd "M-s-O") (lambda()
				(interactive) (insert-char #x14E)))
;;Ŭ
(global-set-key (kbd "M-s-U") (lambda()
				(interactive) (insert-char #x16C)))

;;Maiúsculas Macron
;;Ā
(global-set-key (kbd "C-s-A") (lambda()
				(interactive) (insert-char #x100)))
;;Ē
(global-set-key (kbd "C-s-E") (lambda()
			      (interactive) (insert-char #x112)))
;;Ĭ
(global-set-key (kbd "C-s-I") (lambda()
			      (interactive) (insert-char #x12C)))
;;Ō
(global-set-key (kbd "C-s-O") (lambda()
			      (interactive) (insert-char #x14C)))
;;Ū
(global-set-key (kbd "C-s-U") (lambda()
			      (interactive) (insert-char #x16A)))

;;; init.el ends here.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f5e666fba0ded6ae9be004314ecf5f7feb605cdb84711b5c5ffd81acfb831183" default))
 '(package-selected-packages
   '(catppuccin-theme php-mode ob-php org-mode org-plus-contrib ox-twbs olivetti switch-window company-quickhelp slime-company slime transmission fira-code-mode dired-hide-dotfiles org-bullets google-translate google-this all-the-icons-ibuffer all-the-icons-dired all-the-icons flycheck hungry-delete company-web company emmet-mode elfeed rainbow-delimiters doom-themes emojify erc-hl-nicks expand-region beacon which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
