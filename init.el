;;; init.el --- Arquivo de inicialização pessoal.
;;; Commentary:
;; Autor: Mauricio Carrara.

;;; Code:

;;###############
;;Basic config
;;###############

;;Configura fonte
(set-frame-font "FiraCode-10")

;;Maximized startup
(toggle-frame-maximized)

;;Disable toolbar
(tool-bar-mode -1)

;;Disable scrollbar
(scroll-bar-mode -1)

;;Disable menubar
(menu-bar-mode -1)

;;Change yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;Show cursor highlighted in prog mode
(add-hook 'prog-mode-hook 'hl-line-mode)

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

(defun toogle-font-size ()
  "Alterna entre 2 tamanho de fontes pré determinados."
  (interactive)
  (set-frame-font "Terminus-12"))
(global-set-key (kbd "<f12>") 'toogle-font-size)

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
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

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

;;Auto complete
(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (company-tng-mode)
  (setq company-backends
	'(company-capf
	  company-files
	  company-keywords
	  company-web-html)))

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

(use-package google-translate
  :ensure t)

;;Sytem crafters org-mode impuviments

(defun dw/org-mode-setup ()
  "Org-mode setup."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

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
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;;Selecionar usando shift no org-mode
(setq org-support-shift-select t)

;;Dependências para o Telega
(use-package visual-fill-column
  :ensure t)

(use-package rainbow-identifiers
  :ensure t)

;;Telegram dor Emacs
(use-package telega
  :ensure t
  :load-path  "~/telega.el"
  :commands (telega)
  :defer t)
(setq telega-server-libs-prefix "/usr")

(use-package dired-hide-dotfiles
  :ensure t
  :bind ("C-." . dired-hide-dotfiles-mode))

;;; init.el ends here.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dired-hide-dotfiles telega rainbow-identifiers visual-fill-column visual-fill-colummn all-the-icons-install-fonts t: google-translate google-this all-the-icons-ibuffer all-the-icons-dired all-the-icons flycheck hungry-delete company-web company emmet-mode elfeed rainbow-delimiters doom-themes emojify erc-hl-nicks expand-region beacon which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
