;;; init.el --- Arquivo de inicialização pessoal.
;;; Commentary:
;; Autor: Mauricio Carrara.

;;; Code:

;;###############
;;Basic config
;;###############

;;Definindo impressora PostScript padrão.
(defvar ps-printer-name "//NOTEMAUR/cp1215")

;;Definindo impressora padrão.
(defvar printer-name "//NOTEMAUR/cp1215")

;;Configura fonte
(set-frame-font "Consolas-9")

;;Definir ecoding para UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-16-le) ;;utf-16-le para copiar do clipboard no Windows, se não utf-8.

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

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))
(global-set-key (kbd "C-c m") 'elfeed-play-with-mpv)

(defun er-youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))
(global-set-key (kbd "C-c y") #'er-youtube)

;;###############
;;Secure 3rd party download packages
;;###############

;;TODO
;;https://glyph.twistedmatrix.com/2015/11/editor-malware.html

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

;;Pré visualização de arquivos.
(use-package peep-dired
  :ensure t
  :defer t
  :bind (:map dired-mode-map
	      ("P" . peep-dired))
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4")))

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

(use-package google-maps
  :ensure t)

;;; init.el ends here.
