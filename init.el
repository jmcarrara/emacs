;;maurício Carrara -  init.el
;;

;;###############
;;Basic config
;;###############

;;Maximized startup
(toggle-frame-maximized)

;;Disable toolbar
(tool-bar-mode -1)

;;Disable scrollbar
(scroll-bar-mode -1)

;;Disable menubar
(menu-bar-mode -1)

;;Desabilita imagens no eww (M-I)
(add-hook 'eww-mode-hook 'eww-toggle-images)

;;Change yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;Show cursor highlighted in prog mode
(add-hook 'prog-mode-hook 'hl-line-mode)

;;Line numbers when in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;Human readable units
(setq-default dired-listing-switches "-lha")

;;Set home directory to open files
(setq default-directory "~/")

;;Disable ring bell
(setq ring-bell-function 'ignore)

;;Disable auto save
(setq auto-save-default nil)

;;Disable auto backup
(setq make-backup-file nil)

;;Enable Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;;Auto close
(add-hook 'prog-mode-hook 'electric-pair-mode)

;;Configura fonte - pode ser encontrada em https://juliamono.netlify.app/download/
(set-frame-font "JuliaMono-9")
      
;;###############
;;ERC configuration
;;###############

;;Configuration and improvements.
(setq erc-server "irc.libera.chat"
      erc-nick "mauricioc"
      ;;erc-user-full-name "Name here"
      erc-track-shorten-start 10 ;;Enlarge channel name space in mode line
      erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#systemcrafters" "#openbsd" "#linux"))
      erc-kill-bufer-on-part t
      erc-auto-query 'bury ;;ERC buffer do not pop on direct messages.
      erc-fill-column 100 ;; Largura da coluna das mensagens
      erc-fill-function 'erc-fill-static ;;Define coluna de nicks de tamanho estático
      erc-fill-static-center 20 ;;Define tamanho da coluna de nicks
      ;;erc-track-exclude '(#emacs") ;;Define canais e usuários que não serão rastreados na modeline.
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY") ;Exclue do rastreamento esses tipos de mesagens do modeline.
      ;;erc-track-exclude-server-buffer t ;;Exclue o rastreamento do buffer de mensagens do servidor conectado do modeline.
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY") ;;Esconde esses tipos de mensagem no buffer.
      )

;;###############
;;Personal functions
;;###############

;;Cursor follow a splited window - Uncle Dave Youtube Channel
(defun split-and-follow-horizontaly ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontaly)

(defun split-and-follow-verticaly ()			 
  (interactive)						   
  (split-window-right)
  (balance-windows)					   
  (other-window 1))					   
(global-set-key (kbd "C-x 3") 'split-and-follow-verticaly)

;;###############
;;Secure 3rd party download packages
;;###############

;;TODO
;;https://glyph.twistedmatrix.com/2015/11/editor-malware.html

;;###############   
;;Melpa - packages  
;;###############   
(setq package-enable-at-startup- nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" default))
 '(package-selected-packages
   '(elfeed-tube-mpv elfeed-goodies elfeed rainbow-delimiters doom-themes emojify erc-hl-nicks expand-region beacon pdf-tools which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;;###############   
;;HTML - Config
;;###############
(defun html-setup ()
  (sgml-electric-tag-pair-mode))

(use-package mhtml-mode
  :hook (mhtml-mode . html-setup)
  :config
 ;; (diminish 'sgml-electric-tag-pair-mode)
  (setq-default sgml-basic-offset 2))

;;###############   
;;CSS - Config
;;###############
(use-package css-mode
  :mode ("\\.css\\'"))

;;Rss - feeds
(use-package elfeed
  :ensure t)

(setq elfeed-feeds
      '("https://lukesmith.xyz/index.xml"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
	"https://unixsheikh.com/feed.rss"))

(use-package elfeed-tube-mpv
  :ensure t
  :config (setq elfeed-tube-mov-follow-mode t)
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv)))
