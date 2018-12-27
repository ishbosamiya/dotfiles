(package-initialize)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(flyspell-default-dictionary "english")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (auto-complete-c-headers company flycheck flycheck-clang-analyzer company-c-headers company-rtags flycheck-rtags auto-complete cmake-ide rtags flycheck-apertium))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; auto-complete global
;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;; Disable audible bell
(setq ring-bell-function 'ignore)

;; Ensure that copying from another program and then running a kill
;; command in emacs doesn't cause things to disappear from the
;; clipboard
(setq save-interprogram-paste-before-kill t)

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files
(setq load-prefer-newer t)

;; Show whitespaces at the end of the line
(setq-default show-trailing-whitespace t)

;; Smoothen scrolling
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01)

;; Turn on line numbers for all buffers
(global-linum-mode)

;; Be able to move between buffers more easily, using M-up, M-down,
;; M-left, M-right.
(windmove-default-keybindings 'meta)

;; cmake-ide setup
(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)
(setq rtags-path "/home/ish/rtags/bin")
(add-hook 'c-mode-hook 'rtags-start-process-unless-running) ;; starting rdm (rtags daemon)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
(rtags-enable-standard-keybindings) ;; C-c r as rtags shortcut

;; flycheck setup
(when (locate-library "flycheck-apertium")
  (require 'flycheck-apertium)
  (add-hook 'nxml-mode-hook 'flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  "Configure flycheck-rtags for better experience."
  (flycheck-select-checker 'rtags)
  )
(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

;; c++ mode
(c-add-style "c++-style"
	     '("linux"
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 4)
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (statement-case-open . +)))
	       ))
(defun my-c++-mode-hook ()
  (c-set-style "c++-style")
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1)
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers)
  )
