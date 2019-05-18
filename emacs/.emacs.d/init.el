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
    (clang-format ggtags writegood-mode org cmake-ide rtags auto-complete-c-headers company flycheck flycheck-clang-analyzer company-c-headers company-rtags flycheck-rtags auto-complete flycheck-apertium))))
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

;; Set up gdb to use the many-windows functionality
(setq gdb-many-windows t)

;; Ensure that copying from another program and then running a kill
;; command in emacs doesn't cause things to disappear from the
;; clipboard
(setq save-interprogram-paste-before-kill t)

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files
(setq load-prefer-newer t)

;; Help writing correct text. TODO: Figure out how to make it trigger
;; only in "pure text" buffers
(require 'writegood-mode)
(add-hook 'text-mode-hook
	  (lambda ()
	    (flyspell-mode t)
	    (flyspell-buffer)
	    (writegood-mode t)
	    (visual-line-mode t)))

;; Adding a word to the flycheck dictionary causes it to stop showing
;; the rest of the underlined words. The following adds "advice" to
;; run (flycheck-buffer) after saving a word to the dictionary.
(defun flyspell-buffer-after-pdict-save (&rest _)
  (flyspell-buffer))
(advice-add 'ispell-pdict-save :after
	    #'flyspell-buffer-after-pdict-save)

;; Set C-` to correct word using flyspell, and F9 to flyspell the
;; entire buffer. C-F9 to disable flyspell.
(global-set-key (kbd "C-`")
		'flyspell-correct-word-before-point)
(defun flyspell-enable ()
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode t))
  (flyspell-buffer)
  (message "Turned on flyspell-mode"))
(defun flyspell-disable ()
  (interactive)
  (flyspell-mode -1)
  (message "Turned off flyspell-mode"))
(global-set-key (kbd "<f9>") 'flyspell-enable)
(global-set-key (kbd "C-<f9>") 'flyspell-disable)

;; Speed up flyspell by using no messages
(setq-default flyspell-issue-message-flag nil)


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

;; Let emacs learn and set style from a C file
(defun infer-indentation-style ()
  (interactive)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and
  ;; if neither, we use the tab mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil) (setq indent-tabs-mode t)))
  (message "Inferred indentation"))
(defun c-guess-and-set-style ()		; TODO: Check file size and
					; ask for permission if too
					; large, to speed things up
					; for large files.
  (interactive)
  (let
      ((stylename (concat "guessed-style-" (file-name-base))))
    (c-guess-buffer-no-install)
    (c-guess-install stylename)
    (c-set-style stylename)
    (message (concat "Installed and set " stylename))))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq tab-width 4)
	    (infer-indentation-style)
	    ;; (c-guess-and-set-style)
	    ;; ;; Disabled guessing by default, to speed up file
	    ;; ;; opens for large files.
	    ))

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


;; To use clang format for all c c++ and glsl files
;; This is the standard for Blender
(add-hook 'c-mode-hook (lambda () (add-to-list 'before-save-hook 'clang-format-buffer)))
(add-hook 'c++-mode-hook (lambda () (add-to-list 'before-save-hook 'clang-format-buffer)))
(add-hook 'glsl-mode-hook (lambda () (add-to-list 'before-save-hook 'clang-format-buffer)))
