(package-initialize)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(when (< emacs-major-version 27)
  (package-initialize))

;; Remove annoying UI elements.
;;
;; Instead of using (menu-bar-mode -1) and (tool-bar-mode -1) use this
;; trick to improve startup times. Cannot for (scroll-bar-mode -1).
;;
;; reference:
;; https://github.com/raxod502/radian/issues/180#issuecomment-485284949
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
;; (scroll-bar-mode -1)

(eval-when-compile
  (or (require 'use-package nil t)
      (progn
	(package-refresh-contents)
	(package-install 'use-package)
	(message "On a new system. Just installed use-package!"))))

;; Load custom variables from custom_variables.el file
(setq custom-file (locate-user-emacs-file "custom_variables.el"))
(load custom-file)

;; Allow loading customizations from the nullc0d3r directory
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "nullc0d3r/")))

;; Add emacs-nushell to load-path
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "emacs-nushell/")))

;; set utf-8 as default for all
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; backspace is used for delete-region, makes it similar to other programs
(delete-selection-mode 1)

;; Disable audible bell
(setq ring-bell-function 'ignore)

;; Set up gdb to use the many-windows functionality
(setq gdb-many-windows t)

;; Make minibuffer history persist across sessions
(savehist-mode 1)

;; Ensure that copying from another program and then running a kill
;; command in emacs doesn't cause things to disappear from the
;; clipboard
(setq save-interprogram-paste-before-kill t)

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files
(setq load-prefer-newer t)

;; Prevent C-z from accidentally sending the window to background
(global-unset-key (kbd "C-z"))

;; Flyspell: on-the-fly spell checking
(use-package flyspell
  :config
  ;; unbind `C-;` which is bound to
  ;; `flyspell-auto-correct-previous-word` so that `C-;` remains only
  ;; for iedit-mode
  (unbind-key "C-;" flyspell-mode-map))

;; Help writing correct text. TODO: Figure out how to make it trigger
;; only in "pure text" buffers
(use-package writegood-mode
  :ensure t
  :config
  (add-hook 'text-mode-hook
	  (lambda ()
	    (flyspell-mode t)
	    (flyspell-buffer)
	    (writegood-mode t)
	    (visual-line-mode t))))

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
(cl-assert (version<= "26.0.50" emacs-version) t "Require emacs
version >= 26.0.50 for init.el due to display-line-numbers-mode")
(global-display-line-numbers-mode)

(defun display-line-numbers-relative-toggle (&optional buffer)
  "\
Toggle between relative and absolute line numbers in
display-line-numbers-mode.

Turns on display-line-numbers-mode if not already active."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (if (eq display-line-numbers-type 'relative)
	(setq display-line-numbers-type t)
      (setq display-line-numbers-type 'relative))
    (funcall 'display-line-numbers-mode nil)
    (funcall 'display-line-numbers-mode t)))

(defun display-line-numbers-relative (&optional buffer)
  "\
Switch to relative line numbers in display-line-numbers-mode.

Turns on display-line-numbers-mode if not already active."
  (interactive)
  (display-line-numbers-set-type 'relative buffer))

(defun display-line-numbers-absolute (&optional buffer)
  "\
Switch to absolute line numbers in display-line-numbers-mode.

Turns on display-line-numbers-mode if not already active."
  (interactive)
  (display-line-numbers-set-type t buffer))

(defun display-line-numbers-set-type (type &optional buffer)
  "\
Set line number type for display-line-numbers-mode.

Turns on display-line-numbers-mode if not already active."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (setq display-line-numbers-type type)
    (funcall 'display-line-numbers-mode nil)
    (funcall 'display-line-numbers-mode t)))

;; Define shortcuts for display-line-numbers
(global-set-key (kbd "C-c l t") 'display-line-numbers-relative-toggle)

;; Turn on column-number-mode for all buffers
(column-number-mode t)

;; Be able to move between buffers more easily, using M-up, M-down,
;; M-left, M-right.
(use-package windmove
  :config
  (bind-key* "M-<left>" 'windmove-left)
  (bind-key* "M-<right>" 'windmove-right)
  (bind-key* "M-<up>" 'windmove-up)
  (bind-key* "M-<down>" 'windmove-down)
  (windmove-default-keybindings))

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
	    (setq tab-width 2)
	    (infer-indentation-style)
	    ;; (c-guess-and-set-style)
	    ;; ;; Disabled guessing by default, to speed up file
	    ;; ;; opens for large files.
	    ))

;; c++ mode
(c-add-style "c++-style"
	     '("linux"
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 2)
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
(add-hook 'c++-mode-hook (lambda() (setq comment-start "/* " comment-end "*/")))

;; Turn on global auto completion
(use-package company
  :ensure t
  :delight
  :hook (after-init . global-company-mode)
  :bind (("C-<tab>" . company-complete))
  :custom (company-backends '(company-capf company-gtags)))

;; To use clang format for all c and c++ files
;; This is the standard for Blender
(use-package clang-format+
  :ensure t
  :init
  (add-hook 'c-mode-hook #'clang-format+-mode)
  (add-hook 'c++-mode-hook #'clang-format+-mode)
  )

;; To use gtags, must have run `apt install global exuberant-ctags`
;; first
(use-package ggtags
  :ensure t
  :hook ((c-mode c++-mode glsl-mode) . ggtags-mode)
  :custom
  ;; disable "M-s s" in the ggtags navigation map so that it does not
  ;; override the rg-menu binding when ggtags-mode is enabled manually
  (unbind-key "M-s s" 'ggtags-navigation-map))

;; Blender Addon Development Environment
(setq blender-python-launch-path "/media/ish/data/extra/blender-git/blender_emacs/launch_blender.py")
(setq blender-path "/media/ish/data/extra/blender-git/build_master_ninja/bin/blender")
(setq blender-emacs-python-script "/media/ish/data/extra/blender-git/blender_emacs/__init__.py")
(setq blender-addon-source-path "")
(defun blender-start ()
  (interactive)
  (require 'subr-x)
  (start-process "blender" "*blender-output-buffer*" "python3" blender-python-launch-path "--blender"  blender-path "--blender-emacs" blender-emacs-python-script "--source-path" (string-trim-right (read-shell-command "Addon Location: "))))

;; pdf-tools replacement for docview
;;
;; since it doesn't work well with display-line-numbers-mode, it must
;; be disabled
(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install))
(add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))

;; Major mode for editing OpenSCAD code
(use-package scad-mode
  :ensure t
  :defer t)

;; Preview for OpenSCAD code
(use-package scad-preview
  :ensure t
  :defer t)

;; Org mode settings
(setq org-src-fontify-natively t)

;; Rust configuration
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t
        rust-format-show-buffer nil)
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              ;; Prevent rust from hijacking the nice fold-this mode
              (define-key rust-mode-map (kbd "C-c C-f") nil))))
(use-package cargo
  :ensure t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  ;; cargo clippy no longer requires that nightly only
  ;; unstable-options argument for the workaround
  (setq cargo-process--command-clippy "clippy"))

;; Get some distraction free goodness :)
;; Center the buffer
(use-package olivetti
  :ensure t
  :bind ("C-<f11>" . olivetti-mode)
  :config
  (progn
    (setq olivetti-hide-mode-line t)
    (setq-default olivetti-body-width 116)))

;; Be able to unfill paragraphs
(use-package unfill
  :ensure t
  :bind (("M-Q" . unfill-paragraph)))
(put 'upcase-region 'disabled nil)

;; Shows key bindings
(use-package which-key
  :ensure t
  :demand t
  :config (which-key-mode))

;; amx -- newer fork of smex which stopped development in 2015
(use-package amx
  :ensure t
  :demand t
  :bind (; Replace with amx
	 ("M-x" . amx)
	 ("M-X" . amx-major-mode-commands)
	 ; and maintain old M-x via C-c M-x
	 ("C-c M-x" . execute-extended-command)))

(use-package ido
  :ensure t
  :config (ido-mode t))

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode t))

(ido-everywhere t)

(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode t))

;; Use projectile for easily moving around in projects
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; use comint mode for run command to allow user input through the
  ;; mini buffer for the compilation buffer (comint functionality)
  (setq projectile-run-use-comint-mode t))

;; Handle escape sequence colorization properly for compilation-mode
;; See : https://emacs.stackexchange.com/a/38531
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; sourcetrail is no longer available on melpa sadly, the project is
;; abandoned.
;;
;; Allow sourcetrail interaction, jump from sourcetrail to emacs and
;; such.
;; (use-package sourcetrail
;;   :ensure t)

;; Allow multi line editing.
;; Use using C-; when over a symbol
(use-package iedit
  :ensure t
  :config
  (defun iedit-occurrence-context-lines-change (value &optional buffer)
      "Change the value of `iedit-occurrence-context-lines` by the
given value."
    (unless buffer
      (setq buffer (current-buffer)))
    (with-current-buffer buffer
      (setq iedit-occurrence-context-lines (+ iedit-occurrence-context-lines value))
      (if (< iedit-occurrence-context-lines 0)
	  (setq iedit-occurrence-context-lines 0))
      (iedit-show/hide-context-lines)
      (iedit-show/hide-context-lines)
      (message "iedit-occurrence-context-lines set to %s" iedit-occurrence-context-lines)))
  (defun iedit-occurrence-context-lines-increase (&optional buffer)
    "Increase the value of `iedit-occurrence-context-lines` by 1."
    (interactive)
    (funcall 'iedit-occurrence-context-lines-change 1))
  (defun iedit-occurrence-context-lines-decrease (&optional buffer)
    "Decrease the value of `iedit-occurrence-context-lines` by 1."
    (interactive)
    (funcall 'iedit-occurrence-context-lines-change -1))
  (define-key iedit-lib-keymap (kbd "M-[") 'iedit-occurrence-context-lines-increase)
  (define-key iedit-lib-keymap (kbd "M-]") 'iedit-occurrence-context-lines-decrease))

;; yasnippit, needed by lsp
(use-package yasnippet
  :ensure t)

;; flycheck
;;
;; Need this explicitly so that flycheck is used over flymake by
;; lsp-mode
(use-package flycheck
  :ensure t
  :defer 2
  :config
  ;; disable underlining of errors and warnings by default
  (setq flycheck-highlighting-mode nil)
  (defun flycheck-toggle-highlighting-mode (&optional buffer)
    "Toggle `flycheck-highlighting-mode` between no underlining
and underlining symbols.

Note: `flycheck-highlighting-mode` is not buffer local thus the
mode is toggled globally but only the `buffer` (or
`current-buffer`) is actually refreshed after the toggle."
    (interactive)
    (unless buffer
      (setq buffer (current-buffer)))
    (with-current-buffer buffer
      (let ((set-mode-to (if (eq flycheck-highlighting-mode 'nil)
			     'symbols
			   'nil)))
	(setq flycheck-highlighting-mode set-mode-to)
	;; force flycheck to refresh by turning it off and back on
	(funcall 'flycheck-mode nil)
	(funcall 'flycheck-mode t))))
  (define-key flycheck-mode-map (kbd "C-c f t") 'flycheck-toggle-highlighting-mode))

;; Language server using lsp-mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :bind (("C--" . lsp-iedit-highlights)
	 ("C-c l c" . 'lsp-rust-analyzer-cargo-watch-command-toggle))
  :custom
  ;; use cargo check as default, use
  ;; `lsp-rust-analyzer-cargo-watch-command-toggle` to toggle between
  ;; `clippy` and `check`
  (setq lsp-rust-analyzer-cargo-watch-command "check")
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (yas-global-mode t)
  ;; limit the lsp-signature doc lines to 2 instead of 20. This
  ;; reduces how intrusive lsp is while still being useful. If full
  ;; docs are needed, can use `lsp-signature-toggle-full-docs` (M-a).
  (setq lsp-signature-doc-lines 2)
  ;; do not auto activate lsp-signature, use `lsp-signature-activate`
  ;; (C-S-SPC) to activate it and `lsp-signature-stop` (C-g) to
  ;; disable it.
  (setq lsp-signature-auto-activate nil)
  ;; disable annoying lens features
  (setq lsp-lens-enable nil))

(defun lsp--restart-workspace ()
  "Same as `lsp-restart-workspace` but since that is made obsolete,
it is better to have a custom function for this."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp-workspace-restart it)))

(defun lsp-rust-analyzer-cargo-watch-command-toggle ()
  "Toggle between clippy and check for cargo watch command."
  (interactive)
  (if (string-equal lsp-rust-analyzer-cargo-watch-command "clippy")
      (setq lsp-rust-analyzer-cargo-watch-command "check")
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
  (message "lsp-rust-analyzer-cargo-watch-command set to \"%s\"" lsp-rust-analyzer-cargo-watch-command)
  ;; HACK: to show the message for long enough for the user to read
  ;; it, lsp dumps a lot of messages upon lsp-restart-workspace
  (sleep-for 0.7)
  (lsp--restart-workspace))

;; nice lsp ui features
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after (lsp-mode)
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
         ("C-?" . 'lsp-ui-doc-glance)
         ("C-]" . 'lsp-ui-peek-find-references))
  :init
  ;; Make sure lsp prefers flycheck over flymake
  (setq lsp-prefer-flymake nil)
  ;; Disable the semi-annoying hover-to-see-docs view
  (setq lsp-ui-doc-enable nil))

;; lsp mode for python
(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; a fix to make lsp-mode work
;; might need to `list-packages` and install `dash` from `MELPA`
(use-package dash
  :ensure t)

;; fold-this
(use-package fold-this
  :ensure t
  :demand t
  :bind (("C-c C-f" . fold-this-all)
	 ("C-c C-S-f" . fold-this)
	 ("C-c M-f" . fold-this-unfold-at-point)
	 ("C-c M-F" . fold-this-unfold-all)))

;; general: a easy way to setup keybindings
(use-package general
  :ensure t)

;; Debug Adapter Protocol through dap-mode
(use-package dap-mode
  :ensure t
  :defer 5
  :config
  ;; On a new system need to call `dap-gdb-lldb-setup` to setup the
  ;; vscode extension automagically
  (require 'dap-gdb-lldb)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger"))

  ;; rust debug template
  (dap-register-debug-template "rust debug main"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "rust debug main"
				     :gdbpath "rust-gdb"
                                     :target "${workspaceFolder}/target/debug/main"
                                     :cwd nil))

  ;; rust release template
  (dap-register-debug-template "rust release main"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "rust release main"
				     :gdbpath "rust-gdb"
                                     :target "${workspaceFolder}/target/release/main"
                                     :cwd nil)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  ;; Disable Emacs's normal VCS stuff, since I'm never using it, and
  ;; am using only Magit instead.
  (setq vc-handled-backends nil)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-ignored-files nil t)

  (defcustom file-path-to-commit-heading-replace-regexps
    ;; the ordering matters, these are applied in sequence
    '(;; remove `.emacs.d/init.el`
      ("/\\.emacs\\.d/init\\.el" "")
      ;; remove file extension
      ("\\.[^/]+$" "")
      ;; remove `/` at beginning of path
      ("^/" "")
      ;; replace all `/` of the path with `: `
      ("/" ": "))
    "regexps to run on every file path to convert to commit heading"
    :type '(list regexp string))

  (defun file-path-to-commit-heading (file-path)
    "Convert the given file path to a commit heading"
    (interactive "Ffile-path: ")
    (let ((res (seq-reduce
		(lambda (path r)
		  (message "file-path-to-commit-heading: running `\"%s\" \"%s\"` on `%s`"
			   (nth 0 r) (nth 1 r) path)
		  (replace-regexp-in-string (nth 0 r) (nth 1 r) path))
		file-path-to-commit-heading-replace-regexps
		file-path)))
      (message "file-path-to-commit-heading: final heading: `%s`" res)
      res))

  (defun insert-commit-heading-based-on-staged-files ()
    "Insert commit heading(s) based on staged files. This is done by
running `file-path-to-commit-heading` for each staged file. The
user can then quickly delete the entires that are not required."
    (let* ((staged-files (magit-staged-files))
	   (headings (mapcar 'file-path-to-commit-heading staged-files)))
      (mapc (lambda (heading) (insert (format "%s\n" heading))) headings)))

  ;; Add a hook to add a autogenerated commit message.
  (add-hook
   'git-commit-setup-hook
   'insert-commit-heading-based-on-staged-files))

;; Be able to restart emacs from within emacs
(use-package restart-emacs
  :ensure t)

;; Syntax highlighting for cmake files
(use-package cmake-mode
  :ensure t)

;; Be able to open gnome-terminal with some nice keybindings
(setenv "SHELL" "zsh")
(defun open-gnome-terminal-in-directory (dir)
  (interactive "D")
  (let ((dir (expand-file-name dir)))
    ;; use call-process with the destination nil to detach the process
    (call-process "alacritty" nil 0 nil "--working-directory" dir)))

;; Be able to open alacritty with some nice keybindings
(setenv "SHELL" "zsh")
(defun open-alacritty-in-directory (dir)
  (interactive "D")
  (let ((dir (expand-file-name dir)))
    ;; use call-process with the destination nil to detach the process
    (call-process "alacritty" nil 0 nil "--working-directory" dir)))

(defun open-terminal-in-directory (dir)
  "\
Try to open terminal in directory in the preferencial order of
alacritty > gnome-terminal.

TODO: Make it so that the preferencial order is defined by the
user.
"
  (interactive "D")
  (let ((alacritty-exists (call-process-shell-command "command -v alacritty")))
    (if alacritty-exists
	(open-alacritty-in-directory dir)
      (open-gnome-terminal-in-directory dir))))

(global-set-key (kbd "C-x C-t") 'open-terminal-in-directory)

;; Be able to open nautilus with some nice keybindings
(defun open-nautilus-in-directory (dir)
  (interactive "D")
  (let ((dir (expand-file-name dir)))
    (start-process "nautilus" nil "xdg-open" dir)))
(global-set-key (kbd "C-x C-y") 'open-nautilus-in-directory)

(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  )

;; Be able to use rg from emacs
(use-package rg
  :ensure t
  :defer 2
  :config
  (setq rg-executable "rg") ;; Use rg from the $PATH; allows working
			    ;; via TRAMP too!
  (setq rg-default-alias-fallback "everything")
  :bind (("M-s M-s" . 'rg-dwim)
	 ("M-s s"   . 'rg-menu)))

;; Markdown mode stuff
;;
;; Defaulting to github's markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -t html5"))

;; Simple http server
;;
;; Currently used for markdown previewing
(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

;; Allows for creating a live link to the web browser
;;
;; Currently used for markdown previewing
(use-package impatient-mode
  :ensure t
  :commands impatient-mode)

;; Filter for impatient to add minimal github markdown css styling
(defun impatient-github-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

;; Github markdown preview through simple-httpd creating a filter with
;; `impatient-github-markdown-filter` and making it interactive
;; (update on every change to buffer)
;;
;; reference:
;; https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/
(defun github-markdown-preview ()
  "Github markdown preview."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'impatient-github-markdown-filter)
  (imp-visit-buffer))

;; To make navigation easier
;;
;; It is very similar to `move-to-window-line-top-bottom` but this
;; function works in the file line space where as the inbuilt function
;; `move-to-window-line-top-bottom` works in the virtual window line
;; space.
(defun move-cursor-middle-line-of-window ()
  "Move the cursor to the middle line of the window"
  (interactive)
  (let* ((begin (line-number-at-pos (window-start)))
	 (end (line-number-at-pos (window-end)))
	 (middle (/ (+ end begin) 2)))
    (goto-line middle)))

;; Cycle between snake case, camel case, etc.
(use-package string-inflection
  :ensure t
  :bind ("C-c i" . string-inflection-cycle))

;; Comint mode extras
;;
;; Custom defined - See nullc0d3r/comint-extras.el
(use-package comint-extras)

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Make navigating emacs easier
;;
;; Custom defined - See nullc0d3r/navigation.el
(use-package navigation
  :config
  (global-navigation-mode))

;; Set temporary (until next emacs session) buffer local key binding
;;
;; from: https://www.emacswiki.org/emacs/BufferLocalKeys with
;; modification to make amx update properly
(defun buffer-local-set-key (key func)
  "\
Set temporary buffer local key binding.
"
  (interactive "KSet key on this buffer: \naCommand: ")
  (let* ((mode-name (format "%s-magic" (buffer-name)))
         (name (intern mode-name))
         (map-name (format "%s-map" mode-name))
         (map (intern map-name)))
    (unless (boundp map)
      (set map (make-sparse-keymap)))
    (eval
     `(define-minor-mode ,name
        ,(concat
          "Automagically built minor mode to define buffer-local keys.\n"
          "\\{" map-name "}")
        nil " Editing" ,map))
    (eval
     `(define-key ,map ,key ',func))
    (funcall name t)
    (message "created/updated %s with %s (%s)" mode-name func (key-description key)))
  ;; Force update amx. There seems to be a bug in amx, it does not
  ;; rebuild it's cache after buffer-local-set-key is run. It seems to
  ;; do with the `amx-detect-new-commands` which doesn't pick up these
  ;; new commands. Running `amx-update` force updates it.
  (amx-update))

(global-set-key (kbd "C-c l k") 'buffer-local-set-key)

;; Highlight TODO and similar words
(use-package hl-todo
  :ensure t
  :bind (("C-c t p" . hl-todo-previous)
	 ("C-c t n" . hl-todo-next)
	 ("C-c t o" . hl-todo-occur))
  :config
  (setq hl-todo-wrap-movement nil)
  (global-hl-todo-mode))

;; Profile emacs startup
;;
;; Run `M-x esup RET`
(use-package esup
  :ensure t
  :config
  ;; HACK: to make it not bug out, see
  ;; https://github.com/jschaf/esup/issues/54#issuecomment-651247749
  ;;
  ;; the bug: esup tries to step into the byte-compiled version of
  ;; `cl-lib', and fails horribly
  (setq esup-depth 0))

;; Navigation by jumping within the current visible regions.
(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)
	 (:map isearch-mode-map
	       ("M-j" . avy-isearch)))
  :config
  (setq avy-single-candidate-jump nil))

;; Add automated performance mitigations for files with excessively
;; long lines.
(use-package so-long
  :ensure t
  :init (global-so-long-mode t))

;; Use fzf for fuzzy searching files and such.
(use-package fzf
  :ensure t
  :bind (("C-c C-x C-f" . 'fzf-find-file)))

;; Log the commands used in the buffer/globally.
(use-package command-log-mode
  :ensure t
  :defer t)

;; Make the dired file sizes human readable
(setq dired-listing-switches "-alh")

;; Major mode for nix files.
(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-repl
  :commands (nix-repl))

;; Major mode for yaml files.
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;; Set keyboard shortcuts for dictionary related queries.
(bind-key "C-c d s" 'dictionary-search)

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; Window undo-redo
(winner-mode t)

(use-package nushell-mode)
