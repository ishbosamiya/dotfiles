;;; navigation.el --- Make navigating emacs easier                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ish Bosamiya

;; Author: Ish Bosamiya <ishbosamiya@gmail.com>
;; Keywords: navigation
;; Version: 0.1.0

;; TODO: license

;;; Commentary:

;; Navigation provides extra functions to navigate emacs more easily.
;;
;; TODO: make it a minor mode.

;;; Code:

(defvar-local initial-line-number-type nil
  "`goto-line-relative`'s current line number type temporary
  storage. Not for external use.")

;; Goto line relative to the current line
;;
;; TODO: make it so that the original line number type (absolute or
;; relative) is reverted to when `C-g` is pressed. See isearch's code
;; to understand how it can be done (the key to handling it seems to
;; be to define a new minor mode with keymap that overrides `C-g`).
(defun goto-line-relative (number-of-lines)
  "Goto line relative to the current line"
  (interactive
   (progn
     (let ((line-number-type display-line-numbers-type))
       (setq initial-line-number-type line-number-type)
       ;; display line numbers relatively
       (display-line-numbers-relative)
       (let
	   ;; Read the number of lines to skip to a number
	   ((lines (string-to-number (read-string "Lines to skip (+/-): "))))
	 (list lines)))))
  (let* ((current-line-number (line-number-at-pos))
	 (jump-to-line (+ current-line-number number-of-lines)))
    (goto-line jump-to-line))
  ;; change back to initial line number type
  (display-line-numbers-set-type initial-line-number-type))

;; Set keyboard shortcut for goto-line-relative
(global-set-key (kbd "M-g M-g") 'goto-line-relative)

(provide 'navigation)
;;; navigation.el ends here
