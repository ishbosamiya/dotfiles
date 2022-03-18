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
  "`goto-line-relative`'s initial line number type temporary
  storage.")

(defvar-local display-line-numbers-mode-is-initially-active nil
  "`goto-line-relative`'s display-line-numbers-mode is initially
  active temporary storage.")

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
     (let ((mode-active (if display-line-numbers-mode
			    t nil))
	   (line-number-type display-line-numbers-type))
       (setq initial-line-number-type line-number-type)
       (setq display-line-numbers-mode-is-initially-active mode-active)
       ;; display line numbers relatively
       (display-line-numbers-relative)
       (let
	   ;; Read the number of lines to skip to a number
	   ((lines (string-to-number (read-string "Lines to skip (+/-): "))))
	 (list lines)))))
  (let* ((current-line-number (line-number-at-pos))
	 (jump-to-line (+ current-line-number number-of-lines)))
    (goto-line jump-to-line))
  ;; change back to initial config for display-line-numbers-mode
  (display-line-numbers-set-type initial-line-number-type)
  ;; turn off display-line-numbers-mode if it was not initially
  ;; active
  (unless display-line-numbers-mode-is-initially-active
    (with-current-buffer (current-buffer)
      (funcall 'display-line-numbers-mode -1))))

(defgroup navigation nil
  "Navigation utils."
  :group 'convenience)

;; create keymap for navigation mode
(setq navigation-keymap (make-sparse-keymap))
(define-key navigation-keymap (kbd "M-g M-g") 'goto-line-relative)

;;;###autoload
(define-minor-mode navigation-mode
  "Navigation minor mode to make navigating buffers easier"
  :init-value nil
  :group navigation
  :global nil
  :lighter " Navigation"
  :keymap navigation-keymap)

;; Turn on `navigation-mode` unless it is the mini buffer
(defun navigation--turn-on ()
  "Turn on `navigation-mode'."
  (unless (minibufferp)
    (navigation-mode)))

;;;###autoload
(define-globalized-minor-mode global-navigation-mode
  navigation-mode navigation--turn-on)

(provide 'navigation)
;;; navigation.el ends here
