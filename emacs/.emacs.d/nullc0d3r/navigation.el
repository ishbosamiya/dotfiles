;;; navigation.el --- Make navigating emacs easier                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ish Bosamiya

;; Author: Ish Bosamiya <ishbosamiya@gmail.com>
;; Keywords: navigation
;; Version: 0.1.0

;; TODO: license

;;; Commentary:

;; Navigation provides extra functions to navigate emacs more easily.
;;
;; Features:
;; * `goto-line-relative`

;;; Code:

(defvar goto-line-relative-for-buffer nil
  "stores buffer for which `goto-line-relative` is active (if
  active)")

(defvar dln-mode-is-initially-active -1
  "Was `display-line-numbers-mode` active before
  `goto-line-relative` was called")

(defvar initial-dln-type nil
  "`display-line-numbers-number-type` before `goto-line-relative`
  was called")

(defun goto-line-relative--cleanup ()
  ;; It is not necessary the cleanup is called from the buffer that
  ;; created the `goto-line-relative` context, thus must use that
  ;; buffer.
  (with-current-buffer goto-line-relative-for-buffer
    ;; change back to initial config for display-line-numbers-mode
    (display-line-numbers-set-type initial-dln-type)
    ;; turn off display-line-numbers-mode if it was not initially
    ;; active
    (funcall 'display-line-numbers-mode dln-mode-is-initially-active))
  ;; Remove the keymap override
  (setq overriding-terminal-local-map nil)
  (setq goto-line-relative-for-buffer nil))

;; Goto line relative to the current line
(defun goto-line-relative (&optional number-of-lines)
  "\
Goto line relative to the current line.

The user is asked to provide the number of lines through the
minibuffer if `number-of-lines` is not provided.
"
  (interactive)
  ;; TODO: make sure the context for `goto-line-relative` is removed
  ;; when the window changes (move out of minibuffer). See how
  ;; `isearch-mode` handles it.

  ;; Make `navigation-mode-local-map` override all other maps. This is
  ;; done so that even something `C-g` can be overriden.
  (setq overriding-terminal-local-map navigation-mode-local-map)

  (setq goto-line-relative-for-buffer (current-buffer))
  (let* ((dln-mode-is-initially-active-temp (if display-line-numbers-mode
								 t -1))
	 (initial-dln-type-temp display-line-numbers-type))
    (setq dln-mode-is-initially-active dln-mode-is-initially-active-temp)
    (setq initial-dln-type initial-dln-type-temp)
    ;; display line numbers relatively
    (display-line-numbers-relative)
    ;; read number-of-lines unless already provided
    (unless number-of-lines
      (setq number-of-lines (read-number "Lines to skip (+/-): ")))
    (let* ((current-line-number (line-number-at-pos))
	   (jump-to-line (+ current-line-number number-of-lines)))
      (goto-line jump-to-line)))
  (goto-line-relative--cleanup))

(defgroup navigation nil
  "Navigation utils."
  :group 'convenience)

;; create keymap for navigation mode
(defvar navigation-keymap (let ((map (make-sparse-keymap)))
			    (define-key map (kbd "M-g M-g") 'goto-line-relative)
			    map))

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

;; Navigation mode local keymap
(defvar navigation-mode-local-map (let ((map (make-sparse-keymap)))
				    (define-key map (kbd "C-g") 'navigation-abort)
				    map)
  "\
Create local keymap for navigation mode

It is possible to define keys that would override even the major
mode by setting this map in `overriding-terminal-local-map` when
required to make this map take the highest precedence. This does
mean more care must be taken since even `C-g` can be overridden.
")

(defun navigation-abort ()
  (interactive)
  (when goto-line-relative-for-buffer
    (goto-line-relative--cleanup))
  (minibuffer-keyboard-quit))

(provide 'navigation)
;;; navigation.el ends here
