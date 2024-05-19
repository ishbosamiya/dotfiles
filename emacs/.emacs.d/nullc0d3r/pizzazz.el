;;; pizzazz.el --- Add pizzazz to emacs                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ish Bosamiya

;; Author: Ish Bosamiya <ishbosamiya@gmail.com>
;; Keywords: pizzazz
;; Version: 0.1.0

;; TODO: license

;;; Commentary:

;; Add pizzazz to emacs.
;;
;; The sole purpose of this unholy creation is to prove emacs can do
;; everything other editors can (but better). A friend challenged me,
;; so here it is.
;;
;; TODO: need to mention all the pizzazz it adds
;;
;; # References
;;
;; * <https://www.gnu.org/software/emacs/manual/html_node/elisp/Child-Frames.html>
;;
;; * `zone-pgm-jitter` of `zone`
;;
;; * <https://github.com/codeinthedark/awesome-power-mode>
;;
;; It seems to be called `power mode` in other editors.
;;
;;; Code:

(defgroup pizzazz nil
  "Pizzazz for emacs"
  :group 'convenience)

(defun pizzazz--buffer (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (message "TODO")))

;; create keymap for pizzazz mode
(defvar pizzazz-keymap (let ((map (make-sparse-keymap)))
			 map))

;;;###autoload
(define-minor-mode pizzazz-mode
  "Pizzazz minor mode to add pizzazz to this buffer"
  :init-value nil
  :group pizzazz
  :global nil
  :lighter " Pizzazz"
  :keymap pizzazz-keymap
  (pizzazz--buffer))

(provide 'pizzazz)
;;; pizzazz.el ends here
