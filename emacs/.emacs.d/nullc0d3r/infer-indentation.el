;;; infer-indentation.el --- Infer indentation used in the file                     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ish Bosamiya

;; Author: Ish Bosamiya <ishbosamiya@gmail.com>
;; Keywords: navigation
;; Version: 0.1.0

;; TODO: license

;;; Commentary:

;; Provides minor mode to infer indentation used in the file.

;;; Code:

(defgroup indentation nil
  "Indentation utils."
  :group 'convenience)

(defun infer-indents-tabs-mode ()
  "Infer to set/unset `indent-tabs-mode`.

# Note

Currently, it infers based on how many lines start with ` ` vs
`\t`. This may change in the future"
  (interactive)
  ;; set `indent-tabs-mode` based on how many lines start with spaces
  ;; and tabs
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    ;; if they are equal, default to spaces
    (if (>= space-count tab-count) (setq indent-tabs-mode nil) (setq indent-tabs-mode t)))
  (message "Setting `indent-tabs-mode` to `%s`" indent-tabs-mode))

(defun infer-tab-width ()
  "Infer and set `tab-width` (or similar like `c-basic-offset`
based on active mode).

# Note

Offset is determined by `tab-width` and offsets/levels set by
individual modes like `c-basic-offset`, `js-indent-level`,
etc. This method attempts to set the correct one based on the
mode that is active."
  (interactive)
  (message "TODO: need to implement `infer-tab-width`"))

(defun infer-indentation-style ()
  "Infer and set the indentation style.

This is a collection of calls to required `infer-*` methods."
  (interactive)
  (infer-indents-tabs-mode)
  (infer-tab-width))

;;;###autoload
(define-minor-mode infer-indentation-mode
  "Infer indentation used in the file"
  :init-value nil
  :group indentation
  :global nil
  :lighter " Infer Indentation")

(provide 'infer-indentation)
;;; infer-indentation.el ends here
