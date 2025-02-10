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

(defun gcd (a b)
  "Find the greatest common divisor (GCD) of the given to integers."
  (if (= b 0)
      a
    (gcd b (% a b))))

(defun infer-indent-tabs-mode ()
  "Infer and return if `indent-tabs-mode` should be enabled.

# Note

Currently, it infers based on how many lines start with ` ` vs
`\t`. This may change in the future"
  (interactive)
  ;; set `indent-tabs-mode` based on how many lines start with spaces
  ;; and tabs
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    ;; if they are equal, default to spaces
    (>= space-count tab-count)))

(defun infer-tab-width ()
  "Infer and return tab width used in the file.

# Note

Offset is determined by `tab-width` or indent/offsets/levels set
by individual modes like `c-basic-offset`, `js-indent-level`,
etc. Caller must set the tab width returned by this method based
on the major mode."
  (interactive)
  (let ((num-spaces-to-count (make-hash-table)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (line-beginning-position))
              (line-end (line-end-position))
              (line (buffer-substring-no-properties line-start line-end))
              (num-spaces (- (length line) (length (string-trim-left line)))))
          (when num-spaces
            (puthash num-spaces (+ (gethash num-spaces num-spaces-to-count 0) 1) num-spaces-to-count)))
        (forward-line 1)))
    (remhash 0 num-spaces-to-count)
    (message "num-spaces-to-count=%s" num-spaces-to-count))
  (message "TODO: need to implement `infer-tab-width`")
  tab-width)

(defun infer-indentation-style ()
  "Infer and set the indentation style.

This is a collection of calls to required `infer-*` methods."
  (interactive)
  ;; TODO: need to set not just `indent-tabs-mode` but this actually
  ;; depends on what the major mode is and what it uses for
  ;; indentation (possible to use custom indentation function that
  ;; means `indent-tabs-mode` and thus `tab-width` may not even be
  ;; used)
  (setq indent-tabs-mode (infer-indent-tabs-mode))
  (message "Setting `indent-tabs-mode` to `%s`" indent-tabs-mode)
  (setq tab-width (infer-tab-width))
  (message "Setting `tab-width` to `%s`" tab-width))

;;;###autoload
(define-minor-mode infer-indentation-mode
  "Infer indentation used in the file"
  :init-value nil
  :group indentation
  :global nil
  :lighter " Infer Indentation")

(provide 'infer-indentation)
;;; infer-indentation.el ends here
