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

(defun gcd-of-list (list)
  "Find the greatest common divisor (GCD) of the given to list of integers."
  (--reduce (gcd acc it) list))

(defun infer-indent-tabs-mode ()
  "Infer and return if `indent-tabs-mode` should be enabled.

# Note

Currently, it infers based on how many lines start with ` ` vs
`\t`. This may change in the future"
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
  (let ((num-spaces-to-count (make-hash-table))
        (num-spaces-list (make-list 0 0))
        (gcd nil))
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
    ;; don't want to count 0 spaces
    (remhash 0 num-spaces-to-count)
    ;; make num-spaces-to-count into a list and sort by count
    (maphash (lambda (num-spaces count)
               (push num-spaces num-spaces-list))
             num-spaces-to-count)
    (sort num-spaces-list (lambda (a b)
                            (let ((a-count (gethash a num-spaces-to-count))
                                  (b-count (gethash b num-spaces-to-count)))
                              (if (= a-count b-count)
                                  ;; counts are equal, so pick the
                                  ;; larger number of spaces to pop
                                  ;; first
                                  (> a b)
                                (< a-count b-count)))))
    (message "num-spaces-list=%s" num-spaces-list)
    ;; find the gcd (that is not 1) of the most occurring
    (while (and
            (length> num-spaces-list 0)
            (and
             (setq gcd (gcd-of-list num-spaces-list))
             (= gcd 1)))
      (pop num-spaces-list))
    gcd))

(defun infer-and-set-indentation-style ()
  "Infer and set the indentation style.

This is a collection of calls to required `infer-*` methods and
sets the required variables."
  (interactive)
  ;; TODO: need to set not just `indent-tabs-mode` but this actually
  ;; depends on what the major mode is and what it uses for
  ;; indentation (possible to use custom indentation function that
  ;; means `indent-tabs-mode` and thus `tab-width` may not even be
  ;; used)
  (let ((new-indent-tabs-mode (infer-indent-tabs-mode))
        (new-tab-width (infer-tab-width)))
    (message "setting `indent-tabs-mode` to `%s` was `%s`" new-indent-tabs-mode indent-tabs-mode)
    (setq indent-tabs-mode new-indent-tabs-mode)
    (if new-tab-width
        ((message "setting `tab-width` to `%s` was `%s`" new-tab-width tab-width)
         (setq indent-tabs-mode new-indent-tabs-mode))
      (message "WARN: couldn't infer tab width for buffer, not changing"))))

(provide 'infer-indentation)
;;; infer-indentation.el ends here
