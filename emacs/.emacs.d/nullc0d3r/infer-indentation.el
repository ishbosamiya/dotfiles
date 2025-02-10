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

;;;###autoload
(define-minor-mode infer-indentation-mode
  "Infer indentation used in the file"
  :init-value nil
  :group indentation
  :global nil
  :lighter " Infer Indentation")

(provide 'infer-indentation)
;;; infer-indentation.el ends here
