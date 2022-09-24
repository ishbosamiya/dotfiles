;;; comint-extras.el --- extra functionalities for comint mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ish Bosamiya

;; Author: Ish Bosamiya <ishbosamiya@gmail.com>
;; Keywords: lisp comint compilation
;; Version: 0.1.0

;; TODO: license

;;; Commentary:

;; Comint extras adds additional functionality to comint mode to make
;; life easier.
;;
;; Features:
;;
;; * Switch to compilation-mode for "*compilation*" buffer after
;;   process has exited. It ensures that any other sentinels are also
;;   run. reference:
;;   https://www.eigenbahn.com/2020/05/13/emacs-comint-buffer-auto-close

;;; Code:

(defun compilation-mode-on-exit-sentinel (process output)
  "Process sentinel to set \"*compilation*\" buffer to
compilation-mode once PROCESS dies."
  (unless (process-live-p process)
    (message "comint exited, switching to compilation-mode")
    (let* ((buffer (process-buffer process)))
      (when (string= (buffer-name buffer) "*compilation*")
	(with-current-buffer buffer
	  (compilation-mode))))))

(defvar compilation-mode-on-exit-comint-hook-has-run nil
  "Whether or not `compilation-mode-on-exit-comint-hook' has run
   or not. We need this buffer-local var to prevent the hook from
   running several times, as can happen for example when calling
   `shell'.")

(defun add-compilation-mode-on-exit-sentinel ()
  "Replace current process sentinel with a new sentinel composed
of the current one and
`compilation-mode-on-exit-sentinel'."
  (let* ((buffer (get-buffer "*compilation*")))
    (when buffer
      (let* ((process (get-buffer-process buffer)))
	(if process
	    (let* ((og-sentinel (process-sentinel process))
		   (sentinel-list (-remove #'null
					   (list og-sentinel #'compilation-mode-on-exit-sentinel)))
		   (combined-sentinel
		    (lambda (process line)
		      (--each sentinel-list
			(funcall it process line)))))
	      (setf (process-sentinel process) combined-sentinel))
	  (with-current-buffer buffer
	    (compilation-mode)))))))

(defun async-funcall (function &optional buffer args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.2) nil
               `(lambda ()
                  (with-current-buffer ,buffer ,(cons function args)))))

(defun compilation-mode-on-exit-comint-hook ()
  (unless compilation-mode-on-exit-comint-hook-has-run
    (setq-local compilation-mode-on-exit-comint-hook-has-run t)
    (async-funcall #'add-compilation-mode-on-exit-sentinel (current-buffer))))

(add-hook 'comint-mode-hook #'compilation-mode-on-exit-comint-hook)

(bind-key "C-c C-k" 'kill-compilation comint-mode-map)

(provide 'comint-extras)
;;; comint-extras.el ends here
