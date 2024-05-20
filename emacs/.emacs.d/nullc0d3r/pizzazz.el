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
;; * Camera shake <https://www.youtube.com/watch?v=tu-Qe66AvtY>
;;
;; * <https://github.com/codeinthedark/awesome-power-mode>
;;
;; It seems to be called `power mode` in other editors.
;;
;;; Code:

(defgroup pizzazz nil
  "Pizzazz for emacs"
  :group 'convenience)

(defvar pizzazz-shake-max-amplitude 20.0
  "Maximum amplitude at trauma 1.0 for screenshake in number of pixels")

(defvar pizzazz-shake-trauma-increase 0.3
  "Amount to increase trauma by for screenshake effect")

(defvar pizzazz-shake-time 1.0
  "Amount of time in seconds for screenshake")

(defvar pizzazz--shake-trauma 0.0
  "Current trauma for screenshake, expected to be between 0 and 1
but can go beyond 1")

(defvar pizzazz--shake-timer nil
  "Active screenshake timer. Is `nil` if no screenshake active,
otherwise set to timer running screenshake")

(defun random-between-zero-and-one ()
  "Get a random float between 0.0 and 1.0"
  (/ (random most-positive-fixnum) (float most-positive-fixnum)))

(defun pizzazz--buffer (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (message "TODO")))
(defun calc-screenshake-offset (max_offset shake)
  "Calculate the screenshake offset."
  ;; ideally would use perlin noise but that is not available, so not
  ;; using, it would help make it time dependent but here it doesn't
  ;; matter since there is no slowmo
  (* max_offset shake (random-between-zero-and-one)))

(defun pizzazz-mode--shake-frame (frame)
  "Shake the given frame or reset the timer if no more trauma exists"
  (if (> pizzazz--shake-trauma 0.0)
      (progn
	(let* ((shake (* pizzazz--shake-trauma pizzazz--shake-trauma))
	       (left-offset (truncate (calc-screenshake-offset pizzazz-shake-max-amplitude shake)))
	       (top-offset (truncate (calc-screenshake-offset pizzazz-shake-max-amplitude shake))))
	  (modify-frame-parameters frame
				   '((left . left-offset)
				     (top . top-offset)))
	  (setq pizzazz--shake-trauma (- pizzazz--shake-trauma (/ pizzazz-shake-time pizzazz-shake-max-amplitude)))))
    ;; move frame back to 0 0
    (modify-frame-parameters frame '((left . 0)
				     (top . 0)))
    ;; reset timer
    (cancel-timer pizzazz--shake-timer)
    (setq pizzazz--shake-timer nil)
    ;; ensure trauma is set back to 0.0, don't want it to be negative
    (setq pizzazz--shake-trauma 0.0)))


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
