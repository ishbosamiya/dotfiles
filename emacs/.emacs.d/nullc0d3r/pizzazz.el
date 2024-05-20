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

(defvar pizzazz-shake-max-amplitude 50.0
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

(defvar pizzazz--shake-frame nil
  "Frame to screenshake")

(defvar pizzazz--shake-frame-parent nil
  "Frame for which `pizzazz--shake-frame` was created")

(defvar pizzazz--dummy-buffer nil
  "Dummy buffer")

(defun random-between-zero-and-one ()
  "Get a random float between 0.0 and 1.0"
  (/ (random most-positive-fixnum) (float most-positive-fixnum)))

(defun calc-screenshake-offset (max_offset shake)
  "Calculate the screenshake offset."
  ;; ideally would use perlin noise but that is not available, so not
  ;; using, it would help make it time dependent but here it doesn't
  ;; matter since there is no slowmo
  (* max_offset shake (random-between-zero-and-one)))

(defun pizzazz-mode--shake-frame ()
  "Shake the given frame or reset the timer if no more trauma exists"
  (if (> pizzazz--shake-trauma 0.0)
      (progn
	(let* ((shake (* pizzazz--shake-trauma pizzazz--shake-trauma))
	       (left-offset (truncate (calc-screenshake-offset pizzazz-shake-max-amplitude shake)))
	       (top-offset (truncate (calc-screenshake-offset pizzazz-shake-max-amplitude shake))))
	  (modify-frame-parameters pizzazz--shake-frame
				   '((left . left-offset)
				     (top . top-offset)))
	  (setq pizzazz--shake-trauma (- pizzazz--shake-trauma (/ pizzazz-shake-time pizzazz-shake-max-amplitude)))))
    ;; move frame back to 0 0
    (modify-frame-parameters pizzazz--shake-frame '((left . 0)
				     (top . 0)))
    ;; reset timer
    (cancel-timer pizzazz--shake-timer)
    (setq pizzazz--shake-timer nil)
    ;; ensure trauma is set back to 0.0, don't want it to be negative
    (setq pizzazz--shake-trauma 0.0))
  (message
   "left: %s top: %s"
   (frame-parameter pizzazz--shake-frame 'left)
   (frame-parameter pizzazz--shake-frame 'top)))

(defun pizzazz-mode--create-dummy-buffer ()
  "Create a dummy buffer that has nothing visibile"
  (let ((buffer (get-buffer-create " *pizzazz-mode-dummy-buffer*")))
    (with-current-buffer buffer
      (setq-local mode-line-format nil
                  header-line-format nil
                  frame-title-format ""
                  truncate-lines t
                  cursor-type nil
                  cursor-in-non-selected-windows nil
                  show-trailing-whitespace nil
                  display-line-numbers nil
                  left-fringe-width nil
                  right-fringe-width nil
                  left-margin-width nil
                  right-margin-width nil
                  fringes-outside-margins 0))
    buffer))

(defun pizzazz-mode--post-self-insert-hook ()
  "Hook into `post-self-insert-hook` for `pizzazz-mode`"
  ;; increment trauma
  (setq pizzazz--shake-trauma (+ pizzazz--shake-trauma pizzazz-shake-trauma-increase))
  (if (> pizzazz--shake-trauma 1.0)
      (setq pizzazz--shake-trauma 1.0))
  (unless pizzazz--shake-timer
    ;; set `pizzazz-mode--shake-frame` to be run every couple
    ;; milliseconds
    (setq pizzazz--shake-timer (run-with-timer 0 0.0416 #'pizzazz-mode--shake-frame))))

(defun pizzazz-mode--init ()
  "Initialize `pizzazz-mode`"
  (message "Initializing `pizzazz-mode` in '%s' buffer" (current-buffer))

  ;; create a dummy buffer
  (unless pizzazz--dummy-buffer
    (setq pizzazz--dummy-buffer (pizzazz-mode--create-dummy-buffer)))

  ;; create new child frame and move all windows to this child frame
  ;;
  ;; without creating a child frame, the frame position cannot be
  ;; modified to add the screenshake effect
  (unless pizzazz--shake-frame
    (let* ((frame (selected-frame))
	   (frame-parameters (copy-alist (frame-parameters frame)))
	   (frame-window-configuration (current-window-configuration)))
      ;; Copy given frame parameters, overriding some of its options.
      (dolist (pair `((parent-id . nil)
                      (window-id . nil)
                      (outer-window-id . nil)
                      (left . 0)
                      (top . 0)
                      (parent-frame . ,frame)))
	(setf (alist-get (car pair) frame-parameters nil t) (cdr pair)))

      ;; TODO: just this much is not enough, there are a bunch of
      ;; issues with it, need to figure out and fix them

      ;; create new frame which will act as the base `frame`, this is
      ;; done so that the frame can be moved around (unlike the base
      ;; `frame` which cannot be
      (let ((new-frame (make-frame frame-parameters)))
	(select-frame-set-input-focus new-frame)
	(setq pizzazz--shake-frame new-frame)
	(setq pizzazz--shake-frame-parent frame))

      ;; set `frame` to known state where it cannot be updated
      (with-selected-frame frame
	;; delete other windows, switch to dummy buffer
	(delete-other-windows)
	(switch-to-buffer pizzazz--dummy-buffer)
	;; don't let other buffers be accessed
	(set-window-dedicated-p (get-buffer-window (current-buffer) frame) t))

      ;; TODO: might need to override some frame parameters to prevent
      ;; some issues of frame contention
      ))

  ;; reset any timer
  (when pizzazz--shake-timer
    (cancel-timer pizzazz--shake-timer)
    (setq pizzazz--shake-timer nil))

  ;; reset trauma
  (setq pizzazz--shake-trauma 0.0)

  ;; add to `post-self-insert-hook` (hook run on insertion of a new
  ;; character)
  (add-hook 'post-self-insert-hook #'pizzazz-mode--post-self-insert-hook))

(defun pizzazz-mode--remove ()
  "Removing `pizzazz-mode`"
  (message "Removing `pizzazz-mode` in '%s' buffer" (current-buffer))

  ;; remove from `post-self-insert-hook`
  (remove-hook 'post-self-insert-hook #'pizzazz-mode--post-self-insert-hook)

  (when pizzazz--shake-frame
    ;; restore parent frame
    (let ((buffer (with-selected-frame pizzazz--shake-frame (current-buffer))))
      (delete-frame pizzazz--shake-frame)
      (with-selected-frame pizzazz--shake-frame-parent
	(set-window-dedicated-p
	 (get-buffer-window (current-buffer) pizzazz--shake-frame-parent) nil)
	(switch-to-buffer buffer)))

    (setq pizzazz--shake-frame nil)
    (setq pizzazz--shake-frame-parent nil))

  ;; remove dummy buffer
  (kill-buffer pizzazz--dummy-buffer)
  (setq pizzazz--dummy-buffer nil)

  ;; reset timer
  (when pizzazz--shake-timer
    (cancel-timer pizzazz--shake-timer)
    (setq pizzazz--shake-timer nil)))

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
  (if pizzazz-mode
      (pizzazz-mode--init)
    (pizzazz-mode--remove)))

(provide 'pizzazz)
;;; pizzazz.el ends here
