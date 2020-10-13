;;; preview-it.el --- Preview anything at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-13 12:46:22

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Preview anything at point.
;; Keyword: preview image path file
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jcs-elpa/preview-it

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Preview anything at point.
;;

;;; Code:

(require 'ffap)
(require 'image-mode)

(defgroup preview-it nil
  "Preview anything at point."
  :prefix "preview-it-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/preview-it"))

(defface preview-it-background
  '((((background light)) :background "#B3B3B3")
    (t :background "#272A36"))
  "Background color of the preview buffer."
  :group 'preview-it)

(defcustom preview-it-delay 0.4
  "Seconds delay to show preview."
  :type 'float
  :group 'preview-it)

(defconst preview-it--buffer-name "*preview-it*"
  "Name of the preview buffer.")

(defvar preview-it--image-extensions
  '(".jpg" ".png" ".jpeg" ".gif" ".bmp")
  "List of image extensions.")

(defvar preview-it--frame nil
  "Frame use to display preview buffer.")

(defvar preview-it--timer nil
  "Display timer after hovering.")

(defvar preview-it--max-column nil
  "Record the maximum column, which represent as buffer maximum width.")

(defvar preview-it--max-line nil
  "Record the maximum line, which represent as buffer maximum height.")

(defvar preview-it--frame-parameters
  '((left . -1)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (top . -1)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Frame parameters used to create the frame.")

;;; Util

(defun preview-it--is-contain-list-string-regexp (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some (lambda (lb-sub-str) (string-match-p lb-sub-str in-str)) in-list))

(defun preview-it--kill-timer (timer)
  "Safe way to kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun preview-it--text-file-p (filename)
  "Return non-nil if FILENAME is a text file; not binary file."
  (let ((inhibit-message t)message-log-max)
    (with-current-buffer (find-file-noselect filename :no-warn)
      (prog1 (not (eq buffer-file-coding-system 'no-conversion))
        (kill-buffer)))))

(defun preview-it--max-col ()
  "Return maximum column in buffer."
  (let ((max 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (setq max (max (current-column) max))
        (forward-line 1)))
    max))

(defun preview-it--max-line ()
  "Return maximum line in buffer."
  (line-number-at-pos (point-max) t))

;;; Frame

(defun preview-it--make-frame ()
  "Create frame if it hasn't created."
  (unless preview-it--frame
    (let* ((after-make-frame-functions nil)
           (before-make-frame-hook nil)
           (buffer (get-buffer-create preview-it--buffer-name))
           (params (append preview-it--frame-parameters
                           `((name . "")
                             (default-minibuffer-frame . ,(selected-frame))
                             (minibuffer . ,(minibuffer-window))
                             (left-fringe . 0)
                             (right-fringe . 0)
                             (cursor-type . nil)
                             (background-color . ,(face-background 'preview-it-background nil t)))))
           (window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,params)))))
      (setq preview-it--frame (window-frame window))
      (set-window-dedicated-p window t))))

(defun preview-it--move-frame (&optional x y width height)
  "Move the frame to X, Y, WIDTH and HEIGHT position."
  (preview-it--make-frame)
  (let* ((fcw (frame-char-width)) (fch (frame-char-height))
         (abs-pixel-pos (save-excursion
                          (goto-char (point-min))
                          (window-absolute-pixel-position)))
         (win-left (- (car abs-pixel-pos) fcw))
         (win-top (- (cdr abs-pixel-pos) fch))
         (pixel-x x) (pixel-y y)
         (cur-ln (line-number-at-pos)) (cur-col (current-column))
         (root-frame-width (* fcw (frame-width)))
         (root-frame-height (* fch (frame-height)))
         (vis-frame-width (- root-frame-width (+ win-left (* fcw cur-col))))
         (vis-frame-height (- root-frame-height (+ win-top (* fch cur-ln))))
         display-frame-width display-frame-height
         diff-w diff-h)
    (with-current-buffer preview-it--buffer-name
      (setq preview-it--max-column (preview-it--max-col)
            preview-it--max-line (preview-it--max-line)))
    (if width
        (setq display-frame-width width)
      (setq display-frame-width (* fcw preview-it--max-column))
      (setq width (preview-it--calculated-width)))
    (if height
        (setq display-frame-height height)
      (setq display-frame-height (* fch preview-it--max-line))
      (setq height (preview-it--calculated-height)))
    (unless pixel-x
      (setq cur-col (+ 2 cur-col)
            pixel-x (+ (* fcw cur-col) win-left)))
    (unless pixel-y (setq pixel-y (+ (* fch cur-ln) win-top)))
    ;; Calculate position X
    (when (< vis-frame-width display-frame-width)
      (setq diff-w (- display-frame-width vis-frame-width)
            pixel-x (max (- pixel-x diff-w) 0)))
    ;; Calculate position Y
    (when (< vis-frame-height display-frame-height)
      (setq diff-h (- display-frame-height vis-frame-height)
            pixel-y (max (- pixel-y diff-h) 0)))
    (set-frame-parameter preview-it--frame 'left (round pixel-x))
    (set-frame-parameter preview-it--frame 'top (round pixel-y))
    (set-frame-parameter preview-it--frame 'width (round width))
    (set-frame-parameter preview-it--frame 'height (round height))
    (preview-it--frame-visible t)))

(defun preview-it--frame-visible (vis)
  "Make display frame either invisible/visible by VIS."
  (when preview-it--frame
    (if vis (make-frame-visible preview-it--frame)
      (make-frame-invisible preview-it--frame))))

(defun preview-it--calculated-width ()
  "Calculate window width from current context."
  (min (frame-width) preview-it--max-column))

(defun preview-it--calculated-height ()
  "Calculate window height from current context."
  (min (frame-height) preview-it--max-line))

;;; Core

(defun preview-it--get-info ()
  "Return possible preview information."
  (or (ffap-url-at-point) (ffap-file-at-point)))

;;;###autoload
(defun preview-it ()
  "Preview thing at point."
  (interactive)
  (let ((info (preview-it--get-info)) show-frame-p
        x y width height)
    (when info
      (preview-it--make-frame)
      (with-selected-frame preview-it--frame
        (erase-buffer)
        (setq mode-line-format nil)
        (cond ((jcs-is-file-p info)
               (setq show-frame-p t)
               (cond ((and (preview-it--is-contain-list-string-regexp
                            preview-it--image-extensions info)
                           (ignore-errors (insert-image-file info)))
                      (let ((img-size (ignore-errors (image-size (image-get-display-property) :pixels))))
                        (when img-size
                          (setq width (/ (car img-size) (frame-char-width))
                                height (/ (cdr img-size) (frame-char-height))))))
                     ;; TODO: This method is very slow, need to find other replacement.
                     ((preview-it--text-file-p info)
                      (insert-file-contents info))
                     (t
                      (setq show-frame-p nil))))
              ((url-p info)  ; TODO: The function `url-p' isn't working...
               ;; TODO: Implement url buffer.
               (setq show-frame-p t))))
      (when show-frame-p (preview-it--move-frame x y width height)))))

(defun preview-it--start-preview ()
  "Trigger to start previewing."
  (preview-it--frame-visible nil)
  (preview-it--kill-timer preview-it--timer)
  (setq preview-it--timer (run-with-timer preview-it-delay nil #'preview-it)))

(defun preview-it--stop-preview ()
  "Trigger to stop previewing."
  (preview-it--frame-visible nil))

;;; Entry

(defun preview-it--enable ()
  "Enable 'preview-it-mode'."
  (add-hook 'pre-command-hook #'preview-it--stop-preview nil t)
  (add-hook 'post-command-hook #'preview-it--start-preview nil t))

(defun preview-it--disable ()
  "Disable 'preview-it-mode'."
  (remove-hook 'pre-command-hook #'preview-it--stop-preview t)
  (remove-hook 'post-command-hook #'preview-it--start-preview t)
  (preview-it--stop-preview))

;;;###autoload
(define-minor-mode preview-it-mode
  "Minor mode 'preview-it-mode'."
  :require 'preview-it
  :group 'preview-it
  (if preview-it-mode (preview-it--enable) (preview-it--disable)))

(provide 'preview-it)
;;; preview-it.el ends here
