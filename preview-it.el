;;; preview-it.el --- Preview anything at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-13 12:46:22

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Preview anything at point.
;; Keyword: preview image path file
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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

(defcustom preview-it-width 300
  "Frame width."
  :type 'integer
  :group 'preview-it)

(defcustom preview-it-height 300
  "Frame height."
  :type 'integer
  :group 'preview-it)

(defconst preview-it--frame-name "*preview-it*"
  "Name of the preview frame.")

(defvar preview-it--frame nil
  "Frame use to display preview buffer.")

(defvar preview-it--timer nil
  "Display timer after hovering.")

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

(defun preview-it--kill-timer (timer)
  "Safe way to kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun preview-it--make-frame ()
  "Create frame if it hasn't created."
  (unless preview-it--frame
    (let* ((after-make-frame-functions nil)
           (before-make-frame-hook nil)
           (buffer (get-buffer preview-it--frame-name))
           (params (append preview-it--frame-parameters
                           `((name . ,preview-it--frame-name)
                             (default-minibuffer-frame . ,(selected-frame))
                             (minibuffer . ,(minibuffer-window))
                             (left-fringe . 0)
                             (right-fringe . 0)
                             (width . ,preview-it-width)
                             (height . ,preview-it-height)
                             (cursor-type . nil)
                             (lsp-ui-doc--no-focus . t)
                             (background-color . ,(face-background 'preview-it-background nil t)))))
           (window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,params))))
           (frame (window-frame window)))
      (setq preview-it--frame (make-frame params))
      (set-window-dedicated-p window t)
      )))

(defun preview-it--move-frame (&optional x y width height)
  "Move the frame to X, Y, WIDTH and HEIGHT position."
  (preview-it--make-frame)
  (let ((abs-pixel-pos (window-absolute-pixel-position))
        (pixel-x x) (pixel-y y))
    (unless pixel-x (setq pixel-x (car abs-pixel-pos)))
    (unless pixel-y (setq pixel-y (+ (cdr abs-pixel-pos) (frame-char-height))))
    (set-frame-parameter preview-it--frame 'left pixel-x)
    (set-frame-parameter preview-it--frame 'top pixel-y)
    (preview-it--frame-visible t)))

(defun preview-it--frame-visible (vis)
  "Make display frame either invisible/visible by VIS."
  (when preview-it--frame
    (if vis (make-frame-visible preview-it--frame)
      (make-frame-invisible preview-it--frame))))

;;; Core

(defun preview-it--get-info ()
  "Return possible preview information."
  (or (ffap-url-at-point) (ffap-file-at-point)))

;;;###autoload
(defun preview-it ()
  "Preview thing at point."
  (interactive)
  (when (framep preview-it--frame)
    (delete-frame preview-it--frame)
    (setq preview-it--frame nil))
  (let ((info (preview-it--get-info)))
    (when info
      (preview-it--move-frame)
      (with-selected-frame preview-it--frame
        (erase-buffer)
        (cond ((file-exists-p info)
               (insert-file-contents info))
              ((url-p info)

               )))
      )))

(defun preview-it--start-preview ()
  "Trigger for ready to preview."
  (preview-it--frame-visible nil)
  (preview-it--kill-timer preview-it--timer)
  (setq preview-it--timer (run-with-timer preview-it-delay nil
                                          #'preview-it)))

;;; Entry

(defun preview-it--enable ()
  "Enable `preview-it-mode'."
  (add-hook 'post-command-hook #'preview-it--start-preview nil t))

(defun preview-it--disable ()
  "Disable `preview-it-mode'."
  (remove-hook 'post-command-hook #'preview-it--start-preview t))

;;;###autoload
(define-minor-mode preview-it-mode
  "Minor mode 'preview-it-mode'."
  :require 'preview-it
  :group 'preview-it
  (if preview-it-mode (preview-it--enable) (preview-it--disable)))

(provide 'preview-it)
;;; preview-it.el ends here
