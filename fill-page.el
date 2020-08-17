;;; fill-page.el --- Fill buffer so you don't see empty lines at the end  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-08-16 18:37:21

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Fill buffer so you don't see empty lines at the end.
;; Keyword: fill page buffer
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/fill-page

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
;; Fill buffer so you don't see empty lines at the end.
;;

;;; Code:

(defgroup fill-page nil
  "Fill buffer so you don't see empty lines at the end."
  :prefix "fill-page-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/fill-page"))

;;; Util

(defun fill-page--first-display-line ()
  "Return the first display line number."
  (save-excursion (move-to-window-line 0) (line-number-at-pos nil t)))

(defun fill-page--last-display-line ()
  "Return the last display line number."
  (save-excursion (move-to-window-line -1) (line-number-at-pos nil t)))

(defun fill-page--recenter-positions (type)
  "Return the recenter position value by TYPE."
  (cl-case type (top '(top)) (middle '(middle)) (bottom '(bottom))))

;;; Core

(defun fill-page--max-window-height ()
  "Get possible window height by line height."
  (save-excursion
    (save-window-excursion
      (goto-char (point-min))
      (recenter 1)
      (- (fill-page--last-display-line) (fill-page--first-display-line)))))

(defun fill-page-fill-p (&optional buffer-or-name)
  "Return non-nil, if the page are already filled.
Return nil, if there are unnecessary lines showing at the end of buffer.

The optional argument BUFFER-OR-NAME must be a string or buffer.  Or else
will use the current buffer instead."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (with-current-buffer buffer-or-name
    (let* ((first-ln (fill-page--first-display-line))
           (last-ln (fill-page--last-display-line))
           (con-h (- last-ln first-ln))
           (win-h (fill-page--max-window-height)))
      (<= win-h con-h))))

;;;###autoload
(defun fill-page (&optional buffer-or-name)
  "Fill the page to BUFFER-OR-NAME.

The optional argument BUFFER-OR-NAME must be a string or buffer.  Or else
will use the current buffer instead."
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (with-current-buffer buffer-or-name
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun fill-page--post-command-hook ()
  "For `fill-page' minor mode hook."
  (unless (fill-page-fill-p) (fill-page)))

;;; Entry

(defun fill-page--enable ()
  "Enable `fill-page' in current buffer."
  (add-hook 'post-command-hook #'fill-page--post-command-hook nil t))

(defun fill-page--disable ()
  "Disable `fill-page' in current buffer."
  (remove-hook 'post-command-hook #'fill-page--post-command-hook t))

;;;###autoload
(define-minor-mode fill-page-mode
  "Minor mode 'fill-page-mode'."
  :lighter " F-Pg"
  :group fill-page
  (if fill-page-mode (fill-page--enable) (fill-page--disable)))

(defun fill-page--turn-on-fill-page-mode ()
  "Turn on the 'fill-page-mode'."
  (fill-page-mode 1))

;;;###autoload
(define-globalized-minor-mode global-fill-page-mode
  fill-page-mode fill-page--turn-on-fill-page-mode
  :require 'fill-page)

(provide 'fill-page)
;;; fill-page.el ends here
