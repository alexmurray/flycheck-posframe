;;; flycheck-posframe.el --- Show flycheck error messages using posframe.el

;; Copyright (C) 2018 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/flycheck-posframe
;; Version: 0.1
;; Package-Requires: ((flycheck "0.24") (emacs "26") (posframe "0.1.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show flycheck error messages using posframe.el

;;;; Setup

;; (with-eval-after-load 'flycheck
;;    (require 'flycheck-posframe)
;;    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;;; Code:
(require 'flycheck)
(require 'posframe)

(defgroup flycheck-posframe nil
  "Display Flycheck errors in tooltips using posframe.el."
  :prefix "flycheck-posframe-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/alexmurray/flycheck-posframe"))

(defcustom flycheck-posframe-error-prefix "\u27a4 "
  "String to be displayed before every error line in posframe."
  :group 'flycheck-posframe
  :type 'string
  :package-version '(flycheck-posframe . "0.1"))

(defvar flycheck-posframe-buffer "*flycheck-posframe-buffer*"
  "The posframe buffer name use by flycheck-posframe.")

(defvar flycheck-posframe-old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

(defvar flycheck-posframe-delete-posframe-hooks
  '(pre-command-hook post-command-hook focus-out-hook)
  "The hooks which should trigger automatic removal of the posframe.")

(defun flycheck-posframe-delete-posframe ()
  "Delete messages currently being shown if any."
  (posframe-hide flycheck-posframe-buffer)
  (dolist (hook flycheck-posframe-delete-posframe-hooks)
    (remove-hook hook #'flycheck-posframe-delete-posframe t)))

(defun flycheck-posframe-format-errors (errors)
  "Formats ERRORS messages for display."
  (let* ((messages-and-id (mapcar #'flycheck-error-format-message-and-id
                                  (delete-dups errors)))
         (messages (sort
                    (mapcar
                     (lambda (m) (concat flycheck-posframe-error-prefix m))
                     messages-and-id)
                    'string-lessp)))
    (propertize (mapconcat 'identity messages "\n")
                'face
                '(:inherit posframe-face
                           :underline nil
                           :overline nil
                           :strike-through nil
                           :box nil
                           :slant normal
                           :width normal
                           :weight normal))))

(defun flycheck-posframe-show-posframe (errors)
  "Display ERRORS, using posframe.el library."
  (flycheck-posframe-delete-posframe)
  (when errors
    (posframe-show
     flycheck-posframe-buffer
     :string (flycheck-posframe-format-errors errors)
     :position (point))
    (dolist (hook flycheck-posframe-delete-posframe-hooks)
      (add-hook hook #'flycheck-posframe-delete-posframe nil t))))

;;;###autoload
(define-minor-mode flycheck-posframe-mode
  "A minor mode to show Flycheck error messages in a posframe."
  :lighter nil
  :group 'flycheck-posframe
  (cond
   ;; Use our display function and remember the old one but only if we haven't
   ;; yet configured it, to avoid activating twice.
   ((and flycheck-posframe-mode
         (not (eq flycheck-display-errors-function
                  #'flycheck-posframe-show-posframe)))
    (setq-local flycheck-posframe-old-display-function
                flycheck-display-errors-function)
    (setq-local flycheck-display-errors-function
                #'flycheck-posframe-show-posframe))
   ;; Reset the display function and remove ourselves from all hooks but only
   ;; if the mode is still active.
   ((and (not flycheck-posframe-mode)
         (eq flycheck-display-errors-function
             #'flycheck-posframe-show-posframe))
    (setq-local flycheck-display-errors-function
                flycheck-posframe-old-display-function)
    (setq-local flycheck-posframe-old-display-function nil))))

(provide 'flycheck-posframe)
;;; flycheck-posframe.el ends here
