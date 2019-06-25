;;; flymake-posframe.el --- posframe support for Flymake -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.1
;; Package-requires: ((emacs "26") (posframe "0.3.0"))


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

;;; Code:

(require 'posframe)
(require 'flymake)

(defgroup flymake-posframe nil
  "Group for customize flymake posframe."
  :group 'flymake
  :prefix "flymake-posframe-")

(defcustom flymake-posframe-delay 1
  "Number of seconds before the posframe pops up."
  :group 'flymake-posframe)

(defcustom flymake-posframe-timeout nil
  "Number of seconds to close the posframe."
  :group 'flymake-posframe
  :type 'integer)

(defcustom flymake-posframe-prefix
  (let ((hash (make-hash-table :test 'equal)))
    (puthash ':note "\uf29c" hash)
    (puthash ':warning "\uf06a" hash)
    (puthash ':error "\uf06a" hash)
    (puthash 'eglot-note "\uf29c" hash)
    (puthash 'eglot-warning "\uf06a" hash)
    (puthash 'eglpt-error "\uf06a" hash)
    hash)
  "Prefix hash table for flymake-posframe."
  :group 'flymake-posframe
  :type 'hash-table)

(defcustom flymake-posframe-face
  (let ((hash (make-hash-table :test 'equal)))
    (puthash 'eglot-note 'default hash)
    (puthash 'eglot-warning 'warning hash)
    (puthash 'eglot-error 'error hash)
    (puthash ':note 'default hash)
    (puthash ':warning 'warning hash)
    (puthash ':error 'error hash)
    hash)
  "Faces for error information displayed by flymake-posframe."
  :group 'flymake-posframe
  :type 'hash-table)

(defvar flymake-posframe-hide-posframe-hooks
  '(pre-command-hook post-command-hook focus-out-hook)
  "When one of these event happens, hide posframe buffer.")

(defvar flymake-posframe-buffer "*flymake-posframe-buffer*"
  "Buffer to store linter information.")

(defvar-local flymake-posframe--error-pos 0
  "The current cursor position.")

(defvar-local flymake-posframe--error-line 0
  "The current line number")


(defun flymake-posframe--check-line ()
  "docstring"
  (interactive)
  )

(defun flymake-posframe--get-current-line ()
  "Return the current line number at point."
  (string-to-number (format-mode-line "%l")))


(defun flymake-posframe--get-error (&optional beg end)
  "Get `flymake--diag' between BEG and END, if they are not provided, use `line-beginning-position' and `line-end-position'.

Return a list of errors found between BEG and END.
"
  (let* ((beg (or beg (line-beginning-position)))
         (end (or end (line-end-position)))
         (error-list (flymake--overlays
                      :beg beg
                      :end end)))
    error-list))

(defun flymake-posframe--format-one (err)
  "Format ERR for display."
 (let* ((type (flymake-diagnostic-type err))
           (text (flymake-diagnostic-text err))
           (prefix (gethash type flymake-posframe-prefix))
           (face (gethash type flymake-posframe-face)))
      (propertize (format "%s %s" prefix text) 'face face)))

(defun flymake-posframe--format-info (error-list)
  "Format the information from ERROR-LIST."
  (let* ((err (overlay-get (car error-list) 'flymake-diagnostic))
         (error-list (cdr error-list))
         (out (flymake-posframe--format-one err)))
    (if error-list
        (concat out "\n" (flymake-posframe--format-info error-list))
      out)))

(defun flymake-posframe--write-to-buffer (error-list)
  "Format information of ERROR-LIST and put it into `flymake-posframe-buffer'.  If no `flymake-posframe-buffer', make one."
    (with-current-buffer (get-buffer-create flymake-posframe-buffer)
      (erase-buffer)
      (insert (flymake-posframe--format-info error-list))))

(defun flymake-posframe--show ()
  "Show error information at point."
  (let ((error-list (flymake-posframe--get-error)))
    (when (and (posframe-workable-p)
               error-list
               (null (evil-insert-state-p))
               (null (eq (flymake-posframe--get-current-line)
                         flymake-posframe--error-line)))
      ;; first update output buffer
      (flymake-posframe--write-to-buffer error-list)
      ;; display
      (posframe-show
       flymake-posframe-buffer
       :position (point)
       :timeout flymake-posframe-timeout
       :internal-border-width 1
       :internal-border-color "gray80"
       :left-fringe 1
       :right-fringe 1)
      
      ;; update position info
      (setq-local flymake-posframe--error-line
                  (flymake-posframe--get-current-line))
      (setq-local flymake-posframe--error-pos (point))

      ;; setup remove hook
      (dolist (hook flymake-posframe-hide-posframe-hooks)
        (add-hook hook #'flymake-posframe-hide)))))

;;;###autoload
(defun flymake-posframe-show ()
  "Show error information delaying for `flymake-posframe-delay' second."
  (run-at-time flymake-posframe-delay nil
               #'flymake-posframe--show))

;;;###autoload
(defun flymake-posframe-hide ()
  "Hide error information.

Only need to run once.  Once run, remove itself from the hooks"

  ;; if move cursor, hide posframe
  (unless ((eq (point) flymake-posframe--error-pos))
    (posframe-hide flymake-posframe-buffer)

    (dolist (hook flymake-posframe-hide-posframe-hooks)
      (remove-hook hook #'flymake-posframe-hide))))

;; reset `flymake-posframe--error-line' if move to another line
;; (add-hook 'post-command-hook
;;           (defun flymake-posframe-update-error-line ()
;;               (unless (eq (flymake-posframe--get-current-line)
;;                           flymake-posframe--error-line)
;;                 (setq-local flymake-posframe--error-line 0))))

;;;###autoload
(define-minor-mode flymake-posframe-mode
  "A minor mode to display flymake error message in a posframe."
  :lighter nil
  :group flymake-posframe
  (cond
   (flymake-posframe-mode
    (add-hook 'post-command-hook #'flymake-posframe-show nil 'local))
   (t
    (remove-hook 'post-command-hook #'flymake-posframe-show 'local))))

(provide 'flymake-posframe)
;;; flymake-posframe.el ends here
