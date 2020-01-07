;;; flymake-childframe.el --- childframe frontend to display Flymake message -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.2
;; Package-requires: ((emacs "26"))


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

(require 'flymake)

(defgroup flymake-childframe nil
  "Group for customize flymake childframe."
  :group 'flymake
  :prefix "flymake-childframe-")

(defcustom flymake-childframe-delay 1
  "Number of seconds before the childframe pops up."
  :group 'flymake-childframe
  :type 'integer)

(defcustom flymake-childframe-timeout nil
  "Number of seconds to close the childframe."
  :group 'flymake-childframe
  :type 'integer)

(defcustom flymake-childframe-prefix
  '((note . "?")
    (warning . "!")
    (error . "!!"))
  "Prefix to different messages types."
  :group 'flymake-childframe
  :type 'list)

(defcustom flymake-childframe-face
  '((note . default)
    (warning . warning)
    (error . error))
  "Faces for different messages types."
  :group 'flymake-childframe
  :type 'list)

(defcustom flymake-childframe-message-types
  '(((:note eglot-note) . note)
    ((:warning eglot-warning) . warning)
    ((:error eglot-error) . error))
  "Maps of flymake diagnostic types to message types."
  :group 'flymake-childframe
  :type 'list)

(defcustom flymake-childframe-hide-childframe-hooks
  '(pre-command-hook post-command-hook focus-out-hook)
  "When one of these event happens, hide chlidframe buffer."
  :type 'list
  :group 'flymake-childframe)

(defcustom flymake-childframe-show-conditions
  '((null (evil-insert-state-p))
    (null (eq (flymake-childframe--get-current-line)
              flymake-childframe--error-line)))
  "A list of conditions under which `flymake-childframe' should pop error message."
  :type 'list
  :group 'flymake-childframe)

(defconst flymake-childframe--buffer " *flymake-childframe-buffer*"
  "Buffer to store linter information.")

(defvar flymake-childframe--frame nil
  "Frame to display linter information.")

(defvar-local flymake-childframe--error-pos 0
  "The current cursor position.")

(defvar-local flymake-childframe--error-line 0
  "The current line number")

(defconst flymake-childframe--init-parameters
  '((parent-frame . (window-frame))
    (skip-taskbar . t)
    (minibuffer . nil)
    (visibility . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (min-width . 1)
    (min-height . 1)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (undecorated . t)
    (header-line-format . nil)
    (menu-bar-lines . 0)
    (mode-line-format . nil)
    (unsplittable . t))
  "The initial frame parameters for `flymake-childframe--frame'")

(defun flymake-childframe--get-current-line ()
  "Return the current line number at point."
  (string-to-number (format-mode-line "%l")))

(defun flymake-childframe--get-error (&optional beg end)
  "Get `flymake--diag' between BEG and END, if they are not provided, use `line-beginning-position' and `line-end-position'.  Return a list of errors found between BEG and END."
  (let* ((beg (or beg (line-beginning-position)))
         (end (or end (line-end-position)))
         (error-list (flymake--overlays
                      :beg beg
                      :end end)))
    error-list))

(defun flymake-childframe--get-message-type (type property)
  "Get PROPERTY of flymake diagnostic type TYPE.  PROPERTY can be 'face or 'prefix."
  (let ((key (seq-some
              (lambda (cell)
                (when (memq type (car cell))
                  (cdr cell)))
              flymake-childframe-message-types)))
    (alist-get key (symbol-value
                    (intern (format "flymake-childframe-%s" (symbol-name property)))))))

(defun flymake-childframe--format-one (err)
  "Format ERR for display."
  (let* ((type (flymake-diagnostic-type err))
         (text (flymake-diagnostic-text err))
         (prefix (flymake-childframe--get-message-type type 'prefix))
         (face (flymake-childframe--get-message-type type 'face)))
    (propertize (format "%s %s" prefix text) 'face face)))

(defun flymake-childframe--format-info (error-list)
  "Format the information from ERROR-LIST."
  (let* ((err (overlay-get (car error-list) 'flymake-diagnostic))
         (error-list (cdr error-list))
         (out (flymake-childframe--format-one err)))
    (if error-list
        (concat out "\n" (flymake-childframe--format-info error-list))
      out)))

(defun flymake-childframe--set-frame-size (height width)
  "Set `flymake-chldframe--frame' size based on the content in `flymake-childframe--buffer'."
  (with-current-buffer flymake-childframe--buffer
    (let ((current-width (- (line-end-position) (line-beginning-position)))
          new-height new-width)
      (setq new-width (max width current-width)
            new-height (1+ height))
      (if (= (line-number-at-pos (point)) 1)
          `(,(+ new-width 2) 1)
        (line-move -1)
        (flymake-childframe--set-frame-size new-height new-width)))))

(defun flymake-chlidframe--set-frame-position ()
  "Determine frame position."
  (let* ((x (car (window-absolute-pixel-position)))
         (y (cdr (window-absolute-pixel-position)))
         (off-set (- (+ x (frame-pixel-width flymake-childframe--frame))
                     (frame-pixel-width))))
    (if (> off-set 0)
        `(,(- x off-set) ,(+ y (default-font-height)))
      `(,x ,(+ y (default-font-height))))))

(defun flymake-childframe--show-p (error-list)
  "A set of conditions under which flymake-childframe make and show childframe."
  (eval `(and ,@error-list ,@flymake-childframe-show-conditions)))

(defun flymake-childframe--show ()
  "Show error information at point."
  (when-let ((error-list (flymake-childframe--get-error))
             (current-window (selected-window))
             ((flymake-childframe--show-p error-list)))

    ;; First update buffer information
    (with-current-buffer (get-buffer-create flymake-childframe--buffer)
      (erase-buffer)
      (insert (flymake-childframe--format-info error-list))
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil))

    ;; Then create frame if needed
    (unless (frame-live-p flymake-childframe--frame)
      (setq flymake-childframe--frame
            (make-frame flymake-childframe--init-parameters)))

    (with-selected-frame flymake-childframe--frame
      (delete-other-windows)
      (switch-to-buffer flymake-childframe--buffer)
      (when (featurep 'doom-modeline)
        (setq doom-modeline-current-window current-window)))

    ;; move frame to desirable position
    (apply 'set-frame-position
           `(,flymake-childframe--frame ,@(flymake-chlidframe--set-frame-position)))
    (apply 'set-frame-size
           `(,flymake-childframe--frame ,@(flymake-childframe--set-frame-size 0 0)))
    (set-face-background 'internal-border "gray80" flymake-childframe--frame)

    (redirect-frame-focus flymake-childframe--frame
                          (frame-parent flymake-childframe--frame))

    ;; set hooks
    ;; update position info
    (setq-local flymake-childframe--error-line
                (flymake-childframe--get-current-line))
    (setq-local flymake-childframe--error-pos (point))

    ;; setup remove hook
    (dolist (hook flymake-childframe-hide-childframe-hooks)
      (add-hook hook #'flymake-childframe-hide))

    ;; finally show frame
    (make-frame-visible flymake-childframe--frame)))

(defun flymake-childframe-show ()
  "Show error information delaying for `flymake-childframe-delay' second."
  (run-at-time flymake-childframe-delay nil
               #'flymake-childframe--show))

(defun flymake-childframe-hide ()
  "Hide error information.  Only need to run once.  Once run, remove itself from the hooks."
  ;; if move cursor, hide childframe
  (unless (eq (point) flymake-childframe--error-pos)
    (make-frame-invisible flymake-childframe--frame)

    (dolist (hook flymake-childframe-hide-childframe-hooks)
      (remove-hook hook #'flymake-childframe-hide))))

(defun flymake-childframe-reset-error-line ()
  "Reset the line number for current error to 0."
  (unless (eq (flymake-childframe--get-current-line)
              flymake-childframe--error-line)
    (setq-local flymake-childframe--error-line 0)))

;;;###autoload
(define-minor-mode flymake-childframe-mode
  "A minor mode to display flymake error message in a childframe."
  :lighter nil
  :group flymake-childframe
  (cond
   (flymake-childframe-mode (add-hook 'post-command-hook #'flymake-childframe-show nil 'local)
                            (add-hook 'post-command-hook #'flymake-childframe-reset-error-line nil 'local))
   (t (remove-hook 'post-command-hook #'flymake-childframe-show 'local)
      (remove-hook 'post-command-hook #'flymake-childframe-reset-error-line 'local))))

(provide 'flymake-childframe)
;;; flymake-childframe.el ends here
