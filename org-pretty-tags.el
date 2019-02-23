;;; org-pretty-tags.el --- Surrogates for tags  -*- lexical-binding: t -*-

;; THIS FILE HAS BEEN GENERATED.  For sustainable program-development
;; edit the literate source file "org-pretty-tags.org".  Find also
;; additional information there.

;; Copyright 2019 Marco Wahl
;; 
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: [2019-01-06]
;; Version: 0.1.2
;; Package-Requires: ((emacs "25"))
;; Keywords: reading, outlines
;; URL: https://gitlab.com/marcowahl/org-pretty-tags
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; - Activate the mode with {M-x org-pretty-tags-mode RET}.
;; - Deactivate the mode with a further {M-x org-pretty-tags-mode RET}.
;;
;; Use {M-x customize-variable RET org-pretty-tags-surrogate-strings RET} to
;; define surrogate strings for tags.  E.g. add the pair "money", "$$$".
;; 
;; If you don't like the predefined surrogates then just delete them.
;; 
;; Use {M-x customize-variable RET org-pretty-tags-surrogate-images RET} to
;; define surrogate images for tags.  The definition of the image is
;; expected to be a path to an image.  E.g. add the pair "org", "<path to
;; org icon>".
;;
;; See also the literate source file.  E.g. see https://gitlab.com/marcowahl/org-pretty-tags.


;;; Code:


(require 'org)
(require 'subr-x) ; for `when-let'
(require 'cl-macs) ; for `cl-assert'

(defvar org-pretty-tags-overlays nil
 "Container for the overlays.")

(defgroup org-pretty-tags nil
  "Options for Org Pretty Tags"
  ;; :tag "Org Pretty Tags"
  :group 'org-tags)

;;;###autoload
(defcustom org-pretty-tags-surrogate-strings
  '(("imp" . "☆") ; important stuff.
    ("idea" . "💡") ; inspiration.
    ("money" . "$$$")
    ("easy" . "₰")
    ("music" . "♬"))
  "List of pretty replacements for tags."
  :type '(alist :key-type string :value-type string)
  :group 'org-pretty-tags)

;;;###autoload
(defcustom org-pretty-tags-surrogate-images
  '()
  "List of pretty image replacements for tags."
  :type '(alist :key-type string :value-type string)
  :group 'org-pretty-tags)

(defcustom org-pretty-tags-mode-lighter
  " pretty tags"
  "Text in the mode line to indicate that the mode is on."
  :type 'string
  :group 'org-pretty-tags)

(defun org-pretty-tags-image-cache ()
  "Return a map from tag to image.
Input is `org-pretty-tags-surrogate-images'."
  (mapcar
   (lambda (x)
     (cons (car x)
           (let* ((px-subtract-from-image-height 5)
                  (img
                   (create-image
                    (cdr x)
                    nil nil
                    :height (- (window-font-height) px-subtract-from-image-height)
                    :ascent 'center)))
             (plist-put (cdr img) :type 'imagemagick)
             img)))
   org-pretty-tags-surrogate-images))

(defvar org-pretty-tags-image-cache
  (org-pretty-tags-image-cache)
  "Cache for the image surrogates.")

(defun org-pretty-tags-update-image-cache ()
  "Fill image-cache with surrogate images."
  (setq org-pretty-tags-image-cache (org-pretty-tags-image-cache)))

(defun org-pretty-tags-delete-overlays ()
  "Delete all pretty tags overlays created."
  (while org-pretty-tags-overlays
    (delete-overlay (pop org-pretty-tags-overlays))))

;; POTENTIAL: make sure only tags are changed.
(defun org-pretty-tags-refresh-overlays-agenda ()
  "Create pretty tags overlays for an org agenda buffer."
  (mapc (lambda (x)
          (org-with-point-at 1
            (progn
              (while (re-search-forward
                      (concat ":\\(" (car x) "\\):") nil t)
                (push (make-overlay (match-beginning 1) (match-end 1))
                      org-pretty-tags-overlays)
                (overlay-put (car org-pretty-tags-overlays) 'display (cdr x))))))
        (append org-pretty-tags-surrogate-strings org-pretty-tags-image-cache)))

(defun org-pretty-tags-refresh-overlays-org-mode ()
  "Create the overlays for the tags for the headlines in the buffer."
  (org-with-point-at 1
    (unless (org-at-heading-p)
      (outline-next-heading))
    (let ((surrogates (append org-pretty-tags-surrogate-strings
                              org-pretty-tags-image-cache)))
      (while (not (eobp))
        (cl-assert
         (org-at-heading-p)
         (concat "program logic error."
                 "  please try to reproduce and fix or file a bug report."))
        (org-match-line org-complex-heading-regexp)
        (if (match-beginning 5)
            (let ((tags-end (match-end 5)))
              (goto-char (1+ (match-beginning 5)))
              (while (re-search-forward
                      (concat "\\(.+?\\):") tags-end t)
                (when-let ((surrogate-cons
                            (assoc (buffer-substring (match-beginning 1)
                                                     (match-end 1))
                                                  surrogates)))
                  (push (make-overlay (match-beginning 1) (match-end 1))
                        org-pretty-tags-overlays)
                  (overlay-put (car org-pretty-tags-overlays)
                               'display (cdr surrogate-cons))))))
        (outline-next-heading)))))

(defun org-pretty-tags-refresh-overlays-buffer ()
  "Overlay tags in current buffer.
The mode of the buffer must be either `org-mode' or `org-agenda-mode'."
  (let ((inhibit-read-only t))
    (cond
     ((derived-mode-p 'org-agenda-mode) (org-pretty-tags-refresh-overlays-agenda))
     ((derived-mode-p 'org-mode) (org-pretty-tags-refresh-overlays-org-mode))
     (t (error "Function does not deal with the current context")))))

(defun org-pretty-tags-refresh-overlays-all-buffers ()
  "Overlay tags in all Org buffers."
  (dolist (buf (buffer-list))
    (set-buffer buf)
    (when (derived-mode-p 'org-mode 'org-agenda-mode)
      (org-pretty-tags-refresh-overlays-buffer))))

;;;###autoload
(define-minor-mode org-pretty-tags-mode
  "Display surrogates for tags."
  :lighter org-pretty-tags-mode-lighter
  :global t
  (cond
   (org-pretty-tags-mode
    (org-pretty-tags-update-image-cache)
    (org-pretty-tags-delete-overlays)
    (org-pretty-tags-refresh-overlays-all-buffers)
    (add-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays-buffer)
    (add-hook 'org-ctrl-c-ctrl-c-final-hook #'org-pretty-tags-refresh-overlays-buffer)
    (add-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-overlays-buffer))
   (t
    (org-pretty-tags-delete-overlays)
    (remove-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays-buffer)
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook #'org-pretty-tags-refresh-overlays-buffer)
    (remove-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-overlays-buffer))))


(provide 'org-pretty-tags)

;;; org-pretty-tags.el ends here
