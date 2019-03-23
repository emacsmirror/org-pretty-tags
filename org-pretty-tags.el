;;; org-pretty-tags.el --- Surrogates for tags  -*- lexical-binding: t -*-

;; THIS FILE HAS BEEN GENERATED.  For sustainable program-development
;; edit the literate source file "org-pretty-tags.org".  Find also
;; additional information there.

;; Copyright 2019 Marco Wahl
;; 
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: [2019-01-06]
;; Version: 0.1.5
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

;; In an Org mode buffer:
;; 
;; - Toggle the mode with {M-x org-pretty-tags-mode RET}.
;; - Activate the mode with {C-u M-x org-pretty-tags-mode RET}.
;; - Deactivate the mode with {C-u -1 M-x org-pretty-tags-mode RET}.
;; 
;;
;; - Toggle the global-mode with {M-x org-pretty-tags-global-mode RET}.
;; - Activate the global-mode in every buffer with {C-u M-x org-pretty-tags-global-mode RET}.
;; - Deactivate the global-mode in every buffer with {C-u -1 M-x org-pretty-tags-global-mode RET}.
;; 
;; Refresh agenda buffers (key =g= or =r=) to follow the latest setting
;; of pretty tags in the buffers.
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
(require 'cl-lib) ; for `cl-assert'


;; customizable items

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

;;;###autoload
(defcustom org-pretty-tags-mode-lighter
  " pretty-tags"
  "Text in the mode line to indicate that the mode is on."
  :type 'string
  :group 'org-pretty-tags)


;; buffer local variables

(defvar-local org-pretty-tags-overlays nil
 "Container for the overlays.")


;; auxilliaries

(defun org-pretty-tags-goto-next-visible-agenda-item ()
  "Move point to the eol of the next visible agenda item or else eob."
  (while (progn
           (goto-char (or (next-single-property-change (point) 'org-marker)
                          (point-max)))
           (end-of-line)
           (and (get-char-property (point) 'invisible) (not (eobp))))))

(defun org-pretty-tags-mode-off-in-every-buffer-p ()
  "t if `org-pretty-tags-mode' is of in every Org buffer else nil."
  (let ((alloff t))
    (dolist (buf (buffer-list))
      (when alloff
        (set-buffer buf)
        (when (and (derived-mode-p 'org-mode)
                   org-pretty-tags-mode)
          (setq alloff nil))))
    alloff))


;; get image specifications

(defun org-pretty-tags-image-specs (tags-and-filenames)
  "Return an alist with tag and Emacs image spec.
PRETTY-TAGS-SURROGATE-IMAGES is an list of tag names and filenames."
  (mapcar
   (lambda (x)
     (cons (car x)
           (let ((px-subtract-from-image-height 5))
             (create-image
              (cdr x)
              'imagemagick nil
              :height (- (window-font-height)
                         px-subtract-from-image-height)
              :ascent 'center))))
   tags-and-filenames))


;; create/delete overlays

(defun org-pretty-tags-delete-overlays ()
  "Delete all pretty tags overlays created."
  (while org-pretty-tags-overlays
    (delete-overlay (pop org-pretty-tags-overlays))))

;; POTENTIAL: make sure only tags are changed.
(defun org-pretty-tags-refresh-agenda-lines ()
  "Place pretty tags in agenda lines according pretty tags state of Org file."
  (goto-char (point-min))
  (while (progn (org-pretty-tags-goto-next-visible-agenda-item)
                (not (eobp)))
    (org-pretty-tags-refresh-agenda-line)
    (end-of-line)))

(defun org-pretty-tags-refresh-agenda-line ()
  "Place pretty tags in agenda line."
  (when (with-current-buffer
            (marker-buffer (org-get-at-bol 'org-marker))
          org-pretty-tags-mode)
    (mapc (lambda (x)
            (beginning-of-line)
            (let ((eol (save-excursion (end-of-line) (point))))
              (message "eol %s" eol)
              (while (re-search-forward
                      (concat ":\\(" (car x) "\\):") eol t)
                (push (make-overlay (match-beginning 1) (match-end 1))
                      org-pretty-tags-overlays)
                (overlay-put (car org-pretty-tags-overlays) 'display (cdr x)))))
          (append org-pretty-tags-surrogate-strings
                  (org-pretty-tags-image-specs org-pretty-tags-surrogate-images)))))

(defun org-pretty-tags-refresh-overlays-org-mode ()
  "Create the overlays for the tags for the headlines in the buffer."
  (org-with-point-at 1
    (unless (org-at-heading-p)
      (outline-next-heading))
    (let ((surrogates (append org-pretty-tags-surrogate-strings
                              (org-pretty-tags-image-specs org-pretty-tags-surrogate-images))))
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


;; mode definition

;;;###autoload
(define-minor-mode org-pretty-tags-mode
  "Display surrogates for tags in buffer.
This mode is local to Org mode buffers.

Special: when invoked from an Org agenda buffer the mode gets
applied to every Org mode buffer."
  :lighter org-pretty-tags-mode-lighter
  (unless (derived-mode-p 'org-mode)
      (user-error "org-pretty-tags-mode performs for Org mode only.  Consider org-pretty-tags-mode-global"))
    (org-pretty-tags-delete-overlays)
    (cond
     (org-pretty-tags-mode
      (org-pretty-tags-refresh-overlays-org-mode)
      (add-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays-org-mode)
      (add-hook 'org-ctrl-c-ctrl-c-hook #'org-pretty-tags-refresh-overlays-org-mode)
      (add-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-agenda-lines))
     (t
      (remove-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays-org-mode)
      (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-pretty-tags-refresh-overlays-org-mode)
      (if (org-pretty-tags-mode-off-in-every-buffer-p)
          (remove-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-agenda-lines)))))

;; (add-hook
;;  'org-pretty-tags-global-mode-hook
;;  (lambda ()
;;    (dolist (buf (buffer-list))
;;       (set-buffer buf)
;;       (when (derived-mode-p 'org-agenda-mode)
;;         (org-agenda-redo)))))

;;;###autoload
(define-global-minor-mode org-pretty-tags-global-mode
  org-pretty-tags-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-pretty-tags-mode 1))))


(provide 'org-pretty-tags)

;;; org-pretty-tags.el ends here
