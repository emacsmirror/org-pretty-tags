;;; org-pretty-tags.el --- Surrogates for tags  -*- lexical-binding: t -*-

;; THIS FILE HAS BEEN GENERATED.  see the literate source.

;; Copyright 2019 Marco Wahl
;; 
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: [2019-01-06]
;; Version: 0.0.2
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

;; Use {M-x customize-variable RET org-pretty-tags-surrogate-strings RET} to
;; define surrogate strings for tags.  E.g. add the pair "money", "$$$".
;; 
;; If you don't like the predefined surrogates then just delete them.
;; 
;; Use {M-x customize-variable RET org-pretty-tags-surrogate-images} to
;; define surrogate images for tags.  The definition of the image is
;; expected to be a path to an image.  E.g. add the pair "org", "<path to
;; org icon>".
;;
;; - Activate the mode with {M-x org-pretty-tags-mode RET}.
;; - Deactivate the mode with a further {M-x org-pretty-tags-mode RET}.
;;
;; See also the literate source file.  E.g. see https://gitlab.com/marcowahl/org-pretty-tags.


;;; Code:

;; container for the overlays
;; :PROPERTIES:
;; :ID:       cf2048b2-5f4e-4211-873d-9bce13c53f59
;; :END:


;; [[id:cf2048b2-5f4e-4211-873d-9bce13c53f59][container for the overlays:1]]
(defvar-local org-pretty-tags-overlays nil
 "Container for the overlays of org-pretty-tags-mode.")
;; container for the overlays:1 ends here

;; list of tags with symbols surrogates for plain ascii tags
;; :PROPERTIES:
;; :ID:       16c25206-73c2-422b-8948-979c415b75de
;; :END:


;; [[id:16c25206-73c2-422b-8948-979c415b75de][list of tags with symbols surrogates for plain ascii tags:1]]
;;;###autoload
(defcustom org-pretty-tags-surrogate-strings
  '(("imp" . "☆") ; important stuff.
    ("idea" . "💡") ; inspiration.
    ("money" . "$$$")
    ("easy" . "₰")
    ("music" . "♩"))
  "List of pretty replacements for tags."
  :type '(alist :key-type string :value-type string)
  :group 'org-tags)
;; list of tags with symbols surrogates for plain ascii tags:1 ends here

;; list of image surrogates for plain ascii tags
;; :PROPERTIES:
;; :ID:       cabb8307-a825-485d-9bf4-371d4020ef5b
;; :END:


;; [[id:cabb8307-a825-485d-9bf4-371d4020ef5b][list of image surrogates for plain ascii tags:1]]
;;;###autoload
(defcustom org-pretty-tags-surrogate-images
  '()
  "List of pretty image replacements for tags."
  :type '(alist :key-type string :value-type string)
  :group 'org-tags)
;; list of image surrogates for plain ascii tags:1 ends here

;; cache for the images
;; :PROPERTIES:
;; :ID:       fb26c0bc-a69e-4cd2-8b5a-800682d24706
;; :foo:      foo
;; :END:


;; [[id:fb26c0bc-a69e-4cd2-8b5a-800682d24706][cache for the images:1]]
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
;; cache for the images:1 ends here

;; [[id:fb26c0bc-a69e-4cd2-8b5a-800682d24706][cache for the images:2]]
(defvar org-pretty-tags-image-cache
  (org-pretty-tags-image-cache)
  "Cache for the image surrogates.")
;; cache for the images:2 ends here

;; [[id:fb26c0bc-a69e-4cd2-8b5a-800682d24706][cache for the images:3]]
(defun org-pretty-tags-update-image-cache ()
  "Update `org-pretty-tags-image-cache' from list `org-pretty-tags-surrogate-images'."
  (setq org-pretty-tags-image-cache (org-pretty-tags-image-cache)))
;; cache for the images:3 ends here

;; function to update the tag surrogates
;; :PROPERTIES:
;; :ID:       da436b9c-2eb6-4247-804c-20e18a626ac7
;; :END:


;; [[id:da436b9c-2eb6-4247-804c-20e18a626ac7][function to update the tag surrogates:1]]
(defun org-pretty-tags-delete-overlays ()
  (while org-pretty-tags-overlays
    (delete-overlay (pop org-pretty-tags-overlays))))

(defun org-pretty-tags-refresh-overlays-agenda ()
  (mapc (lambda (x)
          (org-with-point-at 1
            ;; try: make sure only tags are changed.
            (progn
              (while (re-search-forward
                      (concat ":\\(" (car x) "\\):") nil t)
                (push (make-overlay (match-beginning 1) (match-end 1))
                      org-pretty-tags-overlays)
                (overlay-put (car org-pretty-tags-overlays) 'display (cdr x))))))
        (append org-pretty-tags-surrogate-strings org-pretty-tags-image-cache)))

(defun org-pretty-tags-refresh-overlays-org-mode ()
  (assert (derived-mode-p 'org-mode))
  (org-with-point-at 1
    (unless (org-at-heading-p)
      (outline-next-heading))
    (let ((surrogates (append org-pretty-tags-surrogate-strings org-pretty-tags-image-cache)))
      (while (not (eobp))
        (assert (org-at-heading-p) "programm logic error.")
        (org-match-line org-complex-heading-regexp)
        (if (match-beginning 5)
            (let ((tags-end (match-end 5)))
              (goto-char (1+ (match-beginning 5)))
              (while (re-search-forward
                      (concat "\\(.+?\\):") tags-end t)
                (when-let ((surrogate-cons (assoc (buffer-substring (match-beginning 1) (match-end 1))
                                                  surrogates)))
                  (push (make-overlay (match-beginning 1) (match-end 1))
                        org-pretty-tags-overlays)
                  (overlay-put (car org-pretty-tags-overlays) 'display (cdr surrogate-cons))))))
        (outline-next-heading)))))

(defun org-pretty-tags-refresh-overlays ()
  "Overlay tags in current buffer.
The mode of the buffer must be either `org-mode' or `org-agenda-mode'."
  (let ((inhibit-read-only t))
    (org-pretty-tags-delete-overlays)
    (cond
     ((derived-mode-p 'org-agenda-mode) (org-pretty-tags-refresh-overlays-agenda))
     ((derived-mode-p 'org-mode) (org-pretty-tags-refresh-overlays-org-mode))
     (t (error "function does not deal with the current context")))))
;; function to update the tag surrogates:1 ends here

;; a state for the agenda
;; :PROPERTIES:
;; :ID:       76092d7a-c901-48dd-8e03-b0de116fd839
;; :END:

;; the agenda gets rebuild from scratch.  therefore the need to store the
;; pretty tags state.


;; [[id:76092d7a-c901-48dd-8e03-b0de116fd839][a state for the agenda:1]]
(defvar org-pretty-tags-agenda-in-the-mode nil
  "Indicator if the agenda is in pretty-tags-mode.")
;; a state for the agenda:1 ends here

;; define the mode
;; :PROPERTIES:
;; :ID:       a3d9cc59-89aa-4165-a844-90da8531b46f
;; :END:


;; [[id:a3d9cc59-89aa-4165-a844-90da8531b46f][define the mode:1]]
;;;###autoload
(define-minor-mode org-pretty-tags-mode
  "Display surrogates for tags."
  :lighter " pretty tags"
  (cond
   ((derived-mode-p 'org-mode)
    (cond
     (org-pretty-tags-mode
      (org-pretty-tags-update-image-cache)
      (org-pretty-tags-refresh-overlays)
      (add-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays)
      (add-hook 'org-ctrl-c-ctrl-c-final-hook #'org-pretty-tags-refresh-overlays))
     (t
      (org-pretty-tags-delete-overlays)
      (remove-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays)
      (remove-hook 'org-ctrl-c-ctrl-c-final-hook #'org-pretty-tags-refresh-overlays))))
   ((derived-mode-p  'org-agenda-mode)
    (setq org-pretty-tags-agenda-in-the-mode (not org-pretty-tags-agenda-in-the-mode))
    (if org-pretty-tags-agenda-in-the-mode
        (progn
         (org-pretty-tags-update-image-cache)
         (org-pretty-tags-refresh-overlays)
         (add-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-overlays))
     (org-pretty-tags-delete-overlays)
     (remove-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-overlays))
    (setq org-pretty-tags-mode org-pretty-tags-agenda-in-the-mode))
   (t (user-error (concat
                   "Attempt to activate pretty tags mode on non Org mode buffer."
                   "  Doing nothing."
                   "  Effect in Org mode buffer or Org Agenda buffer.")))))
;; define the mode:1 ends here


(provide 'org-pretty-tags)

;;; org-pretty-tags.el ends here
