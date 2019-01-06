;;; org-pretty-tags.el --- Surrogates for tags  -*- lexical-binding: t -*-

;; THIS FILE HAS BEEN GENERATED.  see the literate source.

;; Copyright 2019 Marco Wahl
;;
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: [2019-01-06]
;; Version: 0.0.0
;; Keywords: reading, outliner
;;
;; This file is not part of Emacs.
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

;; See the literate source file.


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

;; function to update the tag surrogates
;; :PROPERTIES:
;; :ID:       da436b9c-2eb6-4247-804c-20e18a626ac7
;; :END:


;; [[id:da436b9c-2eb6-4247-804c-20e18a626ac7][function to update the tag surrogates:1]]
(defun org-pretty-tags-refresh-overlays ()
  "Overlay tags in current buffer."
  (let ((ro buffer-read-only))
    (when ro (setq buffer-read-only nil))
    (let ((tags-to-alternative-images
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
            org-pretty-tags-surrogate-images)))
      (while org-pretty-tags-overlays
        (delete-overlay (pop org-pretty-tags-overlays)))
      (mapc (lambda (x)
              (org-with-point-at 1
                                        ; try: make sure only tags are changed.
                                        ; try: use org functionality to loop over the headings.
                (while (re-search-forward
                        (concat ":\\(" (car x) "\\):") nil t)
                  (when (or (derived-mode-p 'org-agenda-mode)
                            (save-match-data (org-at-heading-p)))
                    (push (make-overlay (match-beginning 1) (match-end 1))
                          org-pretty-tags-overlays)
                    (overlay-put (car org-pretty-tags-overlays) 'display (cdr x))))))
            (append org-pretty-tags-surrogate-strings tags-to-alternative-images))
      (when ro (setq buffer-read-only ro)))))
;; function to update the tag surrogates:1 ends here

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
   (org-pretty-tags-mode
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (user-error "Attempt to activate pretty tags mode on non Org mode buffer.  Doing nothing.  Try with Org mode buffer."))
    (org-pretty-tags-refresh-overlays)
    (add-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-overlays)
    (add-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays)
    (add-hook 'org-ctrl-c-ctrl-c-final-hook
              (lambda ()
                (org-pretty-tags-refresh-overlays)
                nil))
    (add-hook 'org-mode-hook #'org-pretty-tags-refresh-overlays)
    (message "pretty tags overlays installed"))
   (t
    (while org-pretty-tags-overlays
      (delete-overlay (pop org-pretty-tags-overlays)))
    (remove-hook 'org-agenda-finalize-hook #'org-pretty-tags-refresh-overlays)
    (remove-hook 'org-after-tags-change-hook #'org-pretty-tags-refresh-overlays)
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook
                 '(closure
                   (t)
                   nil
                   (org-pretty-tags-refresh-overlays)
                   nil))
    (remove-hook 'org-mode-hook #'org-pretty-tags-refresh-overlays)
    (message "pretty tags overlays removed"))))
;; define the mode:1 ends here


(provide 'org-pretty-tags)

;;; org-pretty-tags.el ends here
