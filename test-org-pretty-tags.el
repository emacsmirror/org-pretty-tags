;;; test-org-pretty-tags.el --- tests  -*- lexical-binding: t -*-


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



;; Tests
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :comments both
;; :ID:       dac141b6-e0a8-4312-8022-90b08fce4c84
;; :END:


;; [[id:dac141b6-e0a8-4312-8022-90b08fce4c84][Tests:1]]
(require 'org-pretty-tags)
;; Tests:1 ends here

;; [[id:dac141b6-e0a8-4312-8022-90b08fce4c84][Tests:2]]
(ert-deftest test-org-pretty-tags-1 ()
  "a glyph overlays a tag."
  (with-temp-buffer
    (insert "* foo :bar:
")
    (org-mode)
    (let ((org-pretty-tags-surrogate-strings
           '(("bar" . "&"))))
      (org-pretty-tags-mode)
      (buffer-substring 8 9)
      (should (get-char-property 8 'display)))))
;; Tests:2 ends here


(provide 'test-org-pretty-tags)

;;; test-org-pretty-tags.el ends here
