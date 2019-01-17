;;; test-org-pretty-tags.el --- tests  -*- lexical-binding: t -*-

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
