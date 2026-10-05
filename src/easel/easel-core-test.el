;;; easel-core-test.el --- tests for easel-core and easel-time -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel-core)
(require 'easel-time)

(ert-deftest easel-core-json-round-trips-null-false-and-empty ()
  (let ((text "{\"a\":[1,null,false,{}],\"b\":true,\"x-easel:transform\":\"lttb\"}"))
    (should (equal (easel-json-parse text)
                   '(:a [1 :null :false nil] :b t :x-easel:transform "lttb")))
    (should (equal (easel-json-encode (easel-json-parse text)) text))))

(ert-deftest easel-core-parse-error-is-data ()
  (let ((plist (easel-test-should-code "PARSE_ERROR" (easel-json-parse "{"))))
    (should (string-match-p "fix the syntax" (plist-get plist :message)))))

(ert-deftest easel-core-every-code-is-an-easel-error-child ()
  (dolist (entry easel-reason-codes)
    (let ((err (should-error (easel-signal (car entry) "m" :x 1) :type 'easel-error)))
      (should (eq (car err) (nth 1 entry)))
      (should (equal (easel-error-plist err)
                     (list :code (car entry) :message "m" :x 1))))))

(ert-deftest easel-core-foreign-errors-become-engine-failed ()
  (should (equal (plist-get (easel-error-plist '(wrong-type-argument numberp "a")) :code)
                 "ENGINE_FAILED")))

(ert-deftest easel-core-content-hash-is-sha256-of-canonical-json ()
  ;; sha256('{"a":2,"b":1}'), computed independently.
  (should (equal (easel-content-hash '(:b 1 :a 2))
                 "sha256:d3626ac30a87e6f7a6428233b3c68299976865fa5508e4267c5415c76af7a772"))
  (should (equal (easel-content-hash '(:b 1 :a 2)) (easel-content-hash '(:a 2 :b 1)))))

(ert-deftest easel-core-pretty-is-sorted-and-stable ()
  (should (equal (easel-json-pretty '(:z [1 2] :a (:k "v") :m [(:n [1 (:deep 1)])]))
                 "{\n  \"a\": {\"k\": \"v\"},\n  \"m\": [\n    {\n      \"n\": [\n        1,\n        {\"deep\": 1}\n      ]\n    }\n  ],\n  \"z\": [1, 2]\n}\n")))

(ert-deftest easel-time-parse-is-utc-and-honours-offsets ()
  (should (= (easel-time-parse "2026-03-01") 1772323200000))
  (should (= (easel-time-parse "2026-03-01T10:30:00Z") 1772361000000))
  (should (= (easel-time-parse "2026-03-01T10:30:00-05:00") 1772379000000))
  (should (= (easel-time-parse "1969-12-31") -86400000))
  (should (= (easel-time-parse 42) 42))
  (should-not (easel-time-parse "March 1")))

(ert-deftest easel-time-fields-and-ms-round-trip ()
  (let ((ms (easel-time-parse "2024-02-29T23:59:59.5Z")))
    (should (equal (easel-time-fields ms)
                   '(:year 2024 :month 2 :day 29 :hours 23 :minutes 59 :seconds 59
                           :milliseconds 500 :weekday 4)))
    (should (= (easel-time-ms 2024 2 29 23 59 59 500) ms))
    (should (= (easel-time-ms 2026 14 1) (easel-time-parse "2027-02-01")))))

(ert-deftest easel-has-no-references-to-its-host-package ()
  "Extraction guard (fc-qx1.11): src/easel names no host package."
  (let ((needle (concat "financial" "-chart"))
        (dir (easel-test-file "src/easel")))
    (dolist (file (directory-files dir t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (and (search-forward needle nil t) file))))))

(provide 'easel-core-test)
;;; easel-core-test.el ends here
