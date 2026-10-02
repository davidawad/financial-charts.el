;;; financial-chart-batch-test.el --- the JSON command line -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'financial-chart-batch)

(defun financial-chart-batch-test--run (cmd &optional json)
  "Run CMD with JSON (a string) as its input file; return (STATUS . STDOUT)."
  (let ((file (and json (make-temp-file "fc-batch-" nil ".json" json))))
    (unwind-protect
        (let* ((out (generate-new-buffer " *fc-batch-out*"))
               (status (let ((standard-output out))
                         (financial-chart-batch-run cmd (or file (and (equal cmd "example") "area"))))))
          (prog1 (cons status (with-current-buffer out (buffer-string)))
            (kill-buffer out)))
      (when file (delete-file file)))))

(defun financial-chart-batch-test--json (s)
  "Parse S as JSON into plists."
  (json-parse-string s :object-type 'plist :false-object nil))

(ert-deftest financial-chart-batch-test-render-text-matches-plot ()
  (let ((r (financial-chart-batch-test--run
            "render" "{\"kind\":\"area\",\"data\":[[1,40],[2,45],[3,50]],\"backend\":\"text\",\"width\":3,\"height\":2}")))
    (should (= (car r) 0))
    (should (equal (cdr r)
                   (concat (substring-no-properties
                            (financial-chart-plot 'area '((1 40) (2 45) (3 50))
                                                  :backend 'text :width 3 :height 2))
                           "\n")))))

(ert-deftest financial-chart-batch-test-labeled-object-and-pairs ()
  (dolist (data '("{\"AAPL\":1200,\"TSLA\":-950}" "[[\"AAPL\",1200],[\"TSLA\",-950]]"))
    (let ((r (financial-chart-batch-test--run
              "render" (format "{\"kind\":\"bars\",\"data\":%s,\"backend\":\"text\"}" data))))
      (should (= (car r) 0))
      (should (string-match-p "AAPL.*\\+1200" (cdr r)))
      (should (string-match-p "TSLA.*-950" (cdr r))))))

(ert-deftest financial-chart-batch-test-ohlc-objects-become-bars ()
  (let ((spec (financial-chart-batch-spec
               '((kind . "ohlc") (data ((open . 1) (high . 2) (low . 0.5) (close . 1.5)))))))
    (should (equal (plist-get spec :data) '((:open 1 :high 2 :low 0.5 :close 1.5))))))

(ert-deftest financial-chart-batch-test-every-example-renders ()
  "`example KIND' output fed back to `render' works for every kind."
  (dolist (k (mapcar #'car financial-chart-kinds))
    (let ((r (financial-chart-batch-test--run
              "render" (json-encode (financial-chart-batch--example k)))))
      (should (= (car r) 0)))))

(ert-deftest financial-chart-batch-test-errors-are-envelopes ()
  (dolist (case '(("{\"kind\":\"pie\",\"data\":[1]}" . "unknown_kind")
                  ("{\"kind\":\"area\",\"data\":[1,\"x\"]}" . "invalid_data")
                  ("{\"data\":[1]}" . "bad_request")
                  ("{not json" . "bad_json")))
    (let* ((r (financial-chart-batch-test--run "render" (car case)))
           (j (financial-chart-batch-test--json (cdr r))))
      (should (= (car r) 1))
      (should (null (plist-get j :ok)))
      (should (equal (plist-get (plist-get j :error) :code) (cdr case)))
      (should (stringp (plist-get (plist-get j :error) :message))))))

(ert-deftest financial-chart-batch-test-explain-and-describe-are-json ()
  (let ((j (financial-chart-batch-test--json
            (cdr (financial-chart-batch-test--run "explain" "{\"kind\":\"line\",\"data\":[1,2,3],\"backend\":\"text\"}")))))
    (should (equal (plist-get j :renderer) "financial-chart-text-line"))
    (should (eq (plist-get j :valid) t)))
  (let ((j (financial-chart-batch-test--json (cdr (financial-chart-batch-test--run "describe")))))
    (should (equal (plist-get j :package) "financial-chart"))))

(provide 'financial-chart-batch-test)
;;; financial-chart-batch-test.el ends here
