;;; easel-expr-test.el --- tests for the expression subset -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel-expr)

(defconst easel-expr-test--row
  (list :a 2 (intern ":b c") "y" :d "2026-03-01" :n :null)
  "A row exercising numbers, odd field names, dates and nulls.")

(defun easel-expr-test--eval (expr)
  "Evaluate EXPR against the test row and a brush param."
  (easel-expr-evaluate expr easel-expr-test--row '(:brush (:x [1 2]) :k 10)))

(ert-deftest easel-expr-evaluates-the-subset ()
  (dolist (case '(("datum.a * 2 + 1" . 5)
                  ("datum['b c'] + \"x\"" . "yx")
                  ("datum.a > 1 && datum.a < 5 ? 'in' : 'out'" . "in")
                  ("year(datum.d) * 100 + month(datum.d)" . 202602)
                  ("!isValid(datum.n)" . t)
                  ("!isValid(datum.missing)" . t)
                  ("-datum.a % 3" . -2.0)
                  ("inrange(datum.a, [1, 3])" . t)
                  ("7 / 2" . 3.5)
                  ("pow(2, 10)" . 1024)
                  ("brush.x[1]" . 2)
                  ("datum.a + k" . 12)
                  ("datum.a == '2'" . t)
                  ("datum.a === 3" . :false)
                  ("null || 'fallback'" . "fallback")
                  ("0 && unknownName" . 0)
                  ("round(2.5) + floor(-0.5)" . 2.0)
                  ("datetime(2026, 2, 1)" . 1772323200000)
                  ("upper('ab') + length('abc')" . "AB3")))
    (should (equal (cons (car case) (easel-expr-test--eval (car case))) case))))

(ert-deftest easel-expr-failures-are-data ()
  (should (equal (plist-get (easel-test-should-code "PARSE_ERROR" (easel-expr-parse "datum.a +"))
                            :position)
                 9))
  (should (equal (plist-get (easel-test-should-code "PARSE_ERROR" (easel-expr-parse "datum.a @ 1"))
                            :position)
                 8))
  (should (equal (plist-get (easel-test-should-code "UNSUPPORTED_FEATURE"
                              (easel-expr-parse "eval('(delete-file x)')"))
                            :function)
                 "eval"))
  (easel-test-should-code "PARSE_ERROR" (easel-expr-parse "datum.a.(1)"))
  (easel-test-should-code "INVALID_INPUT" (easel-expr-test--eval "nosuch + 1")))

(ert-deftest easel-expr-never-calls-lisp ()
  "Lisp-looking input is just an unknown name or a parse error."
  (easel-test-should-code "PARSE_ERROR" (easel-expr-parse "(shell-command \"ls\")"))
  (easel-test-should-code "UNSUPPORTED_FEATURE" (easel-expr-parse "funcall('ignore')")))

(provide 'easel-expr-test)
;;; easel-expr-test.el ends here
