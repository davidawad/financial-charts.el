;;; financial-chart-eas-babel-test.el --- ohlc template in org-babel -*- lexical-binding: t; -*-

;;; Commentary:

;; `#+begin_src eas :template ohlc' (financial-chart's template, eas's
;; ob-eas) with `:as text' gives the deterministic text chart; the
;; result is a golden.  Moved here from eas's babel tests.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'org)
(require 'ox-ascii)
(require 'ob-eas)

(defconst financial-chart-eas-babel-test--org
  "#+name: tbl
| time       |  open |  high |   low | close |
|------------+-------+-------+-------+-------|
| 2026-03-02 | 100.0 | 104.0 |  99.0 | 103.0 |
| 2026-03-03 | 103.0 | 105.0 | 101.0 | 101.5 |
| 2026-03-04 | 101.5 | 106.0 | 101.0 | 105.5 |
| 2026-03-05 | 105.5 | 108.0 | 104.0 | 107.0 |

#+name: candles
#+begin_src eas :template ohlc :data tbl :title \"TSM\" :cols 60 :rows 14 :as text
#+end_src
"
  "Four OHLC bars as a named org table and an ohlc block over them.")

(ert-deftest financial-chart-eas-babel-ohlc-text-result-is-the-deterministic-chart ()
  (let ((eas-views (make-hash-table :test 'equal))
        (eas-babel-sources (make-hash-table :test 'equal))
        (eas-babel-inline--views (make-hash-table :test 'equal))
        (eas-babel-inline--pending nil)
        (eas-babel-target 'text)
        (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (insert financial-chart-eas-babel-test--org)
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "#\\+begin_src eas")
      (beginning-of-line)
      (let ((result (save-excursion (org-babel-execute-src-block))))
        (should (string-match-p "TSM" result))
        (should (equal (eas-view-ids) nil))
        (financial-chart-test-golden "babel-ohlc.txt" result)
        (should (search-forward "#+RESULTS: candles" nil t))))))

(provide 'financial-chart-eas-babel-test)
;;; financial-chart-eas-babel-test.el ends here
