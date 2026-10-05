;;; easel-babel-test.el --- tests for the org-babel surface (fc-qx1.13) -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'org)
(require 'ox-ascii)
(require 'ob-easel)

(defconst easel-babel-test--table
  "#+name: tbl
| time       |  open |  high |   low | close |
|------------+-------+-------+-------+-------|
| 2026-03-02 | 100.0 | 104.0 |  99.0 | 103.0 |
| 2026-03-03 | 103.0 | 105.0 | 101.0 | 101.5 |
| 2026-03-04 | 101.5 | 106.0 | 101.0 | 105.5 |
| 2026-03-05 | 105.5 | 108.0 | 104.0 | 107.0 |
"
  "Four OHLC bars as a named org table.")

(defconst easel-babel-test--block
  "\n#+name: candles\n#+begin_src easel :template ohlc :data tbl :title \"TSM\" :cols 60 :rows 14%s\n%s#+end_src\n"
  "An ohlc block over tbl; format with extra headers and a body.")

(defmacro easel-babel-test--with-org (headers body &rest forms)
  "Run FORMS in an org buffer holding the table and a block with HEADERS and BODY.
Point starts on the block; views and inline state are fresh."
  (declare (indent 2))
  `(let ((easel-views (make-hash-table :test 'equal))
         (easel-babel-sources (make-hash-table :test 'equal))
         (easel-babel-inline--views (make-hash-table :test 'equal))
         (easel-babel-inline--pending nil)
         (easel-babel-target 'text)
         (org-confirm-babel-evaluate nil))
     (with-temp-buffer
       (insert easel-babel-test--table (format easel-babel-test--block ,headers ,body))
       (org-mode)
       (goto-char (point-min))
       (re-search-forward "#\\+begin_src easel")
       (beginning-of-line)
       ,@forms)))

(defun easel-babel-test--execute ()
  "Execute the block at point and return its result."
  (save-excursion (org-babel-execute-src-block)))

(defun easel-babel-test--item-px (view mark i)
  "Centre px of item I of MARK in VIEW's first scene view."
  (let* ((sv (aref (plist-get (easel-view-scene view) :views) 0))
         (m (seq-find (lambda (m) (equal (plist-get m :id) mark)) (plist-get sv :marks)))
         (it (aref (plist-get m :items) i)))
    (vector (+ (plist-get it :x) (/ (plist-get it :w) 2.0))
            (+ (plist-get it :y) (/ (plist-get it :h) 2.0)))))

(defun easel-babel-test--px-pos (view px)
  "Buffer position of the inline result cell under PX in VIEW."
  (let* ((ov (plist-get (easel-babel-inline--entry view) :overlay))
         (cell (plist-get (plist-get (easel-view-scene view) :size) :cell)))
    (save-excursion
      (goto-char (overlay-start ov))
      (forward-line (floor (aref px 1) (aref cell 1)))
      (move-to-column (+ (easel-babel-inline--prefix) (floor (aref px 0) (aref cell 0))))
      (point))))

;;; Data and bindings

(ert-deftest easel-babel-data-reads-tables-results-and-files ()
  (should (equal (plist-get (easel-babel-data '(("x" "y") hline (1 2) (2 3))) :rows)
                 [(:x 1 :y 2) (:x 2 :y 3)]))
  (should (equal (plist-get (easel-babel-data '((:x 1) (:x 2))) :rows) [(:x 1) (:x 2)]))
  (let ((csv (make-temp-file "easel-babel" nil ".csv" "x,y\n1,2\n")))
    (unwind-protect
        (should (equal (plist-get (easel-babel-data csv) :rows) [(:x 1 :y 2)]))
      (delete-file csv)))
  (easel-babel-test--with-org "" ""
    (should (equal (aref (plist-get (easel-babel-data "tbl") :rows) 3)
                   '(:time "2026-03-05" :open 105.5 :high 108.0 :low 104.0 :close 107.0)))
    (should (equal (plist-get (easel-error-plist (should-error (easel-babel-data "missing.csv")
                                                               :type 'easel-not-found))
                              :file)
                   "missing.csv"))
    (let ((err (should-error (easel-babel-data "nope") :type 'easel-not-found)))
      (should (equal (plist-get (easel-error-plist err) :table) "nope")))))

(ert-deftest easel-babel-bindings-take-slots-from-headers-var-and-body ()
  (let* ((template (easel-template-get "line"))
         (b (easel-babel-bindings template
                                  '((:points . "true") (:title . "Daily") (:results . "verbatim")
                                    (:var . ("data" . (("date" "value") hline ("2026-03-02" 1) ("2026-03-03" 2)))))
                                  "{\"y\": \"value\"}")))
    (should (eq (plist-get b :points) t))
    (should (equal (plist-get b :title) "Daily"))
    (should (equal (plist-get b :y) "value"))
    (should (equal (plist-get b :data) [(:date "2026-03-02" :value 1) (:date "2026-03-03" :value 2)]))
    (should-not (plist-member b :results)))
  (should (eq (easel-babel-data-slot (easel-template-get "ohlc")) :bars))
  (should-error (easel-babel-bindings (easel-template-get "line") nil "[1, 2]") :type 'easel-invalid-input)
  (should-error (easel-babel-bindings (easel-template-get "line") nil "{oops") :type 'easel-parse-error))

(ert-deftest easel-babel-block-errors-name-their-fix ()
  (should-error (org-babel-execute:easel "" '((:as . "text"))) :type 'easel-invalid-input)
  (should-error (org-babel-execute:easel "" '((:template . "nope"))) :type 'easel-not-found)
  (should-error (org-babel-execute:easel "" '((:template . "line") (:as . "pdf"))) :type 'easel-invalid-input)
  (should-error (org-babel-execute:easel "" '((:template . "line") (:as . "text"))) :type 'easel-slot-missing)
  (let ((err (should-error (org-babel-execute:easel "" '((:template . "line") (:file . "x.svg")
                                                         (:result-params "verbatim")))
                           :type 'easel-invalid-input)))
    (should (string-match-p ":results file" (cadr err)))))

;;; Outputs

(ert-deftest easel-babel-text-result-is-the-deterministic-text-chart ()
  (easel-babel-test--with-org " :as text" ""
    (let ((result (easel-babel-test--execute)))
      (should (string-match-p "TSM" result))
      (should (equal (easel-view-ids) nil))
      (easel-test-golden "babel-ohlc.txt" result)
      (should (search-forward "#+RESULTS: candles" nil t)))))

(ert-deftest easel-babel-vl-result-is-the-resolved-vega-lite ()
  (easel-babel-test--with-org " :as vl" ""
    (let* ((result (easel-babel-test--execute)) (spec (easel-json-parse result)))
      (should-not (string-match-p "x-easel" result))
      (should (equal (length (plist-get (plist-get spec :data) :values)) 4))
      (should (equal (plist-get spec :title) "TSM"))
      (should (equal (easel-resolve-hash spec)
                     (easel-resolve-hash
                      (easel-resolve "ohlc" (list :title "TSM"
                                                  :bars (plist-get (easel-babel-data
                                                                    '(("time" "open" "high" "low" "close")
                                                                      ("2026-03-02" 100.0 104.0 99.0 103.0)
                                                                      ("2026-03-03" 103.0 105.0 101.0 101.5)
                                                                      ("2026-03-04" 101.5 106.0 101.0 105.5)
                                                                      ("2026-03-05" 105.5 108.0 104.0 107.0)))
                                                                   :rows)))))))))

(ert-deftest easel-babel-file-results-write-by-extension ()
  (let ((dir (make-temp-file "easel-babel" t)))
    (unwind-protect
        (dolist (case '(("c.svg" . "<svg") ("c.json" . "\"$schema\"") ("c.txt" . "TSM")))
          (let ((file (expand-file-name (car case) dir)))
            (easel-babel-test--with-org (format " :results file :file %s" file) ""
              (should (equal (easel-babel-test--execute) file))
              (should (search-forward (format "[[file:%s]]" file) nil t))
              (should (string-match-p (regexp-quote (cdr case))
                                      (with-temp-buffer (insert-file-contents file) (buffer-string)))))))
      (delete-directory dir t))))

(ert-deftest easel-babel-plain-spec-body-takes-data-rows ()
  (easel-babel-test--with-org
      "" ""
    (let ((result (org-babel-execute:easel
                   "{\"mark\": \"line\", \"width\": 200, \"height\": 100,
                     \"encoding\": {\"x\": {\"field\": \"time\", \"type\": \"temporal\"},
                                    \"y\": {\"field\": \"close\", \"type\": \"quantitative\"}}}"
                   '((:data . "tbl") (:as . "text") (:cols . 40) (:rows . 10)))))
      (should (stringp result))
      (should (string-match-p "close" result)))))

(ert-deftest easel-babel-export-yields-text-and-opens-no-view ()
  (easel-babel-test--with-org " :exports results" ""
    (let ((out (org-export-as 'ascii nil nil t)))
      (should (string-match-p "TSM" out))
      (should (string-match-p "date" out))
      (should-not (easel-view-ids))))
  (easel-babel-test--with-org " :exports results" ""
    (let* ((easel-babel-export-as "vl")
           (out (org-export-as 'ascii nil nil t)))
      (should (string-match-p "vega-lite/v6" out))
      (should-not (easel-view-ids)))))

;;; The live view

(ert-deftest easel-babel-view-opens-inline-under-the-block-name ()
  (easel-babel-test--with-org "" ""
    (let ((result (easel-babel-test--execute)))
      (should (equal (easel-view-ids) '("ohlc:candles")))
      (let* ((view (easel-view-get "ohlc:candles"))
             (ov (plist-get (easel-babel-inline--entry view) :overlay)))
        (should (overlayp ov))
        (should (eq (overlay-get ov 'keymap) easel-babel-inline-map))
        (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                       (string-trim-right result "\n+")))
        (should (equal (easel-babel-source-of view) (list :name "tbl" :buffer (current-buffer))))
        (should (equal (easel-action-for view '(:mark "candles" :view "main")) "org-source-row"))))
    ;; Re-running replaces the view rather than adding a second one.
    (easel-babel-test--execute)
    (should (equal (easel-view-ids) '("ohlc:candles")))))

(ert-deftest easel-babel-click-on-a-candle-jumps-to-its-table-row ()
  (easel-babel-test--with-org "" ""
    (easel-babel-test--execute)
    (let* ((view (easel-view-get "ohlc:candles"))
           (inspect (easel-dispatch view (list :type "click" :px (easel-babel-test--item-px view "candles" 2))))
           (click (plist-get inspect :click)))
      (should (equal (plist-get click :action) "org-source-row"))
      (should (eq (plist-get click :ran) t))
      (should (equal (plist-get click :result) "tbl:3"))
      (should (string-prefix-p "2026-03-04 |" (buffer-substring (point) (line-end-position))))
      ;; A replayed log records the click without jumping again.
      (goto-char (point-min))
      (easel-replay view (easel-view-log-entries view))
      (should (eq (plist-get (plist-get (easel-inspect view) :click) :ran) :false))
      (should (= (point) (point-min))))))

(ert-deftest easel-babel-terminal-ret-on-the-result-clicks-the-cell ()
  (easel-babel-test--with-org "" ""
    (easel-babel-test--execute)
    (let* ((view (easel-view-get "ohlc:candles"))
           (px (easel-babel-test--item-px view "candles" 0))
           (pos (easel-babel-test--px-pos view px)))
      (goto-char pos)
      (let ((back (easel-babel-inline-pos-px pos)))
        (should (< (abs (- (aref back 0) (aref px 0))) 7))
        (should (< (abs (- (aref back 1) (aref px 1))) 14)))
      ;; Point motion hovers.
      (easel-babel-inline--post-command)
      (should (equal (plist-get (plist-get (plist-get (easel-inspect view) :hover) :row) :time) "2026-03-02"))
      (should (eq (lookup-key easel-babel-inline-map (kbd "RET")) #'easel-babel-inline-click-at-point))
      (easel-babel-inline-click-at-point)
      (should (equal (plist-get (plist-get (easel-inspect view) :click) :result) "tbl:1"))
      (should (string-prefix-p "2026-03-02 |" (buffer-substring (point) (line-end-position)))))))

(ert-deftest easel-babel-terminal-push-rewrites-the-result ()
  (easel-babel-test--with-org "" ""
    (let* ((before (easel-babel-test--execute))
           (view (easel-view-get "ohlc:candles")))
      (easel-push view [(:time "2026-03-06" :open 107.0 :high 112.0 :low 106.0 :close 111.0 :volume :null)])
      (let ((ov (plist-get (easel-babel-inline--entry view) :overlay))
            (now (substring-no-properties (easel-text-render (easel-view-scene view)))))
        (should-not (equal (string-trim-right before "\n+") (string-trim-right now "\n+")))
        (should (string-match-p "Fri" now))
        (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                       (string-trim-right now "\n+")))
        (should (search-forward "#+end_example" nil t))))))

(ert-deftest easel-babel-terminal-zoom-keys-on-a-plain-spec ()
  (easel-babel-test--with-org "" ""
    (goto-char (point-max))
    (insert "\n#+name: zoomable\n#+begin_src easel :data tbl :cols 50 :rows 12
{\"mark\": \"line\", \"width\": 300, \"height\": 120,
 \"params\": [{\"name\": \"grid\", \"select\": \"interval\", \"bind\": \"scales\"}],
 \"encoding\": {\"x\": {\"field\": \"time\", \"type\": \"temporal\"},
              \"y\": {\"field\": \"close\", \"type\": \"quantitative\"}}}
#+end_src\n")
    (re-search-backward "#\\+begin_src easel :data")
    (let ((before (easel-babel-test--execute))
          (view (easel-view-get "chart:zoomable")))
      (goto-char (overlay-start (plist-get (easel-babel-inline--entry view) :overlay)))
      (should (eq (lookup-key easel-babel-inline-map "+") #'easel-babel-inline-key))
      (let ((unread-command-events nil))
        (cl-letf (((symbol-function 'this-command-keys-vector) (lambda () [?+])))
          (easel-babel-inline-key)))
      (should (eq (plist-get (aref (plist-get (easel-inspect view) :views) 0) :zoomed) t))
      (let ((ov (plist-get (easel-babel-inline--entry view) :overlay)))
        (should-not (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                           (string-trim-right before "\n+")))
        (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                       (string-trim-right (substring-no-properties (easel-text-render (easel-view-scene view)))
                                          "\n+")))))))

(ert-deftest easel-babel-gui-overlay-shows-the-svg ()
  (easel-babel-test--with-org "" ""
    (let ((easel-babel-target 'svg))
      (easel-babel-test--execute)
      (let* ((view (easel-view-get "ohlc:candles"))
             (ov (plist-get (easel-babel-inline--entry view) :overlay))
             (image (overlay-get ov 'display)))
        (should (eq (car image) 'image))
        (should (string-prefix-p "<svg" (plist-get (cdr image) :data)))
        (should track-mouse)
        (let ((area (nth 1 (car (plist-get (cdr image) :map)))))
          (should area)
          (should (eq (lookup-key (overlay-get ov 'keymap) (vector area 'mouse-1)) #'easel-babel-inline-up)))
        (easel-push view [(:time "2026-03-06" :open 107.0 :high 112.0 :low 106.0 :close 111.0 :volume :null)])
        (should-not (eq (overlay-get ov 'display) image))
        (should (string-prefix-p "<svg" (plist-get (cdr (overlay-get ov 'display)) :data)))))))

(ert-deftest easel-babel-open-shows-a-second-view-that-still-jumps ()
  (easel-babel-test--with-org "" ""
    (easel-babel-test--execute)
    (let ((org (current-buffer)) (easel-babel-target 'text))
      (goto-char (overlay-start (plist-get (easel-babel-inline--entry "ohlc:candles") :overlay)))
      (save-window-excursion
        (easel-babel-inline-open)
        (should (equal (easel-view-ids) '("ohlc:candles" "ohlc:candles/full")))
        (should (equal (easel-babel-source-of "ohlc:candles/full") (list :name "tbl" :buffer org)))
        (kill-buffer (easel-view-buffer (easel-view-get "ohlc:candles/full")))))))

(provide 'easel-babel-test)
;;; easel-babel-test.el ends here
