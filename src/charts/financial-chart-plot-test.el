;;; financial-chart-plot-test.el --- ERT tests for financial-chart-plot -*- lexical-binding: t; -*-

;; Pure data -> chart tests: no network, no windows.  Every kind draws
;; through its eas template; the template text goldens live in
;; test/golden/eas-templates/ (financial-chart-templates-test.el).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'financial-chart)

(defconst financial-chart-plot-test--payoff
  (cl-loop for p from 90 to 110 collect (list p (- (* 10 (max 0 (- p 100))) 25)))
  "Long call, strike 100, premium 25 (per 10 contracts-ish).")

(defconst financial-chart-plot-test--straddle
  '((90 50) (95 0) (100 -100) (105 0) (110 50)))

(defun financial-chart-plot-test--example (kind)
  "The example data of KIND's shape."
  (plist-get (alist-get (plist-get (financial-chart--kind kind) :shape) financial-chart-shapes)
             :example))

;; --- pure helpers ---------------------------------------------------------------

(ert-deftest financial-chart-plot-test-series-values-accepts-every-shape ()
  (should (equal (financial-chart-series-values '(1 2 3)) '(1 2 3)))
  (should (equal (financial-chart-series-values '((a 1) (b 2))) '(1 2)))
  (should (equal (financial-chart-series-values '((a . 1) (b . 2))) '(1 2)))
  (should (equal (financial-chart-series-values [4 5]) '(4 5)))
  (should (equal (financial-chart-series-values '((a 1) (b nil) (c 3))) '(1 3)))
  (should (equal (financial-chart-series-xs '((a 1) (b nil) (c 3))) '(a c))))

(ert-deftest financial-chart-plot-test-fmt ()
  (should (equal (financial-chart-fmt 89.3333) "89.3"))
  (should (equal (financial-chart-fmt 89.0) "89"))
  (should (equal (financial-chart-fmt 0.25) "0.2")))

(ert-deftest financial-chart-plot-test-breakevens ()
  (should (equal (financial-chart-payoff-breakevens financial-chart-plot-test--payoff) '(102.5)))
  (should (equal (financial-chart-payoff-breakevens financial-chart-plot-test--straddle) '(95 105)))
  (should (equal (financial-chart-payoff-breakevens '((1 -5) (2 5))) '(1.5)))
  (should (null (financial-chart-payoff-breakevens '((1 1) (2 2))))))

(ert-deftest financial-chart-plot-test-ohlc-closes ()
  (should (equal (financial-chart-ohlc-closes '((:open 1 :high 2 :low 0 :close 1.5 :time 10)))
                 '((10 1.5)))))

;; --- every kind through eas ----------------------------------------------------

(ert-deftest financial-chart-plot-test-every-kind-is-an-eas-template ()
  (dolist (entry financial-chart-kinds)
    (let* ((kind (car entry))
           (data (financial-chart-plot-test--example kind))
           (text (financial-chart-plot kind data :backend 'text :width 50 :height 10))
           (svg (financial-chart-plot kind data :backend 'svg)))
      (should (eas-template-get (plist-get (cdr entry) :template)))
      (should (equal text (financial-chart-eas-render kind data 'text :width 50 :height 10)))
      ;; eas text keeps the datum behind each cell
      (should (text-property-not-all 0 (length text) 'eas-datum nil text))
      (should (string-prefix-p "<svg" svg))
      (should (string-match-p (format "<desc>financial-chart %s: " kind) svg)))))

(ert-deftest financial-chart-plot-test-render-dispatches-by-backend ()
  (should-not (string-prefix-p "<svg" (financial-chart-plot 'area '(1 2 3) :backend 'text)))
  (should (string-match-p "width=\"320\""
                          (financial-chart-plot 'area '(1 2 3) :backend 'svg :pixel-width 320)))
  (should-error (financial-chart-plot 'pie '(1 2)) :type 'financial-chart-unknown-kind))

(ert-deftest financial-chart-plot-test-svg-is-well-formed ()
  (when (fboundp 'libxml-parse-xml-region)
    (dolist (kind '(area payoff bars ohlc))
      (with-temp-buffer
        (insert (financial-chart-plot kind (financial-chart-plot-test--example kind) :backend 'svg
                                      :title "a <b> & c"))
        (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max)))))))))

(ert-deftest financial-chart-plot-test-props-become-slots ()
  (let ((bindings (financial-chart-eas-bindings 'area '((1700000000000 1) (1700086400000 2))
                                                '(:unit "$" :scale log :title "t"))))
    (should (equal (plist-get bindings :x_type) "temporal"))
    (should (equal (plist-get bindings :y_title) "$"))
    (should (equal (plist-get bindings :scale) "log"))
    (should (equal (plist-get bindings :title) "t")))
  (should (equal (plist-get (financial-chart-eas-bindings 'multi '(("A" . (1 2))) '(:normalize 100))
                            :normalize)
                 100))
  (should (string-match-p "font-family=\"Hack\""
                          (financial-chart-plot 'area '(1 2 3) :backend 'svg :font "Hack"))))

(ert-deftest financial-chart-plot-test-log-scale-needs-positive-values ()
  (should (stringp (financial-chart-plot 'line '(1 10 100) :backend 'text :scale 'log)))
  (let ((err (should-error (financial-chart-plot 'area '(1 0 3) :backend 'text :scale 'log)
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "nonpositive_log"))
    (should (= (plist-get (cddr err) :index) 1)))
  (should-error (financial-chart-validate 'area '(1 2) :scale 'cubic)
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-plot-test-sparkline ()
  (let ((spark (financial-chart-sparkline '(1 5 3 8 2 9) :width 12)))
    (should (stringp spark))
    (should (<= (apply #'max (mapcar #'string-width (split-string spark "\n"))) 12)))
  (should (equal (financial-chart-sparkline nil) "")))

;; --- candlestick entry points ---------------------------------------------------

(ert-deftest financial-chart-plot-test-candlestick-entry-points-are-the-ohlc-kind ()
  (let ((bars (financial-chart-plot-test--example 'ohlc))
        (file (make-temp-file "financial-chart-test" nil ".svg")))
    (unwind-protect
        (progn
          (should (equal (financial-chart-render bars 12 60)
                         (financial-chart-plot 'ohlc bars :backend 'text :height 12 :width 60)))
          (should (equal (financial-chart-render-svg bars "TSM")
                         (financial-chart-plot 'ohlc bars :backend 'svg :title "TSM")))
          (should (equal (financial-chart-export-svg bars file "TSM") file))
          (should (string-match-p "<title>TSM</title>"
                                  (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (delete-file file))
    (let ((buf (financial-chart-view bars "TSM" 12)))
      (unwind-protect
          (with-current-buffer buf
            (should (eq (car financial-chart-plot--spec) 'ohlc))
            (should (string-prefix-p "TSM\n\n" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest financial-chart-plot-test-export-png-uses-the-converter ()
  (let* ((bars (financial-chart-plot-test--example 'ohlc))
         (png (make-temp-file "financial-chart-test" nil ".png"))
         seen
         (financial-chart-png-converter
          (lambda (svg-file png-file)
            (setq seen (with-temp-buffer (insert-file-contents svg-file) (buffer-string)))
            (with-temp-file png-file (insert "png")))))
    (unwind-protect
        (progn
          (should (equal (financial-chart-export-png bars png "TSM") png))
          (should (string-match-p "<title>TSM</title>" seen)))
      (delete-file png))))

;; --- insert and the plot buffer ---------------------------------------------------

(ert-deftest financial-chart-plot-test-insert-auto-is-text-without-images ()
  (unless (display-images-p)
    (with-temp-buffer
      (financial-chart-plot-insert 'area '(1 2 3) :width 20 :height 6)
      (should (text-property-not-all (point-min) (point-max) 'eas-datum nil)))
    (with-temp-buffer
      (financial-chart-plot-insert 'area nil)
      (should (equal (buffer-string) "no data")))))

(ert-deftest financial-chart-plot-test-insert-svg-falls-back-without-svg-support ()
  "An Emacs built without SVG images gets the text chart plus a note."
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil)))
    (with-temp-buffer
      (financial-chart-plot-insert 'area '(1 2 3) :backend 'svg :width 20 :height 6)
      (should (string-prefix-p "(this Emacs cannot display SVG" (buffer-string)))
      (should (text-property-not-all (point-min) (point-max) 'eas-datum nil)))))

(ert-deftest financial-chart-plot-test-view-and-toggle ()
  (let ((buf (financial-chart-plot-view 'payoff financial-chart-plot-test--straddle
                                        :title "straddle" :buffer "*financial-chart-test*"
                                        :width 40 :height 8 :backend 'text)))
    (unwind-protect
        (with-current-buffer buf
          (should (derived-mode-p 'financial-chart-plot-mode))
          (should (string-prefix-p "straddle\n\n" (buffer-string)))
          (let ((pos (text-property-not-all (point-min) (point-max) 'help-echo nil)))
            (goto-char pos)
            (financial-chart-plot--inspect-point)
            (should (equal financial-chart-plot--last-inspected-point
                           (get-text-property pos 'help-echo))))
          (financial-chart-plot-toggle-backend)
          (should (eq (plist-get (nth 2 financial-chart-plot--spec) :backend) 'svg)))
      (kill-buffer buf))))

(ert-deftest financial-chart-plot-test-zoom-slices-series-and-resets ()
  (let* ((series (cl-loop for i from 0 below 20 collect (list i i)))
         (buffer (financial-chart-plot-view 'area series :backend 'text :width 40 :height 8
                                            :buffer "*financial-chart-zoom*")))
    (unwind-protect
        (with-current-buffer buffer
          (financial-chart-plot-zoom-in)
          (should (= (cdr financial-chart-plot--zoom-window) (length series)))
          (financial-chart-plot-zoom-reset)
          (should-not financial-chart-plot--zoom-window)
          ;; zoom anchors on the datum at point
          (goto-char (point-min))
          (let (anchor)
            (while (and (not anchor) (< (point) (point-max)))
              (let ((row (get-text-property (point) 'eas-datum)))
                (if (and (integerp row) (<= 5 row 9)) (setq anchor row) (forward-char 1))))
            (should anchor)
            (should (= (financial-chart-plot--index-at-point) anchor))
            (financial-chart-plot-zoom-in)
            (let ((window financial-chart-plot--zoom-window))
              (should (< (- (cdr window) (car window)) (length series)))
              (should (<= (car window) anchor (1- (cdr window))))
              (should (= (length (nth 1 financial-chart-plot--spec)) (length series)))
              (financial-chart-plot-zoom-out)
              (should (> (- (cdr financial-chart-plot--zoom-window)
                            (car financial-chart-plot--zoom-window))
                         (- (cdr window) (car window))))))
          (financial-chart-plot-zoom-reset)
          (should (equal (car (financial-chart-plot--visible-data 'area series)) series)))
      (kill-buffer buffer))))

(ert-deftest financial-chart-plot-test-refresh-is-direct-and-timer-is-gated ()
  (let ((calls 0)
        (fresh '((0 10) (1 20) (2 30)))
        buffer timer)
    (setq buffer
          (financial-chart-plot-view
           'area '((0 1) (1 2))
           :backend 'text :width 30 :height 6
           :buffer "*financial-chart-refresh*"
           :refresh-fn (lambda () (setq calls (1+ calls)) fresh)
           :refresh-interval 3600))
    (unwind-protect
        (with-current-buffer buffer
          (setq timer financial-chart-plot--refresh-timer)
          (should (timerp timer))
          (should (= calls 0))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
            (financial-chart-plot--timer-refresh buffer))
          (should (= calls 0))
          (should (equal (financial-chart-plot-refresh-data) fresh))
          (should (= calls 1))
          (should (equal (nth 1 financial-chart-plot--spec) fresh))
          (financial-chart-plot-toggle-refresh)
          (should-not financial-chart-plot--refresh-enabled)
          (financial-chart-plot-toggle-refresh)
          (should financial-chart-plot--refresh-enabled)
          (setq timer financial-chart-plot--refresh-timer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (should-not (memq timer timer-list))))

(ert-deftest financial-chart-plot-test-refresh-options-are-paired ()
  (should-error (financial-chart-plot-view 'area '(1 2) :refresh-interval 10)
                :type 'financial-chart-error)
  (should-error (financial-chart-plot-view 'area '(1 2) :refresh-fn #'identity)
                :type 'financial-chart-error))

(ert-deftest financial-chart-plot-test-demo-renders-every-kind ()
  (let ((buf (financial-chart-demo)))
    (unwind-protect
        (with-current-buffer buf
          (dolist (k '("area" "line" "payoff" "bars" "sparkline"))
            (should (string-match-p k (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest financial-chart-plot-test-doctor-checks-pass ()
  (dolist (c (financial-chart-plot-doctor-checks))
    (should (stringp (plist-get c :name)))
    (should (memq (plist-get c :status) '(pass skip)))))

(provide 'financial-chart-plot-test)
;;; financial-chart-plot-test.el ends here
