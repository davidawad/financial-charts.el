;;; financial-chart-eas-readout-test.el --- the financial hover readouts -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gy6: every financial template declares an x-eas.readout built
;; from this package's eas components.  Hovering changes only the
;; reserved readout line; the readout is one line at 80 columns for
;; every template; up is green and down red, from the props, else the
;; theme; open, high and low are hidden on line styles; volume is
;; abbreviated.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-mode)

(defconst financial-chart-readout-test--templates
  '("ohlc" "panes" "depth" "payoff" "payoff-curves" "drawdown" "diverging-bars"
    "volume-profile" "ladder" "depth-live")
  "Every template this package ships.")

(defconst financial-chart-readout-test--up '(:time "2026-08-24" :open 100 :high 104 :low 99 :close 102.5 :volume 1234567))
(defconst financial-chart-readout-test--down '(:time "2026-08-25" :open 102.5 :high 103 :low 97 :close 98 :volume 950))

(defun financial-chart-readout-test--bindings (template)
  "Example bindings of TEMPLATE; ohlc with overlays, an oscillator and volume."
  (let ((b (eas-template-example template)))
    (if (equal template "ohlc")
        (append '(:indicators [(:name "sma" :params [2]) (:name "ema" :params [3])] :oscillators [(:name "rsi" :params [3])]
                  :volume t)
                b)
      b)))

(defun financial-chart-readout-test--sources ()
  "(NAME . SOURCE) of every template and two composed charts, with bindings."
  (append (mapcar (lambda (tpl) (list tpl tpl (financial-chart-readout-test--bindings tpl)))
                  financial-chart-readout-test--templates)
          (mapcar (lambda (style) (list (concat "compose-" style)
                                        (financial-chart-compose (financial-chart-compose-example style)) nil))
                  '("candles" "line"))))

(defmacro financial-chart-readout-test--each (var &rest body)
  "Run BODY with VAR bound to a fresh live view of each source."
  (declare (indent 1))
  `(let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t) (eas-action-inhibit t))
     (dolist (source (financial-chart-readout-test--sources))
       (let ((,var (eas-view-open (nth 1 source) :bindings (nth 2 source) :id (car source))))
         ,@body))))

(defun financial-chart-readout-test--line (node datum &rest ctx)
  "NODE rendered on DATUM in CTX and fitted to one line of 80 columns: its spans."
  (car (eas-component-fit (eas-component-render node (append (list :datum datum :path "") ctx)) 80 1)))

(defun financial-chart-readout-test--text (spans)
  "SPANS' text."
  (mapconcat #'car spans ""))

(defun financial-chart-readout-test--style (spans text)
  "The style of the span of SPANS whose text is TEXT."
  (cdr (cl-find text spans :key #'car :test #'equal)))

(defun financial-chart-readout-test--split (buffer)
  "BUFFER's text as (CHART . READOUT)."
  (with-current-buffer buffer
    (let ((beg (text-property-any (point-min) (point-max) 'eas-strip t)))
      (cons (buffer-substring (point-min) beg) (buffer-substring-no-properties beg (point-max))))))

;;; Registration and templates

(ert-deftest financial-chart-readout-components-are-registered ()
  (dolist (name financial-chart-readout-components)
    (should (eas-component-get name))))

(ert-deftest financial-chart-readout-every-template-declares-a-valid-readout ()
  (financial-chart-readout-test--each v
    (should (eq (eas-readout-validate v) t))
    (let ((tree (format "%S" (eas-readout--declared v :readout))))
      (should (cl-some (lambda (c) (string-search (format "%S" c) tree)) financial-chart-readout-components)))
    (should (= (eas-readout-max-lines v) 1))))

(ert-deftest financial-chart-readout-a-fault-names-its-path ()
  (let ((err (should-error (eas-component-validate '(:component "ohlc-readout" :props (:basis "close")) "/x-eas/readout")
                           :type 'eas-error)))
    (should (equal (plist-get (eas-error-plist err) :path) "/x-eas/readout/props/basis"))))

;;; No layout change on hover

(ert-deftest financial-chart-readout-hover-changes-no-other-cell ()
  "Hovering random cells of every template changes only the readout line."
  (financial-chart-readout-test--each v
    (let ((buffer (eas-show v 'text)))
      (unwind-protect
          (with-current-buffer buffer
            (random (format "fc-gy6-%s" (eas-view-id v)))
            (let* ((widths (lambda () (mapcar #'string-width (split-string (buffer-string) "\n"))))
                   (start (financial-chart-readout-test--split buffer))
                   (layout (funcall widths))
                   (readout-line (line-number-at-pos (text-property-any (point-min) (point-max) 'eas-strip t)))
                   (cols (plist-get (eas-view-size v) :cols))
                   (chart-end (length (car start)))
                   (hovers 0) (readouts nil))
              (dotimes (_ 40)
                (goto-char (1+ (random chart-end)))
                (eas-mode--post-command)
                (when (plist-get (eas-view-state v) :hover) (cl-incf hovers))
                (let ((now (financial-chart-readout-test--split buffer)))
                  ;; The chart keeps its lines, each within the chart's
                  ;; columns (the crosshair redraws inside the grid);
                  ;; the readout stays on its own last line.
                  (should (= (length (funcall widths)) (length layout)))
                  (should (cl-every (lambda (w) (<= w cols)) (funcall widths)))
                  (should (= (line-number-at-pos (text-property-any (point-min) (point-max) 'eas-strip t))
                             readout-line))
                  (should-not (string-search "\n" (cdr now)))
                  (should (<= (string-width (cdr now)) cols))
                  (should-not (string-search "readout:" (cdr now)))
                  (push (cdr now) readouts)))
              (should (> hovers 0))
              (should (> (length (delete-dups readouts)) 1))))
        (kill-buffer buffer)))))

;; Without a crosshair, hovering draws nothing in the chart: every cell
;; above the readout, properties too, stays as it was.
(ert-deftest financial-chart-readout-hover-changes-no-cell-without-a-crosshair ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t) (eas-action-inhibit t))
    (dolist (tpl '("ohlc" "diverging-bars" "payoff" "drawdown" "depth" "ladder" "volume-profile"))
      (let* ((bindings (if (equal tpl "ohlc") (append '(:crosshair :false :volume t) (eas-template-example tpl))
                         (eas-template-example tpl)))
             (v (eas-view-open tpl :bindings bindings :id (concat "strict-" tpl)))
             (buffer (eas-show v 'text)))
        (unwind-protect
            (with-current-buffer buffer
              (random (format "fc-gy6-strict-%s" tpl))
              (let* ((start (car (financial-chart-readout-test--split buffer))) (hovers 0))
                (dotimes (_ 40)
                  (goto-char (1+ (random (length start))))
                  (eas-mode--post-command)
                  (when (plist-get (eas-view-state v) :hover) (cl-incf hovers))
                  (should (equal-including-properties (car (financial-chart-readout-test--split buffer)) start)))
                (should (> hovers 0))))
          (kill-buffer buffer))))))

;;; One line at 80 columns

(defun financial-chart-readout-test--rows (view)
  "Up to 60 rows VIEW's scene draws: every datum the readout may read."
  (let (rows)
    (seq-doseq (pane (plist-get (eas-view-scene view) :views))
      (seq-doseq (mark (plist-get pane :marks))
        (seq-doseq (row (plist-get mark :rows)) (push row rows))))
    (seq-take (nreverse rows) 60)))

(ert-deftest financial-chart-readout-one-line-at-80-columns-for-every-template ()
  (financial-chart-readout-test--each v
    (dolist (at '("cursor" "latest"))
      (dolist (row (cons nil (financial-chart-readout-test--rows v)))
        (let* ((ctx (append (list :datum (and row (eas--plist-without row eas-params-row-key))
                                  :env (list :at at :hovered (if row t :false) :view (eas-view-id v)))
                            (eas-readout-context v)))
               (lines (eas-readout-render v 80 nil (eas-readout-atoms v nil ctx)))
               (text (financial-chart-readout-test--text (car lines))))
          (should (= (length lines) 1))
          (should (<= (string-width text) 80))
          (should-not (string-search "readout:" text)))))))

;;; Colour rules

(ert-deftest financial-chart-readout-up-is-green-and-down-red ()
  (let* ((node '(:component "ohlc-readout" :props (:up "green" :down "red")))
         (up (financial-chart-readout-test--line node financial-chart-readout-test--up))
         (down (financial-chart-readout-test--line node financial-chart-readout-test--down)))
    (should (equal (financial-chart-readout-test--text up)
                   "Aug 24, 2026  O 100.00 H 104.00 L 99.00 C 102.50 +2.50 (+2.50%) Vol 1.2M"))
    (dolist (text '("102.50" "+2.50" "(+2.50%)"))
      (should (equal (plist-get (financial-chart-readout-test--style up text) :color) "green")))
    (dolist (text '("98.00" "-4.50" "(-4.39%)"))
      (should (equal (plist-get (financial-chart-readout-test--style down text) :color) "red")))
    ;; Open, high and low are never coloured.
    (should-not (plist-get (financial-chart-readout-test--style up "100.00") :color))))

(ert-deftest financial-chart-readout-colours-follow-the-theme ()
  (let* ((theme '(:financial (:up "#00aa00" :down "#cc0000" :bid "#0000ff")))
         (node '(:component "ohlc-readout"))
         (up (financial-chart-readout-test--line node financial-chart-readout-test--up :theme theme))
         (down (financial-chart-readout-test--line node financial-chart-readout-test--down :theme theme))
         (plain (financial-chart-readout-test--line node financial-chart-readout-test--up)))
    (should (equal (plist-get (financial-chart-readout-test--style up "102.50") :color) "#00aa00"))
    (should (equal (plist-get (financial-chart-readout-test--style down "98.00") :color) "#cc0000"))
    ;; No theme: the palette's up colour.
    (should (equal (plist-get (financial-chart-readout-test--style plain "102.50") :color)
                   financial-chart-palette-up))
    ;; A prop beats the theme.
    (let ((given (financial-chart-readout-test--line '(:component "ohlc-readout" :props (:up "#123456"))
                                                     financial-chart-readout-test--up :theme theme)))
      (should (equal (plist-get (financial-chart-readout-test--style given "102.50") :color) "#123456")))
    ;; Bids follow financial.bid; asks fall back to the down colour.
    (let ((bid (financial-chart-readout-test--line '(:component "order-book-level")
                                                   '(:side "bid" :price 100.5 :price_label "100.50" :size 4 :level 1)
                                                   :theme theme))
          (ask (financial-chart-readout-test--line '(:component "order-book-level")
                                                   '(:side "ask" :price 101 :price_label "101.00" :size 2500 :level 0)
                                                   :theme theme)))
      (should (equal (financial-chart-readout-test--text bid) "BID  100.50  × 4  L 1"))
      (should (equal (plist-get (financial-chart-readout-test--style bid "100.50") :color) "#0000ff"))
      (should (equal (plist-get (financial-chart-readout-test--style bid "4") :color) "#0000ff"))
      (should (equal (plist-get (financial-chart-readout-test--style ask "2.5K") :color) "#cc0000")))))

(ert-deftest financial-chart-readout-change-basis ()
  (let* ((d (append '(:prev_close 105) financial-chart-readout-test--up))
         (open (financial-chart-readout-test--line '(:component "ohlc-readout" :props (:basis "open")) d))
         (prev (financial-chart-readout-test--line '(:component "ohlc-readout" :props (:basis "previous-close")) d)))
    (should (string-search "+2.50 (+2.50%)" (financial-chart-readout-test--text open)))
    (should (string-search "-2.50 (-2.38%)" (financial-chart-readout-test--text prev)))
    (should (equal (plist-get (financial-chart-readout-test--style prev "102.50") :color) financial-chart-palette-down))
    ;; No previous close (the first bar): no change, close uncoloured.
    (let ((first (financial-chart-readout-test--line '(:component "ohlc-readout" :props (:basis "previous-close"))
                                                     financial-chart-readout-test--up)))
      (should-not (string-search "%" (financial-chart-readout-test--text first)))
      (should-not (plist-get (financial-chart-readout-test--style first "102.50") :color)))))

(ert-deftest financial-chart-readout-signed-values ()
  (let ((gain (financial-chart-readout-test--line '(:component "signed-value" :props (:field "pnl" :label "P/L"))
                                                  '(:pnl 12)))
        (loss (financial-chart-readout-test--line '(:component "signed-value"
                                                    :props (:field "drawdown" :format "percent" :sign :false))
                                                  '(:drawdown -0.0375))))
    (should (equal (financial-chart-readout-test--text gain) "P/L +12.00"))
    (should (equal (plist-get (financial-chart-readout-test--style gain "+12.00") :color) financial-chart-palette-up))
    (should (equal (financial-chart-readout-test--text loss) "drawdown -3.75%"))
    (should (equal (plist-get (financial-chart-readout-test--style loss "-3.75%") :color) financial-chart-palette-down))))

;;; Hidden fields, abbreviation, bold pane

(ert-deftest financial-chart-readout-hides-fields-that-do-not-apply ()
  (dolist (style financial-chart-readout-line-styles)
    (let ((text (financial-chart-readout-test--text
                 (financial-chart-readout-test--line (list :component "ohlc-readout" :props (list :style style))
                                                     financial-chart-readout-test--up))))
      (should-not (string-match-p "\\_<[OHL] " text))
      (should (string-search "C 102.50" text))))
  (dolist (style '("candles" "hollow" "ohlc" "heikin-ashi"))
    (should (string-search "O 100.00 H 104.00 L 99.00"
                           (financial-chart-readout-test--text
                            (financial-chart-readout-test--line (list :component "ohlc-readout" :props (list :style style))
                                                                financial-chart-readout-test--up)))))
  ;; Fields the datum lacks, and fields not asked for, are hidden.
  (let ((text (financial-chart-readout-test--text
               (financial-chart-readout-test--line '(:component "ohlc-readout" :props (:fields ["close" "volume"]))
                                                   '(:time "2026-08-24" :open 1 :close 2)))))
    (should (equal text "C 2.00")))
  (should (equal (financial-chart-readout-test--text
                  (financial-chart-readout-test--line '(:component "indicator-values"
                                                        :props (:series [(:field "sma" :label "SMA") (:field "rsi")]))
                                                      '(:close 1 :sma :null :rsi 55.123)))
                 "rsi 55.12")))

(ert-deftest financial-chart-readout-abbreviates-volume ()
  (dolist (case '((1234567 "1.2M") (999 "999") (12345678 "12.3M") (123456789 "123M")
                  (1.5e9 "1.5B") (-2500 "-2.5K") (2e12 "2.0T") (0 "0")))
    (should (equal (financial-chart-readout-abbreviate (car case)) (cadr case))))
  (should (string-search "Vol 950" (financial-chart-readout-test--text
                                    (financial-chart-readout-test--line '(:component "ohlc-readout")
                                                                        financial-chart-readout-test--down))))
  (should (string-search "Vol 1,234,567"
                         (financial-chart-readout-test--text
                          (financial-chart-readout-test--line '(:component "ohlc-readout" :props (:volume_format "number"))
                                                              financial-chart-readout-test--up)))))

(ert-deftest financial-chart-readout-bold-in-the-pane-under-the-cursor ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t) (eas-action-inhibit t))
    (let* ((v (eas-view-open (financial-chart-compose (financial-chart-compose-example "candles")) :id "bold"))
           (d (append '(:sma 101.5 :rsi 48.2) financial-chart-readout-test--up))
           (series '(:component "indicator-values"
                     :props (:series [(:field "sma" :label "SMA" :color "#2962ff" :pane "price")
                                      (:field "rsi" :label "RSI" :color "#9c27b0" :pane "pane-2")])))
           (line (lambda (node pane)
                   (setf (eas-view-state v) (list :hover (list :view pane :row d)))
                   (financial-chart-readout-test--line node d :view v))))
      (let ((in-rsi (funcall line series "pane-2")) (in-price (funcall line series "price")))
        (should (equal (financial-chart-readout-test--style in-rsi "48.20") '(:bold t :color "#9c27b0")))
        (should (equal (financial-chart-readout-test--style in-rsi "101.50") '(:color "#2962ff")))
        (should (equal (financial-chart-readout-test--style in-price "101.50") '(:bold t :color "#2962ff"))))
      (let ((in-volume (funcall line '(:component "ohlc-readout") "volume"))
            (in-price (funcall line '(:component "ohlc-readout") "price")))
        (should (plist-get (financial-chart-readout-test--style in-volume "1.2M") :bold))
        (should-not (plist-get (financial-chart-readout-test--style in-volume "102.50") :bold))
        (should (plist-get (financial-chart-readout-test--style in-price "102.50") :bold))))))

(ert-deftest financial-chart-readout-indicator-colours-come-from-the-scene ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t) (eas-action-inhibit t))
    (let* ((v (eas-view-open "ohlc" :bindings (financial-chart-readout-test--bindings "ohlc") :id "colours"))
           (series (financial-chart-readout--scene-series v)))
      (should (equal (mapcar #'car series) '("sma" "ema" "rsi")))
      (should (equal (nth 1 (assoc "ema" series)) "price"))
      (should-not (equal (nth 1 (assoc "rsi" series)) "price"))
      (should (stringp (nth 2 (assoc "ema" series))))
      (should-not (equal (nth 2 (assoc "ema" series)) (nth 2 (assoc "rsi" series)))))))

;;; Composed charts

(ert-deftest financial-chart-readout-composed-charts-carry-theirs ()
  (let* ((chart (financial-chart-compose-example "line"))
         (spec (financial-chart-compose chart))
         (readout (plist-get (plist-get spec :x-eas) :readout))
         (ohlc (aref (plist-get readout :children) 1)))
    (should (equal (plist-get ohlc :component) "ohlc-readout"))
    (should (equal (plist-get (plist-get ohlc :props) :style) "line"))
    (should (= (plist-get readout :max_lines) 1))
    (should (eq (eas-component-validate (eas--plist-without readout :max_lines) "/x-eas/readout") t))
    ;; A chart's own "readout" replaces it: users swap components.
    (let ((mine '(:component "row" :children [(:component "ohlc-readout" :props (:fields ["close"]))])))
      (should (equal (plist-get (plist-get (financial-chart-compose (append (list :readout mine) chart)) :x-eas)
                                :readout)
                     mine)))))

(provide 'financial-chart-eas-readout-test)
;;; financial-chart-eas-readout-test.el ends here
