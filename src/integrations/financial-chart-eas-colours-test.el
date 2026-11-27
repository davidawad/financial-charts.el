;;; financial-chart-eas-colours-test.el --- distinct overlay colours and legend entries -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty.23, on the synthetic example bars.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defun financial-chart-colours-test--legend (spec pane)
  "The legend (LABELS . COLOURS) of SPEC's PANE: the scale of its series layers."
  (let ((scale (cl-loop for l across (plist-get (aref (plist-get spec :vconcat) pane) :layer)
                        for c = (plist-get (plist-get l :encoding) :color)
                        thereis (and (plist-get c :datum) (plist-get c :scale)))))
    (cons (append (plist-get scale :domain) nil) (append (plist-get scale :range) nil))))

(defun financial-chart-colours-test--group (label)
  "A legend LABEL's overlay: Bollinger and Ichimoku parts share one."
  (cond ((string-prefix-p "Bollinger" label) "bollinger")
        ((member label '("Tenkan-sen" "Kijun-sen" "Chikou Span" "Senkou Span A" "Senkou Span B")) label)
        (t label)))

(ert-deftest financial-chart-colours-near-and-pick ()
  (should (financial-chart-colours-near-p "#00897b" "#26a69a"))
  (should-not (financial-chart-colours-near-p "#2962ff" "#ff9800"))
  (should (equal (financial-chart-colours-pick '("#2962ff")) "#ff9800"))
  (should (equal (financial-chart-colours-pick nil) (car financial-chart-palette))))

(ert-deftest financial-chart-colours-pick-farthest-when-palette-is-spent ()
  "fc-2u6: gruvbox's twelve colours are all close to one of Ichimoku's
five, so an anchored VWAP took the first unused one, an orange beside
Chikou's.  It takes the colour farthest from every colour in use."
  (let ((financial-chart-palette '("#83a598" "#fe8019" "#d3869b" "#8ec07c" "#fabd2f" "#bdae93"
                                   "#458588" "#d65d0e" "#b16286" "#689d6a" "#d79921" "#a89984"))
        (used '("#83a598" "#d3869b" "#d65d0e" "#b8bb26" "#fb4934")))
    (let ((c (financial-chart-colours-pick used)))
      (should-not (member c used))
      (should-not (equal c "#fe8019"))
      (should (equal c "#458588")))))

(ert-deftest financial-chart-colours-overlays-are-distinct ()
  (let* ((bars (plist-get (financial-chart-catalog-example "fibonacci") :bars))
         (spec (financial-chart-compose
                (list :bars bars :price '(:style "candles" :studies ["ichimoku" "bollinger"]
                                          :series [(:indicator "sma" :params [7])]))))
         (legend (financial-chart-colours-test--legend spec 0)))
    (should (member "Bollinger Upper" (car legend)))
    ;; Different overlays never share a colour or a near one.
    (cl-loop for (la . rest) on (cl-mapcar #'cons (car legend) (cdr legend))
             do (dolist (lb rest)
                  (unless (equal (financial-chart-colours-test--group (car la))
                                 (financial-chart-colours-test--group (car lb)))
                    (should (equal (list (car la) (car lb) nil)
                                   (list (car la) (car lb) (financial-chart-colours-near-p (cdr la) (cdr lb))))))))
    ;; The Bollinger channel fill follows its new colour.
    (let ((bollinger (cdr (assoc "Bollinger Upper" (cl-mapcar #'cons (car legend) (cdr legend))))))
      (should (cl-loop for l across (plist-get (aref (plist-get spec :vconcat) 0) :layer)
                       thereis (equal (plist-get (plist-get l :mark) :color) bollinger))))))

(ert-deftest financial-chart-colours-anchored-vwap-joins-the-legend ()
  (let* ((bars (plist-get (financial-chart-catalog-example "fibonacci") :bars))
         (spec (financial-chart-compose
                (list :bars bars :preset "trend"
                      :price '(:annotations [(:type "anchored-vwap" :anchor "low" :label "AVWAP low")]))))
         (legend (cl-mapcar #'cons (car (financial-chart-colours-test--legend spec 0))
                            (cdr (financial-chart-colours-test--legend spec 0))))
         (avwap (cdr (assoc "AVWAP low" legend))))
    (should avwap)
    (dolist (other (remove (assoc "AVWAP low" legend) legend))
      (should-not (financial-chart-colours-near-p avwap (cdr other))))
    ;; Its label is drawn in the same colour; no private keys reach the spec.
    (let ((layers (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil)))
      (should (cl-some (lambda (l) (and (equal (plist-get (plist-get l :mark) :type) "text")
                                        (equal (plist-get (plist-get l :mark) :color) avwap)))
                       layers))
      (should-not (cl-some (lambda (l) (or (plist-member l :legend) (plist-member l :legend-member)
                                           (plist-member l :label-pin)))
                           layers)))
    ;; A colour given by hand is kept.
    (let* ((spec (financial-chart-compose
                  (list :bars bars :price '(:annotations [(:type "anchored-vwap" :label "A" :color "#000000")]))))
           (legend (cl-mapcar #'cons (car (financial-chart-colours-test--legend spec 0))
                              (cdr (financial-chart-colours-test--legend spec 0)))))
      (should (equal (cdr (assoc "A" legend)) "#000000")))))

(provide 'financial-chart-eas-colours-test)
;;; financial-chart-eas-colours-test.el ends here
