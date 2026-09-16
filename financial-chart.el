;;; financial-chart.el --- OHLC candlestick charts, rendered in-buffer -*- lexical-binding: t; -*-

;; Author: David Awad
;; Keywords: comm, tools, finance

;;; Commentary:

;; Pure-Elisp candlestick chart renderer: takes a list of OHLC bars and
;; renders them as a half-block-resolution unicode candlestick chart
;; directly in an Emacs buffer -- no gnuplot, no image/PNG generation,
;; no external process. Built entirely on Emacs's own display engine
;; (`propertize' faces for up/down coloring, unicode box-drawing and
;; block characters for body/wick glyphs).
;;
;; A candle's body (open/close) and wick (high/low) are two independent
;; ranges per column -- this is why a generic single-series sparkline
;; renderer (bar height from a scalar value) can't be reused for OHLC
;; data without real per-row body/wick overlap logic; that logic is
;; this file's `financial-chart--glyph'.
;;
;; Data-source agnostic: `financial-chart-render'/`financial-chart-view'
;; take a plain list of (:open :high :low :close) plists, oldest first.
;; The Schwab bridge at the bottom is soft-wired via `fboundp' so this
;; file never hard-requires schwab-broker.el -- callers who never load
;; it get a renderer that still works against any other bar source
;; (Alpaca, a CSV import, synthetic data for testing, ...).

;;; Code:

(defgroup financial-chart nil
  "OHLC candlestick chart rendering."
  :group 'tools)

(defcustom financial-chart-height 20
  "Number of character rows a rendered chart's body area uses."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-up-face 'success
  "Face used for up candles (close >= open)."
  :type 'face
  :group 'financial-chart)

(defcustom financial-chart-down-face 'error
  "Face used for down candles (close < open)."
  :type 'face
  :group 'financial-chart)

(defun financial-chart--bars-range (bars)
  "Return (LOW . HIGH), the price range spanning every bar's wick in BARS."
  (cons
   (apply #'min (mapcar (lambda (b) (plist-get b :low)) bars))
   (apply #'max (mapcar (lambda (b) (plist-get b :high)) bars))))

(defun financial-chart--row-bounds (min max height row)
  "Return (LOW . HIGH) price bounds for character ROW (0 = bottom row)."
  (let ((unit (/ (- max min) (float height))))
    (cons (+ min (* row unit)) (+ min (* (1+ row) unit)))))

(defun financial-chart--glyph
    (row-low row-high body-low body-high wick-low wick-high)
  "Return the glyph char for one character row given its price bounds.
ROW-LOW/ROW-HIGH bound this text row; BODY-LOW/BODY-HIGH bound the
candle body (min/max of open and close); WICK-LOW/WICK-HIGH bound the
full high/low range."
  (let* ((row-mid (/ (+ row-low row-high) 2.0))
         (overlap-low (max body-low row-low))
         (overlap-high (min body-high row-high)))
    (cond
     ((>= overlap-low overlap-high)
      (if (and (<= wick-low row-high) (>= wick-high row-low))
          ?│
        ?\s))
     ((and (<= overlap-low row-low) (>= overlap-high row-high))
      ?█)
     (t
      (if (>= (/ (+ overlap-low overlap-high) 2.0) row-mid)
          ?▀
        ?▄)))))

(defun financial-chart--candle-glyph (row-low row-high bar)
  "Return the glyph char for BAR at the text row bound by ROW-LOW/ROW-HIGH."
  (let* ((open (plist-get bar :open))
         (close (plist-get bar :close))
         (low (plist-get bar :low))
         (high (plist-get bar :high)))
    (financial-chart--glyph
     row-low row-high (min open close) (max open close) low high)))

(defun financial-chart--candle-face (bar)
  "Return the face BAR's candle should render in."
  (if (>= (plist-get bar :close) (plist-get bar :open))
      financial-chart-up-face
    financial-chart-down-face))

(defun financial-chart--axis-label (min max height row)
  "Return a right-aligned price label for ROW, or an empty string.
Labels the bottom row, the top row, and the vertical middle."
  (let ((mid (/ (1- height) 2)))
    (cond
     ((= row 0)
      (format "%7.2f " min))
     ((= row (1- height))
      (format "%7.2f " max))
     ((= row mid)
      (format "%7.2f " (/ (+ min max) 2.0)))
     (t
      (make-string 8 ?\s)))))

;;;###autoload
(defun financial-chart-render (bars &optional height)
  "Render BARS as a candlestick chart string, oldest bar first.
BARS is a list of (:open :high :low :close) plists. HEIGHT overrides
`financial-chart-height'."
  (unless bars
    (user-error "financial-chart-render: no bars to render"))
  (let* ((height (or height financial-chart-height))
         (range (financial-chart--bars-range bars))
         (min (car range))
         (max
          (if (= (car range) (cdr range))
              (1+ (cdr range))
            (cdr range))))
    (mapconcat (lambda (row)
                 (let ((bounds
                        (financial-chart--row-bounds
                         min max height row)))
                   (concat
                    (financial-chart--axis-label min max height row)
                    (mapconcat
                     (lambda (bar)
                       (propertize
                        (string
                         (financial-chart--candle-glyph
                          (car bounds) (cdr bounds) bar))
                        'face (financial-chart--candle-face bar)))
                     bars
                     ""))))
               (number-sequence (1- height) 0 -1)
               "\n")))

;;;###autoload
(defun financial-chart-view (bars &optional title height)
  "Pop a *financial-chart* buffer rendering BARS as candlesticks.
TITLE, if given, is inserted as a header line. HEIGHT overrides
`financial-chart-height'."
  (let ((buffer (get-buffer-create "*financial-chart*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when title
          (insert title "\n\n"))
        (insert (financial-chart-render bars height) "\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

;; -----------------------------------------------------------------------
;; Schwab bridge -- soft-wired, only usable once schwab-broker.el is loaded
;; (schwab-broker-price-history-sync's real return shape, confirmed against
;; its own test fixture: {symbol, empty, candles: [{open,high,low,close,
;; volume,datetime}, ...]}, datetime in epoch milliseconds).
;; -----------------------------------------------------------------------

(defun financial-chart--schwab-candle->bar (candle)
  "Map one Schwab /pricehistory CANDLE alist to a financial-chart bar plist."
  (list
   :open (alist-get 'open candle)
   :high (alist-get 'high candle)
   :low (alist-get 'low candle)
   :close (alist-get 'close candle)
   :volume (alist-get 'volume candle)
   :time (alist-get 'datetime candle)))

;;;###autoload
(defun financial-chart-schwab-view (symbol &rest keys)
  "Fetch SYMBOL's price history via schwab-broker and view as candlesticks.
KEYS is passed through to `schwab-broker-price-history-sync' verbatim
\(e.g. :period-type \"day\" :period 5 :frequency-type \"minute\"
:frequency 1 for 5 days of 1-minute bars)."
  (interactive (list (read-string "Symbol: ")))
  (unless (fboundp 'schwab-broker-price-history-sync)
    (user-error
     "schwab-broker not loaded -- financial-chart-schwab-view needs it"))
  (let* ((history
          (apply #'schwab-broker-price-history-sync symbol keys))
         (candles (append (alist-get 'candles history) nil))
         (bars
          (mapcar #'financial-chart--schwab-candle->bar candles)))
    (financial-chart-view bars
                          (format "%s"
                                  (or (alist-get 'symbol history)
                                      (upcase symbol))))))

(provide 'financial-chart)
;;; financial-chart.el ends here
