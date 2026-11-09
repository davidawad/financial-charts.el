;;; financial-chart-risk-profile.el --- Options risk-profile (P/L) chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The risk profile of an N-leg position: the at-expiry P/L line, split
;; at its breakevens into profit and loss regions, T+n curves priced with
;; Black-Scholes-Merton, the spot price and a marker per strike.  Drawn by
;; the `risk-profile' eas template (templates/financial/risk-profile.json)
;; as text, svg, or (through eas-output) kitty pixels.
;;
;; A POSITION is a plist
;;
;;   (:spot NUM :date "YYYY-MM-DD" :rate R :dividend-yield Q
;;    :range (LOW HIGH) :dates ("YYYY-MM-DD" ...) :legs (LEG ...))
;;
;; :date is the valuation date (T+0); :dates are further valuation dates
;; drawn as T+n curves (default: none).  :range defaults to spot +-15%.
;; A LEG is a plist, one of
;;
;;   (:type call|put :side long|short :qty CONTRACTS :strike K
;;    :expiry "YYYY-MM-DD" :price PREMIUM :iv V)   ; 100 shares/contract
;;   (:type stock    :side long|short :qty SHARES [:price ENTRY])
;;
;; An option leg needs :price or :iv; the missing one is derived at the
;; valuation date (premium from Black-Scholes, IV by bisection).
;;
;; Semantics follow options-profit.el's engine so that package can call
;; this template: the year is 365 days, an option is repriced at its own
;; remaining time and entry IV, and an expired leg is worth its intrinsic
;; value.  The at-expiry line is the position on the date of its last
;; option expiry, so every option is intrinsic there and the line is
;; piecewise linear with kinks only at the strikes.  Everything below the
;; data shape is pure: a position in, numbers out.  This file does not
;; depend on options-profit.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defconst financial-chart-risk-profile-multiplier 100
  "Shares per option contract.")

;;;; Dates

(defun financial-chart-risk-profile--days (date)
  "Absolute day number of ISO DATE string \"YYYY-MM-DD\"."
  (unless (and (stringp date)
               (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" date))
    (signal 'wrong-type-argument (list 'iso-date date)))
  (calendar-absolute-from-gregorian
   (list (string-to-number (match-string 2 date))
         (string-to-number (match-string 3 date))
         (string-to-number (match-string 1 date)))))

(defun financial-chart-risk-profile--years (from to)
  "Years from ISO date FROM to ISO date TO, never negative (365-day year)."
  (max 0.0 (/ (- (financial-chart-risk-profile--days to)
                 (financial-chart-risk-profile--days from))
              365.0)))

;;;; Black-Scholes-Merton (Merton 1973; Hull, ch. 15 and 17)

(defconst financial-chart-risk-profile--sqrt-2pi (sqrt (* 2 float-pi)))

(defun financial-chart-risk-profile--horner (x coeffs)
  "The polynomial with COEFFS (highest power first) at X."
  (let ((acc 0.0))
    (dolist (c coeffs acc)
      (setq acc (+ (* acc x) c)))))

(defun financial-chart-risk-profile-norm-cdf (x)
  "Standard normal cumulative distribution at X.
Hart's double-precision rational approximation as given in West (2005),
\"Better approximations to cumulative normal functions\"."
  (let* ((ax (abs x))
         (e (exp (* -0.5 ax ax)))
         (tail
          (cond
           ((> ax 37.0) 0.0)
           ((< ax 7.07106781186547)
            (/ (* e (financial-chart-risk-profile--horner
                     ax '(3.52624965998911e-02 0.700383064443688 6.37396220353165
                          33.912866078383 112.079291497871 221.213596169931
                          220.206867912376)))
               (financial-chart-risk-profile--horner
                ax '(8.83883476483184e-02 1.75566716318264 16.064177579207
                     86.7807322029461 296.564248779674 637.333633378831
                     793.826512519948 440.413735824752))))
           (t
            (let* ((b (+ ax 0.65))
                   (b (+ ax (/ 4.0 b)))
                   (b (+ ax (/ 3.0 b)))
                   (b (+ ax (/ 2.0 b)))
                   (b (+ ax (/ 1.0 b))))
              (/ (/ e b) 2.506628274631))))))
    (if (> x 0) (- 1.0 tail) tail)))

(defun financial-chart-risk-profile-intrinsic (kind s k &optional r q tt)
  "Intrinsic value of a KIND (`call' or `put') option on S with strike K.
With rates R, Q and years TT it is the discounted-forward intrinsic."
  (let* ((tt (or tt 0.0))
         (fs (* s (exp (- (* (or q 0.0) tt)))))
         (fk (* k (exp (- (* (or r 0.0) tt))))))
    (max 0.0 (if (eq kind 'call) (- fs fk) (- fk fs)))))

(defun financial-chart-risk-profile-bs-price (kind s k r q v tt)
  "Black-Scholes-Merton price of a KIND option: spot S, strike K, rate R,
dividend yield Q, volatility V (fraction), TT years to expiry.
At TT <= 0 or V <= 0 the price is the intrinsic value."
  (if (or (<= tt 0) (<= v 0) (<= s 0))
      (financial-chart-risk-profile-intrinsic kind s k (if (> tt 0) r 0.0) q (max tt 0.0))
    (let* ((sq (sqrt tt))
           (d1 (/ (+ (log (/ (float s) k)) (* (+ (- r q) (* 0.5 v v)) tt)) (* v sq)))
           (d2 (- d1 (* v sq)))
           (fs (* s (exp (- (* q tt)))))
           (fk (* k (exp (- (* r tt))))))
      (if (eq kind 'call)
          (- (* fs (financial-chart-risk-profile-norm-cdf d1))
             (* fk (financial-chart-risk-profile-norm-cdf d2)))
        (- (* fk (financial-chart-risk-profile-norm-cdf (- d2)))
           (* fs (financial-chart-risk-profile-norm-cdf (- d1))))))))

(defun financial-chart-risk-profile-implied-vol (kind price s k r q tt)
  "Volatility at which a KIND option is worth PRICE, by bisection.
nil when PRICE is outside the no-arbitrage bounds."
  (let ((lo 1e-4) (hi 5.0))
    (when (and (> tt 0)
               (>= price (- (financial-chart-risk-profile-bs-price kind s k r q lo tt) 1e-9))
               (<= price (+ (financial-chart-risk-profile-bs-price kind s k r q hi tt) 1e-9)))
      (dotimes (_ 100)
        (let ((mid (* 0.5 (+ lo hi))))
          (if (< (financial-chart-risk-profile-bs-price kind s k r q mid tt) price)
              (setq lo mid)
            (setq hi mid))))
      (* 0.5 (+ lo hi)))))

;;;; Legs

(defun financial-chart-risk-profile--option-p (leg)
  "Non-nil when LEG is a call or a put."
  (memq (plist-get leg :type) '(call put)))

(defun financial-chart-risk-profile--r (pos) "Rate of POS." (or (plist-get pos :rate) 0.0))
(defun financial-chart-risk-profile--q (pos) "Dividend yield of POS." (or (plist-get pos :dividend-yield) 0.0))

(defun financial-chart-risk-profile-leg-units (leg)
  "Signed shares LEG represents (contracts x 100 for options)."
  (* (if (eq (plist-get leg :side) 'short) -1 1)
     (or (plist-get leg :qty) 0)
     (if (financial-chart-risk-profile--option-p leg)
         financial-chart-risk-profile-multiplier
       1)))

(defun financial-chart-risk-profile--iv (pos leg)
  "Entry volatility of option LEG in POS: its :iv, else implied from :price."
  (or (plist-get leg :iv)
      (or (financial-chart-risk-profile-implied-vol
           (plist-get leg :type) (plist-get leg :price) (plist-get pos :spot)
           (plist-get leg :strike) (financial-chart-risk-profile--r pos)
           (financial-chart-risk-profile--q pos)
           (financial-chart-risk-profile--years (plist-get pos :date) (plist-get leg :expiry)))
          0.0)))

(defun financial-chart-risk-profile--entry (pos leg)
  "Per-share entry price of LEG in POS: premium (derived from IV if absent)
or the stock's entry price (default: spot)."
  (if (financial-chart-risk-profile--option-p leg)
      (or (plist-get leg :price)
          (financial-chart-risk-profile-bs-price
           (plist-get leg :type) (plist-get pos :spot) (plist-get leg :strike)
           (financial-chart-risk-profile--r pos) (financial-chart-risk-profile--q pos)
           (plist-get leg :iv)
           (financial-chart-risk-profile--years (plist-get pos :date) (plist-get leg :expiry))))
    (or (plist-get leg :price) (plist-get pos :spot))))

(defun financial-chart-risk-profile-leg-pnl (pos leg spot date)
  "Dollar P/L of LEG in POS at underlying SPOT on valuation DATE.
An option is repriced with Black-Scholes at its remaining time and entry
IV; on or after its expiry it is worth its intrinsic value."
  (let ((value
         (if (financial-chart-risk-profile--option-p leg)
             (financial-chart-risk-profile-bs-price
              (plist-get leg :type) spot (plist-get leg :strike)
              (financial-chart-risk-profile--r pos) (financial-chart-risk-profile--q pos)
              (max 1e-6 (financial-chart-risk-profile--iv pos leg))
              (financial-chart-risk-profile--years date (plist-get leg :expiry)))
           spot)))
    (* (financial-chart-risk-profile-leg-units leg)
       (- value (financial-chart-risk-profile--entry pos leg)))))

;;;; Position

(defun financial-chart-risk-profile-expiry-date (pos)
  "The date of POS's last option expiry, or its valuation date with no options."
  (let ((expiries (delq nil (mapcar (lambda (leg)
                                      (and (financial-chart-risk-profile--option-p leg)
                                           (plist-get leg :expiry)))
                                    (plist-get pos :legs)))))
    (if expiries
        (car (sort (copy-sequence expiries) (lambda (a b) (string> a b))))
      (plist-get pos :date))))

(defun financial-chart-risk-profile-pnl-at (pos spot date)
  "Total dollar P/L of POS at SPOT on valuation DATE."
  (cl-loop for leg in (plist-get pos :legs)
           sum (financial-chart-risk-profile-leg-pnl pos leg spot date)))

(defun financial-chart-risk-profile-expiry-pnl (pos spot)
  "Dollar P/L of POS at SPOT on the date of its last option expiry."
  (financial-chart-risk-profile-pnl-at pos spot (financial-chart-risk-profile-expiry-date pos)))

(defun financial-chart-risk-profile-range (pos)
  "POS's price range (LOW HIGH): :range, else spot -15% to +15%."
  (or (plist-get pos :range)
      (let ((s (plist-get pos :spot)))
        (list (* s 0.85) (* s 1.15)))))

(defun financial-chart-risk-profile-nodes (pos)
  "Ascending prices where the at-expiry line can bend: the range ends and
every option strike inside the range.  Between two nodes the line is straight."
  (pcase-let* ((`(,lo ,hi) (financial-chart-risk-profile-range pos))
               (strikes (cl-loop for leg in (plist-get pos :legs)
                                 for k = (plist-get leg :strike)
                                 when (and k (> k lo) (< k hi)) collect (float k))))
    (sort (delete-dups (append (list (float lo) (float hi)) strikes)) #'<)))

(defun financial-chart-risk-profile-expiry-points (pos)
  "At-expiry line of POS as ((PRICE PNL) ...) at its nodes.
Exact: the line is piecewise linear."
  (mapcar (lambda (s) (list s (financial-chart-risk-profile-expiry-pnl pos s)))
          (financial-chart-risk-profile-nodes pos)))

(defun financial-chart-risk-profile--crossing (a b)
  "Price where the segment from point A to point B ((PRICE PNL)) crosses zero,
or nil when it does not cross strictly between them."
  (pcase-let ((`(,x0 ,y0) a) (`(,x1 ,y1) b))
    (when (< (* y0 y1) 0)
      (+ x0 (* (- x1 x0) (/ (- y0) (- y1 y0)))))))

(defun financial-chart-risk-profile-breakevens (pos)
  "Prices inside POS's range where the at-expiry P/L crosses zero, ascending."
  (let ((pts (financial-chart-risk-profile-expiry-points pos)))
    (cl-loop for (a b) on pts while b
             for x = (financial-chart-risk-profile--crossing a b)
             when x collect x)))

(defun financial-chart-risk-profile-extremes (pos)
  "Plist (:max-profit P :max-loss L) of the at-expiry line over POS's range.
Both are dollars; :max-loss is negative (or 0.0).  The line is piecewise
linear, so its extremes sit at the nodes."
  (let ((ys (mapcar #'cadr (financial-chart-risk-profile-expiry-points pos))))
    (list :max-profit (max 0.0 (apply #'max ys))
          :max-loss (min 0.0 (apply #'min ys)))))

(defun financial-chart-risk-profile--with-crossings (pts)
  "PTS ((PRICE PNL) ...) with (X 0.0) inserted where a segment crosses zero."
  (cl-loop for (a b) on pts
           collect a
           when (and b (financial-chart-risk-profile--crossing a b))
           collect (list (financial-chart-risk-profile--crossing a b) 0.0)))

(defun financial-chart-risk-profile-regions (pos)
  "The at-expiry line split at its breakevens into regions.
A list of (:side profit|loss :points ((PRICE PNL) ...)) left to right.  A
breakeven (PNL 0.0) ends one region and starts the next; stretches lying
on zero belong to no region."
  (let (regions)
    (cl-loop for (p q) on (financial-chart-risk-profile--with-crossings
                           (financial-chart-risk-profile-expiry-points pos))
             while q
             for sum = (+ (cadr p) (cadr q))
             unless (zerop sum)
             do (let ((side (if (> sum 0) 'profit 'loss))
                      (open (car regions)))
                  (if (and open (eq side (plist-get open :side))
                           (equal p (car (last (plist-get open :points)))))
                      (setcar regions (plist-put open :points
                                                 (append (plist-get open :points) (list q))))
                    (push (list :side side :points (list p q)) regions))))
    (nreverse regions)))

(defun financial-chart-risk-profile-curve (pos date &optional steps)
  "P/L of POS on DATE at STEPS+1 (default 120) prices across its range,
as ((PRICE PNL) ...).  DATE is an ISO string; T+0 is POS's :date."
  (pcase-let ((`(,lo ,hi) (financial-chart-risk-profile-range pos))
              (n (or steps 120)))
    (cl-loop for i from 0 to n
             for s = (+ lo (* (- hi lo) (/ (float i) n)))
             collect (list s (financial-chart-risk-profile-pnl-at pos s date)))))

(defun financial-chart-risk-profile-curve-label (pos date)
  "\"T+N\" for valuation DATE relative to POS's :date."
  (format "T+%d" (- (financial-chart-risk-profile--days date)
                    (financial-chart-risk-profile--days (plist-get pos :date)))))

(defun financial-chart-risk-profile-valuation-dates (pos)
  "The valuation dates drawn as curves: :date and any :dates, ascending."
  (sort (delete-dups (cons (plist-get pos :date) (copy-sequence (plist-get pos :dates))))
        #'string<))

(defun financial-chart-risk-profile-strike-label (leg)
  "Marker text for option LEG: -165P (short put), +170C, -2x165P for two."
  (let ((qty (plist-get leg :qty)))
    (format "%s%s%s%s"
            (if (eq (plist-get leg :side) 'short) "-" "+")
            (if (and qty (> qty 1)) (format "%dx" qty) "")
            (format "%g" (plist-get leg :strike))
            (if (eq (plist-get leg :type) 'call) "C" "P"))))

;;;; Rows for the template

(defconst financial-chart-risk-profile-colors
  '(:loss-line "#ff4d57" :profit-line "#2ecc71" :loss-fill "#8f1d27" :profit-fill "#1c7c3c"
    :curves ("#22d3ee" "#facc15" "#c084fc" "#fb923c" "#a3e635" "#f472b6"))
  "Colors the rows carry; the template draws them as literals.")

(defun financial-chart-risk-profile--color (key)
  "The color for KEY of `financial-chart-risk-profile-colors'."
  (plist-get financial-chart-risk-profile-colors key))

(defun financial-chart-risk-profile--curve-ink (i)
  "The color of the I-th valuation-date curve."
  (let ((colors (financial-chart-risk-profile--color :curves)))
    (nth (mod i (length colors)) colors)))

(defun financial-chart-risk-profile-rows (pos)
  "POS lowered to the template's rows, plists with :kind one of
expiry (:seg :side :price :pnl), curve (:series :price :pnl), spot
\(:price) and strike (:price :label :side :pnl, pinned to the chart floor)."
  (let* ((regions (financial-chart-risk-profile-regions pos))
         (lo-hi (financial-chart-risk-profile-range pos))
         (curves (mapcar (lambda (d)
                           (cons (financial-chart-risk-profile-curve-label pos d)
                                 (financial-chart-risk-profile-curve pos d)))
                         (financial-chart-risk-profile-valuation-dates pos)))
         (floor (apply #'min 0.0
                       (append (mapcar #'cadr (financial-chart-risk-profile-expiry-points pos))
                               (cl-loop for (_ . c) in curves append (mapcar #'cadr c))))))
    (append
     (cl-loop for r in regions for i from 0
              append (cl-loop for (x y) in (plist-get r :points)
                              collect (list :kind "expiry" :seg i
                                            :side (symbol-name (plist-get r :side))
                                            :ink (financial-chart-risk-profile--color
                                                  (if (eq (plist-get r :side) 'profit)
                                                      :profit-line :loss-line))
                                            :fill (financial-chart-risk-profile--color
                                                   (if (eq (plist-get r :side) 'profit)
                                                       :profit-fill :loss-fill))
                                            :price x :pnl y :base 0.0)))
     (cl-loop for (label . c) in curves for i from 0
              for ink = (financial-chart-risk-profile--curve-ink i)
              append (cl-loop for (x y) in c
                              collect (list :kind "curve" :series label :ink ink :price x :pnl y)))
     (list (list :kind "spot" :price (float (plist-get pos :spot))))
     (cl-loop for leg in (plist-get pos :legs)
              for k = (plist-get leg :strike)
              when (and k (>= k (car lo-hi)) (<= k (cadr lo-hi)))
              collect (list :kind "strike" :price (float k)
                            :label (financial-chart-risk-profile-strike-label leg)
                            :side (symbol-name (plist-get leg :side))
                            :ink (financial-chart-risk-profile--color
                                  (if (eq (plist-get leg :side) 'short) :loss-line :profit-line))
                            :pnl floor)))))

(defun financial-chart-risk-profile-bindings (pos _props)
  "Template slots for POS: the price axis ends and the curve legend."
  (pcase-let ((`(,lo ,hi) (financial-chart-risk-profile-range pos))
              (dates (financial-chart-risk-profile-valuation-dates pos)))
    (list :x_min (float lo) :x_max (float hi)
          :series (vconcat (mapcar (lambda (d) (financial-chart-risk-profile-curve-label pos d))
                                   dates))
          :series_colors (vconcat (cl-loop for _ in dates for i from 0
                                           collect (financial-chart-risk-profile--curve-ink i))))))

;;;; Shape

(defun financial-chart-risk-profile--invalid (index field code fmt &rest args)
  "Signal invalid risk-profile data: INDEX, FIELD, CODE, message FMT/ARGS."
  (apply #'financial-chart--invalid index field code fmt args))

(defun financial-chart-risk-profile--check-date (date field index)
  "Signal unless DATE is an ISO date string; FIELD and INDEX locate it."
  (unless (and (stringp date)
               (condition-case nil (financial-chart-risk-profile--days date)
                 (error nil)))
    (financial-chart-risk-profile--invalid
     index field "invalid_date" "%s must be a YYYY-MM-DD date, got %S" field date)))

(defun financial-chart-risk-profile--check-leg (leg i)
  "Signal unless LEG, the I-th leg, is well formed."
  (let ((f (lambda (name) (format "legs[%d].%s" i name))))
    (unless (and (proper-list-p leg) (cl-evenp (length leg)) (keywordp (car leg)))
      (financial-chart-risk-profile--invalid i (funcall f "") "not_a_plist"
                                             "each leg must be a plist"))
    (unless (memq (plist-get leg :type) '(call put stock))
      (financial-chart-risk-profile--invalid i (funcall f "type") "invalid_type"
                                             "leg type must be call, put or stock"))
    (unless (memq (plist-get leg :side) '(long short))
      (financial-chart-risk-profile--invalid i (funcall f "side") "invalid_side"
                                             "leg side must be long or short"))
    (unless (and (financial-chart--finite-p (plist-get leg :qty)) (> (plist-get leg :qty) 0))
      (financial-chart-risk-profile--invalid i (funcall f "qty") "not_positive"
                                             "qty must be a positive number"))
    (when (financial-chart-risk-profile--option-p leg)
      (unless (and (financial-chart--finite-p (plist-get leg :strike))
                   (> (plist-get leg :strike) 0))
        (financial-chart-risk-profile--invalid i (funcall f "strike") "not_positive"
                                               "strike must be a positive number"))
      (financial-chart-risk-profile--check-date (plist-get leg :expiry) (funcall f "expiry") i)
      (unless (or (plist-get leg :price) (plist-get leg :iv))
        (financial-chart-risk-profile--invalid i (funcall f "price") "missing_field"
                                               "option leg needs :price (premium) or :iv"))
      (dolist (key '(:price :iv))
        (when (and (plist-get leg key)
                   (not (and (financial-chart--finite-p (plist-get leg key))
                             (if (eq key :iv) (> (plist-get leg key) 0) (>= (plist-get leg key) 0)))))
          (financial-chart-risk-profile--invalid
           i (funcall f (substring (symbol-name key) 1)) "not_a_number"
           "%s must be a finite non-negative number (IV positive)" key))))))

(defun financial-chart-risk-profile--validate (data)
  "Signal unless DATA is a well-formed risk-profile position."
  (unless (and (proper-list-p data) (cl-evenp (length data)) (keywordp (car data)))
    (financial-chart--invalid nil nil "not_a_plist" "risk-profile data must be a position plist"))
  (let ((spot (plist-get data :spot)) (legs (plist-get data :legs)))
    (unless (and (financial-chart--finite-p spot) (> spot 0))
      (financial-chart--invalid nil "spot" "not_positive" "spot must be a positive number"))
    (financial-chart-risk-profile--check-date (plist-get data :date) "date" nil)
    (dolist (key '(:rate :dividend-yield))
      (when (and (plist-get data key) (not (financial-chart--finite-p (plist-get data key))))
        (financial-chart--invalid nil (substring (symbol-name key) 1) "not_a_number"
                                  "%s must be a finite fraction" key)))
    (unless (and (proper-list-p legs) legs)
      (financial-chart--invalid nil "legs" "no_legs" "a position needs at least one leg"))
    (cl-loop for leg in legs for i from 0
             do (financial-chart-risk-profile--check-leg leg i))
    (when-let* ((range (plist-get data :range)))
      (unless (and (proper-list-p range) (= (length range) 2)
                   (financial-chart--finite-p (car range)) (financial-chart--finite-p (cadr range))
                   (> (car range) 0) (< (car range) (cadr range)))
        (financial-chart--invalid nil "range" "bad_range"
                                  "range must be (LOW HIGH) with 0 < LOW < HIGH")))
    (dolist (d (plist-get data :dates))
      (financial-chart-risk-profile--check-date d "dates" nil)
      (when (string< d (plist-get data :date))
        (financial-chart--invalid nil "dates" "date_before_valuation"
                                  "valuation date %s is before :date %s" d (plist-get data :date))))
    t))

(defun financial-chart-risk-profile--values (data _props)
  "The at-expiry and T+n P/L values of DATA, for summaries."
  (append (mapcar #'cadr (financial-chart-risk-profile-expiry-points data))
          (cl-loop for d in (financial-chart-risk-profile-valuation-dates data)
                   append (mapcar #'cadr (financial-chart-risk-profile-curve data d)))))

(defun financial-chart-risk-profile--sym (v)
  "String V as a symbol; other values unchanged."
  (if (stringp v) (intern v) v))

(defun financial-chart-risk-profile--from-json (data)
  "JSON-parsed position DATA (an alist) as a position plist."
  (let ((legs (mapcar (lambda (leg)
                        (list :type (financial-chart-risk-profile--sym (alist-get 'type leg))
                              :side (financial-chart-risk-profile--sym (alist-get 'side leg))
                              :qty (alist-get 'qty leg)
                              :strike (alist-get 'strike leg)
                              :expiry (alist-get 'expiry leg)
                              :price (alist-get 'price leg)
                              :iv (alist-get 'iv leg)))
                      (append (alist-get 'legs data) nil)))
        (range (alist-get 'range data)))
    (cl-loop for (key . val) in
             `((:spot . ,(alist-get 'spot data)) (:date . ,(alist-get 'date data))
               (:rate . ,(alist-get 'rate data))
               (:dividend-yield . ,(alist-get 'dividend_yield data))
               (:range . ,(and range (append range nil)))
               (:dates . ,(append (alist-get 'dates data) nil))
               (:legs . ,(mapcar (lambda (leg) (cl-loop for (k v) on leg by #'cddr
                                                        when v append (list k v)))
                                 legs)))
             when val append (list key val))))

(defun financial-chart-risk-profile--to-json (data)
  "Position DATA as a JSON-compatible alist."
  (let ((range (plist-get data :range)))
    `((spot . ,(plist-get data :spot)) (date . ,(plist-get data :date))
      ,@(when (plist-get data :rate) `((rate . ,(plist-get data :rate))))
      ,@(when (plist-get data :dividend-yield)
          `((dividend_yield . ,(plist-get data :dividend-yield))))
      ,@(when range `((range . ,(vconcat range))))
      ,@(when (plist-get data :dates) `((dates . ,(vconcat (plist-get data :dates)))))
      (legs . ,(vconcat
                (mapcar (lambda (leg)
                          (cl-loop for (k v) on leg by #'cddr
                                   collect (cons (intern (substring (symbol-name k) 1))
                                                 (if (symbolp v) (symbol-name v) v))))
                        (plist-get data :legs)))))))

(add-to-list 'financial-chart-shapes
             '(risk-profile
               :doc "A position plist (:spot NUM :date \"YYYY-MM-DD\" :rate R :dividend-yield Q :range (LOW HIGH) :dates (\"YYYY-MM-DD\" ...) :legs LEGS); a leg is (:type call|put :side long|short :qty CONTRACTS :strike K :expiry DATE :price PREMIUM :iv V) or (:type stock :side long|short :qty SHARES [:price ENTRY]). Options need :price or :iv. :dates adds T+n curves. JSON: the same keys, dividend_yield, legs as an array."
               :example (:spot 100 :date "2026-11-02" :rate 0.04 :range (80 120)
                         :dates ("2026-11-16")
                         :legs ((:type call :side long :qty 1 :strike 100
                                 :expiry "2026-12-18" :price 4.0 :iv 0.3)
                                (:type call :side short :qty 1 :strike 110
                                 :expiry "2026-12-18" :price 1.5 :iv 0.28)))
               :validator financial-chart-risk-profile--validate
               :values financial-chart-risk-profile--values
               :from-json financial-chart-risk-profile--from-json
               :to-json financial-chart-risk-profile--to-json))

(financial-chart-register-kind
 'risk-profile :shape 'risk-profile :template "risk-profile" :adapter "risk-profile"
 :bindings #'financial-chart-risk-profile-bindings
 :doc "Options risk profile: at-expiry P/L split into profit and loss regions, T+n curves, spot and strike markers.")

(provide 'financial-chart-risk-profile)
;;; financial-chart-risk-profile.el ends here
