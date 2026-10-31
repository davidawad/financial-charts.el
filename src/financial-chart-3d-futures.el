;;; financial-chart-3d-futures.el --- futures curves in 3D -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Adapter and graphics mark for invest-metrics fut.term_structure.
;; A frame has one contract per point; a sequence of metric outputs is
;; replayed across contract tenor (x) and snapshot time (z).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function financial-chart-3d--opt "financial-chart-3d" (graphics name &optional default))
(declare-function financial-chart-3d--field "financial-chart-3d" (graphics channel default))
(declare-function financial-chart-3d--value "financial-chart-3d" (row key))
(declare-function financial-chart-3d--domain "financial-chart-3d" (rows))
(declare-function financial-chart--invalid "financial-chart" (index field code format-string &rest args))
(declare-function financial-chart-3d--replay "financial-chart-3d" (graphics rows field mark))
(declare-function financial-chart-3d-metric-entries "financial-chart-3d" (output))
(declare-function financial-chart-3d-metric-stamp "financial-chart-3d" (entry args keys &optional default))
(declare-function financial-chart-3d-metric-get "financial-chart-3d" (object &rest keys))
(declare-function financial-chart-3d-metric-list "financial-chart-3d" (v))
(declare-function eas-register-adapter "eas-data" (name &rest properties))
(declare-function eas-data-make "eas-data" (rows &optional schema))
(declare-function eas-template-get "eas-template" (name))

(defun financial-chart-3d-futures--get (object &rest keys)
  "First non-nil field among KEYS in OBJECT."
  (seq-some (lambda (key) (financial-chart-3d-metric-get object key)) keys))

(defun financial-chart-3d-futures--shape (contracts)
  "Infer the curve shape from tenor-sorted CONTRACTS."
  (let ((moves (cl-loop for a in contracts for b in (cdr contracts)
                        collect (- (plist-get b :price) (plist-get a :price)))))
    (cond ((= (length contracts) 1) "single")
          ((seq-every-p (lambda (v) (> v 1e-9)) moves) "contango")
          ((seq-every-p (lambda (v) (< v -1e-9)) moves) "backwardation")
          ((seq-every-p (lambda (v) (<= (abs v) 1e-9)) moves) "flat")
          (t "mixed"))))

(defun financial-chart-3d-futures--years (expiry as-of)
  "Calendar years from AS-OF to EXPIRY, or nil when either is not a date."
  (when (and (stringp expiry) (stringp as-of)
             (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" expiry))
    (let* ((year (string-to-number (match-string 1 expiry)))
           (month (string-to-number (match-string 2 expiry)))
           (day (string-to-number (match-string 3 expiry))))
      (when (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" as-of)
        (let ((expiry-time (encode-time 0 0 12 day month year t))
              (as-of-time (encode-time 0 0 12
                                       (string-to-number (match-string 3 as-of))
                                       (string-to-number (match-string 2 as-of))
                                       (string-to-number (match-string 1 as-of)) t)))
          ;; invest-metrics uses ACT/365 for expiry-derived tenor.
          (/ (- (float-time expiry-time) (float-time as-of-time)) (* 365.0 86400.0)))))))

(defun financial-chart-3d-futures--row-p (row)
  "Non-nil when ROW is a futures-curve row: a plist with a :kind."
  (and (keywordp (car-safe row)) (stringp (plist-get row :kind))))

(defun financial-chart-3d-futures--normalized (value)
  "VALUE's futures-curve rows as a list when it already holds rows, else nil.
VALUE is rows in any shape eas hands an adapter: a list or vector of
row plists, data/v1 {schema, rows} or Vega-Lite inline {values}."
  (let ((rows (cond ((and (keywordp (car-safe value)) (plist-member value :rows)
                          (plist-member value :schema))
                     (plist-get value :rows))
                    ((and (keywordp (car-safe value)) (plist-member value :values))
                     (plist-get value :values))
                    ((financial-chart-3d-futures--row-p value) nil)
                    (t value))))
    (when (and (sequencep rows) (not (stringp rows)) (> (length rows) 0)
               (seq-every-p #'financial-chart-3d-futures--row-p rows))
      (append rows nil))))

(defun financial-chart-3d-futures-rows (output &optional as-of)
  "Rows for OUTPUT of fut.term_structure, optionally stamped at AS-OF.
OUTPUT is one metric result or a sequence of {time, output} snapshots.
The metric result has {spot?, rate?, as_of?, contracts}; contracts have
{expiry|years, price, ...}.  Consecutive contracts also receive spread
and annualized_roll, calculated from their prices and tenor gap when
those fields are absent.  OUTPUT that is already these rows (a list,
vector, data/v1 or {values}) comes back as they are, a vector."
  (if-let* ((normalized (financial-chart-3d-futures--normalized output)))
      (vconcat normalized)
    (financial-chart-3d-futures--metric-rows output as-of)))

(defun financial-chart-3d-futures--metric-rows (output as-of)
  "Rows for fut.term_structure OUTPUT stamped at AS-OF.
See `financial-chart-3d-futures-rows'."
  (let ((entries (financial-chart-3d-metric-entries output)) rows)
    (cl-loop for entry in entries for si from 0
             for result = (plist-get entry :output)
             for date = (or (financial-chart-3d-metric-stamp entry nil '(:time :as_of :date))
                            (financial-chart-3d-metric-get result :as_of) as-of "snapshot")
             for spot = (financial-chart-3d-metric-get result :spot)
             for rate = (financial-chart-3d-metric-get result :rate)
             for contracts = (financial-chart-3d-metric-list
                              (financial-chart-3d-metric-get result :contracts))
             do (unless contracts
                  (financial-chart--invalid si "contracts" "missing_contracts"
                                            "fut.term_structure output needs a non-empty contracts array"))
             do (let* ((prepared
                       (cl-loop for c in contracts for ci from 0
                                for expiry = (financial-chart-3d-metric-get c :expiry)
                                for years = (or (financial-chart-3d-metric-get c :years)
                                                (financial-chart-3d-futures--years expiry date))
                                for price = (financial-chart-3d-futures--get c :price :mark :last)
                                do (unless (numberp years)
                                     (financial-chart--invalid ci "years" "missing_years"
                                                               "each futures contract needs years or an expiry date and snapshot date"))
                                do (unless (> years 0)
                                     (financial-chart--invalid ci "years" "expired_contract"
                                                               "futures contract tenor must be positive"))
                                do (unless (numberp price)
                                     (financial-chart--invalid ci "price" "missing_price"
                                                               "each futures contract needs a numeric price"))
                                collect (list :contract c :years years :price price :expiry expiry)))
                   (ordered (sort prepared (lambda (a b) (< (plist-get a :years) (plist-get b :years)))))
                   (shape (or (financial-chart-3d-metric-get result :shape)
                              (financial-chart-3d-futures--shape ordered)))
                   (pairs (make-hash-table :test 'eq)))
                (cl-loop for a in ordered for b in (cdr ordered) for ci from 0
                         when (= (plist-get a :years) (plist-get b :years))
                         do (financial-chart--invalid (1+ ci) "years" "duplicate_expiry"
                                                      "futures contracts must have distinct expiries"))
                (when (and spot (numberp spot))
                  (push (list :kind "spot" :time date :years 0.0 :price spot :basis 0.0 :spot spot :shape shape) rows))
                (cl-loop for a in ordered
                         for b in (cdr ordered)
                         for spread = (- (plist-get b :price) (plist-get a :price))
                         for gap = (- (plist-get b :years) (plist-get a :years))
                         ;; Match fut.term_structure's annualized roll: the
                         ;; logarithmic deferred/near ratio, signed positive
                         ;; for backwardation and negative for contango.
                         for roll = (if (and (> gap 0) (> (plist-get a :price) 0)
                                             (> (plist-get b :price) 0))
                                        (/ (log (/ (float (plist-get a :price))
                                                   (plist-get b :price))) gap)
                                      0.0)
                         do (puthash a (list spread roll) pairs))
                (dolist (p ordered)
                  (let* ((c (plist-get p :contract)) (years (plist-get p :years))
                         (price (plist-get p :price))
                         (basis (or (financial-chart-3d-futures--get c :basis)
                                    (and (numberp spot) (- price spot))))
                         (carry (or (financial-chart-3d-futures--get c :carry)
                                    (and (numberp spot) (> spot 0) (> price 0) (> years 0)
                                         (/ (log (/ (float price) spot)) years))))
                         (yield (or (financial-chart-3d-futures--get c :implied_yield)
                                    (and (numberp rate) (numberp carry) (- rate carry))))
                         (pair (gethash p pairs))
                         (spread (car pair))
                         (roll (cadr pair)))
                    (push (list :kind "contract" :time date :shape shape
                                :symbol (or (financial-chart-3d-metric-get c :symbol) (financial-chart-3d-metric-get c :contract) :null)
                                :expiry (or (financial-chart-3d-metric-get c :expiry) :null)
                                :years years :price price :mark (or (financial-chart-3d-futures--get c :mark) price)
                                :bid (or (financial-chart-3d-futures--get c :bid) :null)
                                :ask (or (financial-chart-3d-futures--get c :ask) :null)
                                :oi (or (financial-chart-3d-futures--get c :oi :open_interest) :null)
                                :volume (or (financial-chart-3d-futures--get c :volume) :null)
                                :basis (or basis :null) :carry (or carry :null)
                                :implied_yield (or yield :null)
                                :spread_to_next (or spread :null)
                                :roll_to_next (or roll :null)
                                :annualized_roll (or roll :null)
                                :roll_sign (cond ((and (numberp spread) (> spread 1e-9)) "contango")
                                                 ((and (numberp spread) (< spread -1e-9)) "backwardation")
                                                 (t "flat"))) rows)))))
    (vconcat (nreverse rows))))

(defun financial-chart-3d-futures--tooltip (r)
  "Tooltip text for contract row R."
  (let ((g (lambda (k) (or (financial-chart-3d--value r k) "-"))))
    (format "%s %s  %.2fy  price %s basis %s\nbid %s ask %s mark %s  OI %s vol %s\ncarry %s implied yield %s roll to next %s"
            (or (financial-chart-3d--value r :symbol) "contract")
            (or (financial-chart-3d--value r :expiry) "") (plist-get r :years)
            (funcall g :price) (funcall g :basis) (funcall g :bid) (funcall g :ask) (funcall g :mark)
            (funcall g :oi) (funcall g :volume) (funcall g :carry) (funcall g :implied_yield)
            (funcall g :roll_to_next))))

(defun financial-chart-3d-futures--x (x low high width)
  "Map X from LOW..HIGH to the chart's horizontal extent WIDTH."
  (+ (- width) (* (/ (- x low) (float (- high low))) (* 2 width))))

(defun financial-chart-3d-futures--y (y low high height)
  "Map Y from LOW..HIGH to chart HEIGHT."
  (* (float height) (/ (- y low) (float (- high low)))))

(defun financial-chart-3d-futures--z (date dates depth)
  "Map DATE in DATES to chart depth DEPTH."
  (let ((i (or (cl-position date dates :test #'equal) 0)))
    (- depth (* (/ i (float (max 1 (1- (length dates))))) (* 2 depth)))))

(defun financial-chart-3d-futures--primitives (graphics rows)
  "Draw futures ROWS as points and ribbons; GRAPHICS is the x-eas configuration."
  (let* ((xk (financial-chart-3d--field graphics "x" "years"))
         (yk (financial-chart-3d--field graphics "y" "price"))
         (zk (financial-chart-3d--field graphics "z" "time"))
         (ck (financial-chart-3d--field graphics "color" "roll_sign"))
         (domain (financial-chart-3d--domain rows))
         (contracts (seq-filter (lambda (r) (equal (plist-get r :kind) "contract")) rows))
         (scale-rows (seq-filter (lambda (r) (and (numberp (plist-get r xk))
                                                  (numberp (plist-get r yk)))) domain))
         (dates (delete-dups (mapcar (lambda (r) (plist-get r zk)) domain)))
         (extent (financial-chart-3d--opt graphics "extent" [12 12]))
         (w (/ (float (aref extent 0)) 2.0))
         (d (/ (float (aref extent 1)) 2.0))
         (xs (mapcar (lambda (r) (plist-get r xk)) scale-rows))
         (ys (mapcar (lambda (r) (plist-get r yk)) scale-rows))
         (xmin (apply #'min 0 xs))
         (xmax (max 1e-9 (apply #'max 0 xs)))
         (ymin (apply #'min ys))
         (ymax (max (+ ymin 1e-9) (apply #'max ys)))
         (height (financial-chart-3d--opt graphics "height" 8))
         (colors (financial-chart-3d--opt graphics "colors"))
         (points nil) (tips nil) (data nil) (point-colors nil)
         (curve nil) (curve-colors nil) (roll nil) (spot-lines nil))
    (dolist (date dates)
      (let ((at (sort (seq-filter (lambda (r) (equal (plist-get r zk) date)) contracts)
                      (lambda (a b) (< (plist-get a xk) (plist-get b xk))))))
        (dolist (r at)
          (push (vector (financial-chart-3d-futures--x (plist-get r xk) xmin xmax w)
                        (financial-chart-3d-futures--y (plist-get r yk) ymin ymax height)
                        (financial-chart-3d-futures--z date dates d))
                points)
          (push (or (plist-get colors (intern (concat ":" (plist-get r ck)))) "#4c78a8") point-colors)
          (push (financial-chart-3d-futures--tooltip r) tips)
          (push r data))
        (cl-loop for (a b) on at while b
                 for x0 = (financial-chart-3d-futures--x (plist-get a xk) xmin xmax w)
                 for x1 = (financial-chart-3d-futures--x (plist-get b xk) xmin xmax w)
                 for y0 = (financial-chart-3d-futures--y (plist-get a yk) ymin ymax height)
                 for y1 = (financial-chart-3d-futures--y (plist-get b yk) ymin ymax height)
                 for z = (financial-chart-3d-futures--z date dates d)
                 for roll-rate = (or (plist-get a :annualized_roll) 0)
                 do (push (vector x0 y0 z x1 y1 z) curve)
                 (push (vector x0 (+ y0 (* 0.7 roll-rate)) z
                               x1 (+ y1 (* 0.7 roll-rate)) z) roll)
                 (push (or (plist-get colors (intern (concat ":" (plist-get a ck)))) "#4c78a8")
                       curve-colors))
        (let ((spot (seq-find (lambda (r) (and (equal (plist-get r :kind) "spot")
                                                (equal (plist-get r zk) date))) rows)))
          (when spot
            (let ((y (financial-chart-3d-futures--y (plist-get spot yk) ymin ymax height))
                  (z (financial-chart-3d-futures--z date dates d)))
              (push (vector (- w) y z w y z) spot-lines))))))
    (list (list :id "contracts" :type "points" :points (vconcat (nreverse points))
                :colors (vconcat (nreverse point-colors)) :size 3
                :tooltips (vconcat (nreverse tips)) :data (vconcat (nreverse data)))
          (list :id "curve" :type "segments" :segments (vconcat (nreverse curve))
                :colors (vconcat (nreverse curve-colors)))
          (list :id "annualized-roll" :type "segments" :segments (vconcat (nreverse roll))
                :color "#eeca3b")
          (list :id "spot-reference" :type "segments" :segments (vconcat (nreverse spot-lines))
                :color "#ffffff"))))

(defun financial-chart-3d-futures--mark (graphics rows)
  "Replay futures ROWS by snapshot time."
  (financial-chart-3d--replay graphics rows "time" #'financial-chart-3d-futures--primitives))

(defun financial-chart-3d-futures--from-metric (_template output args)
  "futures-curve bindings from fut.term_structure OUTPUT and optional ARGS."
  (let* ((rows (financial-chart-3d-futures-rows output (plist-get args :as_of)))
         (shape (or (and (> (length rows) 0) (plist-get (aref rows 0) :shape)) "single")))
    (append (list :data rows :title (or (plist-get args :title) (format "Futures curve · %s" shape)))
            (and (plist-get args :measure) (list :measure (plist-get args :measure))))))

(defconst financial-chart-3d-futures-marks
  '(("fc-futures-curve3d" . financial-chart-3d-futures--mark))
  "The futures curve's 3D mark.")

(defun financial-chart-3d-futures--register-marks (&rest _args)
  (when (featurep 'graphics-eas-marks)
    (remove-hook 'after-load-functions #'financial-chart-3d-futures--register-marks)
    (dolist (mark financial-chart-3d-futures-marks)
      (graphics-eas-define-mark (car mark) (cdr mark)))
    t))
(unless (financial-chart-3d-futures--register-marks)
  (add-hook 'after-load-functions #'financial-chart-3d-futures--register-marks))

(defconst financial-chart-3d-futures-metric-adapters
  '(("fut.term_structure" "futures-curve" financial-chart-3d-futures--from-metric))
  "Adapter for invest-metrics fut.term_structure.")

(eas-register-adapter
 "invest/futures-curve"
 :doc "fut.term_structure output or its normalized futures-curve rows."
 :example (plist-get (eas-template-get "futures-curve") :example)
 :convert (lambda (value)
            ;; The open path hands back what it was given: data/v1 as it
            ;; is, rows (list, vector, {values}) as rows; only a metric
            ;; output is adapted.
            (if (and (keywordp (car-safe value)) (plist-member value :schema) (plist-member value :rows))
                value
              (eas-data-make (financial-chart-3d-futures-rows value)))))

(provide 'financial-chart-3d-futures)
;;; financial-chart-3d-futures.el ends here
