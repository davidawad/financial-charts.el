;;; financial-chart-3d-vol-surface-test.el --- The IV surface, in 2D and in 3D -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these beside financial-chart-3d-test.el, against
;; the graphics.el and eas.el checkouts of `make deps-3d'.  The rows come
;; from the SPY chain fixture (test/fixtures/3d/vol-surface/); the 2D
;; template validates and draws in a subprocess without graphics.el;
;; the 3D variant replays the session and draws its golden
;; (test/golden/3d/vol-surface.{frame,txt}, 60x24, dark).

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(require 'graphics-frame)
(require 'financial-chart-3d-framing-test)
(financial-chart-3d--ensure)

(defun financial-chart-3d-vol-test--chain ()
  "The SPY chain fixture."
  (eas-json-read-file (expand-file-name financial-chart-3d-vol-surface-fixture financial-chart-test-root)))

(defun financial-chart-3d-vol-test--prim (prims id)
  "The primitive of PRIMS whose id is ID."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

(defun financial-chart-3d-vol-test--holes (frame cols rows)
  "Empty cells of FRAME (COLS by ROWS) the drawing encloses.
A cell with no dots, or one in a guide colour (the axes), is open;
the open cells no path of open cells joins to the frame's border are
holes: gaps inside the surface's footprint."
  (let* ((n (* cols rows)) (open (make-vector n nil)) (seen (make-vector n nil)) stack (holes 0))
    (dotimes (i n)
      (let ((cell (graphics-frame-cell frame i)))
        (aset open i (or (= (car cell) 0) (member (cdr cell) financial-chart-3d-framing-guides)))
        (when (and (aref open i) (or (< i cols) (>= i (- n cols)) (zerop (% i cols)) (= (% i cols) (1- cols))))
          (aset seen i t)
          (push i stack))))
    (while stack
      (let* ((i (pop stack)) (c (% i cols)))
        (dolist (j (list (and (>= i cols) (- i cols)) (and (< (+ i cols) n) (+ i cols))
                         (and (> c 0) (1- i)) (and (< c (1- cols)) (1+ i))))
          (when (and j (aref open j) (not (aref seen j)))
            (aset seen j t)
            (push j stack)))))
    (dotimes (i n)
      (when (and (aref open i) (not (aref seen i)) (zerop (car (graphics-frame-cell frame i))))
        (cl-incf holes)))
    holes))

(defconst financial-chart-3d-vol-test--grid
  '((:time "t1" :spot 100 :strike 90 :dte 7 :iv 0.30) (:time "t1" :spot 100 :strike 100 :dte 7 :iv 0.20)
    (:time "t1" :spot 100 :strike 110 :dte 7 :iv 0.24)
    (:time "t1" :spot 100 :strike 90 :dte 30 :iv 0.22) (:time "t1" :spot 100 :strike 110 :dte 30 :iv 0.18))
  "Two expiries by three strikes, the 30-day 100 strike missing.")

;;; Rows

(ert-deftest financial-chart-3d-vol-surface-rows-from-the-fixture ()
  (let* ((chain (financial-chart-3d-vol-test--chain))
         (rows (append (financial-chart-3d-vol-surface-rows chain) nil))
         (times (delete-dups (mapcar (lambda (r) (plist-get r :time)) rows))))
    (should (equal (plist-get chain :symbol) "SPY"))
    (should (= (length times) (length (plist-get chain :snapshots))))
    (should (equal times (sort (copy-sequence times) #'string<)))
    (should (= (length rows) (* (length times) 8 17)))
    (dolist (r rows)
      (should (stringp (plist-get r :expiry)))
      (should (< 0.05 (plist-get r :iv) 1.0))
      ;; dte is the calendar days from the quote to the expiry.
      (should (= (plist-get r :dte) (financial-chart-3d--vol-days (plist-get r :time) (plist-get r :expiry)))))
    ;; The front week's put skew: IV falls from the lowest strike to the money.
    (let ((front (seq-filter (lambda (r) (and (equal (plist-get r :time) (car times)) (= (plist-get r :dte) 7)))
                             rows)))
      (should (> (plist-get (car front) :iv) (plist-get (nth 8 front) :iv))))))

(ert-deftest financial-chart-3d-vol-surface-rows-shapes-and-errors ()
  ;; Snapshots alone; quotes under `quotes'; dte from expiry; no IV dropped.
  (let ((rows (financial-chart-3d-vol-surface-rows
               '((:time "2026-10-30T10:00:00" :spot 100
                        :quotes [(:strike 100 :expiry "2026-11-06" :iv 0.2)
                                 (:strike 105 :expiry "2026-11-06" :iv :null)])))))
    (should (equal (append rows nil)
                   '((:time "2026-10-30T10:00:00" :spot 100 :strike 100 :dte 7 :expiry "2026-11-06" :iv 0.2)))))
  (dolist (case '(((:spot 1 :chain [(:strike 1 :dte 1 :iv 0.1)]) "missing_time")
                  ((:time "2026-10-30" :chain [(:strike 1 :dte 1 :iv 0.1)]) "missing_spot")
                  ((:time "2026-10-30" :spot 1 :chain [(:dte 1 :iv 0.1)]) "missing_strike")
                  ((:time "2026-10-30" :spot 1 :chain [(:strike 1 :iv 0.1)]) "missing_dte")
                  ((:time "2026-10-30" :spot 1 :chain [(:strike 1 :dte 1 :iv -0.1)]) "bad_iv")
                  ((:time "2026-10-30" :spot 1 :chain []) "empty_chain")))
    (let ((err (should-error (financial-chart-3d-vol-surface-rows (list (car case)))
                             :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :code) (cadr case))))))

(ert-deftest financial-chart-3d-vol-surface-example-is-generated ()
  (let ((example (eas-json-read-file (expand-file-name "examples/vol-surface.data.json" financial-chart-test-root)))
        (file (make-temp-file "fc3d-vol" nil ".json")))
    (unwind-protect
        (progn
          (financial-chart-3d-vol-surface-write-example file)
          (should (equal (eas-json-read-file file) example)))
      (delete-file file))
    (should (equal (eas-template-example "vol-surface") example))
    (should (equal (eas-template-example "fc3d/vol-surface") example))))

;;; opt.iv_surface input

(defun financial-chart-3d-vol-test--iv-surface ()
  "invest-vol-surface's opt.iv_surface fixture, verbatim."
  (eas-json-read-file (expand-file-name "test/fixtures/3d/vol-surface/iv_surface.json" financial-chart-test-root)))

(defun financial-chart-3d-vol-test--wide (&optional shift)
  "A synthetic live-sized opt.iv_surface: 24 expiries by 125 strikes.
Spot 500, a put skew and a term structure, IV raised by SHIFT; the far
strikes of the near expiries and a scatter of cells are null."
  (let* ((strikes (cl-loop for i below 125 collect (+ 345.0 (* i 2.5))))
         (dtes (cl-loop for j below 24 collect (+ 3 (* j 15)))))
    (list :strikes (vconcat strikes)
          :moneyness (vconcat (mapcar (lambda (k) (/ k 500.0)) strikes))
          :days_to_expiry (vconcat dtes)
          :iv (vconcat
               (cl-loop for dte in dtes for j from 0 collect
                        (vconcat
                         (cl-loop for k in strikes for i from 0 collect
                                  (let ((m (log (/ k 500.0))))
                                    (if (or (and (< j 4) (or (< i 10) (> i 115))) (zerop (% (+ i (* 7 j)) 17)))
                                        :null
                                      (+ 0.18 (or shift 0) (* 0.5 m m) (* -0.25 m)
                                         (/ 0.06 (sqrt (/ dte 30.0)))))))))))))

(ert-deftest financial-chart-3d-vol-surface-rows-from-opt-iv-surface ()
  (let ((rows (append (financial-chart-3d-vol-surface-rows (financial-chart-3d-vol-test--iv-surface)
                                                           :time "2026-10-30T16:00:00")
                      nil)))
    ;; Six cells, one null: five rows, nearest expiry first.
    (should (equal (mapcar (lambda (r) (list (plist-get r :dte) (plist-get r :strike) (plist-get r :iv))) rows)
                   '((11 769.0 0.2) (11 770.0 0.19) (32 768.0 0.26) (32 769.0 0.24) (32 770.0 0.22))))
    (dolist (r rows)
      (should (equal (plist-get r :time) "2026-10-30T16:00:00"))
      (should (eq (plist-get r :expiry) :null))
      ;; No underlying_price: the spot is strike / moneyness.
      (should (< (abs (- (plist-get r :spot) 769.64)) 0.01))))
  ;; underlying_price wins; the time defaults to now.
  (let ((row (aref (financial-chart-3d-vol-surface-rows
                    (append '(:underlying_price 770.5) (financial-chart-3d-vol-test--iv-surface)))
                   0)))
    (should (= (plist-get row :spot) 770.5))
    (should (string-prefix-p (format-time-string "%Y-%m-%d") (plist-get row :time))))
  (dolist (case '(((:strikes [1] :days_to_expiry [1] :iv [[0.1 0.2]] :underlying_price 1) "bad_shape")
                  ((:strikes [1] :days_to_expiry [1] :iv [[0.1]]) "missing_spot")
                  ((:strikes [] :days_to_expiry [1] :iv [[]] :underlying_price 1) "missing_strike")
                  ((:strikes [1] :iv [[0.1]] :underlying_price 1) "missing_dte")))
    (let ((err (should-error (financial-chart-3d-vol-surface-rows (car case)) :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :code) (cadr case)))))
  (let ((err (should-error (financial-chart-3d-vol-surface-rows [(:time "t" :surface (:iv [[0.1]]))])
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "missing_surface"))))

(ert-deftest financial-chart-3d-vol-surface-opt-iv-surface-opens ()
  ;; The real fixture, verbatim: one keyframe, a surface and an ATM line drawn.
  (let ((view (financial-chart-3d-vol-surface-open (financial-chart-3d-vol-test--iv-surface)
                                                   :symbol "NVDA" :id "fc3d-test-vol-iv")))
    (unwind-protect
        (let ((text (graphics-eas-braille-text (graphics-eas-frame view 60 24) 60)))
          (should (string-match-p "[^⠀\n]" text)))
      (financial-chart-3d--close view))))

(ert-deftest financial-chart-3d-vol-surface-live-size-stays-readable ()
  (let* ((rows (financial-chart-3d-vol-surface-rows (financial-chart-3d-vol-test--wide) :time "2026-10-30T16:00:00"))
         (wire (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "wire") (append rows nil)))
         (mesh (financial-chart-3d-vol-test--prim wire "surface"))
         (atm (financial-chart-3d-vol-test--prim wire "atm"))
         (solid (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") (append rows nil)))
         (surface (financial-chart-3d-vol-test--prim solid "surface"))
         (grid (financial-chart-3d-vol-test--prim solid "grid")))
    (should (< (length rows) (* 24 125)))
    ;; The wire mesh is downsampled to 41 strikes: 24 smiles of 40
    ;; segments and 7 term lines of 23; the ATM line still crosses every
    ;; expiry.
    (should (= (length (plist-get mesh :segments)) (+ (* 24 40) (* 7 23))))
    (should (= (length (plist-get atm :segments)) 23))
    ;; The solid surface keeps every strike, a tooltip per sample, and
    ;; draws 5 smiles of 124 segments and 7 term lines of 23 on it.
    (should (equal (plist-get surface :type) "heightfield"))
    (should (= (length (plist-get surface :heights)) 24))
    (should (seq-every-p (lambda (row) (= (length row) 125)) (plist-get surface :heights)))
    (should (= (length (plist-get surface :tooltips)) (* 24 125)))
    (should (= (length (plist-get grid :segments)) (+ (* 5 124) (* 7 23))))
    (let ((view (financial-chart-3d-vol-surface-open rows :id "fc3d-test-vol-wide")))
      (unwind-protect
          (let* ((frame (graphics-eas-frame view 60 24))
                 (text (graphics-eas-braille-text frame 60))
                 (lines (seq-filter (lambda (l) (string-match-p "[^⠀]" l)) (split-string text "\n" t))))
            ;; Drawn across most of the frame, solid.
            (should (>= (length lines) 12))
            (should (= (financial-chart-3d-vol-test--holes frame 60 24) 0)))
        (financial-chart-3d--close view)))))

(ert-deftest financial-chart-3d-vol-surface-series-replays ()
  (let* ((series (vector (list :time "2026-10-30T10:00:00" :surface (financial-chart-3d-vol-test--wide))
                         (list :time "2026-10-30T12:00:00" :surface (financial-chart-3d-vol-test--wide 0.08))
                         (list :time "2026-10-30T14:00:00" :surface (financial-chart-3d-vol-test--wide 0.02))))
         (rows (financial-chart-3d-vol-surface-rows series))
         (view (financial-chart-3d-vol-surface-open series :symbol "NVDA" :id "fc3d-test-vol-series")))
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :time)) rows))
                   '("2026-10-30T10:00:00" "2026-10-30T12:00:00" "2026-10-30T14:00:00")))
    (unwind-protect
        (let ((timeline (graphics-eas-timeline view)))
          (should (>= (plist-get timeline :keyframes) 3))
          (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
          (let ((open (graphics-eas-frame view 60 24)))
            (financial-chart-3d-seek-end view)
            (should-not (equal open (graphics-eas-frame view 60 24)))))
      (financial-chart-3d--close view))))

;;; The mark

(ert-deftest financial-chart-3d-vol-surface-mark-wire-and-surface ()
  (let* ((wire (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "wire" :strike_every 1)
                                                     financial-chart-3d-vol-test--grid))
         (mesh (financial-chart-3d-vol-test--prim wire "surface"))
         (atm (financial-chart-3d-vol-test--prim wire "atm"))
         ;; The solid surface is the default.
         (solid (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") financial-chart-3d-vol-test--grid))
         (surface (financial-chart-3d-vol-test--prim solid "surface"))
         (grid (financial-chart-3d-vol-test--prim solid "grid"))
         (heights (plist-get surface :heights)))
    (should-not (financial-chart-3d-vol-test--prim wire "grid"))
    ;; Two smiles of two segments and three term lines of one, on the
    ;; surface: the grid's vertices are the heightfield's.
    (should (= (length (plist-get grid :segments)) 7))
    (let ((base (plist-get surface :base)) (scale (plist-get surface :y_scale)))
      (should (< (abs (- (aref (aref (plist-get grid :segments) 0) 1)
                         (+ base (* (aref (aref heights 0) 0) scale))))
                 (* 0.02 4))))
    ;; Two smiles of two segments, three strikes' term lines of one.
    (should (equal (plist-get mesh :type) "segments"))
    (should (= (length (plist-get mesh :segments)) 7))
    (should (= (length (plist-get mesh :colors)) 7))
    ;; The at-the-money term structure: one segment between two expiries.
    (should (= (length (plist-get atm :segments)) 1))
    (should (equal (plist-get surface :type) "heightfield"))
    ;; Rows are expiries, nearest first; the missing cell is interpolated.
    (should (equal heights [[0.3 0.2 0.24] [0.22 0.2 0.18]]))
    (should (equal (plist-get surface :color_range) [0.18 0.3]))
    ;; No rows, or a single strike: an empty surface, not an error.
    (should (equal (plist-get (financial-chart-3d-vol-test--prim
                               (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") nil)
                               "surface")
                              :segments)
                   []))))

(ert-deftest financial-chart-3d-vol-surface-mark-spans-the-session ()
  ;; Under a time encoding the scales are every snapshot's: a quieter
  ;; snapshot draws lower on the same axes instead of filling them.
  (let* ((later (mapcar (lambda (r) (plist-put (copy-sequence r) :iv (- (plist-get r :iv) 0.05)))
                        financial-chart-3d-vol-test--grid))
         (alone (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "surface") later))
         (spanned (let ((graphics-eas-domain-rows financial-chart-3d-vol-test--grid))
                    (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "surface") later))))
    (cl-flet ((range (prims) (mapcar (lambda (v) (/ (round (* v 1000)) 1000.0))
                                     (plist-get (financial-chart-3d-vol-test--prim prims "surface") :color_range))))
      (should (equal (range alone) '(0.13 0.25)))
      (should (equal (range spanned) '(0.13 0.3))))))

;;; Contracts and tooltips

(defun financial-chart-3d-vol-test--schwab ()
  "A Schwab chain for the opt.iv_surface fixture's cells, raw: its
callExpDateMap and putExpDateMap (the 11-day 768 strike has none)."
  (eas-json-read-file (expand-file-name "test/fixtures/3d/vol-surface/schwab-chain.json" financial-chart-test-root)))

(defun financial-chart-3d-vol-test--tooltips (prims)
  "The tooltips of PRIMS's surface, a vector, row-major as its heights."
  (plist-get (financial-chart-3d-vol-test--prim prims "surface") :tooltips))

(ert-deftest financial-chart-3d-vol-surface-contracts-from-a-chain ()
  (let* ((contracts (financial-chart-3d-vol-surface-contracts (financial-chart-3d-vol-test--schwab)))
         (put (seq-find (lambda (c) (equal (plist-get c :symbol) "NVDA  261201P00768000")) contracts)))
    (should (= (length contracts) 10))
    (should (equal (mapcar (lambda (k) (plist-get put k)) '(:side :strike :dte :expiry :iv :bid :ask :mark :delta :oi :volume))
                   '("PUT" 768.0 32 "2026-12-01" 0.26 23.55 23.85 23.7 -0.47 2096 528))))
  ;; A list of contracts; IV in percent or as a fraction; Schwab's -999 is none.
  (should (equal (mapcar (lambda (c) (list (plist-get c :side) (plist-get c :strike) (plist-get c :dte) (plist-get c :iv)))
                         (financial-chart-3d-vol-surface-contracts
                          [(:symbol "A" :putCall "CALL" :strike 100 :daysToExpiration 7 :volatility 18.5)
                           (:symbol "B" :putCall "put" :strikePrice 105 :expirationDate "2026-11-06T20:00:00.000+00:00"
                                    :volatility 0.21)
                           (:symbol "C" :putCall "PUT" :strike 110 :daysToExpiration 7 :volatility -999.0)]))
                 '(("CALL" 100 7 0.185) ("PUT" 105 nil 0.21) ("PUT" 110 7 nil))))
  ;; In a map, a contract takes its side, strike and expiry from where it sits.
  (should (equal (financial-chart-3d-vol-surface-contracts
                  '(("putExpDateMap" . (("2026-11-06:7" . (("100.0" . [(:symbol "X" :bid 1.5)])))))))
                 '((:symbol "X" :side "PUT" :strike 100.0 :dte 7 :expiry "2026-11-06" :bid 1.5 :ask nil :mark nil
                            :last nil :iv nil :delta nil :gamma nil :theta nil :vega nil :oi nil :volume nil))))
  (dolist (case '(("chain" "bad_chain" nil)
                  ([(:putCall "CALL" :daysToExpiration 7)] "bad_contract" "strike")
                  ([(:putCall "STRADDLE" :strike 1 :daysToExpiration 7)] "bad_contract" "putCall")
                  ([(:putCall "CALL" :strike 1)] "bad_contract" "daysToExpiration")))
    (let ((err (should-error (financial-chart-3d-vol-surface-contracts (car case)) :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :code) (nth 1 case)))
      (should (equal (plist-get (cddr err) :field) (or (nth 2 case) "chain"))))))

(ert-deftest financial-chart-3d-vol-surface-tooltips-name-the-contracts ()
  (let* ((rows (financial-chart-3d-vol-surface-rows (financial-chart-3d-vol-test--iv-surface)
                                                    :time "2026-10-30T16:00:00"
                                                    :contracts (financial-chart-3d-vol-test--schwab)))
         (tips (financial-chart-3d-vol-test--tooltips
                (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") (append rows nil))))
         (at (lambda (j i) (aref tips (+ (* j 3) i)))))
    ;; Rows take their contracts and, lacking one, the contracts' expiry.
    (should (equal (plist-get (aref rows 2) :expiry) "2026-12-01"))
    (should (string-prefix-p "CALL NVDA  261201C00768000" (plist-get (aref rows 2) :contracts)))
    ;; Two expiries by three strikes, nearest expiry first.
    (should (= (length tips) 6))
    (should (equal (funcall at 1 0)
                   (concat "K 768  2026-12-01 (32 DTE)\nIV 26.0%  moneyness 0.998\n"
                           "CALL NVDA  261201C00768000  bid 25.19 ask 25.49 mark 25.34  IV 26.0%  Δ 0.53  OI 1411  vol 323\n"
                           "PUT NVDA  261201P00768000  bid 23.55 ask 23.85 mark 23.70  IV 26.0%  Δ -0.47  OI 2096  vol 528")))
    ;; Every quoted cell names its own call and put, and no other.
    (cl-loop for (j i strike date) in '((0 1 769 "261110") (0 2 770 "261110") (1 1 769 "261201") (1 2 770 "261201"))
             do (let ((tip (funcall at j i)))
                  (should (string-match-p (format "^CALL NVDA  %sC00%d000  " date strike) tip))
                  (should (string-match-p (format "^PUT NVDA  %sP00%d000  " date strike) tip))
                  (should (= (cl-count ?\n tip) 3))))
    ;; The 11-day 768 strike has no quote: interpolated, no contract.
    (should (equal (funcall at 0 0)
                   (concat "K 768  2026-11-10 (11 DTE)\nIV 20.0% (interpolated)  moneyness 0.998\n"
                           "interpolated from strike 769 (the nearest): no quote here")))))

(ert-deftest financial-chart-3d-vol-surface-tooltips-without-a-chain ()
  (let ((tips (financial-chart-3d-vol-test--tooltips
               (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") financial-chart-3d-vol-test--grid))))
    ;; Strike, DTE, IV and moneyness; the missing cell between its neighbours.
    (should (equal (append tips nil)
                   '("K 90  7 DTE\nIV 30.0%  moneyness 0.900" "K 100  7 DTE\nIV 20.0%  moneyness 1.000"
                     "K 110  7 DTE\nIV 24.0%  moneyness 1.100" "K 90  30 DTE\nIV 22.0%  moneyness 0.900"
                     "K 100  30 DTE\nIV 20.0% (interpolated)  moneyness 1.000\ninterpolated between strikes 90 and 110: no quote here"
                     "K 110  30 DTE\nIV 18.0%  moneyness 1.100"))))
  ;; An expiry with no IV at this quote time (but at others) is filled
  ;; along each strike's term structure, and says so.
  (let* ((now (seq-filter (lambda (r) (= (plist-get r :dte) 7)) financial-chart-3d-vol-test--grid))
         (prims (let ((graphics-eas-domain-rows financial-chart-3d-vol-test--grid))
                  (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") now)))
         (surface (financial-chart-3d-vol-test--prim prims "surface")))
    (should (equal (plist-get surface :heights) [[0.3 0.2 0.24] [0.3 0.2 0.24]]))
    (should (equal (aref (plist-get surface :tooltips) 3)
                   "K 90  30 DTE\nIV 30.0% (interpolated)  moneyness 0.900\ninterpolated from 7 DTE (the nearest expiry): no quote here"))))

(ert-deftest financial-chart-3d-vol-surface-opens-with-a-chain ()
  ;; Through `open' and through the metric adapter: the surface the view
  ;; draws carries each cell's contracts in its tooltip.
  (dolist (open (list (lambda (id) (financial-chart-3d-vol-surface-open
                                    (financial-chart-3d-vol-test--iv-surface) :time "2026-10-30T16:00:00"
                                    :chain (financial-chart-3d-vol-test--schwab) :id id))
                      ;; Rows already, the chain as a list of contracts.
                      (lambda (id) (financial-chart-3d-vol-surface-open
                                    (financial-chart-3d-vol-surface-rows (financial-chart-3d-vol-test--iv-surface)
                                                                         :time "2026-10-30T16:00:00")
                                    :chain (vconcat (financial-chart-3d--vol-map-contracts
                                                     (financial-chart-3d-vol-test--schwab)))
                                    :id id))
                      (lambda (id) (financial-chart-3d-open
                                    "vol-surface"
                                    :bindings (financial-chart-3d-bindings-from-metric
                                               "vol-surface" "opt.iv_surface" (financial-chart-3d-vol-test--iv-surface)
                                               :time "2026-10-30T16:00:00" :symbol "NVDA"
                                               :chain (financial-chart-3d-vol-test--schwab))
                                    :id id))))
    (let ((view (funcall open "fc3d-test-vol-chain")))
      (unwind-protect
          (progn
            (graphics-eas-frame view 60 24)
            (let* ((shown (graphics-eas-anim-shown (graphics-eas-view-anim (graphics-eas-get view))))
                   (tips (financial-chart-3d-vol-test--tooltips shown)))
              (should (= (length tips) 6))
              (should (string-match-p "^CALL NVDA  261201C00769000  bid 22.37 " (aref tips 4)))
              (should (string-match-p "^PUT NVDA  261110P00770000  bid 10.36 " (aref tips 2)))
              (should (string-match-p "interpolated" (aref tips 0)))))
        (financial-chart-3d--close view)))))

(ert-deftest financial-chart-3d-vol-surface-solid-at-every-size-and-phase ()
  ;; The default draws a solid surface: no gap inside its footprint at
  ;; any framing size and phase.  The wire mesh, for contrast, has them.
  (graphics-ensure-core 'rs)
  (let ((view (financial-chart-3d-open "vol-surface" :id "fc3d-test-vol-solid"))
        holes)
    (unwind-protect
        (dolist (phase financial-chart-3d-framing-phases)
          (graphics-eas-timeline-set view :seconds (* phase (plist-get (graphics-eas-timeline view) :duration))
                                     :playing nil)
          (pcase-dolist (`(,cols . ,rows) financial-chart-3d-framing-sizes)
            (let ((n (financial-chart-3d-vol-test--holes (graphics-eas-frame view cols rows) cols rows)))
              (unless (zerop n) (push (format "%dx%d t=%s: %d holes" cols rows phase n) holes)))))
      (financial-chart-3d--close view))
    (should (equal holes nil)))
  (let* ((rows (financial-chart-3d-vol-surface-rows (financial-chart-3d-vol-test--chain)))
         (view (financial-chart-3d-open "vol-surface" :bindings (list :data rows) :id "fc3d-test-vol-wire")))
    (unwind-protect
        (let ((g (graphics-eas-get view)))
          (graphics-eas-attach view (plist-put (copy-sequence (graphics-eas-view-graphics g)) :style "wire"))
          (should (> (financial-chart-3d-vol-test--holes (graphics-eas-frame view 100 40) 100 40) 0)))
      (financial-chart-3d--close view))))

;;; The 3D view

(ert-deftest financial-chart-3d-vol-surface-replays-the-session ()
  (let ((view (financial-chart-3d-vol-surface-open (financial-chart-3d-vol-test--chain) :id "fc3d-test-vol")))
    (unwind-protect
        (let ((timeline (graphics-eas-timeline view)))
          (should (member "fc-vol-surface3d" (graphics-eas-all-marks)))
          (should (>= (plist-get timeline :keyframes) 8))
          (should (plist-get timeline :loop))
          (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
          (let ((open (graphics-eas-frame view 60 24)))
            (financial-chart-3d-seek-end view)
            (should-not (equal open (graphics-eas-frame view 60 24)))))
      (financial-chart-3d--close view))))

(ert-deftest financial-chart-3d-vol-surface-golden ()
  (graphics-ensure-core 'rs)
  (let* ((dir (expand-file-name "test/golden/3d" financial-chart-test-root))
         (frame (financial-chart-3d-frame "vol-surface"))
         (tmp (make-temp-file "fc3d-vol" nil ".frame")))
    (unwind-protect
        (progn
          (graphics-frame-write-file tmp frame 60 24)
          (should (equal (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally tmp) (buffer-string))
                         (with-temp-buffer (set-buffer-multibyte nil)
                                           (insert-file-contents-literally (expand-file-name "vol-surface.frame" dir))
                                           (buffer-string)))))
      (delete-file tmp))
    (let ((text (graphics-eas-braille-text frame 60)))
      (should (equal text (with-temp-buffer (let ((coding-system-for-read 'utf-8))
                                              (insert-file-contents (expand-file-name "vol-surface.txt" dir)))
                                            (buffer-string))))
      ;; Something drawn across most of the frame, not a blob or a speck.
      (let ((lines (seq-filter (lambda (l) (string-match-p "[^⠀]" l)) (split-string text "\n" t))))
        (should (>= (length lines) 14))))))

;;; Without graphics.el

(ert-deftest financial-chart-3d-vol-surface-validates-without-graphics ()
  (let* ((eas (file-name-directory (locate-library "eas")))
         (root financial-chart-test-root)
         (dirs (mapcar (lambda (d) (expand-file-name d root))
                       '("src" "src/core" "src/indicators" "src/charts" "src/integrations")))
         (form `(progn
                  (require 'financial-chart-3d)
                  (when (or (featurep 'graphics-eas) (financial-chart-3d-available-p))
                    (error "graphics.el was found"))
                  (dolist (name '("vol-surface" "fc3d/vol-surface"))
                    (let ((spec (eas-resolve name (eas-template-example name))))
                      (when (eas-spec-check spec) (error "%s: %S" name (eas-spec-check spec)))
                      (unless (> (length (eas-text-render (eas-compile spec :target 'text :size '(:cols 60 :rows 14))))
                                 0)
                        (error "%s did not draw in 2D" name))))
                  ;; The 2D heatmap is the latest quote's: one cell per strike and expiry.
                  (let* ((rows (financial-chart-3d-vol-surface-rows
                                (eas-json-read-file ,(expand-file-name financial-chart-3d-vol-surface-fixture root))))
                         (svg (eas-svg-render (eas-compile (eas-resolve "vol-surface" (list :data rows))
                                                           :target 'svg :size '(640 . 360))))
                         (cells (with-temp-buffer (insert svg) (count-matches "<rect" (point-min) (point-max))))
                         (all (with-temp-buffer
                                (insert (eas-svg-render (eas-compile (eas-resolve "vol-surface" (list :data (seq-take rows 136)))
                                                                     :target 'svg :size '(640 . 360))))
                                (count-matches "<rect" (point-min) (point-max)))))
                    (unless (= cells all) (error "2D drew %d rects, one snapshot draws %d" cells all))
                    (condition-case err (progn (financial-chart-3d-vol-surface-open rows) (error "no error"))
                      (financial-chart-3d-unavailable (princ (format "OK %s" (plist-get (cddr err) :code))))))))
         (output (with-temp-buffer
                   (list (apply #'call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" eas
                                (append (mapcan (lambda (d) (list "-L" d)) dirs)
                                        (list "--eval" (prin1-to-string form))))
                         (buffer-string)))))
    (should (equal output (list 0 "OK GRAPHICS_MISSING")))))

(provide 'financial-chart-3d-vol-surface-test)
;;; financial-chart-3d-vol-surface-test.el ends here
