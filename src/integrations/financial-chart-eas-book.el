;;; financial-chart-eas-book.el --- live order books streamed as deltas into eas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Live order books (fc-gbo.4).  The caller supplies a snapshot once
;; and then streams deltas; the book is kept here and every frame is
;; the visible book, pushed through eas streaming (frame cap, paused
;; while the pointer is over the chart):
;;
;;   (setq view (financial-chart-book-open
;;               '(:bids [[100.9 2] [100.8 4]] :asks [[101.1 1.5] [101.2 3]])
;;               :template "ladder" :levels 20 :flash 0.6 :show t))
;;   (financial-chart-book-push view
;;     [(:op "update" :side "bid" :price 100.9 :size 3)
;;      (:op "insert" :side "ask" :price 101.15 :size 2)
;;      (:op "delete" :side "ask" :price 101.2)])
;;
;; A delta is {op: insert|update|delete|set, side: bid|ask, price,
;; size}; without op, size 0 deletes and any other size sets.  A batch
;; is atomic: a bad delta (INVALID_DELTA), an insert of a level that
;; exists (DUPLICATE_LEVEL), an update or delete of one that does not
;; (UNKNOWN_LEVEL) or a batch that leaves the best bid above the best
;; ask (CROSSED_BOOK) signals `financial-chart-invalid-book' with
;; :code, :index and :path and changes nothing.
;;
;; Rows (`financial-chart-book-rows') are the nearest LEVELS bids and
;; asks, each {side, price, price_label, size, cumulative, level,
;; changed, mid, spread, label}, plus one {side: "mid"} row at the mid
;; price whose label states mid and spread.  Prices are labelled to the
;; book's tick (a snapshot's or `financial-chart-book-open's :tick, else
;; the smallest step between its prices), so a feed computing prices
;; (100.95 - 0.1 * i) shows 102.15, not 102.14999999999999; incoming
;; prices are rounded to 12 significant digits so such a delta finds
;; the snapshot's level.  The "ladder" and "depth-live" templates
;; (templates/financial/) draw them.
;;
;; eas's push appends rows.  A frame replaces the book by setting the
;; stream window to the frame's row count, so the view keeps exactly
;; the newest snapshot.  While a frame is queued (cap or hover) newer
;; deltas mark the book dirty instead of queueing more snapshots, and
;; the frame after it carries the latest book.  Keyed replacement in
;; push would let a frame carry only the changed levels; that is an
;; eas.el request (docs/design/order-book.md).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'eas-stream)
(require 'financial-chart-core)
(require 'financial-chart-eas)

(declare-function eas-show "eas-mode" (view &optional target))

(define-error 'financial-chart-invalid-book
  "financial-chart: invalid order book or delta" 'financial-chart-error)

(defvar financial-chart-book-levels 20
  "Default price levels drawn per side.")

(defvar financial-chart-book-flash 0.6
  "Default seconds a changed level stays flashed; nil turns flashing off.")

(defvar financial-chart-book-max-fps nil
  "Frame cap of a live book view; nil picks it from the levels drawn.
See `financial-chart-book-default-fps'.")

(defun financial-chart-book-default-fps (levels)
  "The frame cap for LEVELS per side: 10 up to 50, 8 up to 100, else 5.
Measured byte-compiled frames (docs/design/order-book.md) cost up to
20, 36 and 75 ms at 50, 100 and 200 levels, so these caps keep a live
book under about half of one core on either backend."
  (cond ((<= levels 50) 10) ((<= levels 100) 8) (t 5)))

(defvar financial-chart-book-templates '("ladder" "depth-live")
  "Templates that draw `financial-chart-book-rows'.")

(cl-defstruct (financial-chart-book (:constructor financial-chart-book--make) (:copier nil))
  "An order book: price -> size per side, and when each level last changed."
  (bids (make-hash-table :test 'eql))
  (asks (make-hash-table :test 'eql))
  (changed (make-hash-table :test 'equal))
  (deltas 0)
  (fixed-tick nil))

(defun financial-chart-book--fail (code path index format-string &rest args)
  "Signal `financial-chart-invalid-book' with CODE at PATH and INDEX.
The message is FORMAT-STRING applied to ARGS."
  (signal 'financial-chart-invalid-book
          (append (list (apply #'format format-string args) :code code :path path)
                  (and index (list :index index)))))

;;; Snapshots

(defun financial-chart-book--side-key (side)
  "The :bids or :asks key for SIDE (\"bid\", \"ask\", or a symbol), or nil."
  (pcase (if (symbolp side) (symbol-name side) side)
    ((or "bid" "bids" ":bids") :bids)
    ((or "ask" "asks" ":asks") :asks)))

(defun financial-chart-book--table (book side)
  "BOOK's price table for SIDE (:bids or :asks)."
  (if (eq side :bids) (financial-chart-book-bids book) (financial-chart-book-asks book)))

(defun financial-chart-book--price (price)
  "PRICE as a float rounded to 12 significant digits.
A feed that computes prices (100.95 - 0.1 * i) leaves float noise;
rounding makes a delta's price the same key as the snapshot's."
  (float (string-to-number (format "%.12g" (float price)))))

(defun financial-chart-book--level (level)
  "LEVEL ([PRICE SIZE], (PRICE SIZE) or {price, size}) as (PRICE . SIZE), or nil."
  (cond ((keywordp (car-safe level))
         (cons (plist-get level :price) (plist-get level :size)))
        ((and (sequencep level) (not (stringp level)) (= (length level) 2))
         (cons (elt level 0) (elt level 1)))))

(defun financial-chart-book--fill-side (book side levels)
  "Put LEVELS (a snapshot's SIDE) into BOOK, signalling INVALID_BOOK."
  (let ((table (financial-chart-book--table book side)) (index -1)
        (name (substring (symbol-name side) 1)))
    (seq-doseq (raw (if (eq levels :null) nil levels))
      (setq index (1+ index))
      (let* ((level (financial-chart-book--level raw))
             (price (car level)) (size (cdr level))
             (path (format "/%s/%d" name index)))
        (unless (and (numberp price) (> price 0) (numberp size) (> size 0))
          (financial-chart-book--fail "INVALID_BOOK" path index
                                      "%s level %d is %S; levels are [price, size], both positive"
                                      name index raw))
        (setq price (financial-chart-book--price price))
        (when (gethash price table)
          (financial-chart-book--fail "INVALID_BOOK" path index
                                      "%s level %d repeats price %s; each price appears once per side"
                                      name index price))
        (puthash price size table)))))

(defun financial-chart-book--tick-option (tick)
  "TICK (nil or a positive number) as a book's tick; signals INVALID_BOOK."
  (unless (or (null tick) (and (numberp tick) (> tick 0)))
    (financial-chart-book--fail "INVALID_BOOK" "/tick" nil
                                "The tick is %S; a tick is a positive number (or omit it to infer one)" tick))
  (and tick (financial-chart-book--price tick)))

(defun financial-chart-book-make (&optional snapshot)
  "A book holding SNAPSHOT: {bids: [[PRICE, SIZE], ...], asks: [...], tick}.
Levels may also be (PRICE SIZE) lists or {price, size} objects.  The
optional tick (a positive number) is the price increment labels are
printed to; without it the book infers one (`financial-chart-book-tick').
Signals INVALID_BOOK naming the bad level, or CROSSED_BOOK."
  (let ((book (financial-chart-book--make)))
    (unless (and (eas-object-p snapshot)
                 (seq-every-p (lambda (k) (memq k '(:bids :asks :tick))) (eas-plist-keys snapshot)))
      (financial-chart-book--fail "INVALID_BOOK" "" nil
                                  "An order book is an object {bids: [[price, size], ...], asks: [...], tick?}, got %S"
                                  snapshot))
    (setf (financial-chart-book-fixed-tick book) (financial-chart-book--tick-option (plist-get snapshot :tick)))
    (financial-chart-book--fill-side book :bids (plist-get snapshot :bids))
    (financial-chart-book--fill-side book :asks (plist-get snapshot :asks))
    (financial-chart-book--check-crossed book nil)
    book))

(defun financial-chart-book--best (table side)
  "The best price in TABLE for SIDE (highest bid, lowest ask), or nil."
  (let (best)
    (maphash (lambda (price _)
               (when (or (null best) (if (eq side :bids) (> price best) (< price best)))
                 (setq best price)))
             table)
    best))

(defun financial-chart-book--check-crossed (book index)
  "Signal CROSSED_BOOK when BOOK's best bid is above its best ask.
INDEX is the last delta of the batch to report, or nil for a snapshot."
  (let ((bid (financial-chart-book--best (financial-chart-book-bids book) :bids))
        (ask (financial-chart-book--best (financial-chart-book-asks book) :asks)))
    (when (and bid ask (> bid ask))
      (financial-chart-book--fail
       "CROSSED_BOOK" (if index (format "/%d" index) "") index
       "Best bid %s is above best ask %s; send deltas that uncross it or resend the book with financial-chart-book-reset"
       bid ask))))

;;; Deltas

(defun financial-chart-book--delta (delta index)
  "DELTA at INDEX checked, as (OP SIDE PRICE SIZE); signals INVALID_DELTA."
  (let ((path (format "/%d" index)))
    (unless (and delta (eas-object-p delta))
      (financial-chart-book--fail "INVALID_DELTA" path index
                                  "Delta %d is %S; a delta is {op, side, price, size}" index delta))
    (let* ((side (financial-chart-book--side-key (plist-get delta :side)))
           (price (plist-get delta :price))
           (size (plist-get delta :size))
           (op (or (plist-get delta :op) (if (and (numberp size) (zerop size)) "delete" "set"))))
      (unless (member op '("insert" "update" "delete" "set"))
        (financial-chart-book--fail "INVALID_DELTA" (concat path "/op") index
                                    "Delta %d has op %S; ops: insert, update, delete, set" index op))
      (unless side
        (financial-chart-book--fail "INVALID_DELTA" (concat path "/side") index
                                    "Delta %d has side %S; sides: bid, ask" index (plist-get delta :side)))
      (unless (and (numberp price) (> price 0))
        (financial-chart-book--fail "INVALID_DELTA" (concat path "/price") index
                                    "Delta %d has price %S; a price is a positive number" index price))
      (unless (or (equal op "delete") (and (numberp size) (> size 0)))
        (financial-chart-book--fail
         "INVALID_DELTA" (concat path "/size") index
         "Delta %d (%s) has size %S; a size is a positive number (delete removes a level)" index op size))
      (list op side (financial-chart-book--price price) size))))

(defun financial-chart-book--check-op (op side price old index)
  "Signal when OP on SIDE's PRICE (now OLD, or nil) at INDEX is out of sync."
  (let ((name (if (eq side :bids) "bid" "ask")))
    (cond ((and (equal op "insert") old)
           (financial-chart-book--fail
            "DUPLICATE_LEVEL" (format "/%d" index) index
            "Delta %d inserts %s %s, which exists (size %s); use update or set" index name price old))
          ((and (member op '("update" "delete")) (not old))
           (financial-chart-book--fail
            "UNKNOWN_LEVEL" (format "/%d" index) index
            "Delta %d %ss %s %s, which is not in the book; the feed is out of sync: resend the book with financial-chart-book-reset"
            index op name price)))))

(defun financial-chart-book-apply (book deltas &optional now)
  "Apply DELTAS (a vector or list) to BOOK at time NOW; return BOOK.
The batch is atomic: any failure signals `financial-chart-invalid-book'
and leaves BOOK unchanged.  Changed levels are stamped with NOW
\(default `eas-stream-clock') for flashing."
  (let* ((now (or now (funcall eas-stream-clock)))
         (trial (financial-chart-book--make
                 :bids (copy-hash-table (financial-chart-book-bids book))
                 :asks (copy-hash-table (financial-chart-book-asks book))
                 :fixed-tick (financial-chart-book-fixed-tick book)))
         (index -1) touched)
    (seq-doseq (raw deltas)
      (setq index (1+ index))
      (pcase-let* ((`(,op ,side ,price ,size) (financial-chart-book--delta raw index))
                   (table (financial-chart-book--table trial side)))
        (financial-chart-book--check-op op side price (gethash price table) index)
        (if (equal op "delete") (remhash price table) (puthash price size table))
        (push (cons side price) touched)))
    (financial-chart-book--check-crossed trial index)
    (setf (financial-chart-book-bids book) (financial-chart-book-bids trial)
          (financial-chart-book-asks book) (financial-chart-book-asks trial))
    (cl-incf (financial-chart-book-deltas book) (1+ index))
    (dolist (key touched) (puthash key now (financial-chart-book-changed book)))
    book))

;;; Tick size

(defconst financial-chart-book--max-decimals 10
  "The most decimal places a price is printed with.")

(defun financial-chart-book--decimals (number)
  "The fewest decimal places (at most ten) that print NUMBER exactly."
  (let ((d 0))
    (while (and (< d financial-chart-book--max-decimals)
                (let ((y (* number (expt 10.0 d)))) (> (abs (- y (fround y))) 1e-6)))
      (setq d (1+ d)))
    d))

(defun financial-chart-book--fmt (number decimals)
  "NUMBER printed with DECIMALS places."
  (format (format "%%.%df" decimals) number))

(defun financial-chart-book--round (number decimals)
  "NUMBER rounded to DECIMALS places."
  (float (string-to-number (financial-chart-book--fmt number decimals))))

(defun financial-chart-book-tick (book)
  "BOOK's tick and price precision as (TICK . DECIMALS).
TICK is the book's tick option, else the smallest step between two
of its prices (nil with fewer than two).  DECIMALS prints every price:
the tick option's places, else the most any price needs."
  (if-let* ((tick (financial-chart-book-fixed-tick book)))
      (cons tick (financial-chart-book--decimals tick))
    (let (prices (decimals 0) step)
      (dolist (table (list (financial-chart-book-bids book) (financial-chart-book-asks book)))
        (maphash (lambda (p _) (push p prices)) table))
      (setq prices (sort (delete-dups prices) #'<))
      (dolist (p prices) (setq decimals (max decimals (financial-chart-book--decimals p))))
      (cl-loop for (a b) on prices while b
               do (setq step (if step (min step (- b a)) (- b a))))
      (cons (and step (financial-chart-book--round step decimals)) decimals))))

(defun financial-chart-book--mid-decimals (mid decimals)
  "Places for MID between prices of DECIMALS places.
One more than DECIMALS when halving the two prices needs it."
  (if (> (financial-chart-book--decimals mid) decimals) (1+ decimals) decimals))

;;; Rows

(defun financial-chart-book--sorted (book side levels)
  "The nearest LEVELS of BOOK's SIDE as (PRICE . SIZE), best first."
  (let (out)
    (maphash (lambda (p s) (push (cons p s) out)) (financial-chart-book--table book side))
    (setq out (sort out (if (eq side :bids) (lambda (a b) (> (car a) (car b)))
                          (lambda (a b) (< (car a) (car b))))))
    (if (and levels (> (length out) levels)) (seq-take out levels) out)))

(defun financial-chart-book-summary (book)
  "BOOK's top of book: (:best-bid :best-ask :mid :spread :tick :decimals
:bids :asks :deltas).  Mid and spread are rounded to the tick's
precision (DECIMALS, see `financial-chart-book-tick').  Missing prices
and an unknown tick are :null."
  (let* ((bid (financial-chart-book--best (financial-chart-book-bids book) :bids))
         (ask (financial-chart-book--best (financial-chart-book-asks book) :asks))
         (tick (financial-chart-book-tick book))
         (decimals (cdr tick))
         (mid (and bid ask (/ (+ bid ask) 2.0))))
    (list :best-bid (or bid :null) :best-ask (or ask :null)
          :mid (if mid (financial-chart-book--round
                        mid (financial-chart-book--mid-decimals mid decimals))
                 :null)
          :spread (if (and bid ask) (financial-chart-book--round (- ask bid) decimals) :null)
          :tick (or (car tick) :null) :decimals decimals
          :bids (hash-table-count (financial-chart-book-bids book))
          :asks (hash-table-count (financial-chart-book-asks book))
          :deltas (financial-chart-book-deltas book))))

(defun financial-chart-book--mid-row (summary)
  "The {side: \"mid\"} row for book SUMMARY."
  (let* ((spread (plist-get summary :spread))
         (decimals (plist-get summary :decimals))
         (price (seq-find #'numberp (list (plist-get summary :mid) (plist-get summary :best-bid)
                                          (plist-get summary :best-ask) 0)))
         (label (financial-chart-book--fmt
                 price (financial-chart-book--mid-decimals price decimals))))
    (list :side "mid" :price price :price_label label :size 0 :cumulative 0 :level -1 :changed 0
          :mid price :spread (if (numberp spread) spread 0)
          :label (if (numberp spread)
                     (format "mid %s  spread %s" label (financial-chart-book--fmt spread decimals))
                   "spread -"))))

(defun financial-chart-book--side-rows (book side levels mid decimals flash now)
  "Rows of BOOK's nearest LEVELS on SIDE, best first, with MID's fields.
Prices are labelled with DECIMALS places.  A level stamped within
FLASH seconds of NOW has changed 1."
  (let ((cum 0) (level -1) (name (if (eq side :bids) "bid" "ask"))
        (changed (financial-chart-book-changed book)))
    (mapcar (lambda (ps)
              (let ((at (gethash (cons side (car ps)) changed)))
                (setq cum (+ cum (cdr ps)) level (1+ level))
                (list :side name :price (car ps)
                      :price_label (financial-chart-book--fmt (car ps) decimals)
                      :size (cdr ps) :cumulative cum :level level
                      :changed (if (and flash at (< (- now at) flash)) 1 0)
                      :mid (plist-get mid :mid) :spread (plist-get mid :spread) :label "")))
            (financial-chart-book--sorted book side levels))))

(cl-defun financial-chart-book-rows (book &key levels (flash financial-chart-book-flash) now)
  "BOOK's nearest LEVELS per side as eas rows: asks high to low, mid, bids.
Each row is {side, price, price_label, size, cumulative, level, changed,
mid, spread, label}; level 0 is the best price and cumulative sums size
outward from it.  Price_label is the price at the book's tick precision
\(`financial-chart-book-tick'), the mid row's one place finer when the
mid falls between ticks.  Changed is 1 for a level changed within
FLASH seconds of NOW.  The {side: \"mid\"} row sits at the mid price
\(the only side's best, or 0, when a side is empty) and its label
states mid and spread."
  (let* ((now (or now (funcall eas-stream-clock)))
         (levels (or levels financial-chart-book-levels))
         (summary (financial-chart-book-summary book))
         (decimals (plist-get summary :decimals))
         (mid (financial-chart-book--mid-row summary))
         (changed (financial-chart-book-changed book)))
    ;; Forget stamps that can no longer flash.
    (maphash (lambda (k at) (unless (and flash (< (- now at) flash)) (remhash k changed))) changed)
    (vconcat (nreverse (financial-chart-book--side-rows book :asks levels mid decimals flash now))
             (list mid)
             (financial-chart-book--side-rows book :bids levels mid decimals flash now))))

;;; Live views

(defvar financial-chart-book--views (make-hash-table :test 'equal)
  "Live book views by view id.
Each is a plist (:view :book :levels :flash :max-fps :tick :dirty :timer).")

(defun financial-chart-book--live (view)
  "The live book state of VIEW (an id or view); signal NO_BOOK without one."
  (let* ((id (if (eas-view-p view) (eas-view-id view) view))
         (live (gethash id financial-chart-book--views)))
    (unless (and live (eq (gethash id eas-views) (plist-get live :view)))
      (remhash id financial-chart-book--views)
      (financial-chart-book--fail "NO_BOOK" "" nil
                                  "View %S is not a live order book; open one with financial-chart-book-open" id))
    live))

(cl-defun financial-chart-book-open (snapshot &key (template "ladder") levels tick
                                              (flash financial-chart-book-flash)
                                              max-fps title subject size target show)
  "Open a live eas view of the order book SNAPSHOT and return it.
SNAPSHOT is as in `financial-chart-book-make' (or a book).  TEMPLATE is
\"ladder\" or \"depth-live\"; LEVELS per side are drawn (default
`financial-chart-book-levels'); FLASH seconds highlight changed levels
\(nil: off); MAX-FPS caps frames (default `financial-chart-book-max-fps',
else `financial-chart-book-default-fps'); TICK fixes the price
increment labels are printed to (default: the snapshot's tick, else
inferred, see `financial-chart-book-tick').  TITLE fills the template
slot; SUBJECT, SIZE and TARGET are as in `eas-view-open'; SHOW displays
the view.  Feed it with `financial-chart-book-push'."
  (unless (member template financial-chart-book-templates)
    (financial-chart-book--fail "UNKNOWN_TEMPLATE" "/template" nil
                                "No order-book template %S; templates: %s" template
                                (string-join financial-chart-book-templates ", ")))
  (let* ((book (if (financial-chart-book-p snapshot) snapshot (financial-chart-book-make snapshot)))
         (tick (or (financial-chart-book--tick-option tick) (financial-chart-book-fixed-tick book)))
         (levels (or levels financial-chart-book-levels))
         (max-fps (or max-fps financial-chart-book-max-fps (financial-chart-book-default-fps levels)))
         (rows (progn (setf (financial-chart-book-fixed-tick book) tick)
                      (financial-chart-book-rows book :levels levels :flash flash)))
         (view (eas-view-open template :subject subject :size size :target target
                              :bindings (append (list :data rows) (and title (list :title title))))))
    (eas-stream-attach view (list :max-fps max-fps :window (length rows)))
    (puthash (eas-view-id view)
             (list :view view :book book :levels levels :flash flash :max-fps max-fps :tick tick)
             financial-chart-book--views)
    (when show (eas-show view))
    view))

(defun financial-chart-book--queued-p (view)
  "Non-nil when VIEW's stream holds a frame not yet drawn."
  (let ((stream (eas-stream-get view)))
    (and stream (> (eas-stream-queued stream) 0))))

(defun financial-chart-book--offer (live &optional now)
  "Queue LIVE's book (as of NOW) as the next frame unless one is queued.
A queued frame leaves the book dirty; it is offered again once drawn."
  (let ((view (plist-get live :view)))
    (if (financial-chart-book--queued-p view)
        (plist-put live :dirty t)
      (plist-put live :dirty nil)
      (let ((rows (financial-chart-book-rows (plist-get live :book) :levels (plist-get live :levels)
                                             :flash (plist-get live :flash) :now now)))
        (eas-stream-attach view (list :max-fps (plist-get live :max-fps) :window (length rows)))
        (eas-push view rows)))
    (financial-chart-book--schedule-unflash live)))

(defun financial-chart-book--schedule-unflash (live)
  "Arrange a frame that clears LIVE's flashes once they lapse."
  (when-let* ((flash (plist-get live :flash))
              ((and eas-stream-use-timers (not (plist-get live :timer))))
              ((> (hash-table-count (financial-chart-book-changed (plist-get live :book))) 0)))
    (let ((id (eas-view-id (plist-get live :view))))
      (plist-put live :timer
                 (run-at-time flash nil
                              (lambda ()
                                (when-let* ((l (gethash id financial-chart-book--views)))
                                  (plist-put l :timer nil)
                                  (when (eq (gethash id eas-views) (plist-get l :view))
                                    (financial-chart-book--offer l)))))))))

(defun financial-chart-book-push (view deltas &optional now)
  "Apply order-book DELTAS to VIEW's book at NOW and stream the new frame.
DELTAS is a vector or list of {op, side, price, size} (see the
commentary).  The frame waits for the cap and for the pointer to
leave; deltas arriving meanwhile fold into it.  Returns
`financial-chart-book-inspect'."
  (let ((live (financial-chart-book--live view)))
    (financial-chart-book-apply (plist-get live :book) deltas now)
    (financial-chart-book--offer live now)
    (financial-chart-book-inspect view)))

(defun financial-chart-book-reset (view snapshot)
  "Replace VIEW's book with SNAPSHOT (a resync) and stream it.
The view's tick holds unless SNAPSHOT gives one."
  (let* ((live (financial-chart-book--live view))
         (book (financial-chart-book-make snapshot)))
    (unless (financial-chart-book-fixed-tick book)
      (setf (financial-chart-book-fixed-tick book) (plist-get live :tick)))
    (plist-put live :book book)
    (financial-chart-book--offer live)
    (financial-chart-book-inspect view)))

(defun financial-chart-book-get (view)
  "VIEW's live `financial-chart-book'."
  (plist-get (financial-chart-book--live view) :book))

(defun financial-chart-book-inspect (view)
  "VIEW's book summary and stream state as JSON-ready data."
  (let ((live (financial-chart-book--live view)))
    (append (list :view (eas-view-id (plist-get live :view)) :levels (plist-get live :levels)
                  :dirty (if (plist-get live :dirty) t :false))
            (financial-chart-book-summary (plist-get live :book))
            (list :stream (eas-stream-inspect (plist-get live :view))))))

(defun financial-chart-book-close (view)
  "Stop VIEW's book and close the view."
  (let ((live (financial-chart-book--live view)))
    (when (plist-get live :timer) (cancel-timer (plist-get live :timer)))
    (eas-stream-detach (plist-get live :view))
    (remhash (eas-view-id (plist-get live :view)) financial-chart-book--views)
    (eas-view-close (plist-get live :view))))

(defun financial-chart-book--after-frame (view event &rest _)
  "Offer VIEW's dirty book once its queued frame (a push EVENT) is drawn."
  (when (and (equal (plist-get event :type) "push") (not eas-view-replaying))
    (when-let* ((live (gethash (eas-view-id view) financial-chart-book--views))
                ((plist-get live :dirty)))
      (financial-chart-book--offer live))))

(add-hook 'eas-view-dispatch-functions #'financial-chart-book--after-frame)

(provide 'financial-chart-eas-book)
;;; financial-chart-eas-book.el ends here
