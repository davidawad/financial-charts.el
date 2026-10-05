;;; easel-png-test.el --- PNG decoding and size-tolerant comparison -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel-png)

(defmacro easel-png-test--deftest (name &rest body)
  "An ERT test NAME running BODY, skipped without zlib."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (unless (zlib-available-p) (easel-test-skip "this Emacs lacks zlib, needed to decode PNGs"))
     ,@body))

(defun easel-png-test--be32 (n)
  "N as four big-endian bytes."
  (unibyte-string (logand (ash n -24) 255) (logand (ash n -16) 255) (logand (ash n -8) 255) (logand n 255)))

(defun easel-png-test--zlib (data)
  "DATA as a zlib stream of stored (uncompressed) deflate blocks."
  (let ((a 1) (b 0) (out (list (unibyte-string #x78 #x01))) (i 0) (n (length data)))
    (dotimes (k n) (setq a (% (+ a (aref data k)) 65521) b (% (+ b a) 65521)))
    (while (progn
             (let* ((len (min 65535 (- n i))) (final (>= (+ i len) n)))
               (push (unibyte-string (if final 1 0) (logand len 255) (ash len -8)
                                     (logand (lognot len) 255) (logand (ash (lognot len) -8) 255))
                     out)
               (push (substring data i (+ i len)) out)
               (setq i (+ i len))
               (not final))))
    (push (easel-png-test--be32 (logior (ash b 16) a)) out)
    (apply #'concat (nreverse out))))

(defun easel-png-test--filter (row prev bpp type)
  "Unibyte ROW filtered with PNG filter TYPE against PREV (or nil)."
  (let ((out (make-string (length row) 0)))
    (dotimes (i (length row))
      (let* ((a (if (>= i bpp) (aref row (- i bpp)) 0)) (b (if prev (aref prev i) 0))
             (c (if (and prev (>= i bpp)) (aref prev (- i bpp)) 0))
             (pred (pcase type
                     (0 0) (1 a) (2 b) (3 (ash (+ a b) -1))
                     (_ (let* ((p (- (+ a b) c)) (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
                          (cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (t c)))))))
        (aset out i (logand (- (aref row i) pred) 255))))
    out))

(defun easel-png-test--write (w h ctype pixels)
  "A PNG file of W x H, color type CTYPE, from unibyte PIXELS; rows cycle filters 0-4."
  (let* ((bpp (pcase ctype (0 1) (2 3) (4 2) (_ 4))) (stride (* w bpp)) (raw nil) (prev nil)
         (file (make-temp-file "easel-png-test" nil ".png")))
    (dotimes (y h)
      (let ((row (substring pixels (* y stride) (* (1+ y) stride))) (type (% y 5)))
        (push (concat (unibyte-string type) (easel-png-test--filter row prev bpp type)) raw)
        (setq prev row)))
    (let ((chunk (lambda (type data) (concat (easel-png-test--be32 (length data)) type data (easel-png-test--be32 0)))))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert "\x89PNG\r\n\x1a\n"
                (funcall chunk "IHDR" (concat (easel-png-test--be32 w) (easel-png-test--be32 h) (unibyte-string 8 ctype 0 0 0)))
                (funcall chunk "IDAT" (easel-png-test--zlib (apply #'concat (nreverse raw))))
                (funcall chunk "IEND" ""))))
    file))

(defun easel-png-test--pixels (w h bpp)
  "A deterministic unibyte test pattern of W x H pixels of BPP bytes."
  (let ((s (make-string (* w h bpp) 0)))
    (dotimes (i (length s)) (aset s i (% (+ (* i 37) (/ i 3) 11) 256)))
    s))

(easel-png-test--deftest easel-png-decodes-every-filter-and-color-type
  (dolist (ctype '(0 2 4 6))
    (let* ((bpp (pcase ctype (0 1) (2 3) (4 2) (_ 4))) (w 7) (h 10)
           (px (easel-png-test--pixels w h bpp))
           (file (easel-png-test--write w h ctype px)))
      (unwind-protect
          (let ((img (easel-png-read file)))
            (should (equal (list (plist-get img :w) (plist-get img :h)) (list w h)))
            (should (equal (plist-get img :rgba) (if (= bpp 4) px (easel-png--rgba px (* w h) bpp)))))
        (delete-file file)))))

(ert-deftest easel-png-composites-alpha-over-white ()
  (let ((px (unibyte-string 0 0 0 0  10 20 30 255  0 0 0 128)))
    (should (equal (mapcar (lambda (o) (easel-png--rgb-at px o)) '(0 4 8))
                   '((255 255 255) (10 20 30) (127 127 127)))))
  (should (equal (easel-png--rgba (unibyte-string 7 200) 1 2) (unibyte-string 7 7 7 200))))

(easel-png-test--deftest easel-png-rejects-what-it-cannot-read
  (let ((file (make-temp-file "easel-png-test" nil ".png" "not a png")))
    (unwind-protect (easel-test-should-code "INVALID_INPUT" (easel-png-read file))
      (delete-file file))))

(defun easel-png-test--canvas (w h &optional bg)
  "A W x H decoded image filled with BG (default near-white)."
  (let ((bg (append (or bg '(252 252 251)) '(255))) (s (make-string (* 4 w h) 0)))
    (dotimes (k (* w h)) (dotimes (c 4) (aset s (+ (* 4 k) c) (nth c bg))))
    (list :w w :h h :rgba s)))

(defun easel-png-test--rect (img x y w h color)
  "IMG with a W x H rect of COLOR at X Y drawn into it."
  (let ((s (plist-get img :rgba)) (iw (plist-get img :w)))
    (dotimes (j h) (dotimes (i w) (dotimes (c 3) (aset s (+ (* 4 (+ x i (* (+ y j) iw))) c) (nth c color)))))
    img))

(easel-png-test--deftest easel-png-compare-aligns-padded-canvases
  "The same drawing on canvases of different size and origin compares equal."
  (let* ((ref (easel-png-test--rect (easel-png-test--canvas 60 40) 10 10 20 15 '(42 120 214)))
         (native (easel-png-test--rect (easel-png-test--canvas 64 43) 13 12 20 15 '(42 120 214)))
         (r (easel-png-compare native ref)))
    (should (= (plist-get r :ratio) 0))
    (should (equal (plist-get r :size-delta) [4 3]))
    (should (equal (plist-get r :offset) [-3 -2]))))

(easel-png-test--deftest easel-png-compare-counts-real-differences
  (let* ((ref (easel-png-test--rect (easel-png-test--canvas 50 20) 5 5 10 10 '(42 120 214)))
         (native (easel-png-test--rect (easel-png-test--rect (easel-png-test--canvas 50 20) 5 5 10 10 '(42 120 214))
                                       30 5 2 2 '(0 0 0)))
         (r (easel-png-compare native ref)))
    (should (= (plist-get r :ratio) (/ 4.0 1000)))
    (should (equal (plist-get r :size-delta) [0 0])))
  ;; Colors closer than pixelmatch's threshold are not differences.
  (let ((r (easel-png-compare (easel-png-test--canvas 10 10 '(255 255 255)) (easel-png-test--canvas 10 10))))
    (should (= (plist-get r :ratio) 0))))

(easel-png-test--deftest easel-png-reads-the-committed-references
  (let ((img (easel-png-read (easel-test-file "test/conformance/ref/mark-bar.png"))))
    (should (equal (list (plist-get img :w) (plist-get img :h)) '(154 347)))
    (should (= (plist-get (easel-png-compare img img) :ratio) 0))))

(provide 'easel-png-test)
;;; easel-png-test.el ends here
