;;; easel-glyph.el --- braille, eighth-block and box glyphs for text charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The sub-cell glyph primitives text charts are built from: braille
;; (2x4 dots per cell), vertical and horizontal eighth blocks, and
;; masks that sample a value range inside one cell.  Host packages'
;; text renderers alias these definitions instead of keeping copies.

;;; Code:

(defconst easel-glyph-blocks " ▁▂▃▄▅▆▇█"
  "Lower eighth blocks: index N fills N eighths of a cell from the bottom.")

(defconst easel-glyph-left-blocks " ▏▎▍▌▋▊▉█"
  "Left eighth blocks: index N fills N eighths of a cell from the left.")

(defconst easel-glyph-braille-dots [[#x01 #x08] [#x02 #x10] [#x04 #x20] [#x40 #x80]]
  "Braille dot bit for [ROW-IN-CELL][COL-IN-CELL], row 0 at the top.")

(defconst easel-glyph-braille-left-bits '(64 4 2 1)
  "Braille dot bits for vertical samples, from bottom to top, left column.")

(defconst easel-glyph-braille-right-bits '(128 32 16 8)
  "Braille dot bits for vertical samples, from bottom to top, right column.")

(defconst easel-glyph-eighth-block-candidates
  '((0 . ?\s)
    (1 . ?▁) (3 . ?▂) (7 . ?▃) (15 . ?▄)
    (31 . ?▅) (63 . ?▆) (127 . ?▇)
    (128 . ?▔) (240 . ?▀) (255 . ?█))
  "Unicode block masks available to the eighth-resolution renderer.
Bits run from bottom to top. Masks not represented here use the nearest
available mask.")

(defun easel-glyph-range-mask (row-low row-high low high samples)
  "Return a SAMPLES-bit mask for LOW..HIGH inside ROW-LOW..ROW-HIGH.
Bit zero represents the bottom sample. A sample is set when its price
interval overlaps LOW..HIGH."
  (let ((row-height (/ (- row-high row-low) (float samples)))
        (mask 0)
        (index 0))
    (while (< index samples)
      (let ((sample-low (+ row-low (* index row-height)))
            (sample-high (+ row-low (* (1+ index) row-height))))
        (when (< (max sample-low low) (min sample-high high))
          (setq mask (logior mask (ash 1 index)))))
      (setq index (1+ index)))
    mask))

(defun easel-glyph-point-mask (row-low row-high value samples)
  "Return the one-bit SAMPLES mask nearest VALUE in ROW-LOW..ROW-HIGH."
  (let* ((fraction (/ (- value row-low) (- row-high row-low)))
         (index (max 0 (min (1- samples)
                            (floor (* fraction samples))))))
    (ash 1 index)))

(defun easel-glyph-braille-char (mask wick)
  "Encode vertical sample MASK as a Braille character.
When WICK is non-nil, use only the left dot column for a narrower wick."
  (let ((bits 0))
    (dotimes (index 4)
      (when (/= 0 (logand mask (ash 1 index)))
        (setq bits (logior bits (nth index easel-glyph-braille-left-bits)))
        (unless wick
          (setq bits
                (logior bits (nth index easel-glyph-braille-right-bits))))))
    (+ #x2800 bits)))

(defun easel-glyph-eighths-char (mask)
  "Encode eight-sample MASK with the nearest available block-element glyph."
  (let* ((mask (logand mask 255))
         (best (car easel-glyph-eighth-block-candidates))
         (distance (logcount (logxor mask (car best)))))
    (dolist (candidate (cdr easel-glyph-eighth-block-candidates))
      (let ((candidate-distance (logcount (logxor mask (car candidate)))))
        (when (< candidate-distance distance)
          (setq best candidate
                distance candidate-distance))))
    (cdr best)))

(defun easel-glyph-lower (eighths)
  "Lower block filling EIGHTHS (0-8) of a cell."
  (aref easel-glyph-blocks (max 0 (min 8 eighths))))

(defun easel-glyph-left (eighths)
  "Left block filling EIGHTHS (0-8) of a cell."
  (aref easel-glyph-left-blocks (max 0 (min 8 eighths))))

(provide 'easel-glyph)
;;; easel-glyph.el ends here
