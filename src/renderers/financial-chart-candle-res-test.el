;;; financial-chart-candle-res-test.el --- Higher-resolution candle text tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'financial-chart)

(defconst financial-chart-candle-res-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst financial-chart-candle-res-test--fixtures
  (expand-file-name "../../test/fixtures" financial-chart-candle-res-test--dir))
(defconst financial-chart-candle-res-test--bars
  '((:open 3 :high 10 :low 1 :close 7)
    (:open 7 :high 9 :low 2 :close 4)
    (:open 4 :high 8 :low 4 :close 8)))

(defun financial-chart-candle-res-test--golden (name actual)
  "Compare ACTUAL, without text properties, with fixture NAME."
  (let ((file (expand-file-name name financial-chart-candle-res-test--fixtures))
        (text (substring-no-properties actual)))
    (when (getenv "FINANCIAL_CHART_UPDATE_GOLDEN")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file)))
    (should (file-exists-p file))
    (let ((expected (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (buffer-string))))
      (should (equal text expected)))))

(defmacro financial-chart-candle-res-test--with-defaults (&rest body)
  "Run BODY with deterministic candle text settings."
  `(let ((financial-chart-max-bars nil)
         (financial-chart-height 6)
         (financial-chart-candle-width 1)
         (financial-chart-candle-gap 1)
         (financial-chart-candle-style 'block)
         (financial-chart-axis-format "%4.0f ")
         (financial-chart-axis-label-count 2)
         (financial-chart-show-volume nil)
         (financial-chart-show-x-axis nil)
         (financial-chart-indicators nil)
         (financial-chart-oscillators nil))
     ,@body))

(defun financial-chart-candle-res-test--render (style)
  "Render the fixture bars using STYLE."
  (let ((financial-chart-candle-style style))
    (financial-chart-render financial-chart-candle-res-test--bars)))

(ert-deftest financial-chart-candle-res-block-output-stays-legacy-golden ()
  (financial-chart-candle-res-test--with-defaults
   (financial-chart-candle-res-test--golden
    "candle-res-block.txt"
    (financial-chart-candle-res-test--render 'block))
   (should (eq (default-value 'financial-chart-candle-style) 'block))))

(ert-deftest financial-chart-candle-res-braille-golden ()
  (financial-chart-candle-res-test--with-defaults
   (financial-chart-candle-res-test--golden
    "candle-res-braille.txt"
    (financial-chart-candle-res-test--render 'braille))))

(ert-deftest financial-chart-candle-res-eighths-golden ()
  (financial-chart-candle-res-test--with-defaults
   (financial-chart-candle-res-test--golden
    "candle-res-eighths.txt"
    (financial-chart-candle-res-test--render 'eighths))))

(ert-deftest financial-chart-candle-res-styles-preserve-wick-face ()
  (financial-chart-candle-res-test--with-defaults
   (let ((financial-chart-wick-face 'shadow))
     (dolist (style '(braille eighths))
       (let* ((financial-chart-candle-style style)
              (cell (financial-chart--candle-cell
                     5.0 6.0 '(:open 0 :high 10 :low -1 :close 1)))
              (char (aref (car cell) 0)))
         (should (eq (cdr cell) 'shadow))
         (should (if (eq style 'braille)
                   (<= #x2800 char #x28ff)
                   (or (eq char financial-chart-glyph-wick)
                       (memq char '(?▁ ?▂ ?▃ ?▄ ?▅ ?▆ ?▇ ?▔ ?▀ ?█))))))))))

(provide 'financial-chart-candle-res-test)
;;; financial-chart-candle-res-test.el ends here
