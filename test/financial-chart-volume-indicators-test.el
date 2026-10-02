;;; financial-chart-volume-indicators-test.el --- Volume indicator tests -*- lexical-binding: t; -*-

(require 'ert)
(defvar financial-chart-volume-indicators-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-volume-indicators-test--dir))
(require 'financial-chart-volume-indicators)

(defconst financial-chart-volume-indicators-test--bars
  '((:high 10 :low 8 :close 9 :volume 100 :time 1000)
    (:high 12 :low 10 :close 12 :volume 200 :time 2000)
    (:high 12 :low 8 :close 8 :volume 100 :time 3000)
    (:high 12 :low 10 :close 12 :volume 200 :time 4000)))

(ert-deftest financial-chart-volume-indicators-obv-direction-and-flat-close ()
  (should (equal (financial-chart-obv financial-chart-volume-indicators-test--bars)
                 '(100 300 200 400))))

(ert-deftest financial-chart-volume-indicators-ad-line ()
  (should (equal (financial-chart-accumulation-distribution
                  financial-chart-volume-indicators-test--bars)
                 '(0.0 200.0 100.0 300.0))))

(ert-deftest financial-chart-volume-indicators-mfi-warmup-and-window ()
  (let ((values (financial-chart-money-flow-index
                 financial-chart-volume-indicators-test--bars 2)))
    (should (equal (car values) nil))
    (should (equal (cadr values) nil))
    (should (< (abs (- (nth 2 values) 70.83333333333333)) 1e-8))
    (should (< (abs (- (nth 3 values) 70.83333333333333)) 1e-8))))

(ert-deftest financial-chart-volume-indicators-cmf-warmup-and-ratio ()
  (let ((values (financial-chart-chaikin-money-flow
                 financial-chart-volume-indicators-test--bars 2)))
    (should (equal (car values) nil))
    (should (< (abs (- (cadr values) (/ 2.0 3.0))) 1e-8))
    (should (< (abs (- (nth 2 values) (/ 1.0 3.0))) 1e-8))))

(ert-deftest financial-chart-volume-indicators-chaikin-oscillator ()
  (let ((values (financial-chart-chaikin-oscillator
                 financial-chart-volume-indicators-test--bars 2 3)))
    (should (equal (car values) nil))
    (should (equal (cadr values) nil))
    (should (equal (nth 2 values) 0.0))
    (should (< (abs (- (nth 3 values) (/ 100.0 3.0))) 1e-8))))

(ert-deftest financial-chart-volume-indicators-missing-data-resets-cumulative-series ()
  (let ((bars '((:high 10 :low 8 :close 9 :volume 100)
                (:high 10 :low 8 :close 10)
                (:high 12 :low 10 :close 12 :volume 40))))
    (should (equal (financial-chart-obv bars) '(100 nil 40)))
    (should (equal (financial-chart-accumulation-distribution bars)
                   '(0.0 nil 40.0)))
    (should (equal (financial-chart-chaikin-money-flow bars 1) '(0.0 nil 1.0)))))

(ert-deftest financial-chart-volume-indicators-register-provider-neutral-calculators ()
  (let ((series (financial-chart-indicator-evaluate
                 'obv financial-chart-volume-indicators-test--bars)))
    (should (equal (plist-get series :name) 'obv))
    (should (equal (plist-get series :values) '(100 300 200 400)))
    (should (equal (plist-get series :timestamps) '(1000 2000 3000 4000)))
    (should (eq (plist-get series :unit) :volume))))

(provide 'financial-chart-volume-indicators-test)
;;; financial-chart-volume-indicators-test.el ends here

