;;; financial-chart-indicator-oscillators-test.el --- Oscillator tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)
(defvar financial-chart-indicator-oscillators-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-indicator-oscillators-test--dir))
(require 'financial-chart-indicator-oscillators)

(defun financial-chart-indicator-oscillators-test--bars ()
  '((:high 10 :low 8 :close 9 :time 1000)
    (:high 12 :low 9 :close 11 :time 2000)
    (:high 13 :low 10 :close 12 :time 3000)
    (:high 14 :low 11 :close 12 :time 4000)
    (:high 15 :low 12 :close 14 :time 5000)))

(ert-deftest financial-chart-indicator-oscillators-stochastic-values-and-descriptors ()
  (let* ((bars (financial-chart-indicator-oscillators-test--bars))
         (outputs (financial-chart-stochastic bars 3 2))
         (k (car outputs))
         (d (cadr outputs))
         (evaluated (financial-chart-indicator-evaluate 'stochastic bars 3 2)))
    (should (equal (plist-get k :values) '(nil nil 80.0 60.0 80.0)))
    (should (equal (plist-get d :values) '(nil nil nil 70.0 70.0)))
    (should (eq (plist-get k :name) 'stochastic-k))
    (should (equal (plist-get (car evaluated) :timestamps)
                   '(1000 2000 3000 4000 5000)))
    (should (equal (plist-get (cadr evaluated) :params) '(3 2)))))

(ert-deftest financial-chart-indicator-oscillators-williams-r-values ()
  (let ((series (financial-chart-indicator-evaluate
                 'williams-r (financial-chart-indicator-oscillators-test--bars) 2)))
    (should (equal (plist-get series :values)
                   '(nil -25.0 -25.0 -50.0 -25.0)))
    (should (equal (plist-get series :bounds) '(-100 . 0)))))

(ert-deftest financial-chart-indicator-oscillators-ultimate-default-and-parameters ()
  (let* ((bars (cl-loop for close from 10 to 40
                        collect (list :high (1+ close) :low (1- close)
                                      :close close)))
         (default-series
          (financial-chart-indicator-evaluate 'ultimate-oscillator bars))
         (series (financial-chart-indicator-evaluate
                  'ultimate-oscillator bars 1 2 3))
         (values (plist-get series :values)))
    (should (= (cl-position-if #'numberp (plist-get default-series :values)) 28))
    (should (equal (seq-take values 3) '(nil nil nil)))
    (should (= (nth 3 values) 50.0))
    (should (= (length values) (length bars)))))

(ert-deftest financial-chart-indicator-oscillators-missing-fields-stay-aligned ()
  (let* ((bars '((:high 10 :low 8 :close 9)
                 (:high 12 :low 9 :close 11)
                 (:high nil :low 10 :close 12)
                 (:high 14 :low 11 :close 12)
                 (:high 15 :low 12 :close 14)))
         (stochastic (financial-chart-stochastic bars 2 2))
         (williams (financial-chart-williams-r bars 2)))
    (should (equal (plist-get (car stochastic) :values)
                   '(nil 75.0 nil nil 75.0)))
    (should (equal (plist-get (cadr stochastic) :values)
                   '(nil nil nil nil nil)))
    (should (equal williams
                   '(nil -25.0 nil nil -25.0)))))

(ert-deftest financial-chart-indicator-oscillators-register-provider-neutral-names ()
  (let ((names (mapcar (lambda (entry) (plist-get entry :name))
                       (financial-chart-list-indicators))))
    (should (memq 'stochastic names))
    (should (memq 'williams-r names))
    (should (memq 'ultimate-oscillator names)))
  (should-error (financial-chart-stochastic nil 0) :type 'wrong-type-argument))

(provide 'financial-chart-indicator-oscillators-test)
;;; financial-chart-indicator-oscillators-test.el ends here
