;;; financial-chart-interactive-test.el --- plot view interactions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-interactive-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-interactive-test--dir))
(require 'financial-chart)

(defconst financial-chart-interactive-test--series
  '((10 1) (20 3) (30 5) (40 7)))

(ert-deftest financial-chart-interactive-test-series-columns-preserve-text ()
  (dolist (kind '(area line sparkline))
    (let* ((buffer-name (format "*financial-chart-interactive-%s*" kind))
           (buffer (financial-chart-plot-view
                    kind financial-chart-interactive-test--series
                    :backend 'text :width 2 :height 2 :buffer buffer-name))
           (expected (financial-chart-plot kind financial-chart-interactive-test--series
                                           :backend 'text :width 2 :height 2)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           (substring-no-properties expected)))
            (let* ((pos (+ (point-min) (if (eq kind 'sparkline) 0 7)))
                   (point-data (get-text-property pos 'financial-chart-point)))
              (should point-data)
              (should (= (plist-get point-data :x) 15))
              (should (= (plist-get point-data :y) 2))
              (goto-char pos)
              (let (shown)
                (cl-letf (((symbol-function 'message)
                           (lambda (format-string &rest args)
                             (setq shown (apply #'format format-string args)))))
                  (financial-chart-plot--inspect-point))
                (should (equal shown "X: 15  Y: 2")))))
        (kill-buffer buffer)))))

(ert-deftest financial-chart-interactive-test-column-metadata-follows-wide-labels ()
  (let* ((series '((1 123456) (2 234567)))
         (buffer (financial-chart-plot-view
                  'area series :backend 'text :width 2 :height 2
                  :buffer "*financial-chart-interactive-wide-label*")))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((text (buffer-string))
                 (newline (string-match "\n" text))
                 (first-column (+ (point-min) (- newline 2))))
            (should (= (plist-get (get-text-property first-column
                                                       'financial-chart-point)
                                  :x)
                       1))
            (should-not (get-text-property (1- first-column)
                                           'financial-chart-point))))
      (kill-buffer buffer))))

(ert-deftest financial-chart-interactive-test-zoom-slices-series-and-resets ()
  (let* ((series (cl-loop for i from 0 below 20 collect (list i i)))
         (buffer (financial-chart-plot-view 'area series
                                            :backend 'text :width 8 :height 4
                                            :buffer "*financial-chart-interactive-zoom*")))
    (unwind-protect
        (with-current-buffer buffer
          (financial-chart-plot-zoom-in)
          (should (= (cdr financial-chart-plot--zoom-window) (length series)))
          (financial-chart-plot-zoom-reset)
          (goto-char (+ (point-min) 7))
          (let ((anchor (plist-get (get-text-property (point) 'financial-chart-point)
                                   :index)))
            (financial-chart-plot-zoom-in)
            (let ((window financial-chart-plot--zoom-window))
              (should (< (- (cdr window) (car window)) (length series)))
              (should (<= (car window) anchor))
              (should (< anchor (cdr window)))
              (should (= (length (nth 1 financial-chart-plot--spec)) (length series)))
              (financial-chart-plot-zoom-out)
              (should (> (- (cdr financial-chart-plot--zoom-window)
                            (car financial-chart-plot--zoom-window))
                         (- (cdr window) (car window))))))
          (financial-chart-plot-zoom-reset)
          (should-not financial-chart-plot--zoom-window)
          (should (equal (car (financial-chart-plot--visible-data 'area series)) series)))
      (kill-buffer buffer))))

(ert-deftest financial-chart-interactive-test-refresh-is-direct-and-timer-is-gated ()
  (let ((calls 0)
        (fresh '((0 10) (1 20) (2 30)))
        buffer timer)
    (setq buffer
          (financial-chart-plot-view
           'area '((0 1) (1 2))
           :backend 'text :width 4 :height 2
           :buffer "*financial-chart-interactive-refresh*"
           :refresh-fn (lambda () (setq calls (1+ calls)) fresh)
           :refresh-interval 3600))
    (unwind-protect
        (with-current-buffer buffer
          (setq timer financial-chart-plot--refresh-timer)
          (should (timerp timer))
          (should (= calls 0))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
            (financial-chart-plot--timer-refresh buffer))
          (should (= calls 0))
          (should (equal (financial-chart-plot-refresh-data) fresh))
          (should (= calls 1))
          (should (equal (nth 1 financial-chart-plot--spec) fresh))
          (should (string-match-p "3 pts" (buffer-string)))
          (financial-chart-plot-toggle-refresh)
          (should-not financial-chart-plot--refresh-enabled)
          (financial-chart-plot-toggle-refresh)
          (should financial-chart-plot--refresh-enabled)
          (setq timer financial-chart-plot--refresh-timer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (should-not (memq timer timer-list))))

(ert-deftest financial-chart-interactive-test-refresh-options-are-paired ()
  (should-error
   (financial-chart-plot-view 'area '(1 2) :refresh-interval 10)
   :type 'financial-chart-error)
  (should-error
   (financial-chart-plot-view 'area '(1 2) :refresh-fn #'identity)
   :type 'financial-chart-error))

(provide 'financial-chart-interactive-test)
;;; financial-chart-interactive-test.el ends here
