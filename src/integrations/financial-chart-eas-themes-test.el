;;; financial-chart-eas-themes-test.el --- declarative chart themes -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty.31 and fc-gty.34: themes, on synthetic bars.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defun financial-chart-themes-test--bars (n)
  "N synthetic daily bars, a deterministic wave with volume."
  (cl-loop for i from 0 below n
           for c = (+ 100 (* 8 (sin (/ i 9.0))) (* 3 (sin (/ i 2.3))))
           for o = (+ c (* 1.5 (sin (* i 1.7))))
           collect (list :time (format-time-string "%F" (+ 1767225600 (* i 86400)) t)
                         :open o :high (+ (max o c) 1.2) :low (- (min o c) 1.1) :close c
                         :volume (+ 1000 (* 300 (abs (sin (/ i 4.0))))))))

(defun financial-chart-themes-test--chart (&rest keys)
  "A small candle chart with an SMA, a Fibonacci and KEYS."
  (append keys
          (list :bars (vconcat (financial-chart-themes-test--bars 60))
                :price '(:style "candles" :series ["sma"]
                         :annotations [(:type "fibonacci")]))))

(defun financial-chart-themes-test--layers (spec)
  "Every layer of SPEC's panes."
  (cl-loop for pane across (plist-get spec :vconcat)
           append (append (plist-get pane :layer) nil)))

(defun financial-chart-themes-test--strokes (spec)
  "The :stroke of every annotation label mark of SPEC (its halo)."
  (cl-loop for l in (financial-chart-themes-test--layers spec)
           when (string-suffix-p "-labels" (or (plist-get l :name) ""))
           collect (plist-get (plist-get l :mark) :stroke)))

;;; Resolution

(ert-deftest financial-chart-theme-resolves-names-and-objects ()
  (should (equal (plist-get (financial-chart-theme-resolve "gruvbox") :background) "#282828"))
  (should (equal (plist-get (financial-chart-theme-resolve "gruvbox") :down) "#fb4934"))
  (should (equal (plist-get (financial-chart-theme-resolve "gruvbox-light") :background) "#fbf1c7"))
  (should (null (financial-chart-theme-resolve "default")))
  ;; An object overrides its base, key by key.
  (let ((th (financial-chart-theme-resolve '(:base "gruvbox" :up "#00ff00" :palette ["#123456"]))))
    (should (equal (plist-get th :up) "#00ff00"))
    (should (equal (plist-get th :down) "#fb4934"))
    (should (equal (plist-get th :palette) '("#123456"))))
  ;; Nil takes the backend's default.
  (let ((financial-chart-image-theme "dark") (financial-chart-text-theme "terminal"))
    (should (equal (plist-get (financial-chart-theme-resolve nil) :background) "#131722"))
    (should (equal (plist-get (financial-chart-theme-resolve nil 'text) :up) "green")))
  (dolist (bad '("solarized" (:base "nope") (:upp "#fff") 7 (:palette "red")))
    (let ((err (should-error (financial-chart-theme-resolve bad) :type 'financial-chart-invalid-chart)))
      (should (member (plist-get (cddr err) :code) '("UNKNOWN_THEME" "INVALID_THEME")))
      (should (string-prefix-p "/theme" (plist-get (cddr err) :path))))))

(ert-deftest financial-chart-theme-halo-follows-the-background ()
  (should (equal (financial-chart-theme-halo (financial-chart-theme-resolve "gruvbox")) "#282828"))
  (should (equal (financial-chart-theme-halo (financial-chart-theme-resolve "transparent")) "none"))
  (should (equal (financial-chart-theme-halo nil) "#fcfcfb"))
  (should (equal (financial-chart-theme-halo '(:background "#000000" :halo "#111111")) "#111111")))

(ert-deftest financial-chart-theme-terminal-colours-are-ansi-names ()
  ;; Batch has no graphic frame: the names pass to the terminal as they are.
  (should (equal (financial-chart-theme--ansi "brightblue") "brightblue"))
  (should (equal (financial-chart-theme--ansi "#123456") "#123456")))

;;; Compose

(ert-deftest financial-chart-theme-colours-the-chart ()
  (let* ((spec (financial-chart-compose (financial-chart-themes-test--chart :theme "gruvbox")))
         (config (plist-get spec :config))
         (json (eas-json-encode spec)))
    (should (equal (plist-get config :background) "#282828"))
    (should (equal (plist-get (plist-get config :axis) :labelColor) "#ebdbb2"))
    (should (equal (plist-get (plist-get config :title) :color) "#ebdbb2"))
    (should (string-search "#b8bb26" json))
    (should (string-search "#fb4934" json))
    ;; The SMA takes the theme's palette, not the package's blue.
    (should (string-search "#83a598" json))
    (should-not (string-search "#26a69a" json))
    ;; Pinned labels' halo is the background.
    (let ((strokes (financial-chart-themes-test--strokes spec)))
      (should strokes)
      (should (cl-every (lambda (s) (equal s "#282828")) strokes))))
  ;; "colors" still win over the theme.
  (let ((json (eas-json-encode (financial-chart-compose
                                (financial-chart-themes-test--chart :theme "gruvbox" :colors '(:up "#00e676"))))))
    (should (string-search "#00e676" json))
    (should-not (string-search "#b8bb26" json))))

(ert-deftest financial-chart-theme-transparent-has-no-halo ()
  (let ((spec (financial-chart-compose (financial-chart-themes-test--chart :theme "transparent"))))
    (should (equal (plist-get (plist-get spec :config) :background) "transparent"))
    (should (financial-chart-themes-test--strokes spec))
    (should (cl-every #'null (financial-chart-themes-test--strokes spec)))
    (should (string-search "fill=\"transparent\""
                           (financial-chart-compose-render (financial-chart-themes-test--chart :theme "transparent")
                                                           :backend 'svg)))))

(ert-deftest financial-chart-theme-default-image-theme-is-a-defcustom ()
  (should (null (plist-get (financial-chart-compose (financial-chart-themes-test--chart)) :config)))
  (let ((financial-chart-image-theme "gruvbox"))
    (should (equal (plist-get (plist-get (financial-chart-compose (financial-chart-themes-test--chart)) :config)
                              :background)
                   "#282828"))
    (should (string-search "#282828" (financial-chart-compose-render (financial-chart-themes-test--chart)
                                                                     :backend 'svg)))))

(ert-deftest financial-chart-theme-text-uses-the-terminal-colours ()
  (let* ((text (financial-chart-compose-render (financial-chart-themes-test--chart) :width 80 :height 24))
         (fgs (delete-dups (cl-loop for i below (length text)
                                    for f = (get-text-property i 'face text)
                                    when (and (consp f) (keywordp (car f)) (plist-get f :foreground))
                                    collect (plist-get f :foreground)))))
    (should (member "green" fgs))
    (should (member "red" fgs))
    (should (member "blue" fgs))
    (should-not (member "#26a69a" fgs)))
  ;; A chart's own theme wins in text too.
  (let ((text (financial-chart-compose-render (financial-chart-themes-test--chart :theme "gruvbox")
                                              :width 80 :height 24)))
    (should-not (text-property-any 0 (length text) 'face '(:foreground "green") text))))

(ert-deftest financial-chart-theme-in-a-preset ()
  (let* ((chart (financial-chart-themes-test--chart
                 :presets '(:dark-fib (:theme "gruvbox"))
                 :preset "dark-fib" :theme '(:up "#00e676")))
         (expanded (financial-chart-presets-apply chart)))
    ;; An object over a preset's named theme keeps it as the base.
    (should (equal (plist-get expanded :theme) '(:up "#00e676" :base "gruvbox")))
    (let ((spec (financial-chart-compose chart)))
      (should (equal (plist-get (plist-get spec :config) :background) "#282828"))
      (should (string-search "#00e676" (eas-json-encode spec))))))

(provide 'financial-chart-eas-themes-test)
;;; financial-chart-eas-themes-test.el ends here
