;;; eas-demo-candles.el --- demo: OHLC candles and volume as a live eas view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Opens the `ohlc' template (candlesticks: a low-high wick and an
;; open-close body per bar, up bars green) with its volume pane turned
;; on (`volume' slot true), drawn from the bundled 30 daily TSM bars in
;; examples/panes.data.json.  Nothing is fetched.
;;
;; GUI Emacs (SVG):
;;
;;   emacs -Q -L src -L src/eas -l examples/eas-demo-candles.el \
;;         -f eas-demo-candles
;;
;; Terminal (the same view drawn as text; `eas-show' picks text when
;; the frame cannot show SVG):
;;
;;   emacs -nw -Q -L src -L src/eas -l examples/eas-demo-candles.el \
;;         -f eas-demo-candles
;;
;; In the chart buffer the mouse, arrows, +/- and RET drive the view
;; (see `eas-view-mode-map').  The open view is "ohlc:TSM"; ask it what
;; it shows with (eas-inspect "ohlc:TSM").
;;
;; Batch, the deterministic text rendering:
;;
;;   emacs -Q --batch -L src -L src/eas -l examples/eas-demo-candles.el \
;;         --eval '(princ (eas-demo-candles-text))'
;;
;; The same chart from the shell, run in the repository root (the
;; bindings are the bundled file plus "volume": true):
;;
;;   jq '.volume = true' examples/panes.data.json \
;;     | bin/eas render ohlc --data - --backend text --raw

;;; Code:

(require 'eas-template)
(require 'eas-view)
(require 'eas-agent)

(declare-function eas-show "eas-mode" (view &optional target))

(defconst eas-demo-candles-data "examples/panes.data.json"
  "Bundled OHLCV bindings of the demo, relative to the repository.")

(defconst eas-demo-candles-subject "TSM"
  "Subject of the demo view, so its id is ohlc:TSM.")

(defun eas-demo-candles-bindings ()
  "Bindings for the `ohlc' template: the bundled bars, volume pane on."
  (append (list :volume t)
          (eas-json-read-file (expand-file-name eas-demo-candles-data
                                                eas-template--root))))

(defun eas-demo-candles-text ()
  "Return the demo chart rendered as deterministic text.
Same output as the bin/eas command in the Commentary."
  (let ((env (eas-agent "render" "ohlc" :data (eas-demo-candles-bindings)
                        :backend "text")))
    (unless (eq (plist-get env :ok) t)
      (error "Demo render failed: %s %S" (plist-get env :reason)
             (plist-get env :evidence)))
    (plist-get (plist-get env :data) :output)))

(defun eas-demo-candles-open (&optional target)
  "Open (or reopen) the demo's live view with TARGET (default svg).
Return the view.  Nothing is displayed, so this runs in --batch."
  (let ((id (format "ohlc:%s" eas-demo-candles-subject)))
    (when (member id (eas-view-ids))
      (eas-view-close id))
    (eas-view-open "ohlc" :bindings (eas-demo-candles-bindings)
                   :subject eas-demo-candles-subject :target (or target 'svg))))

;;;###autoload
(defun eas-demo-candles ()
  "Show TSM daily candlesticks with a volume pane as a live eas view."
  (interactive)
  (require 'eas-mode)
  (eas-show (eas-demo-candles-open)))

(provide 'eas-demo-candles)
;;; eas-demo-candles.el ends here
