;;; financial-chart-eas-link-demo.el --- linked views demo: two tickers, one bus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `financial-chart-eas-link-demo' (fc-qx1.6) opens the panes template
;; (price, volume and RSI(14) sharing one crosshair and one zoom inside
;; the spec) for two tickers in side-by-side windows, joined on the eas
;; bus "tickers".  Hover or zoom either window and the other follows;
;; `inspect' and `log' on either view say so as data.  The panes
;; template is financial-chart's own, drawn by eas.el.

;;; Code:

(require 'eas)
(require 'financial-chart-eas)

(declare-function eas-link-open "eas-link-bus" (source bus &rest args))
(declare-function eas-show "eas-mode" (view &optional target))

(defconst financial-chart-eas-link-demo-second "examples/panes-demo.data.json"
  "Bindings of the demo's second ticker, relative to the repository.")

(defun financial-chart-eas-link-demo-open (&optional bus)
  "Open the demo's two views on BUS (default \"tickers\"); return them.
Nothing is displayed, so this runs in --batch."
  (let ((bus (or bus "tickers"))
        (root (file-name-directory (directory-file-name financial-chart-eas-templates-root))))
    (list (eas-link-open "panes" bus :bindings (eas-template-example "panes") :subject "TSM"
                         :params '("crosshair" "zoom"))
          (eas-link-open "panes" bus :subject "DEMO"
                         :bindings (eas-json-read-file
                                    (expand-file-name financial-chart-eas-link-demo-second root))))))

;;;###autoload
(defun financial-chart-eas-link-demo ()
  "Show two tickers' price, volume and RSI panes in two linked windows."
  (interactive)
  (let ((views (financial-chart-eas-link-demo-open))
        (display-buffer-overriding-action '((display-buffer-same-window))))
    (delete-other-windows)
    (eas-show (car views))
    (select-window (split-window-right))
    (eas-show (cadr views))))

(provide 'financial-chart-eas-link-demo)
;;; financial-chart-eas-link-demo.el ends here
