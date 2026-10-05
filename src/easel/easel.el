;;; easel.el --- Interactive, agent-drivable charts from declarative JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: data, multimedia, tools

;;; Commentary:

;; easel draws charts configured ahead of time as JSON (a Vega-Lite
;; subset) from data Emacs pipes in, as SVG in GUI frames and text in
;; terminals.  See docs/design/engine.md for the layer contracts.
;;
;; This file loads every layer.  Each layer is also usable alone.

;;; Code:

(require 'easel-core)
(require 'easel-time)
(require 'easel-data)
(require 'easel-adapters)
(require 'easel-data-org)
(require 'easel-expr)
(require 'easel-transform)
(require 'easel-lttb)
(require 'easel-spec)
(require 'easel-transform-domain)
(require 'easel-template)
(require 'easel-resolve)
(require 'easel-describe)
(require 'easel-scale)
(require 'easel-compile)
(require 'easel-hit)
(require 'easel-scene)
(require 'easel-glyph)
(require 'easel-svg)
(require 'easel-text)
(require 'easel-params)
(require 'easel-event)
(require 'easel-zoom)
(require 'easel-reduce)
(require 'easel-view)
(require 'easel-tip)
(require 'easel-action)
(require 'easel-action-org)
(require 'easel-action-drill)
(require 'easel-mode)
(require 'easel-mode-tip)
(require 'easel-crosshair)
(require 'easel-brush)
(require 'easel-stream)
(require 'easel-tty)
(require 'easel-parity)
(require 'easel-chart)
(require 'easel-conformance)

(provide 'easel)
;;; easel.el ends here
