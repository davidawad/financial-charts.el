;;; ob-easel.el --- org-babel support for easel charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The name org-babel looks for: (org-babel-do-load-languages
;; 'org-babel-load-languages '((easel . t))) loads this file, which
;; loads the whole surface (easel-babel.el and its inline view).

;;; Code:

(require 'easel-babel)
(require 'easel-babel-source)
(require 'easel-babel-inline)

(provide 'ob-easel)
;;; ob-easel.el ends here
