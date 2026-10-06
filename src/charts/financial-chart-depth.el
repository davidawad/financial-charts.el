;;; financial-chart-depth.el --- Order-book depth charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;;; Commentary:

;; Order-book depth: the order-book shape, its validator and cumulative
;; levels, and the depth kind drawn by eas's depth template.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defun financial-chart-depth--invalid-level (side index field code fmt &rest args)
  "Signal invalid order-book level INDEX on SIDE (\"bids\" or \"asks\").
FIELD names the level's field (price or size, or nil), CODE the reason
and FMT/ARGS the message.  The error's :field is SIDE.FIELD."
  (financial-chart--invalid index (if field (format "%s.%s" side field) side) code
                            "%s level: %s" side (apply #'format fmt args)))

(defun financial-chart-depth--validate-order-book (book)
  "Signal unless BOOK has positive levels and a non-crossed best bid/ask."
  (unless (and (listp book) (proper-list-p book)
               (cl-evenp (length book))
               (plist-member book :bids) (plist-member book :asks))
    (financial-chart--invalid nil nil "not_an_order_book"
                              "order book must be a plist with :bids and :asks lists"))
  (dolist (side '(:bids :asks))
    (let ((levels (plist-get book side)))
      (unless (and (listp levels) (proper-list-p levels))
        (financial-chart--invalid nil (substring (symbol-name side) 1) "not_a_list"
                                  "%s must be a list of (PRICE SIZE) pairs"
                                  (substring (symbol-name side) 1)))
      (cl-loop for level in levels for index from 0
               do (unless (and (listp level) (proper-list-p level)
                               (= (length level) 2))
                    (financial-chart-depth--invalid-level
                     (substring (symbol-name side) 1) index nil "invalid_level"
                     "expected (PRICE SIZE), got %S" level))
               do (let ((price (car level))
                        (size (cadr level)))
                    (unless (and (numberp price) (> price 0))
                      (financial-chart-depth--invalid-level
                       (substring (symbol-name side) 1) index "price" "not_positive"
                       "price must be a positive number, got %S" price))
                    (unless (and (numberp size) (> size 0))
                      (financial-chart-depth--invalid-level
                       (substring (symbol-name side) 1) index "size" "not_positive"
                       "size must be a positive number, got %S" size))))))
  (let* ((bids (plist-get book :bids))
         (best-bid (car (financial-chart-depth--sorted-levels book :bids 1)))
         (best-ask (car (financial-chart-depth--sorted-levels book :asks 1))))
    (when (and best-bid best-ask (> (car best-bid) (car best-ask)))
      (financial-chart-depth--invalid-level
       "bids" (cl-position best-bid bids :test #'eq) "price" "crossed_book"
       "best bid %s exceeds best ask %s; correct the crossed order-book prices"
       (car best-bid) (car best-ask)))
    t))

(defun financial-chart-depth--sorted-levels (book side &optional limit)
  "Nearest levels on SIDE from BOOK, capped at LIMIT when non-nil."
  (let* ((descending (eq side :bids))
         (levels (sort (copy-sequence (plist-get book side))
                       (lambda (a b)
                         (if descending (> (car a) (car b))
                           (< (car a) (car b)))))))
    (if limit
        (cl-subseq levels 0 (min limit (length levels)))
      levels)))

(defun financial-chart-depth--cumulative-levels (levels)
  "LEVELS in nearest-first order as (PRICE SIZE CUMULATIVE-SIZE) records."
  (let ((total 0))
    (mapcar (lambda (level)
              (setq total (+ total (cadr level)))
              (list (car level) (cadr level) total))
            levels)))

(defun financial-chart-depth--values (data _props)
  "Every level price in order-book DATA, for summaries."
  (mapcar #'car (append (plist-get data :bids) (plist-get data :asks))))

(defun financial-chart-depth--from-json (data)
  "JSON-parsed {\"bids\": [[P, S] ...], \"asks\": [...]} as an order-book plist."
  (append (when (assq 'bids data) (list :bids (alist-get 'bids data)))
          (when (assq 'asks data) (list :asks (alist-get 'asks data)))))

(defun financial-chart-depth--to-json (data)
  "Order-book DATA as a JSON object {\"bids\": [[P, S] ...], \"asks\": [...]}."
  (cl-flet ((levels (key) (apply #'vector (mapcar (lambda (l) (apply #'vector l))
                                                  (plist-get data key)))))
    (list (cons 'bids (levels :bids)) (cons 'asks (levels :asks)))))

(add-to-list 'financial-chart-shapes
             '(order-book
               :doc "Plist (:bids ((PRICE SIZE) ...) :asks ((PRICE SIZE) ...)); positive levels, with best bid no higher than best ask when both exist.  JSON: {\"bids\": [[price, size], ...], \"asks\": [...]}."
               :example (:bids ((100.9375 2.0) (100.875 4.0) (100.8125 3.0)
                                (100.75 5.0) (100.6875 2.5) (100.625 6.0)
                                (100.5625 3.5) (100.5 7.0))
                        :asks ((101.0625 1.5) (101.125 3.0) (101.1875 2.5)
                               (101.25 4.5) (101.3125 3.0) (101.375 5.5)
                               (101.4375 2.0) (101.5 6.0)))
               :validator financial-chart-depth--validate-order-book
               :values financial-chart-depth--values
               :from-json financial-chart-depth--from-json
               :to-json financial-chart-depth--to-json))

(financial-chart-register-kind
 'depth :shape 'order-book :template "depth" :adapter "order-book"
 :doc "Cumulative bid/ask order-book depth chart.")

(provide 'financial-chart-depth)
;;; financial-chart-depth.el ends here
