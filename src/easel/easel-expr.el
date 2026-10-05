;;; easel-expr.el --- the safe Vega expression subset -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; filter and calculate take Vega expression strings.  easel parses a
;; small subset into an AST and interprets it; it never calls `eval'.
;;
;;   literals    numbers, 'strings', "strings", true false null, [a, b]
;;   names       datum, datum.f, datum['f'], param names, PI, E
;;   operators   ?: || && == != === !== < <= > >= + - * / % ! unary -
;;   functions   `easel-expr-functions' (math, type tests, UTC date parts)
;;
;; Semantics follow JavaScript where it matters to charts: + joins
;; strings, a missing field is null, truthiness is JS truthiness, and
;; Vega's month() is 0-based.  Anything else fails as data:
;; PARSE_ERROR with :position, or UNSUPPORTED_FEATURE naming the call.

;;; Code:

(require 'easel-core)
(require 'easel-time)

(defconst easel-expr--token-regexp
  (concat "[ \t\n]*\\(?:"
          "\\([0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)"      ; 1 number
          "\\|'\\(\\(?:[^'\\\\]\\|\\\\.\\)*\\)'"                    ; 2 'string'
          "\\|\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\""                 ; 3 "string"
          "\\|\\([A-Za-z_$][A-Za-z0-9_$]*\\)"                       ; 4 name
          "\\|\\(===\\|!==\\|==\\|!=\\|<=\\|>=\\|&&\\|||\\|[]+*/%<>!?:()[.,-]\\)" ; 5 op
          "\\)")
  "One token of the expression subset.")

(defun easel-expr--tokenize (string)
  "Return the tokens of STRING as (TYPE VALUE POSITION) lists."
  (let ((pos 0) (tokens nil) (len (length string)))
    (while (progn (when (string-match "\\`[ \t\n]+" (substring string pos))
                    (setq pos (+ pos (match-end 0))))
                  (< pos len))
      (unless (eq (string-match easel-expr--token-regexp string pos) pos)
        (easel-signal "PARSE_ERROR"
                      (format "Unexpected character %S at %d in expression %S"
                              (substring string pos (1+ pos)) pos string)
                      :expr string :position pos))
      (push (cond ((match-beginning 1) (list 'num (string-to-number (match-string 1 string)) pos))
                  ((match-beginning 2) (list 'str (easel-expr--unescape (match-string 2 string)) pos))
                  ((match-beginning 3) (list 'str (easel-expr--unescape (match-string 3 string)) pos))
                  ((match-beginning 4) (list 'name (match-string 4 string) pos))
                  (t (list 'op (match-string 5 string) pos)))
            tokens)
      (setq pos (match-end 0)))
    (nreverse tokens)))

(defun easel-expr--unescape (text)
  "Remove backslash escapes from TEXT."
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" text))

;;; Parser (precedence climbing over a token list)

(defvar easel-expr--tokens nil "Remaining tokens while parsing.")
(defvar easel-expr--source nil "The expression being parsed.")

(defun easel-expr--fail (message)
  "Signal PARSE_ERROR with MESSAGE at the current token."
  (let ((pos (if easel-expr--tokens (nth 2 (car easel-expr--tokens)) (length easel-expr--source))))
    (easel-signal "PARSE_ERROR" (format "%s at %d in expression %S" message pos easel-expr--source)
                  :expr easel-expr--source :position pos)))

(defun easel-expr--peek-op (&rest ops)
  "Return the next token's operator when it is one of OPS."
  (let ((tok (car easel-expr--tokens)))
    (and (eq (car tok) 'op) (member (nth 1 tok) ops) (nth 1 tok))))

(defun easel-expr--expect (op)
  "Consume operator OP or fail."
  (unless (easel-expr--peek-op op) (easel-expr--fail (format "Expected %s" op)))
  (pop easel-expr--tokens))

(defconst easel-expr--binary-levels
  '(("||") ("&&") ("==" "!=" "===" "!==") ("<" "<=" ">" ">=") ("+" "-") ("*" "/" "%"))
  "Binary operators from loosest to tightest.")

(defun easel-expr--ternary ()
  "Parse a conditional expression."
  (let ((test (easel-expr--binary 0)))
    (if (not (easel-expr--peek-op "?"))
        test
      (pop easel-expr--tokens)
      (let ((then (easel-expr--ternary)))
        (easel-expr--expect ":")
        (list :cond test then (easel-expr--ternary))))))

(defun easel-expr--binary (level)
  "Parse binary operators at LEVEL and tighter."
  (if (>= level (length easel-expr--binary-levels))
      (easel-expr--unary)
    (let ((left (easel-expr--binary (1+ level))) op)
      (while (setq op (apply #'easel-expr--peek-op (nth level easel-expr--binary-levels)))
        (pop easel-expr--tokens)
        (setq left (list :binary op left (easel-expr--binary (1+ level)))))
      left)))

(defun easel-expr--unary ()
  "Parse a unary expression."
  (if-let* ((op (easel-expr--peek-op "!" "-" "+")))
      (progn (pop easel-expr--tokens) (list :unary op (easel-expr--unary)))
    (easel-expr--postfix (easel-expr--primary))))

(defun easel-expr--args (close)
  "Parse comma-separated expressions up to CLOSE."
  (let (args)
    (unless (easel-expr--peek-op close)
      (push (easel-expr--ternary) args)
      (while (easel-expr--peek-op ",")
        (pop easel-expr--tokens)
        (push (easel-expr--ternary) args)))
    (easel-expr--expect close)
    (nreverse args)))

(defun easel-expr--primary ()
  "Parse a literal, name, array or parenthesized expression."
  (let ((tok (pop easel-expr--tokens)))
    (pcase tok
      ('nil (easel-expr--fail "Unexpected end"))
      (`(num ,n ,_) (list :lit n))
      (`(str ,s ,_) (list :lit s))
      (`(name "true" ,_) (list :lit t))
      (`(name "false" ,_) (list :lit :false))
      (`(name "null" ,_) (list :lit :null))
      (`(name ,name ,_) (list :var name))
      (`(op "(" ,_) (prog1 (easel-expr--ternary) (easel-expr--expect ")")))
      (`(op "[" ,_) (list :array (easel-expr--args "]")))
      (_ (push tok easel-expr--tokens) (easel-expr--fail "Unexpected token")))))

(defun easel-expr--postfix (node)
  "Parse member access and calls following NODE."
  (let (op)
    (while (setq op (easel-expr--peek-op "." "[" "("))
      (pop easel-expr--tokens)
      (setq node
            (pcase op
              ("." (let ((tok (pop easel-expr--tokens)))
                     (unless (eq (car tok) 'name) (easel-expr--fail "Expected a field name"))
                     (list :member node (list :lit (nth 1 tok)))))
              ("[" (prog1 (list :member node (easel-expr--ternary)) (easel-expr--expect "]")))
              ("(" (unless (eq (car node) :var)
                     (easel-expr--fail "Only named functions can be called"))
               (let ((name (nth 1 node)))
                 (unless (assoc name easel-expr-functions)
                   (easel-signal "UNSUPPORTED_FEATURE"
                                 (format "Function %s() is not in the expression subset; available: %s"
                                         name (mapconcat #'car easel-expr-functions " "))
                                 :expr easel-expr--source :function name))
                 (list :call name (easel-expr--args ")")))))))
    node))

(defvar easel-expr--cache (make-hash-table :test 'equal)
  "Parsed expressions keyed by source string.")

(defun easel-expr-parse (string)
  "Parse expression STRING into an AST (cached)."
  (or (gethash string easel-expr--cache)
      (let* ((easel-expr--source string)
             (easel-expr--tokens (easel-expr--tokenize string))
             (ast (easel-expr--ternary)))
        (when easel-expr--tokens (easel-expr--fail "Unexpected trailing input"))
        (puthash string ast easel-expr--cache))))

;;; Evaluation

(defun easel-expr-truthy (value)
  "JavaScript truthiness of VALUE."
  (not (or (memq value '(nil :false :null)) (equal value "")
           (and (numberp value) (or (zerop value) (isnan (float value)))))))

(defun easel-expr--number (value)
  "Coerce VALUE to a number the way JavaScript's unary + does."
  (cond ((numberp value) value)
        ((eq value t) 1) ((memq value '(:false :null nil)) 0)
        ((and (stringp value) (string-match-p "\\`[ \t]*[-+]?[0-9.]+\\([eE][-+]?[0-9]+\\)?[ \t]*\\'" value))
         (string-to-number value))
        ((stringp value) (or (easel-time-parse value) 0.0e+NaN))
        (t 0.0e+NaN)))

(defun easel-expr--string (value)
  "Coerce VALUE to a string the way JavaScript does."
  (cond ((stringp value) value)
        ((eq value t) "true") ((eq value :false) "false") ((memq value '(:null nil)) "null")
        ((and (floatp value) (= value (ffloor value)) (< (abs value) 1e15))
         (number-to-string (truncate value)))
        (t (format "%s" value))))

(defun easel-expr--equal (a b)
  "Loose equality of A and B."
  (cond ((and (numberp a) (numberp b)) (= a b))
        ((or (numberp a) (numberp b)) (ignore-errors (= (easel-expr--number a) (easel-expr--number b))))
        (t (equal a b))))

(defun easel-expr--compare (op a b)
  "Apply comparison OP to A and B."
  (let ((result (if (and (stringp a) (stringp b))
                    (pcase op ("<" (string< a b)) (">" (string< b a))
                           ("<=" (not (string< b a))) (">=" (not (string< a b))))
                  (let ((x (easel-expr--number a)) (y (easel-expr--number b)))
                    (pcase op ("<" (< x y)) (">" (> x y)) ("<=" (<= x y)) (">=" (>= x y)))))))
    (if result t :false)))

(defun easel-expr--arith (op a b)
  "Apply arithmetic OP to A and B."
  (if (and (equal op "+") (or (stringp a) (stringp b)))
      (concat (easel-expr--string a) (easel-expr--string b))
    (let ((x (easel-expr--number a)) (y (easel-expr--number b)))
      (pcase op
        ("+" (+ x y)) ("-" (- x y)) ("*" (* x y))
        ("/" (if (and (zerop y) (integerp x)) (/ (float x) y) (/ (float x) y)))
        ("%" (if (zerop y) 0.0e+NaN (let ((r (mod (float x) (float y))))
                                       (if (and (< x 0) (/= r 0)) (- r (abs y)) r))))))))

(defun easel-expr--member (object key)
  "Return field KEY of OBJECT (a row plist or param value)."
  (cond ((and (vectorp object) (numberp key))
         (if (< -1 key (length object)) (aref object (truncate key)) :null))
        ((and (easel-object-p object) (stringp key))
         (let ((cell (plist-member object (easel-key key))))
           (if cell (cadr cell) :null)))
        (t :null)))

(defun easel-expr-eval (ast datum &optional env)
  "Evaluate AST for row DATUM.  ENV is a plist of param values by name key."
  (pcase ast
    (`(:lit ,v) v)
    (`(:array ,items) (vconcat (mapcar (lambda (i) (easel-expr-eval i datum env)) items)))
    (`(:var ,name)
     (cond ((equal name "datum") datum)
           ((plist-member env (easel-key name)) (plist-get env (easel-key name)))
           ((equal name "PI") float-pi) ((equal name "E") float-e)
           (t (easel-signal "INVALID_INPUT"
                            (format "Unknown name %s in expression; fields are datum.%s, params by name"
                                    name name)
                            :name name))))
    (`(:member ,object ,key)
     (easel-expr--member (easel-expr-eval object datum env) (easel-expr-eval key datum env)))
    (`(:unary ,op ,a)
     (let ((v (easel-expr-eval a datum env)))
       (pcase op ("!" (if (easel-expr-truthy v) :false t))
              ("-" (- (easel-expr--number v))) ("+" (easel-expr--number v)))))
    (`(:cond ,test ,then ,else)
     (easel-expr-eval (if (easel-expr-truthy (easel-expr-eval test datum env)) then else) datum env))
    (`(:binary ,op ,a ,b)
     (pcase op
       ("&&" (let ((x (easel-expr-eval a datum env)))
               (if (easel-expr-truthy x) (easel-expr-eval b datum env) x)))
       ("||" (let ((x (easel-expr-eval a datum env)))
               (if (easel-expr-truthy x) x (easel-expr-eval b datum env))))
       (_ (let ((x (easel-expr-eval a datum env)) (y (easel-expr-eval b datum env)))
            (pcase op
              ((or "==" "===") (if (easel-expr--equal x y) t :false))
              ((or "!=" "!==") (if (easel-expr--equal x y) :false t))
              ((or "<" "<=" ">" ">=") (easel-expr--compare op x y))
              (_ (easel-expr--arith op x y)))))))
    (`(:call ,name ,args)
     (apply (cdr (assoc name easel-expr-functions))
            (mapcar (lambda (arg) (easel-expr-eval arg datum env)) args)))))

(defun easel-expr-evaluate (string datum &optional env)
  "Parse (cached) and evaluate expression STRING for DATUM with ENV."
  (easel-expr-eval (easel-expr-parse string) datum env))

(defun easel-expr--date-part (key &optional offset)
  "Return a function extracting date field KEY (plus OFFSET) from a date."
  (lambda (value)
    (let ((ms (easel-time-parse value)))
      (if ms (+ (or offset 0) (plist-get (easel-time-fields ms) key)) :null))))

(defun easel-expr--num-fn (fn)
  "Wrap numeric FN so its arguments are coerced to numbers."
  (lambda (&rest args) (apply fn (mapcar #'easel-expr--number args))))

(defconst easel-expr-functions
  `(("abs" . ,(easel-expr--num-fn #'abs))
    ("ceil" . ,(easel-expr--num-fn (lambda (x) (float (ceiling x)))))
    ("floor" . ,(easel-expr--num-fn (lambda (x) (float (floor x)))))
    ("round" . ,(easel-expr--num-fn (lambda (x) (float (floor (+ x 0.5))))))
    ("sqrt" . ,(easel-expr--num-fn #'sqrt))
    ("log" . ,(easel-expr--num-fn #'log))
    ("exp" . ,(easel-expr--num-fn #'exp))
    ("pow" . ,(easel-expr--num-fn #'expt))
    ("min" . ,(easel-expr--num-fn #'min))
    ("max" . ,(easel-expr--num-fn #'max))
    ("isValid" . ,(lambda (v) (if (memq v '(nil :null)) :false t)))
    ("isNumber" . ,(lambda (v) (if (numberp v) t :false)))
    ("isString" . ,(lambda (v) (if (stringp v) t :false)))
    ("isDate" . ,(lambda (v) (if (easel-time-string-p v) t :false)))
    ("toNumber" . easel-expr--number)
    ("toString" . easel-expr--string)
    ("toBoolean" . ,(lambda (v) (if (easel-expr-truthy v) t :false)))
    ("toDate" . ,(lambda (v) (or (easel-time-parse v) :null)))
    ("time" . ,(lambda (v) (or (easel-time-parse v) :null)))
    ("year" . ,(easel-expr--date-part :year))
    ("month" . ,(easel-expr--date-part :month -1))
    ("date" . ,(easel-expr--date-part :day))
    ("day" . ,(easel-expr--date-part :weekday))
    ("hours" . ,(easel-expr--date-part :hours))
    ("minutes" . ,(easel-expr--date-part :minutes))
    ("seconds" . ,(easel-expr--date-part :seconds))
    ("datetime" . ,(lambda (y &optional m d h mi s ms)
                     (easel-time-ms y (1+ (or m 0)) (or d 1) h mi s ms)))
    ("length" . ,(lambda (v) (length v)))
    ("upper" . ,(lambda (s) (upcase (easel-expr--string s))))
    ("lower" . ,(lambda (s) (downcase (easel-expr--string s))))
    ("inrange" . ,(lambda (v range)
                    (let ((lo (min (aref range 0) (aref range 1)))
                          (hi (max (aref range 0) (aref range 1))))
                      (if (<= lo (easel-expr--number v) hi) t :false))))
    ("if" . ,(lambda (test a b) (if (easel-expr-truthy test) a b))))
  "Functions callable from expressions: (NAME . FUNCTION).")

(provide 'easel-expr)
;;; easel-expr.el ends here
