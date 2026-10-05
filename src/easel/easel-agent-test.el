;;; easel-agent-test.el --- tests for the agent surface (fc-qx1.10) -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel-agent)
(require 'easel-agent-cli)

(defconst easel-agent-test--brush-spec
  "{\"data\":{\"values\":[{\"x\":1,\"y\":3,\"c\":\"u\"},{\"x\":2,\"y\":5,\"c\":\"v\"},{\"x\":3,\"y\":4,\"c\":\"u\"},
    {\"x\":4,\"y\":8,\"c\":\"v\"},{\"x\":5,\"y\":6,\"c\":\"u\"}]},
  \"width\":200,\"height\":100,
  \"params\":[{\"name\":\"brush\",\"select\":{\"type\":\"interval\",\"encodings\":[\"x\"]}}],
  \"mark\":\"point\",
  \"encoding\":{\"x\":{\"field\":\"x\",\"type\":\"quantitative\"},\"y\":{\"field\":\"y\",\"type\":\"quantitative\"},
              \"color\":{\"field\":\"c\",\"type\":\"nominal\"}}}"
  "Points with an x brush, as JSON (what an agent sends).")

(defconst easel-agent-test--unsupported-spec
  "{\"mark\":\"arc\",\"data\":{\"values\":[{\"a\":1}]},\"encoding\":{\"theta\":{\"field\":\"a\",\"type\":\"quantitative\"}}}"
  "A spec outside the native subset.")

(defun easel-agent-test--bindings ()
  "The line template's example bindings."
  (easel-template-example "line"))

(defmacro easel-agent-test--with-views (&rest body)
  "Run BODY with a fresh view registry holding view \"t\" (the brush spec)."
  `(let ((easel-views (make-hash-table :test 'equal)))
     (easel-agent "open" easel-agent-test--brush-spec :id "t" :backend "text")
     ,@body))

(defun easel-agent-test--shape (env)
  "Assert ENV is a chart/v1 envelope that encodes as JSON; return ENV."
  (should (equal (plist-get env :contract) "chart/v1"))
  (should (memq (plist-get env :ok) '(t :false)))
  (should (plist-member env :data))
  (should (vectorp (plist-get env :next)))
  (should (seq-every-p #'stringp (plist-get env :next)))
  (let ((back (easel-json-parse (easel-json-encode env))))
    (should (equal (plist-get back :contract) "chart/v1")))
  env)

(defun easel-agent-test--ok (env)
  "Assert ENV is a successful envelope; return its data."
  (easel-agent-test--shape env)
  (unless (eq (plist-get env :ok) t)
    (ert-fail (list "expected ok" env)))
  (should-not (plist-member env :reason))
  (plist-get env :data))

(defun easel-agent-test--fail (env &optional code)
  "Assert ENV failed (with reason CODE) and carries next[]; return evidence."
  (easel-agent-test--shape env)
  (should (eq (plist-get env :ok) :false))
  (should (assoc (plist-get env :reason) easel-reason-codes))
  (when code (should (equal (plist-get env :reason) code)))
  (should (stringp (plist-get (plist-get env :evidence) :message)))
  (should (> (length (plist-get env :next)) 0))
  (plist-get env :evidence))

(defconst easel-agent-test--cases
  `(("describe" ("verbs") ("nope"))
    ("example" ("line") ("nope"))
    ("check" ("line" :data ,(lambda () (easel-agent-test--bindings))) ("line"))
    ("explain" ("line" :data ,(lambda () (easel-agent-test--bindings)) :stage "scene")
     ("line" :data ,(lambda () (easel-agent-test--bindings)) :stage "pixels"))
    ("render" ("line" :data ,(lambda () (easel-agent-test--bindings)) :backend "svg")
     (,easel-agent-test--unsupported-spec :backend "text"))
    ("export" ("line" :data ,(lambda () (easel-agent-test--bindings)) :vl t) ("{not json"))
    ("bench" ("line" :data ,(lambda () (easel-agent-test--bindings)) :n 1) ())
    ("doctor" () ("extra" :bogus 1))
    ("views" () (:bogus 1))
    ("open" ("line" :data ,(lambda () (easel-agent-test--bindings)) :subject "s") ("line" :data "[1,2]"))
    ("close" ("t") ("nope"))
    ("inspect" ("t") ("nope"))
    ("dispatch" ("t" "{\"type\":\"key\",\"key\":\"+\"}") ("t" "{\"type\":\"teleport\"}"))
    ("log" ("t" :n 5) ("nope"))
    ("selection" ("t" :as "org") ("t" :as "xml")))
  "VERB, success args and failure args; functions are called for values.")

(defun easel-agent-test--args (args)
  "ARGS with function values called."
  (mapcar (lambda (a) (if (functionp a) (funcall a) a)) args))

(ert-deftest easel-agent-every-verb-has-envelope-cases ()
  (should (equal (sort (mapcar #'car easel-agent-test--cases) #'string<) (easel-agent-verb-names))))

(ert-deftest easel-agent-every-verb-returns-the-envelope ()
  (dolist (case easel-agent-test--cases)
    (easel-agent-test--with-views
      (ert-info ((format "%s ok" (car case)))
        (easel-agent-test--ok (apply #'easel-agent (car case) (easel-agent-test--args (nth 1 case))))))
    (easel-agent-test--with-views
      (ert-info ((format "%s fail" (car case)))
        (easel-agent-test--fail (apply #'easel-agent (car case) (easel-agent-test--args (nth 2 case))))))))

(ert-deftest easel-agent-unknown-verbs-and-options-fail-with-next ()
  (should (string-match-p "verbs: bench" (plist-get (easel-agent-test--fail (easel-agent "draw") "INVALID_INPUT")
                                                   :message)))
  (should (equal (plist-get (easel-agent-test--fail (easel-agent "render" "line" :colour "red")) :option) "colour"))
  (should (member "bin/easel describe verbs" (append (plist-get (easel-agent 'nope) :next) nil))))

(ert-deftest easel-agent-describe-adds-verbs-events-and-reasons ()
  (let ((all (easel-agent-test--ok (easel-agent "describe"))))
    (should (plist-get all :templates))
    (should (equal (plist-get all :contract) "chart/v1"))
    (should (seq-find (lambda (v) (equal (plist-get v :name) "dispatch")) (plist-get all :verbs)))
    (should (equal (length (plist-get all :reasons)) (length easel-reason-codes))))
  (should (equal (easel-plist-keys (easel-agent-test--ok (easel-agent "describe" "events"))) '(:events)))
  (should (equal (easel-plist-keys (easel-agent-test--ok (easel-agent 'describe 'adapters))) '(:adapters))))

(ert-deftest easel-agent-example-renders-as-is ()
  (let ((bindings (easel-agent-test--ok (easel-agent "example" "line"))))
    (easel-agent-test--ok (easel-agent "check" "line" :data bindings))
    (should (string-match-p "Daily value" (plist-get (easel-agent-test--ok (easel-agent "render" "line" :data bindings))
                                                     :output)))))

(ert-deftest easel-agent-check-names-code-path-and-next ()
  (let ((ev (easel-agent-test--fail
             (easel-agent "check" "{\"mark\":\"bar\",\"encoding\":{\"x\":{\"field\":\"a\",\"type\":\"bogus\"}}}")
             "INVALID_INPUT")))
    (should (equal (plist-get ev :path) "/encoding/x/type")))
  (let ((env (easel-agent "check" "line" :data '(:data [(:date "2026-01-01" :value 1)] :y "price"))))
    (easel-agent-test--fail env "FIELD_MISSING")
    (should (member "bin/easel example line" (append (plist-get env :next) nil))))
  (let ((env (easel-agent "check" "line" :data '(:data [(:date "2026-01-01" :value "x")]))))
    (easel-agent-test--shape env))
  ;; Unsupported features still display (static fallback): a warning, not a failure.
  (let* ((env (easel-agent "check" easel-agent-test--unsupported-spec))
         (data (easel-agent-test--ok env)))
    (should (eq (plist-get data :native) :false))
    (should (equal (plist-get (aref (plist-get data :warnings) 0) :path) "/mark"))
    (should (seq-find (lambda (c) (string-match-p "export .* --vl" c)) (plist-get env :next)))))

(ert-deftest easel-agent-render-text-is-the-engine-text-deterministically ()
  (let* ((bindings (easel-agent-test--bindings))
         (expected (substring-no-properties
                    (easel-text-render (easel-compile (easel-resolve "line" bindings) :target 'text
                                                      :size '(:cols 50 :rows 12)))))
         (a (easel-agent "render" "line" :data bindings :cols 50 :rows 12))
         (b (easel-agent "render" "line" :data (easel-json-encode bindings) :cols "50" :rows "12")))
    (should (equal (plist-get (easel-agent-test--ok a) :output) expected))
    (should (equal (plist-get (easel-agent-test--ok b) :output) expected)))
  (should (string-prefix-p "<svg" (plist-get (easel-agent-test--ok
                                              (easel-agent "render" easel-agent-test--brush-spec :backend "svg"))
                                             :output)))
  (easel-agent-test--fail (easel-agent "render" "line" :data (easel-agent-test--bindings) :cols 50) "INVALID_INPUT")
  (easel-agent-test--fail (easel-agent "render" easel-agent-test--brush-spec :data "{}") "INVALID_INPUT"))

(ert-deftest easel-agent-explain-stages-and-export ()
  (let* ((bindings (easel-agent-test--bindings))
         (resolved (easel-resolve "line" bindings)))
    (should (equal (plist-get (easel-agent-test--ok (easel-agent "explain" "line" :data bindings)) :artifact)
                   resolved))
    (let ((compiled (plist-get (easel-agent-test--ok (easel-agent "explain" "line" :data bindings :stage "compile"))
                               :artifact)))
      (should (plist-get (plist-get (aref (plist-get compiled :views) 0) :scales) :x)))
    (should (equal (plist-get (plist-get (easel-agent-test--ok (easel-agent "explain" "line" :data bindings
                                                                            :stage "scene"))
                                         :artifact)
                              :contract)
                   "scene/v1"))
    (let ((vl (easel-agent-test--ok (easel-agent "export" "line" :data bindings :vl t))))
      (should (equal vl resolved))
      (should-not (string-match-p "x-easel" (easel-json-encode vl))))))

(ert-deftest easel-agent-live-loop-open-dispatch-inspect-log-selection ()
  (let ((easel-views (make-hash-table :test 'equal)))
    (should (equal (plist-get (easel-agent-test--ok (easel-agent "open" "line" :data (easel-agent-test--bindings)
                                                                 :subject "daily"))
                              :id)
                   "line:daily"))
    (should (equal (plist-get (aref (easel-agent-test--ok (easel-agent "views")) 0) :id) "line:daily"))
    (easel-agent "open" easel-agent-test--brush-spec :id "pts")
    (should (equal (plist-get (easel-agent-test--ok
                               (easel-agent "dispatch" "pts" "{\"type\":\"wheel\",\"px\":[100,50],\"delta\":-3}"))
                              :last-event)
                   "wheel at [100 50]"))
    (let ((after (easel-agent-test--ok (easel-agent "dispatch" "pts" '(:type "brush" :x [2 4])))))
      (should (string-prefix-p "brush x 2..4" (plist-get after :last-event))))
    (let ((sel (easel-agent-test--ok (easel-agent "selection" "pts"))))
      (should (equal (plist-get sel :n) 3))
      (should (equal (mapcar (lambda (r) (plist-get r :x)) (plist-get sel :rows)) '(2 3 4))))
    (should (string-prefix-p "| x | y | c |"
                             (plist-get (easel-agent-test--ok (easel-agent "selection" "pts" :as "org")) :org)))
    (let ((log (easel-agent-test--ok (easel-agent "log" "pts"))))
      (should (equal (mapcar (lambda (e) (plist-get e :type)) log) '("wheel" "brush")))
      ;; Replaying the log on a fresh view reproduces the selection.
      (easel-agent "open" easel-agent-test--brush-spec :id "replay")
      (easel-agent-test--ok (easel-agent "dispatch" "replay" (easel-json-encode
                                                              (vconcat (mapcar (lambda (e) (plist-get e :event)) log)))))
      (should (equal (plist-get (easel-agent-test--ok (easel-agent "selection" "replay")) :n) 3)))
    (easel-agent-test--ok (easel-agent "close" "pts"))
    (let ((env (easel-agent "inspect" "pts")))
      (easel-agent-test--fail env "VIEW_NOT_FOUND")
      (should (equal (aref (plist-get env :next) 0) "emacsclient --eval '(easel-agent-json \"views\")'")))
    (let ((env (easel-agent "dispatch" "replay" "{\"type\":\"key\",\"key\":\"q\"}")))
      (should (equal (plist-get (easel-agent-test--fail env "EVENT_INVALID") :field) "key"))
      (should (member "bin/easel describe events" (append (plist-get env :next) nil))))))

(ert-deftest easel-agent-json-is-the-envelope-as-json ()
  (let ((easel-views (make-hash-table :test 'equal)))
    (let ((env (easel-json-parse (easel-agent-json "views"))))
      (should (eq (plist-get env :ok) t))
      (should (equal (plist-get env :data) [])))
    (should (equal (plist-get (easel-json-parse (easel-agent-json "inspect" "x")) :reason) "VIEW_NOT_FOUND"))))

(ert-deftest easel-agent-doctor-rows-are-eager-and-typed ()
  (let ((rows (plist-get (easel-agent-test--ok (easel-agent "doctor")) :rows)))
    (should (seq-find (lambda (r) (equal (plist-get r :name) "template:line")) rows))
    (seq-doseq (r rows)
      (should (member (plist-get r :status) '("pass" "fail" "skip")))
      (should (stringp (plist-get r :detail))))))

(ert-deftest easel-agent-bench-measures-every-stage ()
  (let ((ms (plist-get (easel-agent-test--ok (easel-agent "bench" "line" :data (easel-agent-test--bindings) :n 2))
                       :ms)))
    (dolist (k '(:resolve :compile-svg :render-svg :compile-text :render-text :hover))
      (should (numberp (plist-get (plist-get ms k) :mean))))))

(ert-deftest easel-agent-cli-parses-shell-arguments ()
  (should (equal (easel-agent-cli-args "export" '("line" "--data" "b.json" "--vl" "--backend=text"))
                 '(("line" :data "b.json" :vl t :backend "text"))))
  (should (equal (cdr (easel-agent-cli-args "render" '("line" "--raw"))) t))
  (should (equal (car (easel-agent-cli-run '("check" "line"))) 1))
  (let ((out (easel-agent-cli-run '("inspect" "x"))))
    (should (equal (car out) 1))
    (should (string-match-p "emacsclient --eval" (cdr out))))
  (let ((out (easel-agent-cli-run (list "render" "line" "--data" (easel-test-file "examples/line.data.json")
                                        "--cols" "40" "--rows" "10" "--raw"))))
    (should (equal (car out) 0))
    (should (string-match-p "Daily value" (cdr out)))))

(ert-deftest easel-agent-bin-easel-runs-in-batch ()
  (let* ((process-environment (cons (concat "EMACS=" (expand-file-name invocation-name invocation-directory))
                                    process-environment))
         (run (lambda (input &rest args)
                (with-temp-buffer
                  (let ((exit (if input
                                  (progn (insert input)
                                         (apply #'call-process-region (point-min) (point-max)
                                                (easel-test-file "bin/easel") t t nil args))
                                (apply #'call-process (easel-test-file "bin/easel") nil t nil args))))
                    (cons exit (buffer-string)))))))
    (let ((ok (funcall run nil "describe" "verbs")))
      (should (equal (car ok) 0))
      (should (equal (plist-get (easel-json-parse (cdr ok)) :contract) "chart/v1")))
    (let* ((bad (funcall run nil "check" "line"))
           (env (easel-json-parse (cdr bad))))
      (should (equal (car bad) 1))
      (should (equal (plist-get env :reason) "SLOT_MISSING"))
      (should (> (length (plist-get env :next)) 0)))
    (let ((piped (funcall run easel-agent-test--brush-spec "render" "-" "--cols" "30" "--rows" "8" "--raw")))
      (should (equal (car piped) 0))
      (should (string-match-p "┤" (cdr piped))))))

(provide 'easel-agent-test)
;;; easel-agent-test.el ends here
