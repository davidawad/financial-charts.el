;;; easel-spec-test.el --- tests for chart/v1, templates and resolve -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defun easel-spec-test--codes (findings)
  "Return (CODE . PATH) pairs of FINDINGS."
  (mapcar (lambda (f) (cons (plist-get f :code) (plist-get f :path))) findings))

(ert-deftest easel-spec-parse-expands-mark-shorthand ()
  (should (equal (easel-spec-parse "{\"mark\":\"bar\"}") '(:mark (:type "bar"))))
  (should (equal (easel-spec-parse '(:layer [(:mark "line")]))
                 '(:layer [(:mark (:type "line"))]))))

(ert-deftest easel-spec-parse-rejects-non-objects ()
  (easel-test-should-code "INVALID_INPUT" (easel-spec-parse "[1]"))
  (easel-test-should-code "PARSE_ERROR" (easel-spec-parse "{\"mark\":")))

(ert-deftest easel-spec-check-names-each-problem-by-path ()
  (should (equal
           (easel-spec-test--codes
            (easel-spec-check
             "{\"mark\":\"arc\",\"facet\":{},\"transform\":[{\"lookup\":\"k\"}],
               \"encoding\":{\"theta\":{\"field\":\"a\"},
                             \"x\":{\"field\":\"a\",\"type\":\"bogus\"},
                             \"y\":{\"field\":\"b\",\"scale\":{\"type\":\"sqrt\"}}}}"))
           '(("UNSUPPORTED_FEATURE" . "/facet")
             ("UNSUPPORTED_FEATURE" . "/mark")
             ("UNSUPPORTED_FEATURE" . "/encoding/theta")
             ("INVALID_INPUT" . "/encoding/x/type")
             ("UNSUPPORTED_FEATURE" . "/encoding/y/scale/type")
             ("UNSUPPORTED_FEATURE" . "/transform/0")))))

(ert-deftest easel-spec-check-accepts-the-native-vocabulary ()
  (should-not
   (easel-spec-check
    '(:data (:values [(:a 1 :b 2)])
      :params [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")
               (:name "hover" :select (:type "point" :on "pointermove" :nearest t))]
      :transform [(:filter "datum.a > 0") (:calculate "datum.a * 2" :as "c")]
      :layer [(:mark "line" :encoding (:x (:field "a" :type "quantitative")
                                       :y (:field "b" :type "quantitative" :scale (:type "log"))))
              (:mark (:type "rule") :encoding (:x (:field "a" :type "quantitative")
                                                  :opacity (:condition (:param "hover" :value 1)
                                                                       :value 0)))]))))

(ert-deftest easel-spec-check-flags-input-binds-and-expr-params ()
  (should (equal (easel-spec-test--codes
                  (easel-spec-check
                   '(:mark "point"
                     :params [(:name "p" :bind (:input "range"))
                              (:name "q" :expr "1 + 1")])))
                 '(("UNSUPPORTED_FEATURE" . "/params/0/bind")
                   ("UNSUPPORTED_FEATURE" . "/params/1")))))

(ert-deftest easel-spec-check-restricts-to-supported-list ()
  (let ((easel-spec-supported-function (lambda () '("mark/line"))))
    (should (equal (easel-spec-test--codes (easel-spec-check '(:mark "bar")))
                   '(("UNSUPPORTED_FEATURE" . "/mark"))))
    (should-not (easel-spec-check '(:mark "line")))))

(ert-deftest easel-spec-view-without-mark-is-invalid ()
  (should (equal (easel-spec-test--codes (easel-spec-check '(:layer [(:encoding nil)])))
                 '(("INVALID_INPUT" . "/layer/0")))))

;;; Templates

(ert-deftest easel-template-registry-autoloads-templates-dir ()
  (easel-template-reload)
  (should (member "line" (easel-template-names)))
  (should (member "bars" (easel-template-names)))
  (let ((described (easel-template-describe "line")))
    (should (equal (plist-get described :version) "1.0.0"))
    (should (plist-get (plist-get described :slots) :data))
    (should (file-exists-p (plist-get described :example)))
    (should (string-suffix-p "templates/line.json" (plist-get described :path)))))

(ert-deftest easel-template-unknown-is-not-found ()
  (easel-test-should-code "NOT_FOUND" (easel-template-get "no-such-template")))

(ert-deftest easel-template-bind-names-the-slot ()
  (should (equal (plist-get (easel-test-should-code "SLOT_MISSING"
                              (easel-resolve "line" nil))
                            :slot)
                 "data"))
  (should (equal (plist-get (easel-test-should-code "SLOT_TYPE"
                              (easel-resolve "line" '(:data [(:date 1 :value 2)] :points 3)))
                            :slot)
                 "points"))
  (let ((err (easel-test-should-code "FIELD_MISSING"
               (easel-resolve "line" '(:data [(:t 1 :v 2)])))))
    (should (equal (plist-get err :slot) "x"))
    (should (equal (plist-get err :field) "date")))
  (let ((err (easel-test-should-code "SHAPE_INVALID"
               (easel-resolve "line" '(:data [(:date 1) 7])))))
    (should (equal (plist-get err :slot) "data"))
    (should (equal (plist-get err :index) 1)))
  (easel-test-should-code "INVALID_INPUT"
    (easel-resolve "line" '(:data [(:date 1 :value 2)] :colour "red"))))

(ert-deftest easel-template-enum-and-array-items ()
  (easel-template-register
   '(:x-easel (:template "test-enum" :version "1.0.0"
               :slots (:style (:enum ["a" "b"] :default "a")
                       :ids (:type "array" :items "integer" :default [])))
     :mark (:type (:x-easel:slot "style"))))
  (should (equal (easel-template-bind (easel-template-get "test-enum") '(:ids (1 2)))
                 '(:style "a" :ids [1 2])))
  (easel-test-should-code "SLOT_TYPE"
    (easel-template-bind (easel-template-get "test-enum") '(:style "c")))
  (should (equal (plist-get (easel-test-should-code "SLOT_TYPE"
                              (easel-template-bind (easel-template-get "test-enum")
                                                   '(:ids [1 "x"])))
                            :index)
                 1)))

;;; Resolve

(ert-deftest easel-resolve-template-goldens ()
  (dolist (name '("line" "bars"))
    (easel-test-golden (format "resolve-%s.json" name)
                       (easel-json-pretty (easel-resolve name (easel-template-example name))))))

(ert-deftest easel-resolve-when-and-strip ()
  (let ((resolved (easel-resolve "line" (plist-put (easel-template-example "line") :points t))))
    (should (= (length (plist-get resolved :layer)) 2))
    (should-not (plist-get resolved :x-easel))
    (should (equal (plist-get resolved :$schema) easel-spec-schema-url))
    (should (vectorp (plist-get (plist-get resolved :data) :values))))
  (should (= (length (plist-get (easel-resolve "line" (easel-template-example "line")) :layer))
             1)))

(ert-deftest easel-resolve-is-deterministic ()
  (let ((a (easel-resolve "bars" (easel-template-example "bars")))
        (b (easel-resolve "bars" (easel-template-example "bars"))))
    (should (equal (easel-resolve-hash a) (easel-resolve-hash b)))
    (should (string-prefix-p "sha256:" (easel-resolve-hash a)))))

(ert-deftest easel-resolve-materializes-domain-transforms ()
  (let ((easel-transforms nil))
    (easel-register-transform
     "double" :schema '(:field (:type "string" :required t) :as (:type "string" :default "d"))
     :fn (lambda (rows params)
           (let ((in (easel-key (plist-get params :field)))
                 (out (easel-key (plist-get params :as))))
             (seq-map (lambda (row) (append row (list out (* 2 (plist-get row in))))) rows))))
    (let ((resolved (easel-resolve-spec
                     '(:data (:values [(:a 1) (:a 2)])
                       :layer [(:transform [(:x-easel:transform "double" :field "a")
                                            (:filter "datum.d > 2")]
                                :mark "point")]))))
      (should (equal (plist-get (plist-get resolved :data) :values) [(:a 1 :d 2) (:a 2 :d 4)]))
      (should (equal (plist-get (aref (plist-get resolved :layer) 0) :transform)
                     [(:filter "datum.d > 2")])))
    (easel-test-should-code "UNSUPPORTED_FEATURE"
      (easel-resolve-spec '(:data (:values [(:a 1)])
                            :transform [(:filter "true") (:x-easel:transform "double" :field "a")]
                            :mark "point")))
    (easel-test-should-code "INVALID_INPUT"
      (easel-resolve-spec '(:data (:values [(:a 1)])
                            :transform [(:x-easel:transform "double")] :mark "point")))
    (easel-test-should-code "TRANSFORM_UNKNOWN"
      (easel-resolve-spec '(:data (:values [(:a 1)])
                            :transform [(:x-easel:transform "nope")] :mark "point")))))

(ert-deftest easel-resolve-output-passes-bin-chart-check ()
  (easel-test-require-chart)
  (dolist (name (easel-template-names))
    (let ((file (make-temp-file "easel-resolved" nil ".vl.json"
                                (easel-json-encode (easel-resolve name (easel-template-example name))))))
      (unwind-protect
          (should (zerop (call-process easel-test-chart-program nil nil nil "check" file)))
        (delete-file file)))))




(ert-deftest easel-describe-lists-registries ()
  (let ((all (easel-describe)))
    (should (equal (plist-get all :vega-lite) "6.4.1"))
    (should (seq-find (lambda (tpl) (equal (plist-get tpl :name) "line")) (plist-get all :templates)))
    (should (seq-find (lambda (tr) (equal (plist-get tr :name) "lttb")) (plist-get all :transforms)))
    (should (seq-find (lambda (ad) (equal (plist-get ad :name) "csv")) (plist-get all :adapters)))
    (should (stringp (easel-json-encode all))))
  (should (equal (easel-plist-keys (easel-describe "adapters")) '(:adapters)))
  (easel-test-should-code "NOT_FOUND" (easel-describe 'nope)))

(provide 'easel-spec-test)
;;; easel-spec-test.el ends here
