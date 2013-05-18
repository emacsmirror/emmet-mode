;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst zencoding-mode:version "0.5.1")

;; Include the trie data structure for caching
;(require 'zencoding-trie)

(require 'cl)

(defmacro zencoding-defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

(defun zencoding-join-string (lis joiner)
  (mapconcat 'identity lis joiner))

(defun zencoding-get-keys-of-hash (hash)
  (let ((ks nil))
    (maphash #'(lambda (k v) (setq ks (cons k ks))) hash)
    ks))

(defun zencoding-get-vals-of-hash (hash)
  (let ((vs nil))
    (maphash #'(lambda (k v) (setq vs (cons v vs))) hash)
    vs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(defmacro zencoding-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-pif (test-form then-form &rest else-forms)
  "Parser anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-parse (regex nums label &rest body)
  "Parse according to a regex and update the `input' variable."
  `(zencoding-aif (zencoding-regex ,regex input ',(number-sequence 0 nums))
                  (let ((input (elt it ,nums)))
                    ,@body)
                  `,`(error ,(concat "expected " ,label))))

(defmacro zencoding-run (parser then-form &rest else-forms)
  "Run a parser and update the input properly, extract the parsed
   expression."
  `(zencoding-pif (,parser input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  ,@(or else-forms '(it))))

(defmacro zencoding-por (parser1 parser2 then-form &rest else-forms)
  "OR two parsers. Try one parser, if it fails try the next."
  `(zencoding-pif (,parser1 input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  (zencoding-pif (,parser2 input)
                                 (let ((input (cdr it))
                                       (expr (car it)))
                                   ,then-form)
                                 ,@else-forms)))

(defun zencoding-regex (regexp string refs)
  "Return a list of (`ref') matches for a `regex' on a `string' or nil."
  (if (string-match (concat "^" regexp "\\([^\n]*\\)$") string)
      (mapcar (lambda (ref) (match-string ref string))
              (if (sequencep refs) refs (list refs)))
    nil))
