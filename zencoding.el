;; Demo

;; (transform (car (expr "a>b>c+d+e")))
;; => <a><b><c></c><d></d><e></e></b></a>

;; (transform (car (expr "html>head+(body>p)")))
;; => <html><head></head><body><p></p></body></html>

;; (transform (car (expr "html>head+(body>p+(ul>li))")))
;; => [indentation added with xml-mode]
;; <html>
;;   <head>
;;   </head>
;;   <body>
;;     <p>
;;     </p>
;;     <ul>
;;       <li>
;;       </li>
;;     </ul>
;;   </body>
;; </html>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro pif (test-form then-form &rest else-forms)
  "Parser anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@else-forms)))

(defmacro parse (regex nums label &rest body)
  "Parse according to a regex and update the `input' variable."
  `(aif (regex ,regex input ',(number-sequence 0 nums))
        (let ((input (elt it ,nums)))
          ,@body)
        `,`(error ,(concat "expected " ,label))))

(defmacro run (parser then-form &rest else-forms)
  "Run a parser and update the input properly, extract the parsed
   expression."
  `(pif (,parser input)
        (let ((input (cdr it))
              (expr (car it)))
          ,then-form)
        ,@else-forms))

(defmacro por (parser1 parser2 then-form &rest else-forms)
  "OR two parsers. Try one parser, if it fails try the next."
  `(pif (,parser1 input)
        (let ((input (cdr it))
              (expr (car it)))
          ,then-form)
        (pif (,parser2 input)
             (let ((input (cdr it))
                   (expr (car it)))
               ,then-form)
             ,@else-forms)))

(defun regex (regexp string refs)
  "Return a list of (`ref') matches for a `regex' on a `string' or nil."
  (if (string-match (concat "^" regexp "\\(.*\\)$") string)
      (mapcar (lambda (ref) (match-string ref string)) 
              (if (sequencep refs) refs (list refs)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding parsers

(defun expr (input)
  "Parse a zen coding expression."
  (pif (siblings input)
       it
       (pif (parent-child input)
            it
            (pif (pexpr input)
                 it
                 (pif (atom input)
                      it
                      '(error "no match, expecting ( or a-zA-Z0-9"))))))

(defun atom (input)
  "Parse a simple a-zA-Z0-9 atom (e.g. html/head/xsl:if/br)."
  (parse "\\([a-zA-Z0-9]+\\)" 2 "atom, a-zA-Z0-9"
         `((atom . ,(elt it 1)) . ,input)))

(defun pexpr (input)
  "A zen coding expression with parentheses around it."
  (parse "(" 1 "("
         (run expr
              (aif (regex ")" input '(0 1))
                   `(,expr . ,(elt it 1))
                   '(error "expecting `)'")))))

(defun parent-child (input)
  "Parse an atom>e expression, where `n' is an atom and `e' is any 
   expression."
  (run atom
       (let ((parent expr))
         (parse ">" 1 ">"
                (run expr
                     (let ((child expr))
                       `((parent-child ,parent ,child) . ,input))
                     '(error "expected child"))))
       '(error "expected parent")))

(defun sibling (input)
  (por pexpr atom
       it
       '(error "expected sibling")))

(defun siblings (input)
  "Parse an e+e expression, where e is an atom."
  (por pexpr atom
       (let ((parent expr))
         (parse "+" 1 "+"
                (por siblings sibling
                     (let ((child expr))
                       `((siblings ,parent ,child) . ,input))
                     '(error "expected second sibling"))))
       '(error "expected first sibling")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to HTML

(defun make-tag (name &optional content) 
  (concat "<" name ">\n" (if content content "") "</" name ">\n"))

(defun transform (ast)
  (cond 
   ((eq (car ast) 'atom) (make-tag (cdr ast)))
   ((eq (car ast) 'parent-child)
    (let ((parent (cdadr ast))
          (children (transform (caddr ast))))
      (make-tag parent children)))
   ((eq (car ast) 'siblings)
    (let ((sib1 (transform (cadr ast)))
          (sib2 (transform (caddr ast))))
      (concat sib1 sib2)))))