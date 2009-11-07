;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demos

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

;; (transform (car (expr "body.sub-page>div#news.content.a+div#news.content.a")))
;; => [indentation added with xml-mode]
;; <body class="sub-page">
;;   <div id="news" class="content a">
;;   </div>
;;   <div id="news" class="content a">
;;   </div>
;; </body>

;; (transform (car (expr "a#q.x>b#q.x*2")))
;; => <a id="q" class="x"><b id="q" class="x"></b><b id="q" class="x"></b></a>

;; See ``Test cases'' section for a complete set of expression types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(defmacro zencoding-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro zencoding-pif (test-form then-form &rest else-forms)
  "Parser anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@else-forms)))

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
                  ,@else-forms))

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
  (if (string-match (concat "^" regexp "\\(.*\\)$") string)
      (mapcar (lambda (ref) (match-string ref string)) 
              (if (sequencep refs) refs (list refs)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding parsers

(defun zencoding-expr (input)
  "Parse a zen coding expression. This pretty much defines precedence."
  (zencoding-run zencoding-siblings
                 it
                 (zencoding-run zencoding-parent-child
                                it
                                (zencoding-run zencoding-multiplier
                                               it
                                               (zencoding-run zencoding-pexpr
                                                              it
                                                              (zencoding-run zencoding-tag
                                                                             it
                                                                             '(error "no match, expecting ( or a-zA-Z0-9")))))))

(defun zencoding-multiplier (input)
  (zencoding-por zencoding-pexpr zencoding-tag
                 (let ((multiplier expr))
                   (zencoding-parse "\\*\\([0-9]+\\)" 2 "*n where n is a number"
                                    (let ((multiplicand (read (elt it 1))))
                                      `((list ,(make-list multiplicand multiplier)) . ,input))))
                 '(error "expected *n multiplier")))

(defun zencoding-tag (input)
  "Parse a tag."
  (zencoding-run zencoding-tagname
                 (let ((result it) 
                       (tagname (cdr expr)))
                   (zencoding-run zencoding-identifier
                                  (zencoding-tag-classes
                                   `(tag ,tagname ((id ,(cddr expr)))) input)
                                  (zencoding-tag-classes `(tag ,tagname ()) input)))
                 '(error "expected tagname")))
 
(defun zencoding-tag-classes (tag input)
  (zencoding-run zencoding-classes
                 (let ((tagname (cadr tag)) 
                       (props (caddr tag))
                       (classes `(class ,(mapconcat 
                                          (lambda (prop)
                                            (cdadr prop))
                                          (cdr expr)
                                          " "))))
                   `((tag ,tagname ,(append props (list classes))) . ,input))
                 `(,tag . ,input)))

(defun zencoding-tagname (input)
  "Parse a tagname a-zA-Z0-9 tagname (e.g. html/head/xsl:if/br)."
  (zencoding-parse "\\([a-zA-Z][a-zA-Z0-9:-]*\\)" 2 "tagname, a-zA-Z0-9"
                   `((tagname . ,(elt it 1)) . ,input)))

(defun zencoding-pexpr (input)
  "A zen coding expression with parentheses around it."
  (zencoding-parse "(" 1 "("
                   (zencoding-run zencoding-expr
                                  (zencoding-aif (zencoding-regex ")" input '(0 1))
                                                 `(,expr . ,(elt it 1))
                                                 '(error "expecting `)'")))))

(defun zencoding-parent-child (input)
  "Parse an tag>e expression, where `n' is an tag and `e' is any 
   expression."
  (zencoding-run zencoding-multiplier
                 (let* ((items (cadr expr))
                        (rest (zencoding-child-sans expr input)))
                   (if (not (eq (car rest) 'error))
                       (let ((child (car rest))
                             (input (cdr rest)))
                         (cons (cons 'list
                                     (cons (mapcar (lambda (parent)
                                                     `(parent-child ,parent ,child))
                                                   items)
                                           nil))
                               input))
                     '(error "expected child")))
                 (zencoding-run zencoding-tag
                                (zencoding-child expr input)
                                '(error "expected parent"))))

(defun zencoding-child-sans (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-expr
                                  it
                                  '(error "expected child"))))

(defun zencoding-child (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-expr
                                  (let ((child expr))
                                    `((parent-child ,parent ,child) . ,input))
                                  '(error "expected child"))))

(defun zencoding-sibling (input)
  (zencoding-por zencoding-pexpr zencoding-multiplier
                 it
                 (zencoding-run zencoding-tag
                                it
                                '(error "expected sibling"))))

(defun zencoding-siblings (input)
  "Parse an e+e expression, where e is an tag or a pexpr."
  (zencoding-run zencoding-sibling
                 (let ((parent expr))
                   (zencoding-parse "\\+" 1 "+"
                                    (zencoding-run zencoding-expr
                                                   (let ((child expr))
                                                     `((zencoding-siblings ,parent ,child) . ,input))
                                                   '(error "expected second sibling"))))
                 '(error "expected first sibling")))

(defun zencoding-name (input)
  "Parse a class or identifier name, e.g. news, footer, mainimage"
  (zencoding-parse "\\([a-zA-Z][a-zA-Z0-9-_]*\\)" 2 "class or identifer name"
                   `((name . ,(elt it 1)) . ,input)))

(defun zencoding-class (input)
  "Parse a classname expression, e.g. .foo"
  (zencoding-parse "\\." 1 "."
                   (zencoding-run zencoding-name 
                                  `((class ,expr) . ,input)
                                  '(error "expected class name"))))

(defun zencoding-identifier (input)
  "Parse an identifier expression, e.g. #foo"
  (zencoding-parse "#" 1 "#"
                   (zencoding-run zencoding-name `((identifier . ,expr) . ,input))))

(defun zencoding-classes (input)
  "Parse many classes."
  (zencoding-run zencoding-class
                 (zencoding-pif (zencoding-classes input)
                                `((classes . ,(cons expr (cdar it))) . ,(cdr it))
                                `((classes . ,(list expr)) . ,input))
                 '(error "expected class")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to HTML

(defun zencoding-make-tag (tag &optional content) 
  (let ((name (car tag))
        (props (apply 'concat (mapcar
                               (lambda (prop)
                                 (concat " " (symbol-name (car prop))
                                         "=\"" (cadr prop) "\""))
                               (cadr tag)))))
    (concat "<" name props ">" 
            (if content content "")
            "</" name ">")))

(defun zencoding-transform (ast)
  (let ((type (car ast)))
    (cond
     ((eq type 'list)
      (mapconcat 'zencoding-transform (cadr ast) ""))
     ((eq type 'tag)
      (zencoding-make-tag (cdr ast)))
     ((eq type 'parent-child)
      (let ((parent (cdadr ast))
            (children (zencoding-transform (caddr ast))))
        (zencoding-make-tag parent children)))
     ((eq type 'zencoding-siblings)
      (let ((sib1 (zencoding-transform (cadr ast)))
            (sib2 (zencoding-transform (caddr ast))))
        (concat sib1 sib2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test-cases
(defun zencoding-test-cases ()
  (let ((tests '(;; Tags
                 ("a"                      "<a></a>")
                 ("a.x"                    "<a class=\"x\"></a>")
                 ("a#q.x"                  "<a id=\"q\" class=\"x\"></a>")
                 ("a#q.x.y.z"              "<a id=\"q\" class=\"x y z\"></a>")
                 ;; Siblings
                 ("a+b"                    "<a></a><b></b>")
                 ("a+b+c"                  "<a></a><b></b><c></c>")
                 ("a.x+b"                  "<a class=\"x\"></a><b></b>")
                 ("a#q.x+b"                "<a id=\"q\" class=\"x\"></a><b></b>")
                 ("a#q.x.y.z+b"            "<a id=\"q\" class=\"x y z\"></a><b></b>")
                 ("a#q.x.y.z+b#p.l.m.n"    "<a id=\"q\" class=\"x y z\"></a><b id=\"p\" class=\"l m n\"></b>")
                 ;; Parent > child
                 ("a>b"                    "<a><b></b></a>")
                 ("a>b>c"                  "<a><b><c></c></b></a>")
                 ("a.x>b"                  "<a class=\"x\"><b></b></a>")
                 ("a#q.x>b"                "<a id=\"q\" class=\"x\"><b></b></a>")
                 ("a#q.x.y.z>b"            "<a id=\"q\" class=\"x y z\"><b></b></a>")
                 ("a#q.x.y.z>b#p.l.m.n"    "<a id=\"q\" class=\"x y z\"><b id=\"p\" class=\"l m n\"></b></a>")
                 ("a>b+c"                  "<a><b></b><c></c></a>")
                 ("a>b+c>d"                "<a><b></b><c><d></d></c></a>")
                 ;; Multiplication
                 ("a*1"                    "<a></a>")
                 ("a*2"                    "<a></a><a></a>")
                 ("a*2+b*2"                "<a></a><a></a><b></b><b></b>")
                 ("a*2>b*2"                "<a><b></b><b></b></a><a><b></b><b></b></a>")
                 ("a>b*2"                  "<a><b></b><b></b></a>")
                 ("a#q.x>b#q.x*2"          "<a id=\"q\" class=\"x\"><b id=\"q\" class=\"x\"></b><b id=\"q\" class=\"x\"></b></a>")
                 ;; ;; Parentheses
                 ("(a)"                    "<a></a>")
                 ("(a)+(b)"                "<a></a><b></b>")
                 ("a>(b)"                  "<a><b></b></a>")
                 ("(a>b)>c"                "<a><b></b></a>")
                 ("(a>b)+c"                "<a><b></b></a><c></c>")
                 ("z+(a>b)+c+k"            "<z></z><a><b></b></a><c></c><k></k>")
                 ("(a)*2"                  "<a></a><a></a>")
                 ("((a)*2)"                "<a></a><a></a>")
                 ("((a)*2)"                "<a></a><a></a>")
                 ("(a>b)*2"                "<a><b></b></a><a><b></b></a>")
                 ("(a+b)*2"                "<a></a><b></b><a></a><b></b>"))))
    (mapcar (lambda (input)
              (let ((expected (cadr input))
                    (actual (zencoding-transform (car (zencoding-expr (car input))))))
                (if (not (equal expected actual))
                    (error (concat "Assertion " (car input) " failed:"
                                   expected
                                   " == "
                                   actual)))))
            tests)
    (concat (number-to-string (length tests)) " tests performed. All OK.")))
