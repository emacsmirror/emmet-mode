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
  (run siblings
       it
       (run parent-child
            it
            (run pexpr
                 it
                 (run tag
                      it
                      '(error "no match, expecting ( or a-zA-Z0-9"))))))

(defun tag (input)
  "Parse a tag."
  (run tagname
       (let ((result it) 
             (tagname (cdr expr)))
         (run identifier
              (tag-classes `(tag ,tagname ((id ,(cddr expr)))) input)
              (tag-classes `(tag ,tagname ()) input)))
       '(error "expected tagname")))

(defun tag-classes (tag input)
  (run classes
       (let ((tagname (cadr tag)) 
             (props (caddr tag))
             (classes `(class ,(mapconcat 
                                (lambda (prop)
                                  (cdadr prop))
                                (cdr expr)
                                " "))))
         `((tag ,tagname ,(append props (list classes))) . ,input))
       `(,tag . ,input)))

(defun tagname (input)
  "Parse a tagname a-zA-Z0-9 tagname (e.g. html/head/xsl:if/br)."
  (parse "\\([a-zA-Z0-9:-]+\\)" 2 "tagname, a-zA-Z0-9"
         `((tagname . ,(elt it 1)) . ,input)))

(defun pexpr (input)
  "A zen coding expression with parentheses around it."
  (parse "(" 1 "("
         (run expr
              (aif (regex ")" input '(0 1))
                   `(,expr . ,(elt it 1))
                   '(error "expecting `)'")))))

(defun parent-child (input)
  "Parse an tag>e expression, where `n' is an tag and `e' is any 
   expression."
  (run tag
       (let ((parent expr))
         (parse ">" 1 ">"
                (run expr
                     (let ((child expr))
                       `((parent-child ,parent ,child) . ,input))
                     '(error "expected child"))))
       '(error "expected parent")))

(defun sibling (input)
  (por pexpr tag
       it
       '(error "expected sibling")))

(defun siblings (input)
  "Parse an e+e expression, where e is an tag or a pexpr."
  (por pexpr tag
       (let ((parent expr))
         (parse "\\+" 1 "+"
                (por siblings sibling
                     (let ((child expr))
                       `((siblings ,parent ,child) . ,input))
                     '(error "expected second sibling"))))
       '(error "expected first sibling")))

(defun name (input)
  "Parse a class or identifier name, e.g. news, footer, mainimage"
  (parse "\\([a-zA-Z][a-zA-Z0-9-_]*\\)" 2 "class or identifer name"
         `((name . ,(elt it 1)) . ,input)))

(defun class (input)
  "Parse a classname expression, e.g. .foo"
  (parse "\\." 1 "."
         (run name 
              `((class ,expr) . ,input)
              '(error "expected class name"))))

(defun identifier (input)
  "Parse an identifier expression, e.g. #foo"
  (parse "#" 1 "#"
         (run name `((identifier . ,expr) . ,input))))

(defun classes (input)
  "Parse many classes."
  (run class
       (pif (classes input)
            `((classes . ,(cons expr (cdar it))) . ,(cdr it))
            `((classes . ,(list expr)) . ,input))
       '(error "expected class")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to HTML

(defun make-tag (tag &optional content) 
  (let ((name (car tag))
        (props (apply 'concat (mapcar
                        (lambda (prop)
                          (concat " " (symbol-name (car prop))
                                  "=\"" (cadr prop) "\""))
                        (cadr tag)))))
    (concat "<" name props ">\n" 
            (if content content "")
            "</" name ">\n")))

(defun transform (ast)
  (let ((type (car ast)))
    (cond
     ((eq type 'tag)
      (make-tag (cdr ast)))
     ((eq type 'tagname) (make-tag (cdr ast)))
     ((eq type 'parent-child)
      (let ((parent (cdadr ast))
            (children (transform (caddr ast))))
        (make-tag parent children)))
     ((eq type 'siblings)
      (let ((sib1 (transform (cadr ast)))
            (sib2 (transform (caddr ast))))
        (concat sib1 sib2))))))

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

;; (transform (car (expr "body.sub-page>div#news.content.a+div#news.content.a")))
;; => [indentation added with xml-mode]
;; <body class="sub-page">
;;   <div id="news" class="content a">
;;   </div>
;;   <div id="news" class="content a">
;;   </div>
;; </body>
