;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML abbrev

(zencoding-defparameter
 zencoding-tag-aliases-table
 (gethash "aliases" (gethash "html" zencoding-snippets)))

(defun zencoding-expr (input)
  "Parse a zen coding expression with optional filters."
  (zencoding-pif (zencoding-parse "\\(.*?\\)|" 2 "expr|filter" it)
                 (let ((input (elt it 1))
                       (filters (elt it 2)))
                   (zencoding-pif (zencoding-extract-filters filters)
                                  (zencoding-filter input it)
                                  it))
                 (zencoding-filter input (zencoding-default-filter))))

(defun zencoding-subexpr (input)
  "Parse a zen coding expression with no filter. This pretty much defines precedence."
  (zencoding-run zencoding-siblings
                 it
                 (zencoding-run zencoding-parent-child
                                it
                                (zencoding-run zencoding-multiplier
                                               it
                                               (zencoding-run zencoding-pexpr
                                                              it
                                                              (zencoding-run zencoding-text
                                                                             it
                                                                             (zencoding-run zencoding-tag
                                                                                            it
                                                                                            '(error "no match, expecting ( or a-zA-Z0-9"))))))))

(defun zencoding-extract-filters (input)
  "Extract filters from expression."
  (zencoding-pif (zencoding-parse "\\([^\\|]+?\\)|" 2 "" it)
                 (let ((filter-name (elt it 1))
                       (more-filters (elt it 2)))
                   (zencoding-pif (zencoding-extract-filters more-filters)
                                  (cons filter-name it)
                                  it))
                 (zencoding-parse "\\([^\\|]+\\)" 1 "filter name" `(,(elt it 1)))))

(defun zencoding-filter (input filters)
  "Construct AST with specified filters."
  (zencoding-pif (zencoding-subexpr input)
                 (let ((result (car it))
                       (rest (cdr it)))
                   `((filter ,filters ,result) . ,rest))
                 it))

(defun zencoding-default-filter ()
  "Default filter(s) to be used if none is specified."
  (let* ((file-ext (car (zencoding-regex ".*\\(\\..*\\)" (or (buffer-file-name) "") 1)))
         (defaults '(".html" ("html")
                     ".htm"  ("html")
                     ".haml" ("haml")
                     ".clj"  ("hic")))
         (default-else      '("html"))
         (selected-default (member file-ext defaults)))
    (if selected-default
        (cadr selected-default)
      default-else)))

(defun zencoding-numbering (input)
  (zencoding-parse
   "\\(\\$+\\)" 2 "numbering, $"
   (let ((doller (elt it 1)))
     (zencoding-pif (zencoding-parse
                     "@\\([0-9-][0-9]*\\)" 2 "numbering args"
                     (let* ((args (read (elt it 1)))
                            (direction  (not (or (eq '- args) (minusp args))))
                            (base       (if (eq '- args) 1 (abs args))))
                       `((n ,(length doller) ,direction ,base) . ,input)))
                    it
                    `((n ,(length doller) t 1) . ,input)))))

(defun zencoding-split-numbering-expressions (input)
  (labels
      ((iter (input)
             (zencoding-aif (zencoding-regex "\\([^$]*\\)\\(\\$.*\\)" input '(1 2))
                (let ((prefix (car it))
                      (input (cadr it)))
                  (if (and (< 0 (length prefix)) ; check if ..\\$... or ...$...
                           (string-equal (substring prefix -1) "\\"))
                      `(,(store-substring prefix (- (length prefix) 1) ?$)
                        ,@(iter (substring input 1)))
                    (let ((res (zencoding-numbering input)))
                      `(,prefix ,(car res) ,@(iter (cdr res))))))
                (list input))))
    (let ((res (iter input)))
      (if (every #'stringp res)
          (apply #'concat res)
        `(numberings ,@res)))))

(defun zencoding-instantiate-numbering-expression (i lim exp)
  (labels ((instantiate
            (i lim exps)
            (apply #'concat
                   (mapcar
                    (lambda (exp)
                      (if (listp exp)
                          (let ((digits (second exp))
                                (direction (third exp))
                                (base (fourth exp)))
                            (let ((num (if direction (+ base i)
                                         (- (+ lim (- base 1)) i))))
                              (format (concat "%0" (format "%d" digits) "d") num)))
                        exp)) exps)))
           (search
            (i lim exp)
            (if (listp exp)
                (if (eql (car exp) 'numberings)
                    (instantiate i lim (cdr exp))
                  ;; Should do like this for real searching.
                  ;; But stack overflow occurs.
                  ;; (cons (search-numberings i lim (car exp))
                  ;;       (search-numberings i lim (cdr exp)))
                  (mapcar (lambda (exp) (search i lim exp)) exp))
              exp)))
    (search i lim exp)))

(defun zencoding-multiply-expression (multiplicand exp)
  (loop for i to (- multiplicand 1) collect
        (zencoding-instantiate-numbering-expression i multiplicand exp)))

(defun zencoding-multiplier (input)
  (zencoding-pif (zencoding-run zencoding-pexpr
                                it
                                (zencoding-run zencoding-tag
                                               it
                                               (zencoding-run zencoding-text
                                                              it
                                                              '(error "expected *n multiplier"))))
                 (let* ((expr (car it)) (input (cdr it))
                        (multiplier expr))
                   (zencoding-parse "\\*\\([0-9]+\\)" 2 "*n where n is a number"
                                    (let ((multiplicand (read (elt it 1))))
                                      `((list ,(zencoding-multiply-expression
                                                multiplicand
                                                multiplier)) . ,input))))))

(defun zencoding-tag (input)
  "Parse a tag."
  (zencoding-run
   zencoding-tagname
   (let ((tagname (cadr expr))
         (has-body? (cddr expr)))
     (zencoding-pif
      (zencoding-run zencoding-identifier
                     (zencoding-tag-classes
                      `(tag (,tagname ,has-body? ,(cddr expr))) input)
                     (zencoding-tag-classes
                      `(tag (,tagname ,has-body? nil)) input))
      (let ((tag-data (cadar it)) (input (cdr it)))
        (zencoding-pif (zencoding-run
                        zencoding-props
                        (let ((props (cdr expr)))
                          `((tag ,(append tag-data (list props))) . ,input))
                        `((tag ,(append tag-data '(nil))) . ,input))
                       (let ((expr (car it)) (input (cdr it)))
                         (destructuring-bind (expr . input)
                             (zencoding-tag-text expr input)
                           (zencoding-expand-tag-alias expr input)))))))
   (zencoding-default-tag input)))

(defun zencoding-get-first-tag (expr)
  (if (listp expr)
      (if (listp (car expr))
          (zencoding-get-first-tag (car expr))
        (if (eql (car expr) 'tag)
            expr
          (zencoding-get-first-tag (cdr expr))))
    nil))

(defun zencoding-expand-tag-alias (tag input)
  (let ((tag-data (cadr tag)))
    (let ((tag-name (car tag-data)))
      (zencoding-aif
       (gethash tag-name zencoding-tag-aliases-table)
       (let ((expr (if (stringp it)
                       (zencoding-subexpr it)
                     it)))
         (prog1
             (let ((rt (copy-tree expr)))
               (let ((first-tag-data (cadr (zencoding-get-first-tag rt))))
                 (setf (second first-tag-data) (second tag-data))
                 (setf (third first-tag-data)  (third tag-data))
                 (setf (fourth first-tag-data)
                       (remove-duplicates
                        (append (fourth first-tag-data)
                                (fourth tag-data)) :test #'string=))
                 (setf (fifth first-tag-data)
                       (remove-duplicates
                        (append (fifth first-tag-data)
                                (fifth tag-data))
                        :test #'(lambda (p1 p2)
                                  (eql (car p1) (car p2)))))
                 (setf (sixth first-tag-data) (sixth tag-data))
                 (setf (cdr rt) (concat (cdr rt) input))
                 rt))
           (puthash tag-name expr zencoding-tag-aliases-table)))
       `(,tag . ,input)))))

(defun zencoding-default-tag (input)
  "Parse a #id or .class"
  (zencoding-parse "\\([#|\\.]\\)" 1 "tagname"
                   (zencoding-tag (concat "div" (elt it 0)))))

(defun zencoding-tag-text (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-text
                   (let ((txt (cadr expr)))
                     `((tag ,(append tag-data (list txt))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-tag-props (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-props
                   (let ((props (cdr expr)))
                     `((tag ,(append tag-data (list props))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-props (input)
  "Parse many props."
    (zencoding-run zencoding-prop
                   (zencoding-pif (zencoding-props input)
                                  `((props . ,(cons expr (cdar it))) . ,(cdr it))
                                  `((props . ,(list expr)) . ,input))))

(defun zencoding-prop (input)
  (zencoding-parse
   " " 1 "space"
   (zencoding-run
    zencoding-name
    (let ((name (cdr expr)))
      (zencoding-pif (zencoding-prop-value name input)
                     it
                     `((,(read name) "") . ,input))))))

(defun zencoding-prop-value (name input)
  (zencoding-pif (zencoding-parse "=\"\\(.*?\\)\"" 2
                                  "=\"property value\""
                                  (let ((value (elt it 1))
                                        (input (elt it 2)))
                                    `((,(read name) ,value) . ,input)))
                 it
                 (zencoding-parse "=\\([^\\,\\+\\>\\{\\}\\ )]*\\)" 2
                                  "=property value"
                                  (let ((value (elt it 1))
                                        (input (elt it 2)))
                                    `((,(read name) ,value) . ,input)))))

(defun zencoding-tag-classes (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-classes
                   (let ((classes (mapcar (lambda (cls) (cdadr cls))
                                          (cdr expr))))
                     `((tag ,(append tag-data (list classes))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-tagname (input)
  "Parse a tagname a-zA-Z0-9 tagname (e.g. html/head/xsl:if/br)."
  (zencoding-parse "\\([a-zA-Z!][a-zA-Z0-9:!$@-]*\/?\\)" 2 "tagname, a-zA-Z0-9"
                   (let* ((tag-spec (elt it 1))
                          (empty-tag (zencoding-regex "\\([^\/]*\\)\/" tag-spec 1))
                          (tag (zencoding-split-numbering-expressions
                                (if empty-tag (car empty-tag) tag-spec))))
                     `((tagname . (,tag . ,(not empty-tag))) . ,input))))

(defun zencoding-text (input)
  "A zen coding expression innertext."
  (zencoding-parse "{\\(.*?\\)}" 2 "inner text"
                   (let ((txt (zencoding-split-numbering-expressions (elt it 1))))
                     `((text ,txt) . ,input))))

(defun zencoding-pexpr (input)
  "A zen coding expression with parentheses around it."
  (zencoding-parse "(" 1 "("
                   (zencoding-run zencoding-subexpr
                                  (zencoding-aif (zencoding-regex ")" input '(0 1))
                                                 `(,expr . ,(elt it 1))
                                                 '(error "expecting `)'")))))

(defun zencoding-parent-child (input)
  "Parse an tag>e expression, where `n' is an tag and `e' is any
   expression."
  (defun listing (parents child input)
    (let ((len (length parents)))
      `((list ,(map 'list
                    (lambda (parent i)
                      `(parent-child ,parent
                                     ,(zencoding-instantiate-numbering-expression i len child)))
                    parents
                    (loop for i to (- len 1) collect i))) . ,input)))
  (zencoding-run zencoding-multiplier
                 (let* ((items (cadr expr))
                        (rest (zencoding-child-sans expr input)))
                   (if (not (eq (car rest) 'error))
                       (let ((child (car rest))
                             (input (cdr rest)))

                         (zencoding-aif (zencoding-regex "^" input '(0 1))
                                                   (let ((input (elt it 1)))
                                                     (zencoding-run zencoding-subexpr
                                                                    `((sibling ,(car (listing items child "")) ,expr) . ,input)
                                                                    (listing items child input)))
                                                   (listing items child input)))
                     '(error "expected child")))
                 (zencoding-run zencoding-tag
                                (zencoding-child expr input)
                                '(error "expected parent"))))

(defun zencoding-child-sans (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-subexpr
                                  it
                                  '(error "expected child"))))

(defun zencoding-child (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-subexpr
                                  (let ((child expr))
                                    (zencoding-aif (zencoding-regex "^" input '(0 1))
                                                   (let ((input (elt it 1)))
                                                     (zencoding-run zencoding-subexpr
                                                                    `((sibling (parent-child ,parent ,child) ,expr) . ,input)
                                                                    `((parent-child ,parent ,child) . ,input)))
                                                   `((parent-child ,parent ,child) . ,input)))
                                  '(error "expected child"))))

(defun zencoding-sibling (input)
  (zencoding-por zencoding-pexpr zencoding-multiplier
                 it
                 (zencoding-run zencoding-tag
                                it
                                (zencoding-run zencoding-text
                                               it
                                               '(error "expected sibling")))))

(defun zencoding-siblings (input)
  "Parse an e+e expression, where e is an tag or a pexpr."
  (zencoding-run zencoding-sibling
                 (let ((parent expr))
                   (zencoding-parse
                    "\\+" 1 "+"
                    (zencoding-run
                     zencoding-subexpr
                     (let ((child expr))
                       `((sibling ,parent ,child) . ,input))
                     (zencoding-expand parent input))))
                 '(error "expected first sibling")))

(defun zencoding-expand (parent input)
  "Parse an e+ expression, where e is an expandable tag"
  (let* ((parent-tag (car (cadr parent))))
    (setf (caadr parent) (concat parent-tag "+"))
    (destructuring-bind (parent . input)
        (zencoding-expand-tag-alias parent input)
      (zencoding-pif (zencoding-parse "+\\(.*\\)" 1 "+expr"
                                      (zencoding-subexpr (elt it 1)))
                     `((sibling ,parent ,@it))
                     `(,parent . ,input)))))

(defun zencoding-name (input)
  "Parse a class or identifier name, e.g. news, footer, mainimage"
  (zencoding-parse "\\([a-zA-Z$@][a-zA-Z0-9$@_:-]*\\)" 2 "class or identifer name"
                   `((name . ,(zencoding-split-numbering-expressions
                               (elt it 1))) . ,input)))

(defun zencoding-class (input)
  "Parse a classname expression, e.g. .foo"
  (zencoding-parse "\\." 1 "."
                   (zencoding-run zencoding-name
                                  `((class ,expr) . ,input)
                                  '(error "expected class name"))))
(defun zencoding-identifier (input)
  "Parse an identifier expression, e.g. #foo"
  (zencoding-parse "#" 1 "#"
                   (zencoding-run zencoding-name
                                  `((identifier . ,expr) . ,input))))

(defun zencoding-classes (input)
  "Parse many classes."
  (zencoding-run zencoding-class
                 (zencoding-pif (zencoding-classes input)
                                `((classes . ,(cons expr (cdar it))) . ,(cdr it))
                                `((classes . ,(list expr)) . ,input))
                 '(error "expected class")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to string

(defvar zencoding-leaf-function nil
  "Function to execute when expanding a leaf node in the
  Zencoding AST.")

(zencoding-defparameter
 zencoding-tag-settings-table
 (gethash "tags" (gethash "html" zencoding-preferences)))

(zencoding-defparameter
 zencoding-tag-snippets-table
 (gethash "snippets" (gethash "html" zencoding-snippets)))

(defvar zencoding-filters
  '("html" (zencoding-primary-filter zencoding-make-html-tag)
    "c"    (zencoding-primary-filter zencoding-make-commented-html-tag)
    "haml" (zencoding-primary-filter zencoding-make-haml-tag)
    "hic"  (zencoding-primary-filter zencoding-make-hiccup-tag)
    "e"    (zencoding-escape-xml)))

(defun zencoding-primary-filter (input proc)
  "Process filter that needs to be executed first, ie. not given output from other filter."
  (if (listp input)
      (let ((tag-maker (cadr proc)))
        (zencoding-transform-ast input tag-maker))
    nil))

(defun zencoding-process-filter (filters input)
  "Process filters, chain one filter output as the input of the next filter."
  (let ((filter-data (member (car filters) zencoding-filters))
        (more-filters (cdr filters)))
    (if filter-data
        (let* ((proc   (cadr filter-data))
               (fun    (car proc))
               (filter-output (funcall fun input proc)))
          (if more-filters
              (zencoding-process-filter more-filters filter-output)
            filter-output))
      nil)))

(defun zencoding-make-tag (tag-maker tag-info &optional content)
  "Extract tag info and pass them to tag-maker."
  (let* ((name      (pop tag-info))
         (has-body? (pop tag-info))
         (id        (pop tag-info))
         (classes   (pop tag-info))
         (props     (pop tag-info))
         (txt       (pop tag-info))
         (settings  (gethash name zencoding-tag-settings-table)))
    (funcall tag-maker name has-body? id classes props txt settings
             (if content content
               (if zencoding-leaf-function (funcall zencoding-leaf-function))))))

(defun zencoding-hash-to-list (hash &optional proc)
  (unless proc (setq proc #'cons))
  (loop for key being the hash-keys of hash using (hash-values val)
        collect (funcall proc key val)))

(defun zencoding-merge-tag-props (default-table tag-props)
  (if default-table
      (let ((tbl (copy-hash-table default-table)))
        (loop for prop in tag-props do
              (puthash (symbol-name (car prop)) (cadr prop) tbl))
        (zencoding-hash-to-list tbl 'list))
    tag-props))

(defun zencoding-html-snippets-instantiate-lambda (src)
  (let ((lines (mapcar
                #'(lambda (src)
                    (if (string-match "^\\(.*\\)${child}\\(.*\\)$" src)
                        (mapcar (lambda (i)
                                  (match-string i src))
                                '(1 2))
                      (list src)))
                (split-string src "\n"))))
    (labels
        ((iter
          (l m a b)
          (if l
              (if (< 1 (length (car l)))
                  (iter (cdr l)
                        'b
                        (cons (caar l)  a)
                        (cons (cadar l) b))
                (if (eql m 'a)
                    (iter (cdr l) m (cons (caar l) a) b)
                  (iter (cdr l) m a (cons (caar l) b))))
            (if b
                `(lambda (contents)
                   (concat
                    ,(zencoding-join-string (reverse a) "\n")
                    contents
                    ,(zencoding-join-string (reverse b) "\n")))
              `(lambda (contents)
                 (concat
                  ,(zencoding-join-string (reverse a) "\n")
                  contents))))))
      (eval (iter lines 'a nil nil)))))

(defun zencoding-make-html-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create HTML markup string"
  (zencoding-aif
   (gethash tag-name zencoding-tag-snippets-table)

   (let ((fn (if (stringp it)
                 (zencoding-html-snippets-instantiate-lambda it)
               it)))
     (prog1
         (funcall fn content)
       (puthash tag-name fn zencoding-tag-snippets-table)))

   (let* ((id           (zencoding-concat-or-empty " id=\"" tag-id "\""))
          (classes      (zencoding-mapconcat-or-empty " class=\"" tag-classes " " "\""))
          (props        (let* ((tag-props-default
                                (and settings (gethash "defaultAttr" settings)))
                               (merged-tag-props
                                (zencoding-merge-tag-props
                                 tag-props-default
                                 tag-props)))
                          (zencoding-mapconcat-or-empty
                           " " merged-tag-props " " nil
                           (lambda (prop)
                             (let ((key (car prop)))
                               (concat (if (symbolp key) (symbol-name key) key)
                                       "=\"" (cadr prop) "\""))))))
          (content-multiline? (and content (string-match "\n" content)))
          (block-tag?         (and settings (gethash "block" settings)))
          (self-closing?      (and (not (or tag-txt content))
                                   (or (not tag-has-body?)
                                       (and settings (gethash "selfClosing" settings)))))
          (lf                 (if (or content-multiline? block-tag?) "\n")))
     (concat "<" tag-name id classes props
             (if self-closing? "/>"
               (concat ">"
                       (if tag-txt
                           (if (or content-multiline? block-tag?)
                               (zencoding-indent tag-txt)
                             tag-txt))
                       (if content
                           (if (or content-multiline? block-tag?)
                               (zencoding-indent content)
                             content))
                       lf
                       "</" tag-name ">"))))))

(defun zencoding-make-commented-html-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create HTML markup string with extra comments for elements with #id or .classes"
  (let ((body (zencoding-make-html-tag tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)))
    (if (or tag-id tag-classes)
        (let ((id      (zencoding-concat-or-empty "#" tag-id))
              (classes (zencoding-mapconcat-or-empty "." tag-classes ".")))
          (concat "<!-- " id classes " -->\n"
                  body
                  "\n<!-- /" id classes " -->"))
      body)))

(defun zencoding-make-haml-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create HAML string"
  (let ((name    (if (and (equal tag-name "div")
                          (or tag-id tag-classes))
                     ""
                   (concat "%" tag-name)))
        (id      (zencoding-concat-or-empty "#" tag-id))
        (classes (zencoding-mapconcat-or-empty "." tag-classes "."))
        (props   (zencoding-mapconcat-or-empty
                  "{" tag-props ", " "}"
                  (lambda (prop)
                    (concat ":" (symbol-name (car prop)) " => \"" (cadr prop) "\"")))))
    (concat name id classes props
            (if tag-txt
                (zencoding-indent tag-txt))
            (if content
                (zencoding-indent content)))))

(defun zencoding-make-hiccup-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create Hiccup string"
  (let* ((id      (zencoding-concat-or-empty "#" tag-id))
         (classes (zencoding-mapconcat-or-empty "." tag-classes "."))
         (props   (zencoding-mapconcat-or-empty
                   " {" tag-props ", " "}"
                   (lambda (prop)
                     (concat ":" (symbol-name (car prop)) " \"" (cadr prop) "\""))))
         (content-multiline? (and content (string-match "\n" content)))
         (block-tag? (and settings (gethash "block" settings))))
    (concat "[:" tag-name id classes props
            (if tag-txt
                (let ((tag-txt-quoted (concat "\"" tag-txt "\"")))
                  (if (or content-multiline? block-tag?)
                      (zencoding-indent tag-txt-quoted)
                    (concat " " tag-txt-quoted))))
            (if content
                (if (or content-multiline? block-tag?)
                    (zencoding-indent content)
                  (concat " " content)))
            "]")))

(defun zencoding-make-text (tag-maker text)
  (cond
   ((eq tag-maker 'zencoding-make-hiccup-tag)
    (concat "\"" text "\""))
   (t text)))

(defun zencoding-concat-or-empty (prefix body &optional suffix)
  "Return prefixed suffixed text or empty string."
  (if body
      (concat prefix body suffix)
    ""))

(defun zencoding-mapconcat-or-empty (prefix list-body delimiter &optional suffix map-fun)
  "Return prefixed suffixed mapconcated text or empty string."
  (if list-body
      (let* ((mapper (if map-fun map-fun 'identity))
             (body (mapconcat mapper list-body delimiter)))
        (concat prefix body suffix))
    ""))

(defun zencoding-escape-xml (input proc)
  "Escapes XML-unsafe characters: <, > and &."
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "&" "&amp;"
     (if (stringp input)
         input
       (zencoding-process-filter (zencoding-default-filter) input))))))

(defun zencoding-html-transform (input)
  (let ((ast (car (zencoding-expr input))))
    (when (not (eq ast 'error))
      (zencoding-transform-ast-with-filters ast))))

(defun zencoding-transform-ast-with-filters (ast-with-filters)
  "Transform AST (containing filter data) into string."
  (let ((filters (cadr ast-with-filters))
        (ast (caddr ast-with-filters)))
    (zencoding-process-filter filters ast)))

(defun zencoding-transform-ast (ast tag-maker)
  "Transform AST (without filter data) into string."
  (let ((type (car ast)))
    (cond
     ((eq type 'list)
      (mapconcat (lexical-let ((make-tag-fun tag-maker))
                   #'(lambda (sub-ast)
                       (zencoding-transform-ast sub-ast make-tag-fun)))
                 (cadr ast)
                 "\n"))
     ((eq type 'tag)
      (zencoding-make-tag tag-maker (cadr ast)))
     ((eq type 'text)
      (zencoding-make-text tag-maker (cadr ast)))
     ((eq type 'parent-child)
      (let ((parent (cadadr ast))
            (children (zencoding-transform-ast (caddr ast) tag-maker)))
        (zencoding-make-tag tag-maker parent children)))
     ((eq type 'sibling)
      (let ((sib1 (zencoding-transform-ast (cadr ast) tag-maker))
            (sib2 (zencoding-transform-ast (caddr ast) tag-maker)))
        (concat sib1 "\n" sib2))))))

(defun zencoding-indent (text)
  "Indent the text"
  (if text
      (replace-regexp-in-string "\n" "\n    " (concat "\n" text))
    nil))

