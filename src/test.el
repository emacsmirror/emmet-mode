;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test-cases

(load-file (concat (file-name-directory load-file-name) "../zencoding-mode.el"))

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))
(defparameter *zencoding-test-cases* nil)

(defun zencoding-test-cases (&rest args)
  (let ((cmd (car args)))
    (flet
        ((run-cases
          (cases)
          (block outer
            (loop for c in cases
                  for i to (1- (length cases)) do
                  (let ((expected (mapconcat 'identity (cdr c) "\n"))
                        (actual (zencoding-transform (car (zencoding-expr (car c))))))
                    (when (not (equal expected actual))
                      (princ
                       (concat "*** [FAIL] | \"" name "\" " (number-to-string i) "\n\n"
                               (car c) "\t=>\n\n"
                               "Expected\n" expected "\n\nActual\n" actual "\n\n"))
                      (return-from outer 'fail)))))))
    (cond ((eql cmd 'assign)
           (let ((name (cadr args))
                 (defs (caddr args)))
             (let ((place (assoc name *zencoding-test-cases*)))
               (if place
                   (setf (cdr place) defs)
                 (setq *zencoding-test-cases*
                       (cons (cons name defs) *zencoding-test-cases*))))))
          (t
           (loop for test in (reverse *zencoding-test-cases*) do
                 (let ((name (symbol-name (car test)))
                       (cases (cdr test)))
                   (let ((res (run-cases cases)))
                     (if (not (eql res 'fail))
                         (princ (concat "    [PASS] | \"" name "\" "
                                        (number-to-string (length cases)) " tests.\n")))))))))))

(defmacro define-zencoding-test-case (name &rest tests)
  `(zencoding-test-cases 'assign ',name
                         ',(loop for x on tests by #'cddr collect (cons (car x) (cadr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML-abbrev tests

(define-zencoding-test-case Tags
  "a"                      ("<a></a>")
  "a.x"                    ("<a class=\"x\"></a>")
  "a#q.x"                  ("<a id=\"q\" class=\"x\"></a>")
  "a#q.x.y.z"              ("<a id=\"q\" class=\"x y z\"></a>")
  "#q"                     ("<div id=\"q\">"
                            "</div>")
  ".x"                     ("<div class=\"x\">"
                            "</div>")
  "#q.x"                   ("<div id=\"q\" class=\"x\">"
                            "</div>")
  "#q.x.y.z"               ("<div id=\"q\" class=\"x y z\">"
                            "</div>"))

(define-zencoding-test-case Empty-tags
  "a/"                     ("<a/>")
  "a/.x"                   ("<a class=\"x\"/>")
  "a/#q.x"                 ("<a id=\"q\" class=\"x\"/>")
  "a/#q.x.y.z"             ("<a id=\"q\" class=\"x y z\"/>"))

(define-zencoding-test-case Self-closing-tags
  "input type=text"        ("<input type=\"text\"/>")
  "img"                    ("<img/>")
  "img>metadata/*2"        ("<img>"
                            "    <metadata/>"
                            "    <metadata/>"
                            "</img>"))

(define-zencoding-test-case Siblings
  "a+b"                    ("<a></a>"
                            "<b></b>")
  "a+b+c"                  ("<a></a>"
                            "<b></b>"
                            "<c></c>")
  "a.x+b"                  ("<a class=\"x\"></a>"
                            "<b></b>")
  "a#q.x+b"                ("<a id=\"q\" class=\"x\"></a>"
                            "<b></b>")
  "a#q.x.y.z+b"            ("<a id=\"q\" class=\"x y z\"></a>"
                            "<b></b>")
  "a#q.x.y.z+b#p.l.m.n"    ("<a id=\"q\" class=\"x y z\"></a>"
                            "<b id=\"p\" class=\"l m n\"></b>"))

(define-zencoding-test-case Tag-expansion
  "table+"                 ("<table>"
                            "    <tr>"
                            "        <td>"
                            "        </td>"
                            "    </tr>"
                            "</table>")
  "dl+"                    ("<dl>"
                            "    <dt></dt>"
                            "    <dd></dd>"
                            "</dl>")
  "ul+"                    ("<ul>"
                            "    <li></li>"
                            "</ul>")
  "ul++ol+"                ("<ul>"
                            "    <li></li>"
                            "</ul>"
                            "<ol>"
                            "    <li></li>"
                            "</ol>")
  "ul#q.x.y m=l+"          ("<ul id=\"q\" class=\"x y\" m=\"l\">"
                            "    <li></li>"
                            "</ul>"))

(define-zencoding-test-case Parent-child
  "a>b"                    ("<a><b></b></a>")
  "a>b>c"                  ("<a><b><c></c></b></a>")
  "a.x>b"                  ("<a class=\"x\"><b></b></a>")
  "a#q.x>b"                ("<a id=\"q\" class=\"x\"><b></b></a>")
  "a#q.x.y.z>b"            ("<a id=\"q\" class=\"x y z\"><b></b></a>")
  "a#q.x.y.z>b#p.l.m.n"    ("<a id=\"q\" class=\"x y z\"><b id=\"p\" class=\"l m n\"></b></a>")
  "#q>.x"                  ("<div id=\"q\">"
                            "    <div class=\"x\">"
                            "    </div>"
                            "</div>")
  "a>b+c"                  ("<a>"
                            "    <b></b>"
                            "    <c></c>"
                            "</a>")
  "a>b+c>d"                ("<a>"
                            "    <b></b>"
                            "    <c><d></d></c>"
                            "</a>"))

(define-zencoding-test-case Multiplication
  "a*1"                    ("<a></a>")
  "a*2"                    ("<a></a>"
                            "<a></a>")
  "a/*2"                   ("<a/>"
                            "<a/>")
  "a*2+b*2"                ("<a></a>"
                            "<a></a>"
                            "<b></b>"
                            "<b></b>")
  "a*2>b*2"                ("<a>"
                            "    <b></b>"
                            "    <b></b>"
                            "</a>"
                            "<a>"
                            "    <b></b>"
                            "    <b></b>"
                            "</a>")
  "a>b*2"                  ("<a>"
                            "    <b></b>"
                            "    <b></b>"
                            "</a>")
  "a#q.x>b#q.x*2"          ("<a id=\"q\" class=\"x\">"
                            "    <b id=\"q\" class=\"x\"></b>"
                            "    <b id=\"q\" class=\"x\"></b>"
                            "</a>")
  "a#q.x>b/#q.x*2"         ("<a id=\"q\" class=\"x\">"
                            "    <b id=\"q\" class=\"x\"/>"
                            "    <b id=\"q\" class=\"x\"/>"
                            "</a>"))

(define-zencoding-test-case Numbering
  "a.$x*3"                 ("<a class=\"1x\"></a>"
                            "<a class=\"2x\"></a>"
                            "<a class=\"3x\"></a>")
  "ul>li.item$*3"          ("<ul>"
                            "    <li class=\"item1\"></li>"
                            "    <li class=\"item2\"></li>"
                            "    <li class=\"item3\"></li>"
                            "</ul>")
  "ul>li.item$$$*3"        ("<ul>"
                            "    <li class=\"item001\"></li>"
                            "    <li class=\"item002\"></li>"
                            "    <li class=\"item003\"></li>"
                            "</ul>")
  "ul>li.item$@-*2"        ("<ul>"
                            "    <li class=\"item2\"></li>"
                            "    <li class=\"item1\"></li>"
                            "</ul>")
  "ul>li.item$@-1000*2"    ("<ul>"
                            "    <li class=\"item1001\"></li>"
                            "    <li class=\"item1000\"></li>"
                            "</ul>")
  "a.$*2>b.$$@-*3"         ("<a class=\"1\">"
                            "    <b class=\"03\"></b>"
                            "    <b class=\"02\"></b>"
                            "    <b class=\"01\"></b>"
                            "</a>"
                            "<a class=\"2\">"
                            "    <b class=\"03\"></b>"
                            "    <b class=\"02\"></b>"
                            "    <b class=\"01\"></b>"
                            "</a>")

  "(div>(a#id$$*2)+b.c$@-3+c#d$)*2"
  ("<div>"
   "    <a id=\"id01\"></a>"
   "    <a id=\"id02\"></a>"
   "    <b class=\"c4\"></b>"
   "    <c id=\"d1\"></c>"
   "</div>"
   "<div>"
   "    <a id=\"id01\"></a>"
   "    <a id=\"id02\"></a>"
   "    <b class=\"c3\"></b>"
   "    <c id=\"d2\"></c>"
   "</div>")

  "a:b$$$-c$$@-:d$@-3-e$$@100/#b.c$*3"
  ("<a:b001-c03:d5-e100 id=\"b\" class=\"c1\"/>"
   "<a:b002-c02:d4-e101 id=\"b\" class=\"c2\"/>"
   "<a:b003-c01:d3-e102 id=\"b\" class=\"c3\"/>")

  "ul>li.item${name: item$ price: $\\$}*3"
  ("<ul>"
   "    <li class=\"item1\">name: item1 price: 1$</li>"
   "    <li class=\"item2\">name: item2 price: 2$</li>"
   "    <li class=\"item3\">name: item3 price: 3$</li>"
   "</ul>"))

(define-zencoding-test-case Properties
  "a x"                    ("<a x=\"\"></a>")
  "a x="                   ("<a x=\"\"></a>")
  "a x=\"\""               ("<a x=\"\"></a>")
  "a x=y"                  ("<a x=\"y\"></a>")
  "a x=\"y\""              ("<a x=\"y\"></a>")
  "a x=\"()\""             ("<a x=\"()\"></a>")
  "a x m"                  ("<a x=\"\" m=\"\"></a>")
  "a x= m=\"\""            ("<a x=\"\" m=\"\"></a>")
  "a x=y m=l"              ("<a x=\"y\" m=\"l\"></a>")
  "a/ x=y m=l"             ("<a x=\"y\" m=\"l\"/>")
  "a#foo x=y m=l"          ("<a id=\"foo\" x=\"y\" m=\"l\"></a>")
  "a.foo x=y m=l"          ("<a class=\"foo\" x=\"y\" m=\"l\"></a>")
  "a#foo.bar.mu x=y m=l"   ("<a id=\"foo\" class=\"bar mu\" x=\"y\" m=\"l\"></a>")
  "a/#foo.bar.mu x=y m=l"  ("<a id=\"foo\" class=\"bar mu\" x=\"y\" m=\"l\"/>")
  "a x=y+b"                ("<a x=\"y\"></a>"
                            "<b></b>")
  "a x=y+b x=y"            ("<a x=\"y\"></a>"
                            "<b x=\"y\"></b>")
  "a x=y>b"                ("<a x=\"y\"><b></b></a>")
  "a x=y>b x=y"            ("<a x=\"y\"><b x=\"y\"></b></a>")
  "a x=y>b x=y+c x=y"      ("<a x=\"y\">"
                            "    <b x=\"y\"></b>"
                            "    <c x=\"y\"></c>"
                            "</a>"))

(define-zencoding-test-case Parentheses
  "(a)"                    ("<a></a>")
  "(a)+(b)"                ("<a></a>"
                            "<b></b>")
  "a>(b)"                  ("<a><b></b></a>")
  "(a>b)>c"                ("<a><b></b></a>")
  "(a>b)+c"                ("<a><b></b></a>"
                            "<c></c>")
  "z+(a>b)+c+k"            ("<z></z>"
                            "<a><b></b></a>"
                            "<c></c>"
                            "<k></k>")
  "(a)*2"                  ("<a></a>"
                            "<a></a>")
  "((a)*2)"                ("<a></a>"
                            "<a></a>")
  "((a))*2"                ("<a></a>"
                            "<a></a>")
  "(a>b)*2"                ("<a><b></b></a>"
                            "<a><b></b></a>")
  "(a+b)*2"                ("<a></a>"
                            "<b></b>"
                            "<a></a>"
                            "<b></b>"))

(define-zencoding-test-case Text
  "a{Click me}"            ("<a>Click me</a>")
  "a>{Click me}*3"         ("<a>"
                            "    Click me"
                            "    Click me"
                            "    Click me"
                            "</a>")
  "a{click}+b{here}"       ("<a>click</a>"
                            "<b>here</b>")
  "a>{click}+b{here}"      ("<a>"
                            "    click"
                            "    <b>here</b>"
                            "</a>")

  "p>{Click }+a{here}+{ to continue}"
  ("<p>"
   "    Click "
   "    <a>here</a>"
   "     to continue"
   "</p>")

  "p{Click }+a{here}+{ to continue}"
  ("<p>"
   "    Click "
   "</p>"
   "<a>here</a>"
   " to continue"))

(define-zencoding-test-case Climb-up
  "a>b>c^d"                ("<a>"
                            "    <b><c></c></b>"
                            "    <d></d>"
                            "</a>")
  "a>b>c^^d"               ("<a><b><c></c></b></a>"
                            "<d></d>")
  "a*2>b*2>c^d"            ("<a>"
                            "    <b><c></c></b>"
                            "    <b><c></c></b>"
                            "    <d></d>"
                            "</a>"
                            "<a>"
                            "    <b><c></c></b>"
                            "    <b><c></c></b>"
                            "    <d></d>"
                            "</a>")

  "div+a>p>span{foo}+em>b^^^p"
  ("<div>"
   "</div>"
   "<a>"
   "    <p>"
   "        <span>foo</span>"
   "        <em><b></b></em>"
   "    </p>"
   "</a>"
   "<p>"
   "</p>")

  "div+div>p>span+em^blockquote{foo}"
  ("<div>"
   "</div>"
   "<div>"
   "    <p>"
   "        <span></span>"
   "        <em></em>"
   "    </p>"
   "    <blockquote>"
   "        foo"
   "    </blockquote>"
   "</div>"))

(define-zencoding-test-case Filter-comment
  "a.b|c"                  ("<!-- .b -->"
                            "<a class=\"b\"></a>"
                            "<!-- /.b -->")
  "#a>.b|c"                ("<!-- #a -->"
                            "<div id=\"a\">"
                            "    <!-- .b -->"
                            "    <div class=\"b\">"
                            "    </div>"
                            "    <!-- /.b -->"
                            "</div>"
                            "<!-- /#a -->"))

(define-zencoding-test-case Filter-HAML
  "a|haml"                 ("%a")
  "a#q.x.y.z|haml"         ("%a#q.x.y.z")
  "a#q.x x=y m=l|haml"     ("%a#q.x{:x => \"y\", :m => \"l\"}")
  "div|haml"               ("%div")
  "div.footer|haml"        (".footer")
  ".footer|haml"           (".footer")

  "p>{This is haml}*2+a href=#+br|haml"
  ("%p"
   "    This is haml"
   "    This is haml"
   "    %a{:href => \"#\"}"
   "    %br"))

(define-zencoding-test-case Filter-Hiccup
  "a|hic"                  ("[:a]")
  "a#q.x.y.z|hic"          ("[:a#q.x.y.z]")
  "a#q.x x=y m=l|hic"      ("[:a#q.x {:x \"y\", :m \"l\"}]")
  ".footer|hic"            ("[:div.footer]")
  "p>a href=#+br|hic"      ("[:p"
                            "    [:a {:href \"#\"}]"
                            "    [:br]]")

  "#q>(a*2>b{x})+p>{m}+b|hic"
  ("[:div#q"
   "    [:a [:b \"x\"]]"
   "    [:a [:b \"x\"]]"
   "    [:p"
   "        \"m\""
   "        [:b]]]"))

(define-zencoding-test-case Filter-escape
  "script src=&quot;|e"    ("&lt;script src=\"&amp;quot;\"&gt;"
                            "&lt;/script&gt;"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS-abbrev tests


;; start
(zencoding-test-cases)