;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test-cases

(load-file (concat (file-name-directory load-file-name) "../zencoding-mode.el"))

(zencoding-defparameter *zencoding-test-cases* nil)

(defun zencoding-test-cases (&rest args)
  (let ((cmd (car args)))
    (flet
        ((run-cases
          (fn cases)
          (loop for c in cases
                for i to (1- (length cases)) do
                (let ((expected (cdr c))
                      (actual (funcall fn (car c))))
                  (when (not (equal expected actual))
                    (princ
                     (concat "*** [FAIL] | \"" name "\" " (number-to-string i) "\n\n"
                             (format "%s" (car c)) "\t=>\n\n"
                             "Expected\n" (format "%s" expected) "\n\nActual\n" (format "%s" actual) "\n\n"))
                    (return 'fail))))))
      (cond ((eql cmd 'assign)
             (let ((name (cadr args))
                   (fn   (caddr args))
                   (defs (cadddr args)))
               (let ((place (assoc name *zencoding-test-cases*)))
                 (if place
                     (setf (cdr place) (cons fn defs))
                   (setq *zencoding-test-cases*
                         (cons (cons name (cons fn defs)) *zencoding-test-cases*))))))
            (t
             (loop for test in (reverse *zencoding-test-cases*) do
                   (let ((name  (symbol-name (car test)))
                         (fn    (cadr test))
                         (cases (cddr test)))
                     (let ((res (run-cases fn cases)))
                       (if (not (eql res 'fail))
                           (princ (concat "    [PASS] | \"" name "\" "
                                          (number-to-string (length cases)) " tests.\n")))))))))))

(defmacro define-zencoding-transform-test-case (name fn &rest tests)
  `(zencoding-test-cases 'assign ',name
                         ,fn
                         ',(loop for x on tests by #'cddr collect
                                 (cons (car x)
                                       (zencoding-join-string (cadr x)
                                                              "\n")))))

(defmacro define-zencoding-transform-html-test-case (name &rest tests)
  `(define-zencoding-transform-test-case ,name
     'zencoding-html-transform
     ,@tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML-abbrev tests

(define-zencoding-transform-html-test-case Tags
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

(define-zencoding-transform-html-test-case Empty-tags
  "a/"                     ("<a/>")
  "a/.x"                   ("<a class=\"x\"/>")
  "a/#q.x"                 ("<a id=\"q\" class=\"x\"/>")
  "a/#q.x.y.z"             ("<a id=\"q\" class=\"x y z\"/>"))

(define-zencoding-transform-html-test-case Self-closing-tags
  "input type=text"        ("<input type=\"text\"/>")
  "img"                    ("<img/>")
  "img>metadata/*2"        ("<img>"
                            "    <metadata/>"
                            "    <metadata/>"
                            "</img>"))

(define-zencoding-transform-html-test-case Siblings
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

(define-zencoding-transform-html-test-case Tag-expansion
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

(define-zencoding-transform-html-test-case Parent-child
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

(define-zencoding-transform-html-test-case Multiplication
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

(define-zencoding-transform-html-test-case Numbering
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

(define-zencoding-transform-html-test-case Properties
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

(define-zencoding-transform-html-test-case Parentheses
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

(define-zencoding-transform-html-test-case Text
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

(define-zencoding-transform-html-test-case Climb-up
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

(define-zencoding-transform-html-test-case Filter-comment
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

(define-zencoding-transform-html-test-case Filter-HAML
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

(define-zencoding-transform-html-test-case Filter-Hiccup
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

(define-zencoding-transform-html-test-case Filter-escape
  "script src=&quot;|e"    ("&lt;script src=\"&amp;quot;\"&gt;"
                            "&lt;/script&gt;"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS-abbrev tests

(defmacro define-zencoding-unit-test-case (name fn &rest tests)
  `(zencoding-test-cases 'assign ',name
                         ,fn
                         ',(loop for x on tests by #'cddr collect
                                 (cons (car x) (cadr x)))))

(define-zencoding-unit-test-case CSS-toknize
  #'zencoding-css-toknize
  ""                     ("")
  "abc"                  ("abc")
  "abc+"                 ("abc+")
  "abc+cde"              ("abc" "cde")
  "abc++cde"             ("abc+" "cde")
  "abc+cde+"             ("abc" "cde+")
  "abc++cde+"            ("abc+" "cde+")
  "ab:c+0p0x#aa+p0+cde+" ("ab:c+0p0x#aa" "p0" "cde+")
  "ab+#0+p+#c+x++cde+"   ("ab+#0" "p+#c" "x+" "cde+"))

(define-zencoding-unit-test-case CSS-parse-arg-number
  #'zencoding-css-arg-number
  ""                     (error "expected css number arguments")
  "0"                    (("0" "px") . "")
  "0-1-2"                (("0" "px") . "1-2")
  "-100"                 (("-100" "px") . "")
  "-10e-20"              (("-10" "em") . "-20")
  "35p#a"                (("35" "%") . "#a"))

(define-zencoding-unit-test-case CSS-parse-arg-color
  #'zencoding-css-arg-color
  ""                     (error "expected css color argument")
  "abc"                  (error "expected css color argument")
  "#x"                   (error "expected css color argument")
  "#a"                   ("#aaaaaa" . "")
  "#09"                  ("#090909" . "")
  "#3D5-2"               ("#33DD55" . "-2")
  "#1a2B-3"              ("#1a2B1a" . "-3")
  "#1A2b3x"              ("#1A2b31" . "x")
  "#1a2B3Cx"             ("#1a2B3C" . "x")
  "#1A2B3C4D-2"          ("#1A2B3C" . "4D-2"))

(define-zencoding-unit-test-case CSS-parse-args
  #'zencoding-css-parse-args
  ""                     nil
  "1-2--3-4"             (("1" "px") ("2" "px") ("-3" "px") ("4" "px"))
  "-10-2p-30#abc"        (("-10" "px") ("2" "%") ("-30" "px") "#aabbcc")
  "1p2x3-4e5x"           (("1" "%") ("2" "ex") ("3" "px") ("4" "em") ("5" "ex"))
  "#abc#de#f-3"          ("#aabbcc" "#dedede" "#ffffff" ("-3" "px")))

(define-zencoding-unit-test-case CSS-exprs
  #'zencoding-css-expr
  ""                     (("" nil))
  "cl:l+ov:h+bg+"        (("cl:l" nil) ("ov:h" nil) ("bg+" nil))
  "m10-auto"             (("m" nil ("10" "px") "auto"))
  "bg++c"                (("bg+" nil) ("c" nil))
  "m+0-10-10--20+p0-0"   (("m+" nil ("0" "px") ("10" "px") ("10" "px") ("-20" "px"))
                          ("p" nil ("0" "px") ("0" "px")))
  "bg+#abc#bc#c-3"       (("bg+" nil "#aabbcc" "#bcbcbc" "#cccccc" ("-3" "px"))))

(defmacro define-zencoding-transform-css-test-case (name &rest tests)
  `(define-zencoding-transform-test-case ,name
     'zencoding-css-transform
     ,@tests))

(define-zencoding-transform-css-test-case CSS-transform
  "m0+p0-1p2e3x"         ("margin:0px;"
                          "padding:0px 1% 2em 3ex;")
  "p!+m10e!+f"           ("padding: !important;"
                          "margin:10em !important;"
                          "font:;")
  "fs"                   ("font-style:italic;"))

;; start
(zencoding-test-cases)