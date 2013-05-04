;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CSS abbrev:

(zencoding-defparameter
 zencoding-css-unit-aliases
 (gethash "unitAliases" (gethash "css" zencoding-preferences)))
(defun zencoding-css-arg-number (input)
  (zencoding-parse
   " *\\(\\(?:-\\|\\)[0-9.]+\\)\\(\\(?:-\\|e\\|p\\|x\\)\\|\\)" 3 "css number arguments"
   (cons (list (elt it 1)
               (let ((unit (elt it 2)))
                 (gethash unit zencoding-css-unit-aliases "px")))
         input)))

(defun zencoding-css-arg-color (input)
  (zencoding-parse
   " *#\\([0-9a-fA-F]\\{1,6\\}\\)" 2 "css color argument"
   (cons (let* ((n (elt it 1))
                (l (length n)))
           (concat
            "#"
            (substring
             (cond ((= l 1) (concat (make-list 6 (string-to-char n))))
                   ((= l 2) (concat n n n))
                   ((= l 3) (concat
                             (loop for c in (string-to-list n)
                                   append (list c c))))
                   (t (concat n n)))
             0 6)))
         input)))

(defun zencoding-css-arg-something (input)
  (zencoding-parse
   " *\\([^ ]+\\)" 2 "css argument"
   (cons (elt it 1) input)))

(defun zencoding-css-parse-arg (input)
  (zencoding-run zencoding-css-arg-number it
                 (zencoding-run zencoding-css-arg-color it
                                (zencoding-run zencoding-css-arg-something it
                                               (if (equal input "")
                                                   it
                                                 (cons input ""))))))

(defun zencoding-css-important-p (input)
  (let ((len (length input)))
    (and (< 0 len)
         (char-equal (aref input (1- len)) ?!))))

(defun zencoding-css-parse-args (args)
  (when args
    (let ((rt nil))
      (loop
       (zencoding-pif (zencoding-css-parse-arg args)
                      (progn (push (car it) rt)
                             (setf args (cdr it)))
                      (return (nreverse rt)))))))

(defun zencoding-css-split-args (exp)
  (zencoding-aif
   (string-match "\\(?:[ #0-9$]\\|-[0-9]\\)" exp)
   (list (substring exp 0 it) (substring exp it))
   (list exp nil)))

(defun zencoding-css-split-vendor-prefixes (input)
  (zencoding-parse
   "\\(-[wmso]+-\\|-\\|\\)\\(.*\\)" 3 "css vendor prefixes"
   (list (elt it 2)
         (let ((vp (elt it 1)))
           (if (not (string= vp ""))
               (if (string= vp "-") 'auto
                 (string-to-list (subseq vp 1 -1))))))))

(defun zencoding-css-subexpr (exp)
  (let* ((importantp (zencoding-css-important-p exp)))
    (destructuring-bind (exp vp)
        (zencoding-css-split-vendor-prefixes exp)
      (destructuring-bind (key args)
          (zencoding-css-split-args (if importantp (subseq exp 0 -1) exp))
        `(,key ,vp
               ,importantp
               ,@(zencoding-css-parse-args args))))))

(defun zencoding-css-toknize (str)
  (let* ((i (split-string str "+"))
         (rt nil))
    (loop
     (let ((f (first i))
           (s (second i)))
       (if f
           (if (and s (or (string= s "")
                          (string-match "^\\(?:[ #0-9$]\\|-[0-9]\\)" s)))
               (progn
                 (setf rt (cons (concat f "+" s) rt))
                 (setf i (cddr i)))
             (progn
               (setf rt (cons f rt))
               (setf i (cdr i))))
         (return (nreverse rt)))))))

(defun zencoding-css-expr (input)
  (mapcar #'zencoding-css-subexpr
          (zencoding-css-toknize input)))

(zencoding-defparameter
 zencoding-css-snippets
 (gethash "snippets" (gethash "css" zencoding-snippets)))

(zencoding-defparameter
 zencoding-css-unitless-properties
 (gethash "unitlessProperties" (gethash "css" zencoding-preferences)))

(zencoding-defparameter
 zencoding-css-unitless-properties-regex
 (concat "^\\(:?" (zencoding-join-string
                   zencoding-css-unitless-properties "\\|")
         "\\):.*$"))

(defun zencoding-css-instantiate-lambda (str)
  (flet ((split-string-to-body
          (str args-sym)
          (let ((rt '(concat)) (idx-max 0))
            (loop for i from 0 to 255 do
                  (zencoding-aif
                   (string-match "\\(?:|\\|${\\(?:\\([0-9]\\):\\|\\)\\(?:\\(.+?\\)\\|\\)}\\)" str)
                   (destructuring-bind (mat idx def)
                       (mapcar (lambda (ref) (match-string ref str)) '(0 1 2))
                     (setf rt
                           `((or
                              (nth ,(let ((cur-idx (if idx (1- (string-to-int idx)) i)))
                                      (setf idx-max (max cur-idx idx-max)))
                                   ,args-sym)
                              ,(or def ""))
                             ,(substring str 0 it) ;; ordered to reverse
                             ,@rt))
                     (setf str (substring str (+ it (length mat)))))
                   ;; don't use nreverse. cause bug in emacs-lisp.
                   (return (cons idx-max (reverse (cons str rt)))))))))
    (let ((args (gensym)))
      (destructuring-bind (idx-max . body) (split-string-to-body str args)
        (eval
         `(lambda (&rest ,args)
            (progn
              (when (nthcdr ,idx-max ,args)
                (setf (nthcdr ,idx-max ,args)
                      (list (zencoding-join-string
                             (nthcdr ,idx-max ,args) " "))))
              ,body)))))))

(zencoding-defparameter
 zencoding-vendor-prefixes-properties
 (gethash "vendorPrefixesProperties" (gethash "css" zencoding-preferences)))
(zencoding-defparameter
 zencoding-vendor-prefixes-default
 (list "webkit" "moz" "ms" "o"))
(defun zencoding-css-transform-vendor-prefixes (line vp)
  (let ((key (subseq line 0 (or (position ?: line) (length line)))))
    (let ((vps (if (eql vp 'auto)
                   (gethash key
                            zencoding-vendor-prefixes-properties
                            zencoding-vendor-prefixes-default)
                 (mapcar (lambda (v)
                           (cond ((= v ?w) "webkit")
                                 ((= v ?m) "moz")
                                 ((= v ?s) "ms")
                                 ((= v ?o) "o")))
                         vp))))
      (zencoding-join-string
       (append (mapcar (lambda (v) (concat "-" v "-" line)) vps)
               (list line))
       "\n"))))

(defun zencoding-css-transform-exprs (exprs)
  (zencoding-join-string
   (mapcar
    #'(lambda (expr)
        (let ((basement
               (zencoding-aif
                (gethash (car expr) zencoding-css-snippets)
                (let ((set it) (fn nil) (unitlessp nil))
                  (if (stringp set)
                      (progn
                        ;; new pattern
                        ;; creating print function
                        (setf fn (zencoding-css-instantiate-lambda set))
                        ;; get unitless or no
                        (setf unitlessp
                              (not (null (string-match
                                          zencoding-css-unitless-properties-regex set))))
                        ;; caching
                        (puthash (car expr) (cons fn unitlessp) zencoding-css-snippets))
                    (progn
                      ;; cache hit.
                      (setf fn (car set))
                      (setf unitlessp (cdr set))))
                  (apply fn
                         (mapcar
                          #'(lambda (arg)
                              (if (listp arg)
                                  (if unitlessp (car arg)
                                    (apply #'concat arg))
                                arg))
                          (cdddr expr))))
                (concat (car expr) ":"
                        (zencoding-join-string
                         (mapcar #'(lambda (arg)
                                     (if (listp arg) (apply #'concat arg) arg))
                                 (cdddr expr)) " ")
                        ";"))))
          (let* ((separator-pos (position ?: basement))
                 (basement (concat (subseq basement 0 (1+ separator-pos)) " "
                                   (subseq basement (1+ separator-pos)))))
            (let ((line
                   (if (caddr expr)
                       (concat (subseq basement 0 -1) " !important;")
                     basement)))
              (zencoding-aif
               (cadr expr)
               (zencoding-css-transform-vendor-prefixes line it)
               line)))))
    exprs)
   "\n"))

(defun zencoding-css-transform (input)
  (zencoding-css-transform-exprs (zencoding-css-expr input)))