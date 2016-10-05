(defun pattern-equal (pattern expression)
  (cond
    ;; Symbols that can't be bound first
    ((eq pattern t) expression)
    ((eq pattern nil) (not expression))
    ((keywordp pattern) (eql pattern expression))
    ;; Symbol that can be bound
    ((symbolp pattern) t)
    ;; Lists
    ((listp pattern) (and (pattern-equal (car pattern)
                                         (car expression))
                          (pattern-equal (cdr pattern)
                                         (cdr expression))))
    ;; Strings, arrays
    ;; string= for case-sensitivity, or string-equal for insensitivity?
    ((stringp pattern) (string= pattern expression))
    ((arrayp pattern) (equal pattern expression))
    ;; Anything else
    (t (eql pattern expression))
    ))

(defun make-registrar ()
  "MAKE-REGISTRAR => REGISTRAR

REGISTRAR is a function:
  REGISTRAR :create => new symbol (ala GENSYM)
  REGISTRAR :list => list of all symbols generated
"
  (let (register)
    (lambda (arg)
      (ecase arg
        (:create (push (gensym) register) (car register))
        (:list register)))))

(defun normalize-pattern (pattern generator)
  "Replace constants and _ in PATTERN with symbols from GENERATOR"
  (cond ((or (constantp pattern) (eq '_ pattern)) (funcall generator))
        ((listp pattern) (cons (normalize-pattern (car pattern)
                                                     generator)
                               (normalize-pattern (cdr pattern)
                                                     generator)))
        (t pattern)))

(defmacro pattern-bind (pattern expression &body body)
  "Bind variables in PATTERN to values in EXPRESSION, then evaluate BODY

Constants and _ are ignored."
  (let* ((registrar (make-registrar))
         (generator (lambda () (funcall registrar :create)))
         (safe-pattern (normalize-pattern pattern generator))
         (gensyms (funcall registrar :list)))
    `(destructuring-bind
         ;; Practice safe binding; wrap your patterns
         ;; (We allow atomic patterns; DESTRUCTURING-BIND requires a list)
         (,safe-pattern) (list ,expression)
       (declare (ignore ,@gensyms))
       ,@body)))

(defmacro pattern-case (keyform &body cases)
  "PATTERN-CASE KEYFORM (PATTERN FORM*)*

Evaluate first FORM* whose PATTERN matches KEYFORM. Symbols in PATTERN
are bound to the matching expression in KEYFORM.

PATTERN => match if
list => PATTERN and EXPRESSION have the same structure
        and elements of PATTERN match the corresponding elements in EXPRESSION
atom => PATTERN and EXPRESSION are EQUAL
assignable symbol => always matches; bind PATTERN to EXPRESSION
_ => always matches but remains unbound; multiple _ may appear in PATTERN

EX:
(defun factorial (n)
  (pattern-case n
    (0 1)
    (n (* n (factorial (1- n))))))

(defun kth (k list)
  (pattern-case (list k list)
    ((_ nil) (error \"bad K for LIST\"))
    ((0 (x . _)) x)
    ((k (_ . xs)) (kth (1- k) xs))))
"
  ;; Evaluate KEYFORM exactly once
  (let ((keyvalue (gensym)))
    `(let ((,keyvalue ,keyform))
       (cond ,@(loop :for case in cases
                     :for pattern = (first case)
                     :for body = (rest case)
                     :collect `((pattern-equal ',pattern ,keyvalue)
                                (pattern-bind ,pattern ,keyvalue
                                  ,@body)))
             ;; No patterns matched
             (t (error "no matching PATTERN found for KEYFORM"))))))
