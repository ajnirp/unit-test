;; simple unit-test framework
;; almost all the code here was taken directly
;; from Peter Seibel's Practical Common Lisp

(defvar *test-name* nil)

; name = name of the test function
; parameters = list of the parameters the test function takes
; body = list of the equality assertions passed to the test function
; this is a macro that expands into a function
; it binds the global variable *test-name* to the name of the function
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
    ; (let ((*test-name* (append *test-name* (list ',name))))
    (let ((*test-name* (cons ',name *test-name*)))
      ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check-one (form)
  `(report-result ,form ',form))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))
    ; this also works
    ; ,@(loop for f in forms collect `(check-one ,f))))

; 'and' without the short circuiting behaviour
; remember, when we use with-gensyms, 'result'
; actually is the name of a variable that is our result
; 'result' is not our result itself
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let  ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))