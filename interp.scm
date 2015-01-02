;Author:  Judson Martin
;Class:  CS220
;Assignment:  Lab 6, Scheme Interpreter

;the global-environment. duh
(define global-env (list
                    (cons 'read read)
                    (cons 'number? number?)
                    (cons 'string? string?)
                    (cons 'boolean? boolean?)
                    (cons 'char? char?)
                    (cons 'null? null?)
                    (cons 'symbol? symbol?)
                    (cons 'list? list?)
                    (cons 'equal? equal?)
                    (cons '<= <=)
                    (cons '>= >=)
                    (cons '< <)
                    (cons '> >)
                    (cons '+ +)
                    (cons '- -)
                    (cons '* *)
                    (cons '/ /)
                    (cons 'list list)
                    (cons 'car car)
                    (cons 'cdr cdr)
                    (cons 'cons cons)
                    (cons 'set-car! set-car!)
                    (cons 'set-cdr! set-cdr!)
                    ))

;taken from page 280 of "The Scheme Programming Language" by Kent Dybvig
(define (readfile filename)
  (let ((p (open-input-file filename)))
    (let f ((x (read p)))
      (if (eof-object? x)
          (begin
            (close-port p)
            '())
          (begin
          (evaluate x  global-env) (f (read p)))))))

;so that #f is not returned if a symbol is not found, instead the user is told
(define (handle-symbol symb env)
  (if (equal? (assoc symb env) #f)
      (begin (display "\n;Unbound variable: ") ;unbound variable
             (display (string symb))
             "*Unspec*")                     ;returns unspec so other processes
      (cdr (assoc symb env))                 ;no to stop if unbound var
      ))

(define (fix-format expr)                    ;for the syntactic-sugar form of
  (evaluate                                  ;define
   (list 'define (caar expr) (list 'lambda (cdar expr) (cadr expr)))
   global-env)
  )

;Chris helped my figure out what define needed to say
(define (handle-define expr env)
  (if (list? (car expr))             ;if in syntactic-sugar form
      (fix-format expr)              ;other function re-words and handles it
      (let ( (head (car global-env)) (tail (cdr global-env)) )
        (set-cdr! global-env (cons (cons (car head) (cdr head)) tail))
        (set-car! global-env (cons (car expr) (evaluate (cadr expr) env)))
        (car expr)
        )))

;binds pairs and adds them on to the end of the environment
(define (let-help pairs env)
  (if (equal? '() pairs)
      env
      (cons (cons (caar pairs) (cadar pairs)) (let-help (cdr pairs) env))
      ))

;treat like begin, with augmented local environment
(define (handle-let expr env)
  (evaluate (cons 'begin (cdr expr)) (let-help (car expr) env))
  )

;pretty basic if
(define (handle-if expr env)
  (if (evaluate (car expr) env)
      (evaluate (cadr expr) env)
      (evaluate (caddr expr) env)
      ))

;evaluates conditions until a true one is found, then evaluates assoc code
(define (handle-cond expr env)
  (if (equal? '() expr)       ;book says all conds must hav one true condition
      (begin (display "All conditions failed, please try again.\n") "*Unspec*")
      (if (equal? 'else (caar expr))
          (evaluate (cadar expr) env)
          (if (evaluate (caar expr) env)
              (evaluate (cadar expr) env)
              (handle-cond (cdr expr) env))
          )))

;begin is pretty straight forward
(define (handle-begin lst env)
  (if (equal? '() (cdr lst))
      (evaluate (car lst) env)
      (begin (evaluate (car lst) env) (handle-begin (cdr lst) env))
      ))

;could this be any simpler?
(define (handle-lambda expr env)
  (list 'closure expr env)
  )

;takes an assoc list and cons' it onto an environment, returning the new env
(define (create-env pairs env)
  (if (equal? (cdr pairs) '())
      (cons (car pairs) env)
      (cons (car pairs) (create-env (cdr pairs) env))
      ))

;takes a list and evaluates all members of it, takes care of nested statements
(define (eval-all expr env)
  (if (equal? '() expr)
      '()
      (cons (evaluate (car expr) env) (eval-all (cdr expr) env))
      ))

;takes two lists and returns association list
(define (zip lst1 lst2)
  (if (equal? lst1 '())
      '()
      (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))
      ))

;name is needed in case an error message is sent
(define (function-apply name func args)
  (if (equal? (length (cadadr func)) (length args)) ;checks right # arguments
      (evaluate (caddadr func) (create-env (zip (cadadr func) args)
                                           (caddr func)))
      (mess-up name (length (cadadr func)) (+ (length args) 1))
      )) ;if wrong number arguments, error sent and unspec returned
         ;otherwise disect closure and execute in temp environment

;handles all regular forms that are defined in the environment
(define (handle-else expr env)
  (if (equal? #f (assoc (car expr) env))
      (begin
        (display "\nFailed to find procedure: ") ;if not in environment
        (display (string (car expr)))
        "*Unspec*")                              ;returns unspec
      (let ( (func (cdr (assoc (car expr) env))) ;
             (args (eval-all (cdr expr) env)) )
        (if (check-unspec? args) "*Unspec*"      ;no unspecified arguments
            (if (procedure? func)                    ;distinguish procedure &
                (apply func args)                    ;function, handle accord
                (function-apply (car expr) func args)
                )))))

;applies a closure to a list of arguments
(define (my-apply name expr env)
  (let ( (func (evaluate (car expr) env))
         (args (eval-all (cdr expr) env)) ) ;make sure arguments are eval'd
    (if (check-unspec? args) "*Unspec*"     ;and none of unspecified
        (function-apply name func args)
        )))

;if an unspecified object is ever being passed as an argument I want to stop
;executing before it causes an error, and just return unspecified.  this allows
;errors in nested functions not to break the interpreter
(define (check-unspec? expr)             ;takes list of arguments
  (if (equal? '() expr)
      #f
      (if (equal? "*Unspec*" (car expr)) ;if unspec is an argument
          #t
          (check-unspec? (cdr expr))
          )))

;evaluate.  i wonder what this does...
(define (evaluate expr env)
  (cond
   ((equal? "*Unspec*" expr) "*Unspec*") ;to make sure i don't evaluate unspec
   ((number? expr) expr)
   ((string? expr) expr)
   ((boolean? expr) expr)
   ((char? expr)  expr)
   ((null? expr) null)  ;allows user to type in user-init-env for eval
   ((equal? 'user-initial-environment expr) global-env)
   ((symbol? expr) (handle-symbol expr env))
   ((list? expr)
    (cond
          ;all the if's are checking # of arguments before executing
          ;if number is wrong, error sent and unspec returned
     ((equal? '() expr) '())
     ((check-unspec? expr) "*Unspec*")
     ((equal? 'define (car expr)) (if (equal? 3 (length expr))
                                      (handle-define (cdr expr) env)
                                      (mess-up 'define 2 (length expr))))
     ((equal? 'quote (car expr)) (if (equal? 2 (length expr))
                                      (cadr expr)
                                      (mess-up 'quote 1 (length expr))))
     ((equal? 'let (car expr)) (if (<= 3 (length expr))
                                      (handle-let (cdr expr) env)
                                      (mess-up2 'let 2 (length expr))))
     ((equal? 'if (car expr)) (if (equal? 4 (length expr))
                                      (handle-if (cdr expr) env)
                                      (mess-up 'if 3 (length expr))))
     ((equal? 'cond (car expr)) (if (<= 2 (length expr))
                                      (handle-cond (cdr expr) env)
                                      (mess-up2 'cond 1 (length expr))))
     ((equal? 'begin (car expr)) (if (<= 2 (length expr))
                                      (handle-begin (cdr expr) env)
                                      (mess-up2 'begin 1 (length expr))))
     ((equal? 'lambda (car expr)) (if (equal? 3 (length expr))
                                      (handle-lambda expr env)
                                      (mess-up 'lambda 2 (length expr))))
     ((equal? 'eval (car expr)) (if (equal? 3 (length expr))
                                    (evaluate 
                                     (evaluate (cadr expr) env)
                                     (evaluate (caddr expr) env))
                                    (mess-up 'eval 2 (length expr))))
     ((list? (car expr)) (my-apply 'anonymous-lambda expr env))
     ((equal? 'assoc (car expr)) (if (equal? 3 (length expr))
                                      (my-assoc (cadr expr) (caddr expr))
                                      (mess-up 'assoc 2 (length expr))))
     ((equal? 'map (car expr)) (if (equal? 3 (length expr))
                                      (my-map (cadr expr) (caddr expr))
                                      (mess-up 'map 2 (length expr))))
     ((equal? 'reduce (car expr)) (if (equal? 4 (length expr))
                                      (my-reduce
                                       (cadr expr) (caddr expr) (cadddr expr))
                                      (mess-up 'reduce 3 (length expr))))
     ((equal? 'display (car expr)) (if (equal? 2 (length expr))
                                       (my-display (cadr expr))
                                       (mess-up 'display 1 (length expr))))
     ((equal? 'apply (car expr)) (if (equal? 3 (length expr))
                                     (my-apply
                                      (cadr expr) (cdr expr) env)
                                     (mess-up 'apply 2 (length expr))))
     ((equal? 'load (car expr)) (handle-load (cadr expr)))
     ((equal? 'caar (car expr)) (my-caar (cadr expr)))
     ((equal? 'cddr (car expr)) (my-cddr (cadr expr)))
     ((equal? 'cdar (car expr)) (my-cdar (cadr expr)))
     ((equal? 'cdadr (car expr)) (my-cdadr (cadr expr)))
     ((equal? 'cadr (car expr)) (my-cadr (cadr expr)))
     ((equal? 'caddr (car expr)) (my-caddr (cadr expr)))
     ((equal? 'cadddr (car expr)) (my-cadddr (cadr expr)))
     ((equal? 'cadar (car expr)) (my-cadar (cadr expr)))
     ((equal? 'cadadr (car expr)) (my-cadadr (cadr expr)))
     ((equal? 'caddadr (car expr)) (caddadr (cadr expr)))
     ((equal? 'cadddadr (car expr)) (cadddadr (cadr expr)))
     (else (handle-else expr env))
     ))))

;this handles error messages, and returns unspec for the functions above
(define (mess-up func allow given)
  (begin
    (display ";The procedure '") (display func)
    (display "' has been called with ") (display (- given 1))
    (display " argument(s); it requires exactly ") (display allow)
    (display " argument(s).\n") "*Unspec*"))

;in case it should say 'at least' instead of 'exactly
(define (mess-up2 func allow given)
  (begin
    (display ";The procedure '") (display func)
    (display "' has been called with ") (display (- given 1))
    (display " argument(s); it requires at least ") (display allow)
    (display " argument(s).\n") "*Unspec*"))

;display function, given instruction for unspec and to avoid infinite loops
(define (my-display item)
  (begin
    (cond
     ((equal? "*Unspec*" item) (display "\n;Unspecified return value\n"))
     ((string? item) (begin
                       (display "\n;Value: \"")
                       (display item) (display "\"\n")))
     ((list? item) (begin
                     (display "\n;Value: ")
                     (cond
                      ((equal? '() item) (display item))
                      ((equal? 'closure (car item))
                       (display (list 'closure (cadr item) 'env)))
                      ((equal? 'lambda (car item))
                       (display (list 'closure item 'env)))
                      (else (display item)))
                     (display "\n")))
     (else (begin (display "\n;Value: ") (display item) (display "\n"))))
     ))

;so that the interpreter can be interpreted
(define (my-caar lst) (car (car lst)))
(define (my-cddr lst) (cdr (cdr lst)))
(define (my-cdar lst) (cdr (car lst)))
(define (my-cdadr lst) (cdr (car (cdr lst))))
(define (my-cadr lst) (car (cdr lst)))
(define (my-caddr lst) (car (cdr (cdr lst))))
(define (my-cadddr lst) (car (cdr (cdr (cdr lst)))))
(define (my-cadar lst) (car (cdr (car lst))))
(define (my-cadadr lst) (car (cdr (car (cdr lst)))))
(define (caddadr lst) (car (cdr (cdr (car (cdr lst))))))
(define (cadddadr lst) (car (cdr (cdr (cdr (car (cdr lst)))))))

;built in function
(define (my-assoc symbol lst)
  (if (null? lst)
      (if (equal? symbol (caar lst))
          (car lst)
          (assoc symbol (cdr lst))
          )))

;and another
(define (my-map func lst)
  (if (equal? '() lst)
      '()
      (cons (func (car lst)) (my-map func (cdr lst)))
      ))

;and the last one
(define (my-reduce func n lst)
  (if (equal? '() lst)
      n
      (func (car lst) (my-reduce func n (cdr lst)))
      ))

;list lenght, is this to determine right number of arguments
(define (length lst)
  (if (equal? '() lst)
      0
      (+ 1 (length (cdr lst)))
      ))

;and finally the repl loop that starts it all off
(display "\n\n1 ]=> ")
(define (start-interp)
  (let ((input (read)))
    (if (equal? 'exit input)
        '()
        (begin
          (my-display (evaluate input global-env))
          (display "\n1 ]=> ")
          (start-interp)
          ))))

(start-interp)
(display "\nHappy Happy Joy Joy.")
