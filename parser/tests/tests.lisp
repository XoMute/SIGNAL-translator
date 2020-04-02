(uiop:define-package :translator/parser/tests/tests
    (:use :cl :common
          :translator/lexer/all
          :translator/parser/parser)
  (:nicknames :parser-tests))

(in-package :parser-tests)

(defmacro define-test (name (program tree))
  (let ((var (gensym)))
    `(defun ,name ()
       (let ((*keywords* (make-hash-table :test 'eq))
             (*identifiers* (make-hash-table :test 'eq))
             (*constants* (make-hash-table :test 'eq))
             (*mc-delimiters* (make-hash-table :test 'equal)))
         (let ((,var (parser (lexer (make-string-input-stream ,program)))))
           (or (equalp ,var ,tree)
               (progn
                 (warn "Test ~A failed!" ',name)
                 nil)))))))

(define-test test.1
    ("
PROGRAM TEST;
VAR VAR1:INTEGER;
    VAR2: FLOAT      ;
    VARN: INTEGER;
BEGIN (* start of a program *)
  WHILE (* some random comment ************) 1 <> VARN
  DO (* multiline comment
      *
      *
      *

      *)
    WHILE VAR1 >= VAR2
    DO

    ENDWHILE;
  ENDWHILE;
END.
"
     '(:PROGRAM (:PROC-ID (:ID "TEST"))
       (:BLOCK
           (:VAR-DECLS
            (:DECL-LIST
             ((:DECL (:VAR-ID (:ID "VAR1")) (:ATTR "INTEGER"))
              (:DECL (:VAR-ID (:ID "VAR2")) (:ATTR "FLOAT"))
              (:DECL (:VAR-ID (:ID "VARN")) (:ATTR "INTEGER")))))
         (:STMT-LIST
          ((:STMT
            (:WHILE
                (:COND-EXPR (:EXPR (:UINT 1)) (:COMP-OP "<>")
                            (:EXPR (:VAR-ID (:ID "VARN"))))
              (:STMT-LIST
               ((:STMT
                 (:WHILE
                     (:COND-EXPR (:EXPR (:VAR-ID (:ID "VAR1"))) (:COMP-OP ">=")
                                 (:EXPR (:VAR-ID (:ID "VAR2"))))
                   (:STMT-LIST)))))))))))))

(define-test test.2
    ("
PROGRAM TEST;
VAR VAR1:INTEGER;
    VAR2: FLOAT      ;
    VARN: INTEGER;
BEGIN (* start of a program *)
  WHILE (* some random comment ************) 1 <> VARN
  DO (* multiline comment
      *
      *
      *

      *)
    WHILE VAR1 >= VAR2
    DO

    ENDWHILE;
  ENDWHILE;
END.
"
     '(:PROGRAM (:PROC-ID (:ID "TEST"))
       (:BLOCK
           (:VAR-DECLS
            (:DECL-LIST
             ((:DECL (:VAR-ID (:ID "VAR1")) (:ATTR "INTEGER"))
              (:DECL (:VAR-ID (:ID "VAR2")) (:ATTR "FLOAT"))
              (:DECL (:VAR-ID (:ID "VARN")) (:ATTR "INTEGER")))))
         (:STMT-LIST
          ((:STMT
            (:WHILE
                (:COND-EXPR (:EXPR (:UINT 1)) (:COMP-OP "<>")
                            (:EXPR (:VAR-ID (:ID "VARN"))))
              (:STMT-LIST
               ((:STMT
                 (:WHILE
                     (:COND-EXPR (:EXPR (:VAR-ID (:ID "VAR1"))) (:COMP-OP ">=")
                                 (:EXPR (:VAR-ID (:ID "VAR2"))))
                   (:STMT-LIST)))))))))))))

