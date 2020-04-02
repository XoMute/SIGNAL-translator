(uiop:define-package :translator/translator
    (:use :cl
          :translator/lexer/all
          :translator/parser/parser
          :common)
  (:export #:translate))

(in-package :translator/translator)

(defun translate (pathname)
  (let ((*keywords* (make-hash-table :test 'eq))
        (*identifiers* (make-hash-table :test 'eq))
        (*constants* (make-hash-table :test 'eq))
        (*mc-delimiters* (make-hash-table :test 'equal)))
    (let ((lexems (with-open-file (in pathname :direction :input)
                    (lexer in))))
      (parser lexems))))
