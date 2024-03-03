(defmodule shrimp-test
  (export all))

(defun test_1 
  ([(map 'wait-list '())]
   (io:format "wait-list was empty~n" '()))
  ([(map 'wait-list (cons head tail))]
   (io:format "wait-list ~p ~p~n" (list head tail))))


(defun alpha
  (((map 'alpha src-alpha) (map 'alpha dst-alpha))
   (+ src-alpha (* dst-alpha (- 1.0 src-alpha)))))


(defun test-lambda-reply
  []
  (let [(receiver-pid (spawn (lambda (x)(clj:inc x))))]
    (io:format "pid ~p~n" (list receiver-pid))))

(defun catcher
  ([] (catcher '[])))

(defun catcher
  ([state]
   (receive
     ((tuple 'return caller-pid) 
        (! caller-pid state))
     (message
       (catcher (cons message state))))))

(defun get-message
  [catcher-pid]
    (! catcher-pid (tuple 'return (self)))
    (receive
      (message message)
      (after 1000 'ok)))


(defun send-message (calling-pid msg)
  (let ((spawned-pid (spawn 'shrimp-test 'print-result ())))
    (! spawned-pid (tuple calling-pid msg))))

;(defun test
;  (['#M(wait-list '())]
;   (io:format "wait-list was empty~n"))
;  (['#M(wait-list (list head tail))]
;   (io:format "wait-list head ~p tail ~p" '(,head ,tail))))
