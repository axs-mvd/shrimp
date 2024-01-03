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

;(defun test
;  (['#M(wait-list '())]
;   (io:format "wait-list was empty~n"))
;  (['#M(wait-list (list head tail))]
;   (io:format "wait-list head ~p tail ~p" '(,head ,tail))))
