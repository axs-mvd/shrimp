(defmodule shrimp-sup
  (behaviour supervisor)
  (export (start_link 0))
  (export (init 1)))

(defun server-name []
  'shrimp-sup)

(defun start_link []
  (supervisor:start_link
    `#(local ,(server-name)) (MODULE) '[]))

(defun init [_args]
  (let [(flags #M(strategy one_for_one
                  intensity 1
                  period 5))
         (pool-sup-spec (tuple 'shrimp-pool-sup (tuple 'shrimp-pool-sup 'start_link '()) 
                               'permanent 'brutal_kill 'supervisor (list 'shrimp-pool-sup)))]
    `#(ok #(,flags [,pool-sup-spec]))))
