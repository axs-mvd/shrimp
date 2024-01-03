(defmodule shrimp-pool-sup
  (behaviour supervisor)
  (export (start_link 0))
  (export (init 1))
  (export (new-pool 2)))

(defun server-name []
  'shrimp-pool-sup)

(defun start_link []
  (supervisor:start_link
    `#(local ,(server-name)) (MODULE) '[]))

(defun init [_args]
  (let [(flags #M(strategy one_for_one
                  intensity 1
                  period 5))]
    `#(ok #(,flags []))))

(defun new-pool 
  ([name pool-args]
   (supervisor:start_child
         (MODULE)
     (tuple name (tuple 'shrimp-pool 'start (list pool-args)) 
            'permanent 'brutal_kill 'worker (list 'shrimp-pool)))))

(defun stop-pool 
  ([name pool-args]
   (supervisor:start_child
         (MODULE)
     (tuple name (tuple 'shrimp-pool 'start (list pool-args)) 
            'permanent 'brutal_kill 'worker (list 'shrimp-pool)))))


