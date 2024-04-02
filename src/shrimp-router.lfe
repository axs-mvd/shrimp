(defmodule shrimp-router
  (behaviour gen_server)
  (export (start 1))
  (export (init 1))
  (export (initial-state 1))
  (export (terminate 2))
  (export (handle_cast 2))
  (export (handle_call 3))

(defun callback-module () (MODULE))

(defun add-route
  ([router (map 'name name
                'regexp regexp
                'backend backend)]
   'ok))

(defun rm-route
  ([router pool-name]
   'ok))

(defun add-backend 
  ([router (map 'name name 
                'pools pools
                'balance-fn 'round-robin)]
   'ok))

(defun rm-backend
  ([router backend] 
   'ok))

(defun add-pool
  ([router (map 'url url
                'name name)]
   'ok))

(defun rm-pool
  ([router pool-name]
   'ok))

(defun route 
  ([router req]
   'reply))

(defun initial-state 
  ([] 
   (map 'pools (list)
        'backends (list)
        'routes (list))))

;; server behaviour
(defun start [init-args]
  (gen_server:start_link (callback-module)
                         (initial-state init-args)
                         (genserver-opts)))

(defun init [initial-state]
  (tuple 'ok initial-state))

(defun handle_cast [_message state-data]
  (tuple 'noreply state-data))
  
(defun handle_call
  ([(tuple 'add-route param) caller state-data]
   (let* [(tuple reply new-state-data) (do-add-route param state-data)]
     (tuple 'reply reply new-state))
   )
  )


