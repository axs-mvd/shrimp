(defmodule shrimp-pool
  (behaviour gen_server)
  (export (start 1))
  (export (init 1))
  (export (initial-state 1))
  (export (terminate 2))
  (export (handle_cast 2))
  (export (handle_call 3))
  (export (acquire 1))
  (export (down 1))
  (export (up 1))
  (export (inc 1))
  (export (status? 1))
  (export (count 1))
  (export (update 2))
  (export (release 2))
  (export (do-reply 2)))

(defun callback-module () (MODULE))

;; API 
(defun acquire [server]
  (gen_server:call server 'acquire))

(defun release [server conn]
  (gen_server:call server
                   (tuple 'release conn)))

(defun status? [server]
  (gen_server:call server 'status?))

(defun count [server]
  (gen_server:call server 'count))

(defun update [server conf]
  (gen_server:call server
                   (tuple 'update conf)))

(defun down [server]
  (gen_server:call server 'drain))

(defun inc [server]
  (gen_server:call server 'inc))

(defun up [server]
  (gen_server:call server 'up))

;; internal functions

(defun genserver-opts () '())

(defun unknown-command () #(error "Unknown command."))

(defun initial-state 
  ([(map 'pool-size n 
         'base-url url)] 
   (map 'pool-size n
        'base-url url
        'draining 0
        'status 'up
        'wait-list '()
        'pool (lists:map (lambda [_] (let [((tuple 'ok conn) (hackney:connect url))]
                                       conn)) 
                           (lists:seq 1 n)))))

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
  (['acquire caller state-data]
   (do-acquire caller state-data))

  (['inc caller state]
   (do-inc caller state))

  ([(tuple 'update conf) _caller state]
   (tuple 'reply 'ok (maps:merge state
                                 (maps:with (list 'pool-size 'base-url) 
                                            conf))))

  ([(tuple 'release conn) caller state]
   (do-release conn caller state))

  (['up _caller (= (map 'status 'down) state)]
   (tuple 'reply 'ok (initial-state (maps:with (list 'base-url 'pool-size) 
                                               state))))

  (['up _caller (= (map 'status status) state)]
   (tuple 'reply (tuple 'error status) state))

  (['count caller (= (map 'pool pool) state)]
   (tuple 'reply (length pool) state))

  (['status? _caller (= (map 'status status) state)]
   (tuple 'reply status state))

  (['drain caller state]
   (do-drain caller state))

  ([message _caller state-data]
   (tuple 'reply (unknown-command) state-data)))

;; the real thing
(defun do-drain
  ([caller (= (map 'pool-size n
                   'pool pool
                   'waiting waiters
                   'status 'up) state)]
   (let [(curr-size (length pool))]
     (lists:foreach (hackney:close) pool)
     (lists:foreach (lambda [waiter]
                      (gen_server:reply waiter 
                                        (tuple 'error 'draining)))
                    waiters)
     (cond 
       ((== curr-size n) 
        (tuple 'reply 'ok (maps:merge state 
                                      (map 'pool '()
                                           'status 'down
                                           'waiting '()))))
       (else 
         (tuple 'noreply 
                (maps:merge state 
                            (map 'pool '() 
                                 'waiting '()
                                 'status 'draining
                                 'waiting-for-drain caller
                                 'draining (- n curr-size))))))))

  ([_caller (= (map 'status status)
               state)] (when (orelse (== status 'down) 
                                     (== status 'draining)))
   (tuple 'reply (tuple 'error status) state)))

(defun do-inc
  ([caller (= (map 'status 'up
                   'pool pool
                   'base-url url) state)]
   (let [((tuple 'ok conn) (hackney:connect url))]
     (tuple 'reply 'ok (maps:merge state
                                   (map 'pool (cons conn pool))))))

  ([caller (= (map 'status status) state)]
   (tuple 'reply (tuple 'error status) state)))

(defun do-acquire
  ([caller (= (map 'status status)
              state)] (when (orelse (== status 'down) 
                                    (== status 'draining)))
   (tuple 'reply (tuple 'error status) state))

  ([caller (= (map 'pool '()
                   'wait-list wait-list)
               state)]
   (tuple 'noreply 
          (maps:merge state 
                      (map 'pool '()
                           'wait-list (lists:append wait-list (list caller))))))

  ([_caller (= (map 'pool (cons conn pool)
                    'wait-list '()) 
               state)]
   (tuple 'reply (tuple 'ok conn) 
          (maps:merge state 
                      (map 'pool pool
                           'wait-list '())))))

(defun do-release
  ([conn _caller (= (map 'status 'down) state)] 
   ;; this shouldn't ever happen
   (tuple 'reply (tuple 'error 'down) state))

  ([conn caller (= (map 'pool pool
                        'wait-list '()
                        'status 'up)
                   state)]
   (tuple 'reply 'ok 
          (maps:merge state
                      (map 'pool (cons conn pool)
                           'wait-list '()))))

  ([conn caller (= (map 'status 'draining
                        'draining 1
                        'waiting-for-drain waiter)
                   state)]
   (hackney:close conn)
   ;; this is the last conn I'm going to close,
   ;; so, I'll reply ok to the drain caller and set status
   ;; to 'down and call it a day
   (shrimp-pool:do-reply waiter 'ok)
   (tuple 'reply 'ok 
          (maps:merge state
                      (map 'pool '()
                           'status 'down
                           'waiting-for-drain 'none
                           'draining 0))))

  ([conn caller (= (map 'status 'draining
                        'draining n)
                   state)]
   (hackney:close conn)
   (tuple 'reply 'ok 
          (maps:merge state
                      (map 'draining (clj:dec n)))))

  ([conn caller (= (map 'pool '()
                        'wait-list (cons waiting waitlist))
                   state)]
   (let
     [('ok (shrimp-pool:do-reply waiting (tuple 'ok conn)))]
     (tuple 'reply 'ok 
            (maps:merge state
                        (map 'wait-list waitlist))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACHTUNG
;; I need this passthrough function to properly test it
;; as meck doesn't allow to mock gen_server funs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-reply 
  [proc reply]
  (gen_server:reply proc reply))

(defun terminate [_reason _state]
  'ok)
