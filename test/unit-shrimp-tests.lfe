(defmodule unit-shrimp-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest shrimp-pool-init
  (let [(_ (meck:expect 'hackney 'connect 1 (tuple 'ok 'connection)))
        (r (shrimp-pool:initial-state (map 'pool-size 3 
                                           'base-url 'url)))
        (m (map 'pool-size 3
                'base-url 'url
                'draining 0
                'status 'up
                'wait-list '()
                'pool '(connection connection connection)))]
  (is (== m r))))

(deftest shrimp-pool-simple-acquire
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '()
                     'pool '(connection connection connection)))
         (new-state (maps:merge state
                                (map 'pool '(connection connection))))]
    (is (== (tuple 'reply (tuple 'ok 'connection) new-state)
            (shrimp-pool:handle_call 'acquire 'caller state)))))

(deftest shrimp-pool-acquire-empty-pool
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '()
                     'pool '()))
         (new-state (maps:merge state
                                (map 'pool '()
                                     'wait-list (list 'caller))))]
    (is (== (tuple 'noreply new-state)
            (shrimp-pool:handle_call 'acquire 'caller state)))))

(deftest shrimp-pool-acquire-with-waitlist
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '(caller1)
                     'pool '()))
         (new-state (maps:merge state
                                (map 'pool '()
                                     'wait-list (list 'caller1 'caller2))))]
    (is (== (tuple 'noreply new-state)
            (shrimp-pool:handle_call 'acquire 'caller2 state)))))

(deftest shrimp-pool-acquire-with-waitlist-2
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '(caller1 caller2)
                     'pool '()))
         (new-state (maps:merge state
                                (map 'pool '()
                                     'wait-list (list 'caller1 'caller2 'caller3))))]
    (is (== (tuple 'noreply new-state)
            (shrimp-pool:handle_call 'acquire 'caller3 state)))))

(deftest shrimp-pool-acquire-on-drain
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 1
                     'status 'draining
                     'wait-list '()
                     'pool '()))]
    (is (== (tuple 'reply (tuple 'error 'draining) state)
            (shrimp-pool:handle_call 'acquire 'caller state)))))

(deftest shrimp-pool-acquire-on-stop
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 1
                     'status 'down
                     'wait-list '()
                     'pool '()))]
    (is (== (tuple 'reply (tuple 'error 'down) state)
            (shrimp-pool:handle_call 'acquire 'caller state)))))

(deftest shrimp-pool-release-empty-pool
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '()
                     'pool '()))]
    (is (== (tuple 'reply 'ok (maps:merge state
                                          (map 'pool (list 'connection))))
            (shrimp-pool:handle_call (tuple 'release 'connection) 'caller state)))))

(deftest shrimp-pool-simple-release
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '()
                     'pool (list 'connection1)))]
    (is (== (tuple 'reply 'ok (maps:merge state
                                          (map 'pool (list 'connection2 'connection1))))
            (shrimp-pool:handle_call (tuple 'release 'connection2) 'caller state)))))

(deftest shrimp-pool-release-with-empty-pool
  (let* [(state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list '()
                     'pool '()))]
    (is (== (tuple 'reply 'ok (maps:merge state
                                          (map 'pool (list 'connection))))
            (shrimp-pool:handle_call (tuple 'release 'connection) 'caller state)))))

(deftest shrimp-pool-release-with-empty-pool-and-someone-waiting
  (let* [(_ (meck:expect 'shrimp-pool 'do-reply 2 'ok))
         (state (map 'pool-size 3
                     'base-url 'url
                     'draining 0
                     'status 'up
                     'wait-list (list 'someone)
                     'pool '()))]
    (is (== (tuple 'reply 'ok (maps:merge state
                                          (map 'wait-list '() 
                                               'pool '())))
            (shrimp-pool:handle_call (tuple 'release 'connection) 'caller state)))))

(deftest shrimp-pool-release-on-drain
  (let* [(_ (meck:expect 'hackney 'close 1 'ok))
         (state (map 'pool-size 3
                     'base-url 'url
                     'draining 1
                     'status 'draining
                     'wait-list '()
                     'pool '()))]
    (is (== (tuple 'reply 'ok (maps:merge state
                                          (map 'status 'draining
                                               'draining 0)))
            (shrimp-pool:handle_call (tuple 'release 'connection) 'caller state)))))

(deftest shrimp-pool-release-on-drain-while-waiting-for-the-last
  (let* [(_ (meck:expect 'hackney 'close 1 'ok))
         (_ (meck:expect 'shrimp-pool 'do-reply 2 'ok))
         (state (map 'pool-size 3
                     'base-url 'url
                     'draining 1
                     'status 'draining
                     'wait-list '()
                     'waiting-for-drain 'waiter
                     'pool '()))]
    (is (== (tuple 'reply 'ok (maps:merge state
                                          (map 'status 'down
                                               'draining 0
                                               'waiting-for-drain 'none)))
            (shrimp-pool:handle_call (tuple 'release 'connection) 'caller state)))))

(deftest shrimp-pool-release-on-down
  (let* [(state (map 'status 'down))]
    (is (== (tuple 'reply (tuple 'error 'down) 
                   (maps:merge state 
                               (map 'status 'down)))
            (shrimp-pool:handle_call (tuple 'release 'connection) 'caller state)))))

