#|
@doc
  shrimp public API
@end
|#

(defmodule shrimp-app
  (behaviour application)

  ;; Application callbacks
  (export (start 2)
          (stop 1)
          (router-match-spec 0)))

;;; API
(defun start [_type _args]
  (let* ((dispatch (cowboy_router:compile (router-match-spec)))
         (`#(ok ,_pid)
           (cowboy:start_http
             'lfe_http_listener
             100
             '(#(port 8080))
             `(#(env (#(dispatch ,dispatch)))))))
    (shrimp-sup:start_link)))

(defun stop [_state]
  'ok)

(defun router-match-spec []
  "Returns the match spec we will be using for our router"
  '(#(_ (#("/[...]" shrimp-handler ())))))

