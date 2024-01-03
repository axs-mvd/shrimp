(defmodule shrimp-handler
  (export all))

(defun init [_type req opts]
  `#(ok ,req 'no-state))

(defun handle [req state]
  (let* ((conn (get-connection))
         ((tuple 'ok res) (reply (forward req conn))))
    (tuple 'ok res 'still-no-state)))

(defun terminate [_reason _req _state]
  'ok)

(defun forward [original-req conn]
  (tuple (hackney:send_request
           conn 
           (tuple (hackney_method 
                    (let [((tuple m  _) (cowboy_req:method original-req))] 
                      m))
                  (url original-req)
                  (headers original-req)
                  (payload original-req)))
         original-req))

(defun reply
  ([(tuple (tuple 'ok status headers ref) req)]
   (let [((tuple 'ok body) (hackney:body ref))
         (_ (io:format "I'm about to reply ~p~n" (list (list status headers))))] 
     (cowboy_req:reply status headers body req)))
  ([(tuple (tuple 'error reason) req)]
   (let [(_ (io:format "I'm about to reply error~n" '()))] 
    (cowboy_req:reply 502 '() #"" req))))

(defun hackney_method 
  ([#"GET"] 'get)
  ([#"POST"] 'post)
  ([#"DELETE"] 'delete)
  ([#"PUT"] 'put)
  ([#"INFO"] 'info))

(defun url [req]
  (let ((base (get-backend-base))
        (path (binary:bin_to_list 
                (let [((tuple p  _) (cowboy_req:path req))] 
                  p))))
   ; (binary:list_to_bin (lists:append base path))))
    (binary:list_to_bin path)))

(defun get-backend-base []
  "http://localhost:9999")

(defun get-connection []
  (let [((tuple 'ok conn) (hackney:connect (get-backend-base)))
        (_ (io:format "creating a connection to ~p ~n" 
                      (list (get-backend-base))))]
    conn))

(defun release-connection [conn]
  (let [(_ (io:format "releasing connection" '()))]
    (hackney:close conn)))

(defun headers [req]
  (let [((tuple h  _) (cowboy_req:headers req))] 
    h))

(defun payload [req]
  (let [((tuple 'ok data _) (cowboy_req:body req))] 
       data))

(defun store [filename term] 
   (file:write_file filename (erlang:term_to_binary term)))

(defun fetch [filename]
    (let (((tuple 'ok data) (file:read_file filename)))
        (erlang:binary_to_term data)))
