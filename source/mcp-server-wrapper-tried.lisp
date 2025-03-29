;;;; mcp-server-wrapper-trynow.lisp
;;;; Improved MCP (Model-Claude Protocol) wrapper implementation in Common Lisp
;;;; This allows Claude to communicate directly with Gendl via JSON-RPC

(in-package :gendl-mcp)

;;
;; This file assumes *http-port* is already set and *http-server* is
;; already bound to an http server instance, this is part of Gendl's
;; standard startup bootstrapping
;;

(defparameter *gendl-port* *http-port*
  "Port on which the Gendl HTTP server is listening")
(defparameter *gendl-port-host* (uiop:getenv "HTTP_HOST_PORT")
  "Port visible on container host on which the Gendl HTTP server will appear to be listening (for reporting messages)")

;;; Configuration parameters
(defparameter *mcp-port* *http-port*
  "Port on which the MCP server will listen")
(defparameter *mcp-port-host* *gendl-port-host*
  "Port visible on container host on which MCP server will appear to listen (for reporting messages)")

(defparameter *gendl-host* "127.0.0.1" ;; sometimes "localhost" runs into ipv6 issues - need to fix ipv6 on CCL. 
  "Host on which the Gendl server is running")

(defparameter *debug-mode* t
  "Enable debug output")

(defvar *mcp-server* (or *http-server* (progn (gendl:start-gendl!) *http-server*))
  "The MCP server instance")

(defun debug-log (format-string &rest args)
  "Log a debug message if debug mode is enabled"
  (when *debug-mode*
    (apply #'format *error-output* (concatenate 'string "MCP-DEBUG: " format-string "~%") args)
    (force-output *error-output*)))

;;; JSON utility functions
(defun parse-json (json-string)
  "Parse a JSON string into a Lisp object"
  (cl-json:decode-json-from-string json-string))

(defun generate-json (lisp-object)
  "Generate a JSON string from a Lisp object"
  (cond
    ;; Special case for text responses with content array
    ((and (listp lisp-object) 
          (assoc :result lisp-object)
          (assoc :content (cdr (assoc :result lisp-object))))
     (cl-json:encode-json-to-string lisp-object))
    
    ;; Standard plist conversion
    ((and (listp lisp-object) 
          (evenp (length lisp-object))
          (every #'keywordp (loop for x in lisp-object by #'cddr collect x)))
     (cl-json:encode-json-to-string (plist-to-alist lisp-object)))
    
    ;; Already an alist
    (t (cl-json:encode-json-to-string lisp-object))))

(defun plist-to-alist (plist)
  "Convert a plist to an alist"
  (loop for (key value) on plist by #'cddr
        collect (cons key value)))

(defun alist-to-plist (alist)
  "Convert an alist to a plist"
  (loop for (key . value) in alist
        nconc (list key value)))

(defun alist-to-json (alist)
  "Convert an alist directly to JSON"
  (with-output-to-string (ss)
    (cl-json:encode-json-alist alist ss)))

;;; MCP Response Functions

(defun make-json-rpc-response (id result)
  "Create a properly formatted JSON-RPC response"
  `((:jsonrpc . "2.0")
    (:id . ,id)
    (:result . ,result)))

(defun make-json-rpc-error (id code message &optional data)
  "Create a properly formatted JSON-RPC error response"
  `((:jsonrpc . "2.0")
    (:id . ,id)
    (:error . ,(append `((:code . ,code)
                         (:message . ,message))
                       (when data `((:data . ,data)))))))

(defun make-text-response (id text)
  "Create a properly formatted MCP text response with content array"
  (make-json-rpc-response id 
                        `((:content . ,(list `((:type . "text")
                                              (:text . ,(if (stringp text) 
                                                           text 
                                                           (format nil "~A" text)))))))))

;;; MCP Protocol Request Handlers

(defun handle-initialize-request (params id)
  "Handle MCP initialization requests"
  (debug-log "Handling initialize request with id: ~A and params: ~S" id params)
  
  (make-json-rpc-response id 
                         `((:protocolVersion . "2024-11-05")
                           (:capabilities . ((:experimental . ())
                                            (:prompts . ((:listChanged . nil)))
                                            (:resources . ((:subscribe . nil) 
                                                          (:listChanged . nil)))
                                            (:tools . ((:listChanged . nil)))))
                           (:serverInfo . ((:name . "gendl-mcp-lisp")
                                          (:version . "1.0.0"))))))

(defun handle-list-request (endpoint id)
  "Generic handler for list-type requests"
  (debug-log "Handling ~A request" endpoint)
  
  (handler-case
      (if (string= endpoint "tools/list")
          ;; Handle tools list specifically
          (let* ((response (http-request "/mcp/tools-list" :method "GET"))
                 (tools-data (if (stringp (getf response :response))
                               (parse-json (getf response :response))
                               `((:tools . ())))))
            (make-json-rpc-response id tools-data))
          ;; For other list types, return empty array
          (make-json-rpc-response id 
                                 `((,(cond ((string= endpoint "resources/list") :resources)
                                          ((string= endpoint "prompts/list") :prompts)
                                          (t (make-keyword (string-upcase endpoint))))
                                    . ()))))
    (error (e)
      (debug-log "Error in ~A: ~A" endpoint e)
      (make-json-rpc-error id -32603 (format nil "Error processing ~A: ~A" endpoint e)))))

;;; Tool Handling

(defun handle-tool-call (params id)
  "Handle MCP tools/call requests"
  (let* ((tool-name (getf (alist-to-plist params) :name))
         (tool-args (getf (alist-to-plist params) :arguments)))
    
    (debug-log "Handling tool call: ~A with args: ~S" tool-name tool-args)
    
    (handler-case
        (cond
          ;; Tool: ping_gendl
          ((string= tool-name "ping_gendl")
           (make-text-response id (ping-gendl)))
          
          ;; Tool: lisp_eval
          ((string= tool-name "lisp_eval")
           (let ((code (getf tool-args :code)))
             (if code
                 (make-text-response id (lisp-eval code))
                 (make-json-rpc-error id -32602 "Missing required parameter: code"))))
          
          ;; Tool: http_request
          ((string= tool-name "http_request")
           (let ((path (getf tool-args :path))
                 (method (getf tool-args :method "GET"))
                 (headers (getf tool-args :headers))
                 (body (getf tool-args :body)))
             (if path
                 (let ((response (http-request path :method method :headers headers :body body)))
                   (make-text-response id (getf response :response)))
                 (make-json-rpc-error id -32602 "Missing required parameter: path"))))
          
          ;; Tool: query_gendl_kb
          ((string= tool-name "query_gendl_kb")
           (let ((query (getf tool-args :query)))
             (if query
                 (let ((response (http-request "/mcp/query-kb"
                                              :method "POST"
                                              :headers '(("Content-Type" . "application/json"))
                                              :body (alist-to-json `((:query . ,query))))))
                   (make-text-response id (getf response :response)))
                 (make-json-rpc-error id -32602 "Missing required parameter: query"))))
          
          ;; Unknown tool
          (t (make-json-rpc-error id -32601 (format nil "Unknown tool: ~A" tool-name))))
      
      (error (e)
        (debug-log "Error in tool call: ~A" e)
        (make-json-rpc-error id -32603 (format nil "Error calling tool: ~A" e))))))

;;; Main request dispatcher

(defun dispatch-request (method params id)
  "Dispatch the request to the appropriate handler"
  (debug-log "Dispatching method: ~A with params: ~S" method params)
  
  (cond
    ;; MCP protocol initialization
    ((string= method "initialize")
     (handle-initialize-request params id))
    
    ;; MCP list-type endpoints
    ((member method '("tools/list" "resources/list" "prompts/list") :test #'string=)
     (handle-list-request method id))
    
    ;; MCP tool call
    ((string= method "tools/call")
     (handle-tool-call params id))
    
    ;; Unknown method
    (t (make-json-rpc-error id -32601 (format nil "Method not found: ~A" method)))))

;;; Server-Sent Events (SSE) handling for MCP events

(defun handle-sse-connection (req ent)
  "Handle Server-Sent Events (SSE) connection for MCP events"
  (debug-log "Setting up SSE connection")
  (let ((path (request-uri req))
        (query-string (request-query req))
        (headers (net.aserve::request-headers req)))
    
    (debug-log "SSE connection path: ~A" path)
    (debug-log "SSE connection query string: ~S" query-string)
    (debug-log "SSE connection headers: ~S" headers)
    
    (with-http-response (req ent :content-type "text/event-stream")
      (with-http-body (req ent :headers '((:cache-control . "no-cache")
                                          (:connection . "keep-alive")
                                          (:access-control-allow-origin . "*")))
        
        ;; Just send heartbeats
        (flet ((send-heartbeat ()
                 (format *html-stream* "data: {\"jsonrpc\":\"2.0\",\"method\":\"connected\",\"params\":null}~%~%")
                 (force-output *html-stream*)))
          
          ;; Send initial heartbeat
          (debug-log "Sending first heartbeat")
          (send-heartbeat)
          
          ;; Send periodic heartbeats with timeout
          (let ((max-heartbeats 120))  ;; 1 hour at 30-second intervals
            (dotimes (counter max-heartbeats)
              (sleep 30)
              (debug-log "Sending heartbeat ~A of ~A" (1+ counter) max-heartbeats)
              (unless (open-stream-p *html-stream*)
                (debug-log "Connection closed by client")
                (return))
              (send-heartbeat))))))))

;;; HTTP POST Request handling

(defun handle-json-rpc-request (req ent)
  "Handle JSON-RPC requests"
  (let ((request-body (get-request-body req)))
    (debug-log "Processing JSON-RPC request: ~A" request-body)
    
    (handler-case
        (let* ((request-data (parse-json request-body))
               (request-plist (alist-to-plist request-data))
               (jsonrpc (getf request-plist :jsonrpc))
               (method (getf request-plist :method))
               (params (getf request-plist :params))
               (id (getf request-plist :id)))
          
          (debug-log "Parsed JSON-RPC request: method=~A, id=~A" method id)
          
          (let* ((response (cond
                             ((not (equal jsonrpc "2.0"))
                              (make-json-rpc-error id -32600 "Invalid Request: Not JSON-RPC 2.0"))
                             ((not method)
                              (make-json-rpc-error id -32600 "Invalid Request: No method specified"))
                             (t (dispatch-request method params id))))
                 (json-response (cl-json:encode-json-to-string response)))
            
            (debug-log "Sending JSON-RPC response (~A chars): ~A" 
                       (length json-response)
                       (if (> (length json-response) 500)
                           (concatenate 'string (subseq json-response 0 500) "...")
                           json-response))
            
            (with-http-response (req ent :content-type "application/json")
              (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
                (write-string json-response *html-stream*)))))
      
      (error (e)
        (debug-log "Error processing JSON-RPC request: ~A" e)
        (let ((error-response (cl-json:encode-json-to-string 
                              (make-json-rpc-error "null" -32603 
                                                 (format nil "Internal error: ~A" e)))))
          (with-http-response (req ent :content-type "application/json")
            (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
              (write-string error-response *html-stream*))))))))

;;; Main MCP HTTP handler

(defun setup-mcp-handler (req ent)
  "Handle incoming MCP requests via HTTP"
  (let ((method (make-keyword (request-method req)))
        (content-type (header-slot-value req :content-type))
        (content-length (let ((value (header-slot-value req :content-length)))
                          (when value (parse-integer value))))
        (path (request-uri req)))
    
    (debug-log "MCP request received: ~A ~A (~A bytes) path: ~A" 
              method content-type content-length path)
    
    (case method
      ;; GET requests establish SSE connection
      (:get
       (debug-log "Handling GET request as SSE connection")
       (handle-sse-connection req ent))
      
      ;; POST requests handle JSON-RPC
      (:post
       (debug-log "Handling POST request with content-type: ~A" content-type)
       (handle-json-rpc-request req ent))
      
      ;; Unhandled HTTP method
      (otherwise
       (debug-log "Unhandled HTTP method: ~A" method)
       (with-http-response (req ent :content-type "application/json")
         (with-http-body (req ent)
           (write-string 
            "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Method not allowed\"},\"id\":null}" 
            *html-stream*)))))))

;;; Tool implementation functions

(defun ping-gendl ()
  "Ping the Gendl server to check if it's ready"
  (debug-log "Pinging Gendl server")
  (handler-case
      (progn
        (http-request "/mcp/ping-gendl" :method "GET")
        "Gendl MCP server is ready for Claude interaction!")
    (error (e)
      (debug-log "Error pinging Gendl: ~A" e)
      (error "Gendl server is not responding: ~A" e))))

(defun lisp-eval (code-string)
  "Evaluate Lisp code in the Gendl environment"
  (debug-log "Evaluating Lisp code: ~A" code-string)
  (let ((response (http-request "/mcp/lisp-eval"
                              :method "POST"
                              :headers '(("Content-Type" . "application/json"))
                              :body (alist-to-json `((:code . ,code-string))))))
    (if (stringp (getf response :response))
        (let ((result (handler-case 
                          (parse-json (getf response :response))
                        (error (e) 
                          (debug-log "Error parsing result: ~A" e)
                          (getf response :response)))))
          (cond ((and (listp result) (getf result :result))
                 (format nil "Result: ~A" (getf result :result)))
                ((and (listp result) (getf result :error))
                 (error "Error executing code: ~A" (getf result :error)))
                (t (getf response :response))))
        (getf response :response))))

(defun http-request (path &key (host *gendl-host*) (method "GET") headers body)
  "Make an HTTP request to the Gendl server"
  (debug-log "Making HTTP request: ~A ~A~@[ headers: ~S~]~@[ body: ~S~]" 
             method path headers (and body (if (> (length body) 500) 
                                            (concatenate 'string (subseq body 0 500) "...")
                                            body)))
  
  (let ((uri (format nil "http://~A:~D~A" host *gendl-port* path)))
    (multiple-value-bind (response response-code response-headers redirected-url)
        (net.aserve.client:do-http-request uri  
                                          :method (make-keyword method)
                                          :headers headers
                                          :content body)
      
      (debug-log "HTTP response status: ~D" response-code)
      
      (cond ((and (<= 200 response-code 299))
             (list :response response 
                   :response-code response-code
                   :response-headers response-headers 
                   :redirected-url redirected-url))
            (t (error "HTTP request failed: ~D" response-code))))))

;;; End of file
