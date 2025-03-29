;;;; mcp-server-wrapper-simple.lisp
;;;; Simplified MCP server implementation for direct use with mcp-remote proxy
;;;; This allows Claude to communicate directly with Gendl via JSON-RPC

(in-package :gendl-mcp)

;;; Configuration parameters
(defparameter *gendl-port* *http-port* "Port on which the Gendl HTTP server is listening")
(defparameter *gendl-host* "127.0.0.1" "Host on which the Gendl server is running")
(defparameter *debug-mode* t "Enable debug output")

;;; Logging helper
(defun debug-log (format-string &rest args)
  "Log a debug message if debug mode is enabled"
  (when *debug-mode*
    (apply #'format *error-output* (concatenate 'string "MCP-DEBUG: " format-string "~%") args)
    (force-output *error-output*)))

;;; JSON utilities
(defun parse-json (json-string)
  "Parse a JSON string into a Lisp object"
  (cl-json:decode-json-from-string json-string))

(defun generate-json (lisp-object)
  "Generate a JSON string from a Lisp object"
  (cl-json:encode-json-to-string lisp-object))

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

;;; MCP Protocol Handlers
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

(defun handle-tools-list-request (id)
  "Handle MCP tools/list requests"
  (debug-log "Handling tools/list request")
  
  (make-json-rpc-response id 
                         `((:tools . ,(list
                                       ;; ping_gendl tool
                                       `((:name . "ping_gendl")
                                         (:description . "Check if the Gendl server is available")
                                         (:inputSchema . ((:type . "object")
                                                         (:properties . ())
                                                         (:required . ()))))
                                       
                                       ;; lisp_eval tool
                                       `((:name . "lisp_eval")
                                         (:description . "Evaluate Lisp code on the Gendl server")
                                         (:inputSchema . ((:type . "object")
                                                         (:properties . ((:code . ((:type . "string")
                                                                                  (:description . "The Lisp code to evaluate")))))
                                                         (:required . ("code")))))
                                       
                                       ;; http_request tool
                                       `((:name . "http_request")
                                         (:description . "Make an HTTP request to any endpoint on the Gendl server")
                                         (:inputSchema . ((:type . "object")
                                                         (:properties . ((:method . ((:type . "string")
                                                                                    (:description . "HTTP method (GET, POST, PUT, DELETE, etc.)")
                                                                                    (:default . "GET")))
                                                                         (:path . ((:type . "string")
                                                                                  (:description . "The path part of the URL (e.g., /color-map)")))
                                                                         (:body . ((:type . "string")
                                                                                  (:description . "Request body for POST, PUT, etc.")))
                                                                         (:headers . ((:type . "object")
                                                                                      (:description . "HTTP headers as key-value pairs")
                                                                                      (:additionalProperties . ((:type . "string")))))))
                                                         (:required . ("path")))))
                                       
                                       ;; query_gendl_kb tool
                                       `((:name . "query_gendl_kb")
                                         (:description . "Search the Gendl documentation knowledge base for information about Gendl/GDL")
                                         (:inputSchema . ((:type . "object")
                                                         (:properties . ((:query . ((:type . "string")
                                                                                   (:description . "The search query about Gendl/GDL")))))
                                                         (:required . ("query"))))))))))

(defun handle-resources-list-request (id)
  "Handle MCP resources/list requests"
  (debug-log "Handling resources/list request")
  (make-json-rpc-response id `((:resources . ()))))

(defun handle-prompts-list-request (id)
  "Handle MCP prompts/list requests"
  (debug-log "Handling prompts/list request")
  (make-json-rpc-response id `((:prompts . ()))))

;;; Tool Handlers
(defun handle-tool-call (params id)
  "Handle MCP tools/call requests"
  (let* ((params-plist (cond ((listp params) 
                             (if (consp (car params))
                                 ;; Convert alist to plist
                                 (loop for (key . value) in params
                                       nconc (list (intern (string key) :keyword) value))
                                 params))
                            (t (list :error "Invalid params"))))
         (tool-name (getf params-plist :name))
         (tool-args (getf params-plist :arguments)))
    
    (debug-log "Handling tool call: ~A with args: ~S" tool-name tool-args)
    
    (handler-case
        (cond
          ;; Tool: ping_gendl
          ((string= tool-name "ping_gendl")
           (make-text-response id "Gendl MCP server is ready for Claude interaction!"))
          
          ;; Tool: lisp_eval
          ((string= tool-name "lisp_eval")
           (let ((code (getf tool-args :code)))
             (if code
                 (make-text-response id (format nil "Result: ~A" (eval (read-from-string code))))
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
                 (make-text-response id (format nil "Query result for: ~A" query))
                 (make-json-rpc-error id -32602 "Missing required parameter: query"))))
          
          ;; Unknown tool
          (t (make-json-rpc-error id -32601 (format nil "Unknown tool: ~A" tool-name))))
      
      (error (e)
        (debug-log "Error in tool call: ~A" e)
        (make-json-rpc-error id -32603 (format nil "Error calling tool: ~A" e))))))

;;; Request Dispatcher
(defun dispatch-request (method params id)
  "Dispatch the request to the appropriate handler"
  (debug-log "Dispatching method: ~A with params: ~S" method params)
  
  (cond
    ;; MCP protocol initialization
    ((string= method "initialize")
     (handle-initialize-request params id))
    
    ;; MCP list-type endpoints
    ((string= method "tools/list")
     (handle-tools-list-request id))
    
    ((string= method "resources/list")
     (handle-resources-list-request id))
    
    ((string= method "prompts/list")
     (handle-prompts-list-request id))
    
    ;; MCP tool call
    ((string= method "tools/call")
     (handle-tool-call params id))
    
    ;; Unknown method
    (t (make-json-rpc-error id -32601 (format nil "Method not found: ~A" method)))))

;;; HTTP Handlers
(defun handle-sse-connection (req ent)
  "Handle Server-Sent Events (SSE) connection for MCP events"
  (debug-log "Setting up SSE connection")
  (let ((path (uri-path (request-uri req))))
    (debug-log "SSE connection path: ~A" path)
    
    (with-http-response (req ent :content-type "text/event-stream" :response *response-ok*)
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

(defun handle-json-rpc-request (req ent)
  "Handle JSON-RPC requests"
  (let ((request-body (get-request-body req)))
    (debug-log "Processing JSON-RPC request: ~A" request-body)
    
    (handler-case
        (let* ((request-data (parse-json request-body))
               (jsonrpc (cdr (assoc :jsonrpc request-data)))
               (method (cdr (assoc :method request-data)))
               (params (cdr (assoc :params request-data)))
               (id (cdr (assoc :id request-data))))
          
          (debug-log "Parsed JSON-RPC request: method=~A, id=~A" method id)
          
          (let* ((response (cond
                             ((not (equal jsonrpc "2.0"))
                              (make-json-rpc-error id -32600 "Invalid Request: Not JSON-RPC 2.0"))
                             ((not method)
                              (make-json-rpc-error id -32600 "Invalid Request: No method specified"))
                             (t (dispatch-request method params id))))
                 (json-response (generate-json response)))
            
            (debug-log "Sending JSON-RPC response: ~A" 
                       (if (> (length json-response) 500)
                           (concatenate 'string (subseq json-response 0 500) "...")
                           json-response))
            
            (with-http-response (req ent :content-type "application/json" :response *response-ok*)
              (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
                (write-string json-response *html-stream*)))))
      
      (error (e)
        (debug-log "Error processing JSON-RPC request: ~A" e)
        (let ((error-response (generate-json 
                              (make-json-rpc-error "null" -32603 
                                                 (format nil "Internal error: ~A" e)))))
          (with-http-response (req ent :content-type "application/json" :response *response-ok*)
            (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
              (write-string error-response *html-stream*))))))))

;;; Main MCP HTTP handler
(defun setup-mcp-handler (req ent)
  "Handle incoming MCP requests via HTTP"
  (let ((method (make-keyword (request-method req)))
        (path (uri-path (request-uri req))))
    
    (debug-log "MCP request received: ~A path: ~A" method path)
    
    (cond
      ;; Handle SSE connection request
      ((and (eq method :get) (equal path "/mcp/sse"))
       (debug-log "Handling GET request to /mcp/sse as SSE connection")
       (handle-sse-connection req ent))
      
      ;; Handle JSON-RPC requests (both SSE and regular endpoint for compatibility)
      ((and (eq method :post) (or (equal path "/mcp") (equal path "/mcp/")))
       (debug-log "Handling POST request to /mcp as JSON-RPC")
       (handle-json-rpc-request req ent))
      
      ;; Handle GET to root MCP endpoint - redirect to SSE endpoint
      ((and (eq method :get) (or (equal path "/mcp") (equal path "/mcp/")))
       (debug-log "Redirecting GET request to /mcp to /mcp/sse")
       (with-http-response (req ent :response *response-moved-permanently*)
         (setf (reply-header-slot-value req :location) "/mcp/sse")
         (with-http-body (req ent)
           (html (:html (:body (:p "Redirecting to /mcp/sse")))))))
      
      ;; Unhandled request
      (t
       (debug-log "Unhandled request: ~A ~A" method path)
       (with-http-response (req ent :response *response-not-found*)
         (with-http-body (req ent)
           (html (:html (:body (:p "Not found"))))))))))

;;; Utility functions
(defun http-request (path &key (host *gendl-host*) (method "GET") headers body)
  "Make an HTTP request to the Gendl server"
  (debug-log "Making HTTP request: ~A ~A" method path)
  
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

;;; Define URL handlers
(with-all-servers
    (server)
    (publish :server server :path "/mcp" :function 'setup-mcp-handler)
    (publish :server server :path "/mcp/" :function 'setup-mcp-handler)
    (publish :server server :path "/mcp/sse" :function 'setup-mcp-handler))

(debug-log "MCP endpoints registered at /mcp, /mcp/, and /mcp/sse")
