;;;; mcp-server-final.lisp
;;;; MCP server implementation specifically designed for use with mcp-remote
;;;; This correctly implements the SSE and JSON-RPC endpoints as expected by the Model Context Protocol

(in-package :gendl-mcp)

;;; Configuration parameters
(defparameter *gendl-port* *http-port* "Port on which the Gendl HTTP server is listening")
(defparameter *gendl-host* "127.0.0.1" "Host on which the Gendl server is running")
(defparameter *debug-mode* t "Enable debug output")

;;; Logging helper
(defun debug-log (format-string &rest args)
  "Log a debug message if debug mode is enabled"
  (when *debug-mode*
    (apply #'format *error-output* (concatenate 'string "[MCP-DEBUG] " format-string "~%") args)
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
  "Handle MCP initialization requests with robust error handling"
  (debug-log "Handling initialize request with id: ~A and params: ~S" id params)
  
  (handler-case
      (make-json-rpc-response id 
                             `((:protocolVersion . "2024-11-05")
                               (:capabilities . ((:experimental . ())
                                               (:prompts . ((:listChanged . nil)))
                                               (:resources . ((:subscribe . nil) 
                                                             (:listChanged . nil)))
                                               (:tools . ((:listChanged . nil)))))
                               (:serverInfo . ((:name . "gendl-mcp-lisp")
                                             (:version . "1.0.0")))))
    (error (e)
      (debug-log "Error in initialize request handler: ~A" e)
      (make-json-rpc-error id -32603 (format nil "Internal error in initialize: ~A" e)))))

(defun handle-tools-list-request (id)
  "Handle MCP tools/list requests"
  (debug-log "Handling tools/list request with id: ~A" id)
  
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
  (debug-log "Handling resources/list request with id: ~A" id)
  (make-json-rpc-response id `((:resources . ()))))

(defun handle-prompts-list-request (id)
  "Handle MCP prompts/list requests"
  (debug-log "Handling prompts/list request with id: ~A" id)
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
  "Dispatch the request to the appropriate handler with better logging"
  (debug-log "Dispatching method: ~A with params: ~S and id: ~A" method params id)
  
  (handler-case
      (cond
        ;; MCP protocol initialization
        ((string= method "initialize")
         (debug-log "Handling initialize request with id: ~A" id)
         (handle-initialize-request params id))
        
        ;; MCP list-type endpoints
        ((string= method "tools/list")
         (debug-log "Handling tools/list request with id: ~A" id)
         (handle-tools-list-request id))
        
        ((string= method "resources/list")
         (debug-log "Handling resources/list request with id: ~A" id)
         (handle-resources-list-request id))
        
        ((string= method "prompts/list")
         (debug-log "Handling prompts/list request with id: ~A" id)
         (handle-prompts-list-request id))
        
        ;; MCP tool call
        ((string= method "tools/call")
         (debug-log "Handling tools/call request with id: ~A" id)
         (handle-tool-call params id))
        
        ;; Unknown method
        (t
         (debug-log "Unknown method: ~A" method)
         (make-json-rpc-error id -32601 (format nil "Method not found: ~A" method))))
    
    (error (e)
      (debug-log "Error dispatching request: ~A" e)
      (make-json-rpc-error id -32603 (format nil "Internal error in dispatch: ~A" e)))))



(defun handle-json-rpc-request (req ent)
  "Handle JSON-RPC requests with enhanced error handling"
  (debug-log "Processing JSON-RPC request from ~A" (request-uri req))
  
  (handler-case
      (let ((request-body (get-request-body req)))
        (debug-log "Request body: ~A" request-body)
        
        (if (or (null request-body) (string= request-body ""))
            (progn
              (debug-log "Empty request body")
              (with-http-response (req ent :response *response-bad-request*)
                (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
                  (write-string (generate-json 
                                (make-json-rpc-error "null" -32700 "Parse error: Empty request body"))
                               *html-stream*))))
            
            (handler-case
                (let* ((request-data (parse-json request-body))
                       (jsonrpc (rest (assoc :jsonrpc request-data)))
                       (method (rest (assoc :method request-data)))
                       (params (rest (assoc :params request-data)))
                       (id (rest (assoc :id request-data))))
                  
                  (debug-log "Parsed JSON-RPC request: method=~A, id=~A, params=~A" 
                            method id params)
                  
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
                    
                    ;; Ensure proper CORS headers for all responses
                    (with-http-response (req ent :content-type "application/json")
                      (with-http-body (req ent :headers '((:access-control-allow-origin . "*")
                                                         (:cache-control . "no-cache")
                                                         (:access-control-allow-methods . "GET, POST, OPTIONS")))
                        (write-string json-response *html-stream*)))))
              
              (error (e)
                (debug-log "Error parsing JSON-RPC request: ~A" e)
                (with-http-response (req ent :content-type "application/json")
                  (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
                    (write-string (generate-json 
                                  (make-json-rpc-error "null" -32700 
                                                     (format nil "Parse error: ~A" e)))
                                 *html-stream*)))))))
    
    (error (e)
      (debug-log "Critical error processing JSON-RPC request: ~A" e)
      (with-http-response (req ent :content-type "application/json")
        (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
          (write-string (generate-json 
                        (make-json-rpc-error "null" -32603 
                                           (format nil "Internal error: ~A" e)))
                       *html-stream*))))))

;;; Improved SSE connection handler with better resilience
(defun handle-sse-connection (req ent)
  "Handle Server-Sent Events (SSE) connection for MCP events with improved resilience"
  (debug-log "Setting up SSE connection at ~A" (request-uri req))
  
  ;; Use more complete headers for better SSE compatibility
  (with-http-response (req ent :content-type "text/event-stream")
    (with-http-body (req ent :headers '((:cache-control . "no-cache, no-transform")
                                       (:connection . "keep-alive")
                                       (:access-control-allow-origin . "*")
                                       (:content-type . "text/event-stream")))
      
      ;; Send initial connected event with proper formatting and flush buffer
      (debug-log "Sending connected event")
      (format *html-stream* "id: connected~%data: {\"jsonrpc\":\"2.0\",\"method\":\"connected\",\"params\":null}~%~%")
      (force-output *html-stream*)
      (debug-log "Connected event sent successfully")
      
      ;; Add a comment line which helps with some SSE clients
      (format *html-stream* ": keepalive comment~%~%")
      (force-output *html-stream*)
      
      ;; Create heartbeat thread with improved handling
      (let ((req-stream *html-stream*))
        (bt2:make-thread
         (lambda()
           (debug-log "Starting heartbeat thread")
           (let ((counter 0))
             (loop
               (handler-case
                   (progn
                     ;; Use shorter interval for first few heartbeats, then longer
                     (if (< counter 3)
                         (sleep 5)  ;; Very short interval for first heartbeats
                         (sleep 15)) ;; Normal interval after connection established
                     
                     (incf counter)
                     (debug-log "Heartbeat attempt ~A" counter)
                     
                     ;; Check if stream is open before trying to write
                     (if (open-stream-p req-stream)
                         (handler-case
                             (progn
                               ;; First send a comment as a keep-alive
                               (format req-stream ": keepalive ~A~%~%" counter)
                               (force-output req-stream)
                               
                               ;; Then send the actual heartbeat event
                               (format req-stream "id: heartbeat-~A~%data: {\"jsonrpc\":\"2.0\",\"method\":\"heartbeat\",\"params\":null}~%~%" counter)
                               (force-output req-stream)
                               (debug-log "Heartbeat ~A sent successfully" counter))
                           
                           ;; Handle any errors during write
                           (error (e)
                             (debug-log "Error while writing heartbeat ~A: ~A" counter e)
                             (return-from nil nil)))
                         
                         ;; If stream is closed, exit the thread
                         (progn
                           (debug-log "Stream closed, stopping heartbeat thread")
                           (return-from nil nil))))
                 
                 ;; Catch any other errors in the main loop
                 (error (e)
                   (debug-log "Critical error in heartbeat thread: ~A" e)
                   (return-from nil nil)))))
           
           (debug-log "Heartbeat thread exiting gracefully"))))
      
      ;; Block the main thread to keep connection alive
      (ignore-errors
       (loop
         (sleep 60))))))

;;; Main MCP HTTP handlers for specific endpoints
(defun handle-mcp-sse (req ent)
  "Handle the /mcp/sse endpoint for SSE connections"
  (debug-log "MCP SSE request received at /mcp/sse")
  (handle-sse-connection req ent))

(defun handle-mcp-json-rpc (req ent)
  "Handle the /mcp endpoint for JSON-RPC requests and SSE connections"
  (debug-log "At 1 in handle-mcp-json-rpc")
  (debug-log "path is ~a" (uri-path (request-uri req)))
  (let ((method (make-keyword (request-method req))))
    (debug-log "MCP request received at /mcp: ~A" method)
    
    (case method
      (:post 
       (debug-log "Handling JSON-RPC POST request at /mcp")
       (handle-json-rpc-request req ent))
      
      (:options 
       ;; Handle CORS preflight requests
       (debug-log "Handling CORS preflight request")
       (with-http-response (req ent)
         (with-http-body (req ent :headers '((:access-control-allow-origin . "*")
                                           (:access-control-allow-methods . "GET, POST, OPTIONS")
                                           (:access-control-allow-headers . "Content-Type, Authorization, Accept")
                                           (:access-control-max-age . "86400")))
           (write-string "" *html-stream*))))
      
      (:get
       ;; Check if this is an SSE connection request
       (let ((accept-header (header-slot-value req :accept)))
         (debug-log "Accept header: ~A" accept-header)
         (if (and accept-header (search "text/event-stream" accept-header))
             (progn
               (debug-log "Handling SSE connection request at /mcp")
               (handle-sse-connection req ent))
             
             ;; For GET requests that aren't SSE, return 405
             (progn
               (debug-log "Non-SSE GET request rejected")
               (with-http-response (req ent :response *response-method-not-allowed*)
                 (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
                   (write-string "Method not allowed" *html-stream*)))))))
      
      ;; For any other methods, return 405 Method Not Allowed
      (otherwise
       (with-http-response (req ent :response *response-method-not-allowed*)
         (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
           (write-string "Method not allowed" *html-stream*)))))))

;;; Handle test endpoint
(defun handle-mcp-test (req ent)
  "Handle test endpoint for verifying server functionality"
  (debug-log "MCP test request received")
  (with-http-response (req ent :content-type "text/plain")
    (with-http-body (req ent :headers '((:access-control-allow-origin . "*")))
      (write-string "MCP server is running correctly" *html-stream*))))

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

;;; Register URL handlers for specific endpoints
(defun register-mcp-handlers ()
  "Register the MCP endpoints with the HTTP server"
  (with-all-servers
      (server)
      ;; Register the /mcp/sse endpoint for SSE connections
      (publish :server server :path "/mcp/sse" :function 'handle-mcp-sse)
  
      ;; Register the /mcp endpoint for JSON-RPC requests
      (publish :server server :path "/mcp" :function 'handle-mcp-json-rpc :content-type "*/*")
  
      ;; Add specific endpoint for model-context-protocol connections
      (publish :server server :path "/mcp/initialize" :function 'handle-mcp-json-rpc :content-type "*/*")
      
      ;; Add test endpoint
      (publish :server server :path "/mcp/test" :function 'handle-mcp-test))
  
  (debug-log "MCP endpoints registered:
  - /mcp/sse for SSE connections
  - /mcp for JSON-RPC requests
  - /mcp/initialize for direct initialization requests
  - /mcp/test for connection testing"))

;; Register the handlers when this file is loaded
(register-mcp-handlers)
