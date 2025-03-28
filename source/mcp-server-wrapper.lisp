;;;; gendl-mcp-wrapper.lisp
;;;; MCP (Model-Claude Protocol) wrapper implementation in Common Lisp
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

;;; JSON parsing and generation 
;; We're assuming cl-json library for this implementation
;; Replace with actual library of choice

(defun parse-json (json-string)
  "Parse a JSON string into a Lisp object"
  (cl-json:decode-json-from-string json-string))

(defun generate-json (lisp-object)
  "Generate a JSON string from a Lisp object"
  (cl-json:encode-json-to-string lisp-object))

(defun alist-to-json (alist)
  (with-output-to-string (ss)
    (json:encode-json-alist alist ss)))


;;; JSON-RPC implementation

(defun make-json-rpc-response (id result &optional error)
  "Create a JSON-RPC response object"
  (let ((response (list :jsonrpc "2.0" :id id)))
    (if error
        (setf (getf response :error) error)
        (setf (getf response :result) result))
    response))

(defun make-json-rpc-error (id code message &optional data)
  "Create a JSON-RPC error response"
  (let ((error-obj (list :code code :message message)))
    (when data
      (setf (getf error-obj :data) data))
    (make-json-rpc-response id nil error-obj)))

(defun handle-json-rpc-request (request-json)
  "Process a JSON-RPC request"
  (debug-log "Received request: ~S" request-json)
  (handler-case
      (let* ((request (parse-json request-json))
             (jsonrpc (getf request :jsonrpc))
             (method (getf request :method))
             (params (getf request :params))
             (id (getf request :id)))
        
        (cond
          ((not (equal jsonrpc "2.0"))
           (make-json-rpc-error id -32600 "Invalid Request: Not JSON-RPC 2.0"))
          
          ((not method)
           (make-json-rpc-error id -32600 "Invalid Request: No method specified"))
          
          (t
           (dispatch-request method params id))))
    
    (error (e)
      (debug-log "Error processing request: ~A" e)
      (make-json-rpc-error "null" -32603 (format nil "Internal error: ~A" e)))))

(defun dispatch-request (method params id)
  "Dispatch the request to the appropriate handler"
  (debug-log "Dispatching method: ~A with params: ~S" method params)
  
  (cond
    ((string= method "ping_gendl")
     (make-json-rpc-response id (ping-gendl)))
    
    ((string= method "lisp_eval")
     (let ((code (getf params :code)))
       (if code
           (make-json-rpc-response id (lisp-eval code))
           (make-json-rpc-error id -32602 "Invalid params: code is required"))))
    
    ((string= method "http_request")
     (let ((path (getf params :path))
           (method (getf params :method "GET"))
           (headers (getf params :headers nil))
           (body (getf params :body nil)))
       (if path
           (make-json-rpc-response id (http-request path :method method :headers headers :body body))
           (make-json-rpc-error id -32602 "Invalid params: path is required"))))
    
    (t
     (make-json-rpc-error id -32601 (format nil "Method not found: ~A" method)))))


(defun setup-mcp-handler (req ent)
  "Handle incoming MCP requests via HTTP"
  (let ((content-type (net.aserve:header-slot-value req :content-type))
        (content-length (let ((value (net.aserve:header-slot-value req :content-length)))
			  (when value (parse-integer value))))
	(reply-stream (request-reply-stream req))
	(request-body (get-request-body req)))

    (debug-log (format nil "setup-mcp-handler called with content-type ~s and content-length ~s...

and content-length class: ~a~%"
		       content-type content-length (class-of content-length)))
    (cond
      ((not (and content-type (search "application/json" content-type :test #'char-equal)))
       (net.aserve:with-http-response (req ent :response *response-bad-request*)
         (net.aserve:with-http-body (req ent)
           (format reply-stream "Content-Type must be application/json"))))
      
      ((or (not content-length) (zerop content-length))
       (net.aserve:with-http-response (req ent :response *response-bad-request*)
         (net.aserve:with-http-body (req ent)
           (format reply-stream "Nonzero Content-Length header is required"))))
      
      (t
       (debug-log "Received raw request: ~S" request-body)
         
       (let ((response (handle-json-rpc-request request-body)))
         (net.aserve:with-http-response (req ent :content-type "application/json")
           (net.aserve:with-http-body (req ent)
             (let ((json-response (generate-json response)))
               (debug-log "Sending response: ~S" json-response)
               (format reply-stream "~A" json-response)))))))))



;;; Implementation of RPC methods

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


;;
;; Why call on to another endpont? Why not just call a Lisp function
;; directly from here? then i suppose the /mcp/lisp-eval http endpoint could
;; become obscelete.
;; 
(defun lisp-eval (code-string)
  "Evaluate Lisp code in the Gendl environment"
  (debug-log "Evaluating Lisp code: ~A" code-string)
  (let ((response (http-request "/mcp/lisp-eval"
                               :method "POST"
                               :headers '(("Content-Type" . "application/json"))
                               :body (alist-to-json `((:code . ,code-string))))))
    (if (stringp response)
        (let ((result (parse-json response)))
          (cond ((getf result :result)
                 (format nil "Result: ~A" (getf result :result)))
                ((getf result :error)
                 (error "Error executing code: ~A" (getf result :error)))
                (t response)))
        response)))

(defun http-request (path &key (host *gendl-host*) (method "GET") headers body)
  "Make an HTTP request to the Gendl server. Body expected to be in json format already."
  (debug-log "Making HTTP request: with:
 method:  ~A
 path:    ~A
 headers: ~a
 body:    ~a
" method path headers body)
  (let ((uri (format nil "http://~A:~D~A" host *gendl-port* path)))
    (multiple-value-bind (response response-code response-headers redirected-url)
	;;
	;; FLAG might have to deal with binary/bivalent content or
	;; response.  might have to specify things in the
	;; do-http-request call such as content-type, external-format,
	;; query, redirect, cookies...
        (net.aserve.client:do-http-request uri  
	  :method (make-keyword method)
          :headers headers
          :content body)


      (debug-log "HTTP response status: ~D" response-code)

      (cond ((and (<= 200 response-code 299)
		  (stringp response))
	     (list :response response :response-code response-code
		   :response-headers response-headers :redirected-url redirected-url))
	    ((<= 200 response-code 299)
	     (error "Unexpected response type"))
	    (t (error "HTTP request failed: ~D" response-code))))))


;;; Initialization

;;; Usage examples:
#|

;; Test ping
(gendl-mcp:ping-gendl)

;; Evaluate Lisp code
(gendl-mcp:lisp-eval "(+ 2 3)")

;; Make an HTTP request
(gendl-mcp:http-request "/some-endpoint" :method "GET")

|#
