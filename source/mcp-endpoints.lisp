(in-package :gendl-mcp)

;;;; MCP Supported Endpoints

;; Define supported MCP endpoints for common Gendl operations
;; including those needed for MCP handshaking e.g. /mcp/tools/list

(defun initialize-standard-endpoints ()
  "Set up standard MCP endpoints."
  
  (with-all-servers
      (server)
   
      ;; Basic ping endpoint for alive test
      (publish
       :path "/mcp/ping-gendl"
       :server server
       :function
       #'(lambda(req ent)
           (with-http-response (req ent)
             (with-http-body (req ent)
               (let ((stream (request-reply-stream req)))
		 (format stream
			 "Gendl MCP server is ready for Claude interaction!"))))))

      ;; General-purpose evaluation including compile-file, load-file,
      ;; ql:quickload of systems etc. - powerful so need to be careful
      ;; with this.
      (publish :path "/mcp/lisp-eval"
	       :server server
               :content-type "application/json"
               :function 
               #'(lambda (req ent)
		   (let* ((json-input (with-input-from-string
					  (stream (get-request-body req))
					(json:decode-json stream)))
			  (code (rest (assoc :code json-input)))
			  result error success)
		     (handler-case
			 (progn
			   (setq result (eval (read-safe-string code)))
			   (setq success t))
		       (error (condition)
			 (setq error (format nil "~a" condition))
			 (setq success nil)))
		     (with-http-response (req ent)
		       (with-http-body (req ent)
			 (json:encode-json-alist 
			  `(("success" . ,success)
			    (result . ,(format nil "~s" result))
			    ,(when error `("error" . ,error)))
			  (request-reply-stream req)))))))

   
      ;; Tools list endpoint - required for MCP initialization
      (publish
       :path "/mcp/tools/list"
       :server server
       :content-type "application/json"
       :function
       #'(lambda(req ent)
           (with-http-response (req ent)
             (with-http-body (req ent)
               (let ((stream (request-reply-stream req)))
		 (write-string (generate-mcp-tool-description) stream))))))


      ;; Configuration endpoint - should give the specs which need to be written
      ;; into claude_desktop_config.json. 
      (publish
       :path "/mcp/specs"
       :server server
       :function
       #'(lambda(req ent)
           (with-http-response (req ent)
             (with-http-body (req ent)
	       (let ((stream (request-reply-stream req)))
		 (write-string (generate-mcp-openapi-spec) stream))))))))

;; Generic HTTP request handler - allows Claude to interact with any Gendl HTTP endpoint
(publish :path "/mcp/http-request"
         :server *default-aserve-server*
         :content-type "application/json"
         :function 
         #'(lambda (req ent)
             (let* ((json-input (with-input-from-string
                                   (stream (get-request-body req))
                                 (json:decode-json stream)))
                    (method (or (cdr (assoc :method json-input)) "GET"))
                    (path (cdr (assoc :path json-input)))
                    (headers (cdr (assoc :headers json-input)))
                    (query-params (cdr (assoc :query-params json-input)))
                    (body (cdr (assoc :body json-input)))
                    result success status headers-out redirect response-error)
               
               ;; Validate required parameters
               (unless path
                 (with-http-response (req ent)
                   (with-http-body (req ent)
                     (json:encode-json-alist
                      `(("success" . nil)
                        ("error" . "Missing required parameter: path"))
                      (request-reply-stream req))))
                 (return-from nil))
               
               ;; Handle the actual HTTP request internally
               (handler-case
                   (multiple-value-bind (body-result result-status headers-result redirect-location)
                       (net.aserve.client:do-http-request 
                        (format nil "http://127.0.0.1:~A~A" 
                                net.aserve::*aserve-port* path)
                        :method method
                        :query query-params
                        :headers headers
                        :content body
                        :format :text)
                     (setq result body-result
                           status result-status
                           headers-out headers-result
                           redirect redirect-location
                           success t))
                 (error (condition)
                   (setq response-error (format nil "~a" condition))
                   (setq success nil)))
               
               ;; Return the response with structured data
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (json:encode-json-alist
                    `(("success" . ,success)
                      ("status" . ,status)
                      ("body" . ,result)
                      ("headers" . ,(when headers-out 
                                      (loop for (k . v) in headers-out
                                            collect (cons (string-downcase k) v))))
                      ("redirect" . ,redirect)
                      ,(when response-error `("error" . ,response-error)))
                    (request-reply-stream req))))))

(initialize-standard-endpoints)


