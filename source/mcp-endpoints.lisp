(in-package :gendl-mcp)

;;;; MCP Supported Endpoints

;; Define supported MCP endpoints for common Gendl operations
;; including those needed for MCP handshaking e.g. /mcp/tools/list

(defun initialize-standard-endpoints ()
  "Set up standard MCP endpoints."
  
  (with-all-servers
      (server)

      ;; toplevel mcp handler. 
      (net.aserve:publish :path "/mcp"
			  :server server
			  :content-type "application/json"
			  :function 'setup-mcp-handler)
      
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
	       #+nil
	       #'(lambda (req ent)
		   (format t "At 1~%")
		   (with-http-response (req ent)
		     (with-http-body (req ent)
		       (write-string "Body: " (request-reply-stream req))
		       (write-string (get-request-body req) (request-reply-stream req)))))

	       
               #'(lambda (req ent)
		   (format t "At 1~%")
		   (let* ((body-string (get-request-body req))
			  (json-input (when (plusp (length body-string))
					(with-input-from-string (stream body-string)
					  (json:decode-json stream))))
			  (code (rest (assoc :code json-input)))
			  result
			  (error (unless json-input
				   (format nil "Malformed or missing input.
This endpoint needs a json object with {code: <lisp-expression>}. What we got was:

  ~s

" body-string)))
			  success)
		     (when (and (null error) code)
		       (handler-case
			   (progn
			     (setq result (eval (read-safe-string code)))
			     (setq success t))
			 (error (condition)
			   (setq error (format nil "~a" condition))
			   (setq success nil))))
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


(initialize-standard-endpoints)


