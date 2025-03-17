;;;; mcp-lisp-api.lisp
;;;;
;;;; Provides an HTTP endpoint for Lisp evaluation to support the MCP integration

(in-package :gendl-mcp)

(with-all-servers
    (server)
;;; Define the lisp-eval endpoint
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
			(request-reply-stream req))))))))

(format t "~%MCP Lisp API endpoint loaded~%")
