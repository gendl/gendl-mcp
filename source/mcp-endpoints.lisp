(in-package :gendl-mcp)

;;;; MCP Standard Endpoints

;; Define standard MCP endpoints for common Gendl operations

(defun initialize-standard-endpoints ()
  "Set up standard MCP endpoints."
  
  (with-all-servers
   (server)
   
   ;; Basic ping endpoint
   (publish
    :path "/mcp/claude/ping"
    :server server
    :function
    #'(lambda(req ent)
        (with-http-response (req ent)
          (with-http-body (req ent)
            (let ((stream (request-reply-stream req)))
              (format stream
                      "Gendl MCP server is ready for Claude interaction!"))))))

   ;; REQUIRED ENDPOINTS FOR CLAUDE MCP

   ;; Tools list endpoint
   (publish
    :path "/mcp/tools/list"
    :server server
    :function
    #'(lambda(req ent)
        (with-http-response (req ent)
          (with-http-body (req ent)
            (let ((stream (request-reply-stream req)))
              (json:encode-json
               `((:tools . (
                           (("name" . "ping_gendl")
                            ("description" . "Check if the Gendl server is available")
                            ("inputSchema" . (("properties" . nil)
                                              ("required" . nil)
                                              ("type" . "object"))))
                           
                           (("name" . "list_objects")
                            ("description" . "List all available Gendl objects")
                            ("inputSchema" . (("properties" . nil)
                                              ("required" . nil)
                                              ("type" . "object"))))
                           
                           (("name" . "make_object")
                            ("description" . "Create a new Gendl object instance")
                            ("inputSchema" . (("properties" . (("type" . (("type" . "string")
                                                                          ("description" . "The Gendl object type to create")))
                                                               ("parameters" . (("type" . "object")
                                                                                ("description" . "Parameters for object creation")))))
                                              ("required" . ("type"))
                                              ("type" . "object"))))
                           
                           (("name" . "send_message")
                            ("description" . "Send a message to a Gendl object")
                            ("inputSchema" . (("properties" . (("objectId" . (("type" . "integer")
                                                                              ("description" . "ID of the Gendl object")))
                                                               ("message" . (("type" . "string")
                                                                             ("description" . "Message to send to the object")))
                                                               ("args" . (("type" . "object")
                                                                          ("description" . "Arguments for the message (optional)")))))
                                              ("required" . ("objectId" "message"))
                                              ("type" . "object"))))
                           )))
               stream))))))

   ;; Resources list endpoint
   (publish
    :path "/mcp/resources/list"
    :server server
    :function
    #'(lambda(req ent)
        (with-http-response (req ent)
          (with-http-body (req ent)
            (let ((stream (request-reply-stream req)))
              (json:encode-json
               '((:resources . ()))
               stream))))))

   ;; Prompts list endpoint
   (publish
    :path "/mcp/prompts/list"
    :server server
    :function
    #'(lambda(req ent)
        (with-http-response (req ent)
          (with-http-body (req ent)
            (let ((stream (request-reply-stream req)))
              (json:encode-json
               '((:prompts . ()))
               stream))))))

   ;; Tool call endpoint - using query parameters
   (publish
    :path "/mcp/tools/call"
    :server server
    :function
    #'(lambda(req ent)
        (let ((tool-name (cdr (assoc "tool" (request-query req)))))
          (with-http-response (req ent)
            (with-http-body (req ent)
              (let ((stream (request-reply-stream req)))
                (format stream "Tool call to ~a not implemented yet" tool-name)))))))

   ;; Other original endpoints

   (publish
    :path "/mcp/tools"
    :server server
    :function
    #'(lambda(req ent)
        (with-http-response (req ent)
          (with-http-body (req ent)
            (let ((stream (request-reply-stream req)))
              (write-string (generate-mcp-tool-description) stream))))))

   (publish
    :path "/mcp/specs"
    :server server
    :function
    #'(lambda(req ent)
        (with-http-response (req ent)
          (with-http-body (req ent)
            (let ((stream (request-reply-stream req)))
              (write-string (generate-mcp-openapi-spec) stream))))))

   
   (publish :path "/mcp/objects"
            :server server 
            :function #'(lambda(req ent)
                          (with-http-response (req ent)
                            (with-http-body (req ent)
                              (let ((stream (request-reply-stream req)))
                                ;; return nil for testing
                                (json:encode-json nil stream))))))

   (publish :path "/mcp/make-object"
            :server server
            :function #'(lambda(req ent)
                          (with-http-response (req ent)
                            (with-http-body (req ent)
                              (let ((stream (request-reply-stream req)))
                                ;; return 0 for testing
                                (json:encode-json 0 stream))))))

   (publish :path "/mcp/the-object"
            :server server
            :function #'(lambda(req ent)
                          (with-http-response (req ent)
                            (with-http-body (req ent)
                              (let ((stream (request-reply-stream req)))
                                ;; return :%%unbound%% for testing
                                (json:encode-json :%%unbound%% stream))))))

   ;; Shorthand for the-object.
   ;; FLAG -- copy ^^above^^ `the-object`  code into here whenever changed:
   (publish :path "/mcp/theo"
            :server server
            :function #'(lambda(req ent)
                          (with-http-response (req ent)
                            (with-http-body (req ent)
                              (let ((stream (request-reply-stream req)))
                                ;; return :%%unbound%% for testing
                                (json:encode-json :%%unbound%% stream))))))))
