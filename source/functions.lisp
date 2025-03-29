(in-package :gendl-mcp)

;;;; MCP Protocol for Claude integration

;; This file contains functions to help define the Model Context Protocol
;; for Claude to interact with Gendl objects

(defun generate-mcp-tool-description ()
  "Generate a tool description for Claude MCP integration."
  ;; Generate JSON tool descriptions in the format expected by MCP
  (with-output-to-string (ss)
    (json:encode-json
     `((:tools .
               ,(vector
                 (make-tool-def
                  :name "ping_gendl"
                  :description "Check if the Gendl server is available"
                  :properties nil
                  :required nil)
                
                 (make-tool-def
                  :name "lisp_eval"
                  :description "Evaluate Lisp code on the Gendl server"
                  :properties '(("code" . (("type" . "string")
                                           ("description" . "The Lisp code to evaluate"))))
                  :required '("code"))

                 (make-tool-def
                  :name "http_request"
                  :description "Make an HTTP request to any endpoint on the Gendl server"
                  :properties '(("path" . (("type" . "string")
                                          ("description" . "The path part of the URL (e.g., /color-map)")))
                               ("method" . (("type" . "string")
                                           ("description" . "HTTP method (GET, POST, PUT, DELETE, etc.)")
                                           ("default" . "GET")))
                               ("headers" . (("type" . "object")
                                            ("description" . "HTTP headers as key-value pairs")
                                            ("additionalProperties" . (("type" . "string")))))
                               ("body" . (("type" . "string")
                                        ("description" . "Request body for POST, PUT, etc.")))
                               ("rawResponse" . (("type" . "boolean")
                                               ("description" . "If true, return the raw response instead of parsing it")
                                               ("default" . "false"))))
                  :required '("path"))

                 (make-tool-def
                  :name "query_gendl_kb"
                  :description "Search the Gendl documentation knowledge base for information about Gendl/GDL, a General-purpose Declarative Language"
                  :properties '(("query" . (("type" . "string")
                                           ("description" . "The search query about Gendl/GDL"))))
                  :required '("query"))
                 
                 ))) ss)))


(defun make-tool-def (&key name description properties required)
  "Helper function to create a consistent tool definition structure."
  `(("name" . ,name)
    ("description" . ,description)
    ("inputSchema" . (("type" . "object")
                     ("properties" . ,(or properties (make-hash-table)))
                     ("required" . ,(or required (vector)))))))

;; Configuration for Claude desktop
(defun generate-mcp-openapi-spec (&key (local? t))
  "Generate an OpenAPI specification for Claude MCP integration."
  ;; Returns string with API spec suitable for inclusion in claude-desktop-json.conf
  (let ((local-endpoint "http://127.0.0.1:9081/mcp")
        (prod-endpoint "https://gornskew.com/mcp"))
    (format nil "~
{
  \"tools\": {
    \"gendl\": {
      \"url\": \"~A\"
    }
  }
}
"
            (if local? local-endpoint prod-endpoint))))

