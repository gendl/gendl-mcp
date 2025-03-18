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
                  :description "Evaluate Lisp code in the Gendl environment"
                  :properties '(("code" . (("type" . "string")
                                           ("description" . "Lisp code to evaluate"))))
                  :required '("code"))
                 
                 ))) ss)))


(defun make-tool-def (&key name description properties required)
  "Helper function to create a consistent tool definition structure."
  `(("name" . ,name)
    ("description" . ,description)
    ("inputSchema" . (("type" . "object")
                     ("properties" . ,(or properties (make-hash-table)))
                     ("required" . ,(or required (vector)))))))

;; Configuration for Claude desktop
(defun generate-mcp-openapi-spec ()
  "Generate an OpenAPI specification for Claude MCP integration."
  ;; Returns string with API spec suitable for inclusion in claude-desktop-json.conf
  "
{
  \"command\": \"wsl\",
  \"args\": [\"node\", \"/home/dcooper8/gornskew/xfer/gendl-mcp-new-broken/mcp-format-wrapper.js\"],
  \"env\": {
      \"NODE_ENV\": \"production\",
      \"DEBUG\": \"*\"
  },
  \"persistent\": true,
  \"timeout\": 60000
}
"
)
