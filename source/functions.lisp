(in-package :gendl-mcp)

;;;; MCP Protocol for Claude integration

;; This file contains functions to help define the Model Context Protocol
;; for Claude to interact with Gendl objects

(defun generate-mcp-tool-description ()
  "Generate a tool description for Claude MCP integration."
  ;; Generate JSON tool descriptions in the format expected by MCP
  (with-output-to-string (stream)
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
                 
                 (make-tool-def
                  :name "list_objects"
                  :description "List all available Gendl objects"
                  :properties nil
                  :required nil)
                 
                 (make-tool-def
                  :name "make_object"
                  :description "Create a new Gendl object instance"
                  :properties '(("type" . (("type" . "string")
                                          ("description" . "The type of Gendl object to create")))
                               ("parameters" . (("type" . "object")
                                               ("description" . "Parameters for object creation"))))
                  :required '("type"))
                 
                 (make-tool-def
                  :name "send_message"
                  :description "Send a message to a Gendl object"
                  :properties '(("objectId" . (("type" . "string")
                                              ("description" . "ID of the object to send the message to")))
                               ("message" . (("type" . "string")
                                            ("description" . "The message to send to the object")))
                               ("args" . (("type" . "object")
                                         ("description" . "Optional arguments for the message"))))
                  :required '("objectId" "message"))))))
    stream))

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