(in-package :gendl-mcp)

;;;; MCP Protocol for Claude integration

;; This file contains functions to help define the Model Context Protocol
;; for Claude to interact with Gendl objects

(defun generate-mcp-tool-description ()
  "Generate a tool description for Claude MCP integration."
  ;; Generate JSON tool descriptions
  (with-output-to-string (stream)
    (json:encode-json
     `((:tools .
               (
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
     stream)))

(defun generate-mcp-openapi-spec ()
  "Generate an OpenAPI specification for Claude MCP integration."
  ;; Returns string with API spec suitable for inclusion in claude-desktop-json.conf
  "
{
  \"openapi\": \"3.0.0\",
  \"info\": {
    \"title\": \"Gendl MCP API\",
    \"description\": \"Model Context Protocol API for Claude to interact with Gendl objects\",
    \"version\": \"1.0.0\"
  },
  \"servers\": [
    {
      \"url\": \"http://localhost:9081\",
      \"description\": \"Local Gendl server\"
    }
  ],
  \"paths\": {
    \"/mcp/claude/ping\": {
      \"get\": {
        \"summary\": \"Check if Gendl MCP server is ready\",
        \"description\": \"Returns a simple text message indicating the Gendl MCP server is running and ready for Claude interaction\",
        \"operationId\": \"pingServer\",
        \"responses\": {
          \"200\": {
            \"description\": \"Successful response indicating server is ready\",
            \"content\": {
              \"text/plain\": {
                \"schema\": {
                  \"type\": \"string\",
                  \"example\": \"Gendl MCP server is ready for Claude interaction!\"
                }
              }
            }
          }
        }
      }
    },
    \"/mcp/tools/list\": {
      \"get\": {
        \"summary\": \"List available tools\",
        \"description\": \"Returns a list of available tools for Claude to use\",
        \"operationId\": \"listTools\",
        \"responses\": {
          \"200\": {
            \"description\": \"List of available tools\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"tools\": {
                      \"type\": \"array\",
                      \"items\": {
                        \"type\": \"object\"
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    \"/mcp/resources/list\": {
      \"get\": {
        \"summary\": \"List available resources\",
        \"description\": \"Returns a list of available resources\",
        \"operationId\": \"listResources\",
        \"responses\": {
          \"200\": {
            \"description\": \"List of available resources\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"resources\": {
                      \"type\": \"array\",
                      \"items\": {
                        \"type\": \"object\"
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    \"/mcp/prompts/list\": {
      \"get\": {
        \"summary\": \"List available prompts\",
        \"description\": \"Returns a list of available prompts\",
        \"operationId\": \"listPrompts\",
        \"responses\": {
          \"200\": {
            \"description\": \"List of available prompts\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"prompts\": {
                      \"type\": \"array\",
                      \"items\": {
                        \"type\": \"object\"
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    \"/mcp/objects\": {
      \"get\": {
        \"summary\": \"List Gendl objects\",
        \"description\": \"Returns a list of all registered Gendl objects\",
        \"operationId\": \"listObjects\",
        \"responses\": {
          \"200\": {
            \"description\": \"List of Gendl objects\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"array\",
                  \"items\": {
                    \"type\": \"object\"
                  }
                }
              }
            }
          }
        }
      }
    },
    \"/mcp/make-object\": {
      \"post\": {
        \"summary\": \"Create a new Gendl object\",
        \"description\": \"Creates a new Gendl object and returns its ID\",
        \"operationId\": \"makeObject\",
        \"requestBody\": {
          \"required\": true,
          \"content\": {
            \"application/json\": {
              \"schema\": {
                \"type\": \"object\",
                \"properties\": {
                  \"type\": {
                    \"type\": \"string\",
                    \"description\": \"The type of Gendl object to create\"
                  },
                  \"parameters\": {
                    \"type\": \"object\",
                    \"description\": \"Parameters for the object creation\"
                  }
                },
                \"required\": [\"type\"]
              }
            }
          }
        },
        \"responses\": {
          \"200\": {
            \"description\": \"Successful object creation\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"integer\",
                  \"description\": \"ID of the created object\"
                }
              }
            }
          }
        }
      }
    },
    \"/mcp/theo\": {
      \"get\": {
        \"summary\": \"Send a message to a Gendl object\",
        \"description\": \"Sends a message to a Gendl object and returns the result\",
        \"operationId\": \"sendMessage\",
        \"parameters\": [
          {
            \"name\": \"object\",
            \"in\": \"query\",
            \"required\": true,
            \"schema\": {
              \"type\": \"integer\",
              \"description\": \"ID of the Gendl object\"
            }
          },
          {
            \"name\": \"message\",
            \"in\": \"query\",
            \"required\": true,
            \"schema\": {
              \"type\": \"string\",
              \"description\": \"Message to send to the object\"
            }
          }
        ],
        \"responses\": {
          \"200\": {
            \"description\": \"Result of the message\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\"
                }
              }
            }
          }
        }
      },
      \"post\": {
        \"summary\": \"Send a message with arguments to a Gendl object\",
        \"description\": \"Sends a message with arguments to a Gendl object and returns the result\",
        \"operationId\": \"sendMessageWithArgs\",
        \"parameters\": [
          {
            \"name\": \"object\",
            \"in\": \"query\",
            \"required\": true,
            \"schema\": {
              \"type\": \"integer\",
              \"description\": \"ID of the Gendl object\"
            }
          },
          {
            \"name\": \"message\",
            \"in\": \"query\",
            \"required\": true,
            \"schema\": {
              \"type\": \"string\",
              \"description\": \"Message to send to the object\"
            }
          }
        ],
        \"requestBody\": {
          \"required\": true,
          \"content\": {
            \"application/json\": {
              \"schema\": {
                \"type\": \"object\",
                \"description\": \"Arguments for the message\"
              }
            }
          }
        },
        \"responses\": {
          \"200\": {
            \"description\": \"Result of the message\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\"
                }
              }
            }
          }
        }
      }
    }
  }
}
"
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (initialize-standard-endpoints))
