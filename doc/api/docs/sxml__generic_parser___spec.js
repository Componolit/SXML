GNATdoc.Documentation = {
  "label": "SXML.Generic_Parser",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Match_Type",
          "qualifier": "",
          "line": 30,
          "column": 9,
          "src": "srcs/sxml-generic_parser.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 30,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_Type",
                      "href": "docs/sxml__generic_parser___spec.html#L30C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_OK",
                      "href": "docs/sxml__generic_parser___spec.html#L30C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 31,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_None",
                      "href": "docs/sxml__generic_parser___spec.html#L31C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 32,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_Invalid",
                      "href": "docs/sxml__generic_parser___spec.html#L32C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 33,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_Out_Of_Memory",
                      "href": "docs/sxml__generic_parser___spec.html#L33C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 34,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_None_Wellformed",
                      "href": "docs/sxml__generic_parser___spec.html#L34C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 35,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_Depth_Limit",
                      "href": "docs/sxml__generic_parser___spec.html#L35C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 36,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_Trailing_Data",
                      "href": "docs/sxml__generic_parser___spec.html#L36C24"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/sxml__generic_parser___spec.html#L30C9"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Result of a parsing operation\n"
                }
              ]
            }
          ],
          "literals": [
            {
              "label": "Match_OK",
              "line": 30,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "XML document parsed successfully\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Match_None",
              "line": 31,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "No XML data found\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Match_Invalid",
              "line": 32,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Malformed XML data found\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Match_Out_Of_Memory",
              "line": 33,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Out of context buffer memory, increase\n"
                    },
                    {
                      "kind": "span",
                      "text": "generic Context_Size parameter when\n"
                    },
                    {
                      "kind": "span",
                      "text": "instanciating package\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Match_None_Wellformed",
              "line": 34,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Document is not wellformed\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Match_Depth_Limit",
              "line": 35,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Recursion depth exceeded\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Match_Trailing_Data",
              "line": 36,
              "column": 24,
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Document successful parsed, but there is\n"
                    },
                    {
                      "kind": "span",
                      "text": "trailing data after it\n"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      "label": "Simple types"
    },
    {
      "entities": [
        {
          "label": "Parse",
          "qualifier": "",
          "line": 48,
          "column": 14,
          "src": "srcs/sxml-generic_parser.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 48,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Parse",
                      "href": "docs/sxml__generic_parser___spec.html#L48C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Data",
                      "href": "docs/sxml__generic_parser___spec.html#L48C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Content_Type",
                      "href": "docs/sxml___spec.html#L41C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 49,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Document",
                      "href": "docs/sxml__generic_parser___spec.html#L49C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Document_Type",
                      "href": "docs/sxml___spec.html#L80C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 50,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Offset",
                      "href": "docs/sxml__generic_parser___spec.html#L50C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Natural"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 51,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Result",
                      "href": "docs/sxml__generic_parser___spec.html#L51C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Match_Type",
                      "href": "docs/sxml__generic_parser___spec.html#L30C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "with"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 52,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      Global => (In_Out => (State)),"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 53,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      Post   => Offset < Data'Length"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Parse an XML file\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Data",
              "line": 48,
              "column": 21,
              "type": {
                "label": "SXML.Content_Type",
                "docHref": "docs/sxml___spec.html#L41C12"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Input data\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Document",
              "line": 49,
              "column": 21,
              "type": {
                "label": "SXML.Document_Type",
                "docHref": "docs/sxml___spec.html#L80C12"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Document\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Offset",
              "line": 50,
              "column": 21,
              "type": {
                "label": "Natural"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Input location after parsing or error\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "Result",
              "line": 51,
              "column": 21,
              "type": {
                "label": "SXML.Generic_Parser.Match_Type",
                "docHref": "docs/sxml__generic_parser___spec.html#L30C9"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Result of operation\n"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    }
  ]
};