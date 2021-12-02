{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "inspector_ui.castle-user-interface" }
'{' + LineEnding +
'  "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'  "FullSize" : true,' + LineEnding +
'  "Name" : "InspectorRoot",' + LineEnding +
'  "$Children" : [' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'      "Border" : {' + LineEnding +
'        "$$ClassName" : "TBorder",' + LineEnding +
'        "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'        "Right" : 1.0000000000000000E+001' + LineEnding +
'      },' + LineEnding +
'      "ColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 8.9999997615814209E-001' + LineEnding +
'      },' + LineEnding +
'      "HeightFraction" : 2.5000000000000000E-001,' + LineEnding +
'      "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'      "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'      "Name" : "RectLog",' + LineEnding +
'      "WidthFraction" : 7.5000000000000000E-001,' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'            "Blue" : 7.5294119119644165E-001,' + LineEnding +
'            "Green" : 7.5294119119644165E-001,' + LineEnding +
'            "Red" : 7.5294119119644165E-001' + LineEnding +
'          },' + LineEnding +
'          "Height" : 3.0000000000000000E+001,' + LineEnding +
'          "Name" : "HeaderLog",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelLogHeader",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "Log (10)"' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleButton",' + LineEnding +
'              "AutoSize" : false,' + LineEnding +
'              "Caption" : "X",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonLogHide",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "Width" : 3.0000000000000000E+001' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleButton",' + LineEnding +
'              "Caption" : "Clear",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "HorizontalAnchorDelta" : -3.2000000000000000E+002,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonLogClear",' + LineEnding +
'              "PaddingVertical" : 5.0000000000000000E+000,' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'              "Caption" : "Auto Scroll To Bottom",' + LineEnding +
'              "HorizontalAnchorDelta" : -5.0000000000000000E+001,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Name" : "CheckboxLogAutoScroll",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleScrollView",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Top" : 3.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "FullSize" : true,' + LineEnding +
'          "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'          "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'          "Name" : "ScrollLogs",' + LineEnding +
'          "ScrollArea" : {' + LineEnding +
'            "$$ClassName" : "TInternalCastleScrollArea",' + LineEnding +
'            "AutoSizeToChildren" : true,' + LineEnding +
'            "Name" : "ScrollArea",' + LineEnding +
'            "$Children" : [' + LineEnding +
'              {' + LineEnding +
'                "$$ClassName" : "TCastleLabel",' + LineEnding +
'                "Border" : {' + LineEnding +
'                  "$$ClassName" : "TBorder",' + LineEnding +
'                  "AllSides" : 2.0000000000000000E+000' + LineEnding +
'                },' + LineEnding +
'                "Name" : "LabelLog",' + LineEnding +
'                "Text" : [' + LineEnding +
'                  "-------------------- Rendering Context Initialization begin",' + LineEnding +
'                  "OpenGL information (detected by castle-editor):",' + LineEnding +
'                  "",' + LineEnding +
'                  "--------",' + LineEnding +
'                  "Version:",' + LineEnding +
'                  "  Version string: 4.6.0 NVIDIA 470.82.00",' + LineEnding +
'                  "  Version parsed: major: 4, minor: 6, release exists: True, release: 0, vendor-specific information: \"NVIDIA 470.82.00\"",' + LineEnding +
'                  "  Vendor-specific version parsed: major: 470, minor: 82, release: 0",' + LineEnding +
'                  "  Vendor: NVIDIA Corporation",' + LineEnding +
'                  "  Vendor type: Nvidia",' + LineEnding +
'                  "",' + LineEnding +
'                  "  Renderer: NVIDIA GeForce GTX 660/PCIe/SSE2",' + LineEnding +
'                  "  Fglrx (ATI on Linux): False",' + LineEnding +
'                  "  Mesa: False",' + LineEnding +
'                  "",' + LineEnding +
'                  "  Buggy glGenerateMipmap(EXT): False",' + LineEnding +
'                  "  Buggy GL_LIGHT_MODEL_TWO_SIDE: False",' + LineEnding +
'                  "  Buggy VBO: False",' + LineEnding +
'                  "  Buggy shader shadow map: False",' + LineEnding +
'                  "  Buggy FBO rendering to multi-sampling texture: False",' + LineEnding +
'                  "  Buggy FBO rendering to cube map texture: False",' + LineEnding +
'                  "  Buggy swap buffers with non-standard glViewport: False",' + LineEnding +
'                  "  Buggy 32-bit depth buffer: False",' + LineEnding +
'                  "  Buggy GLSL gl_FrontFacing: False",' + LineEnding +
'                  "  Buggy GLSL read varying: False",' + LineEnding +
'                  "  Buggy Pure Shader Pipeline: False",' + LineEnding +
'                  "  Buggy Texture Size Above 2048: False",' + LineEnding +
'                  "  Buggy Shader Bump Mapping num_steps: False",' + LineEnding +
'                  "",' + LineEnding +
'                  "------------------------",' + LineEnding +
'                  "Real versions available:",' + LineEnding +
'                  "(checks both version string and actual functions availability in GL library, to secure from buggy implementations)",' + LineEnding +
'                  ""' + LineEnding +
'                ],' + LineEnding +
'                "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'              }' + LineEnding +
'            ]' + LineEnding +
'          },' + LineEnding +
'          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'          "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'        }' + LineEnding +
'      ]' + LineEnding +
'    },' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'      "Border" : {' + LineEnding +
'        "$$ClassName" : "TBorder",' + LineEnding +
'        "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'        "Top" : 1.0000000000000000E+001' + LineEnding +
'      },' + LineEnding +
'      "ColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'        "Blue" : 2.6666668057441711E-001,' + LineEnding +
'        "Green" : 2.6666668057441711E-001,' + LineEnding +
'        "Red" : 2.6666668057441711E-001' + LineEnding +
'      },' + LineEnding +
'      "HeightFraction" : 1.5000000596046448E-001,' + LineEnding +
'      "HorizontalAnchorParent" : "hpMiddle",' + LineEnding +
'      "HorizontalAnchorSelf" : "hpMiddle",' + LineEnding +
'      "Name" : "RectProfiler",' + LineEnding +
'      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'      "WidthFraction" : 5.0000000000000000E-001,' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'            "Blue" : 7.5294119119644165E-001,' + LineEnding +
'            "Green" : 7.5294119119644165E-001,' + LineEnding +
'            "Red" : 7.5294119119644165E-001' + LineEnding +
'          },' + LineEnding +
'          "Height" : 3.0000000000000000E+001,' + LineEnding +
'          "Name" : "HeaderProfiler",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelProfilerHeader",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "Frame Profiler"' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleButton",' + LineEnding +
'              "AutoSize" : false,' + LineEnding +
'              "Caption" : "X",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonProfilerHide",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "Width" : 3.0000000000000000E+001' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'              "Caption" : "Log Details",' + LineEnding +
'              "HorizontalAnchorDelta" : -5.0000000000000000E+001,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Name" : "CheckboxProfilerDetailsInLog",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Top" : 3.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "FullSize" : true,' + LineEnding +
'          "Name" : "ProfilerGraph",' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                "Green" : 1.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'              "Name" : "Label2",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "FPS = 60.0"' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 5.0196081399917603E-001,' + LineEnding +
'                "Blue" : 0.0000000000000000E+000,' + LineEnding +
'                "Red" : 0.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Height" : 2.0000000000000000E+000,' + LineEnding +
'              "Name" : "RectangleControl5",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'              "HorizontalAnchorDelta" : -5.0000000000000000E+000,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Name" : "ProfilerLegend",' + LineEnding +
'              "VerticalAnchorDelta" : -5.0000000000000000E+000,' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Name" : "HorizontalGroup1",' + LineEnding +
'                  "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Blue" : 0.0000000000000000E+000' + LineEnding +
'                      },' + LineEnding +
'                      "Height" : 1.0000000000000000E+001,' + LineEnding +
'                      "Name" : "RectangleControl2",' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                      "Width" : 1.0000000000000000E+001' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleLabel",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                        "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                        "Green" : 1.0000000000000000E+000,' + LineEnding +
'                        "Red" : 1.0000000000000000E+000' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "HorizontalAnchorDelta" : 1.5000000000000000E+001,' + LineEnding +
'                      "Name" : "Label3",' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "Update"' + LineEnding +
'                      ],' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Name" : "HorizontalGroup4",' + LineEnding +
'                  "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorDelta" : -1.6499998092651367E+001,' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Blue" : 0.0000000000000000E+000,' + LineEnding +
'                        "Green" : 4.7843137383460999E-001,' + LineEnding +
'                        "Red" : 4.7843137383460999E-001' + LineEnding +
'                      },' + LineEnding +
'                      "Height" : 1.0000000000000000E+001,' + LineEnding +
'                      "Name" : "RectangleControl6",' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                      "Width" : 1.0000000000000000E+001' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleLabel",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                        "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                        "Green" : 1.0000000000000000E+000,' + LineEnding +
'                        "Red" : 1.0000000000000000E+000' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "HorizontalAnchorDelta" : 1.5000000000000000E+001,' + LineEnding +
'                      "Name" : "Label6",' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "Physics"' + LineEnding +
'                      ],' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Name" : "HorizontalGroup2",' + LineEnding +
'                  "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorDelta" : -3.2999996185302734E+001,' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Green" : 4.0392157435417175E-001,' + LineEnding +
'                        "Red" : 4.0392157435417175E-001' + LineEnding +
'                      },' + LineEnding +
'                      "Height" : 1.0000000000000000E+001,' + LineEnding +
'                      "Name" : "RectangleControl3",' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                      "Width" : 1.0000000000000000E+001' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleLabel",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                        "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                        "Green" : 1.0000000000000000E+000,' + LineEnding +
'                        "Red" : 1.0000000000000000E+000' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "HorizontalAnchorDelta" : 1.5000000000000000E+001,' + LineEnding +
'                      "Name" : "Label4",' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "Render"' + LineEnding +
'                      ],' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Name" : "HorizontalGroup3",' + LineEnding +
'                  "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorDelta" : -4.9499992370605469E+001,' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Green" : 6.9411766529083252E-001,' + LineEnding +
'                        "Red" : 6.9411766529083252E-001' + LineEnding +
'                      },' + LineEnding +
'                      "Height" : 1.0000000000000000E+001,' + LineEnding +
'                      "Name" : "RectangleControl4",' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                      "Width" : 1.0000000000000000E+001' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleLabel",' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                        "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                        "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                        "Green" : 1.0000000000000000E+000,' + LineEnding +
'                        "Red" : 1.0000000000000000E+000' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "HorizontalAnchorDelta" : 1.5000000000000000E+001,' + LineEnding +
'                      "Name" : "Label5",' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "Swap (V-Sync Wait)"' + LineEnding +
'                      ],' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        }' + LineEnding +
'      ]' + LineEnding +
'    },' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'      "Border" : {' + LineEnding +
'        "$$ClassName" : "TBorder",' + LineEnding +
'        "AllSides" : 1.0000000000000000E+001' + LineEnding +
'      },' + LineEnding +
'      "ColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 8.9999997615814209E-001' + LineEnding +
'      },' + LineEnding +
'      "HeightFraction" : 7.5000000000000000E-001,' + LineEnding +
'      "Name" : "RectHierarchy",' + LineEnding +
'      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'      "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'            "Blue" : 7.5294119119644165E-001,' + LineEnding +
'            "Green" : 7.5294119119644165E-001,' + LineEnding +
'            "Red" : 7.5294119119644165E-001' + LineEnding +
'          },' + LineEnding +
'          "Height" : 3.0000000000000000E+001,' + LineEnding +
'          "Name" : "HeaderHierarchy",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelHierarchyHeader",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "Hierarchy"' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleButton",' + LineEnding +
'              "AutoSize" : false,' + LineEnding +
'              "Caption" : "X",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonHierarchyHide",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "Width" : 3.0000000000000000E+001' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleScrollView",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Bottom" : 3.0000000000000000E+001,' + LineEnding +
'            "Top" : 3.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "FullSize" : true,' + LineEnding +
'          "Name" : "ScrollView3",' + LineEnding +
'          "ScrollArea" : {' + LineEnding +
'            "$$ClassName" : "TInternalCastleScrollArea",' + LineEnding +
'            "AutoSizeToChildren" : true,' + LineEnding +
'            "Name" : "ScrollArea",' + LineEnding +
'            "$Children" : [' + LineEnding +
'              {' + LineEnding +
'                "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                "Name" : "HierarchyRowParent",' + LineEnding +
'                "$Children" : [' + LineEnding +
'                  {' + LineEnding +
'                    "$$ClassName" : "TCastleButton",' + LineEnding +
'                    "Alignment" : "hpLeft",' + LineEnding +
'                    "AutoSize" : false,' + LineEnding +
'                    "Caption" : "HierarchyRowTemplate",' + LineEnding +
'                    "CustomBackground" : true,' + LineEnding +
'                    "CustomBackgroundDisabled" : {' + LineEnding +
'                      "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "CustomBackgroundFocused" : {' + LineEnding +
'                      "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "CustomBackgroundNormal" : {' + LineEnding +
'                      "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "CustomBackgroundPressed" : {' + LineEnding +
'                      "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "CustomColorDisabledPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                      "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                      "Green" : 1.0000000000000000E+000,' + LineEnding +
'                      "Red" : 1.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "CustomColorFocusedPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                      "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                      "Green" : 1.0000000000000000E+000,' + LineEnding +
'                      "Red" : 1.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "CustomColorNormalPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                      "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                      "Green" : 1.0000000000000000E+000,' + LineEnding +
'                      "Red" : 1.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "CustomColorPressedPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                      "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                      "Green" : 1.0000000000000000E+000,' + LineEnding +
'                      "Red" : 1.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "Height" : 3.0000000000000000E+001,' + LineEnding +
'                    "Image" : {' + LineEnding +
'                      "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "Name" : "HierarchyRowTemplate",' + LineEnding +
'                    "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                    "PaddingVertical" : 5.0000000000000000E+000,' + LineEnding +
'                    "TextAlignment" : "hpLeft",' + LineEnding +
'                    "TintFocusedPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Blue" : 4.8235294222831726E-001' + LineEnding +
'                    },' + LineEnding +
'                    "TintNormalPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Alpha" : 0.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "TintPressedPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Blue" : 0.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "Toggle" : true,' + LineEnding +
'                    "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                    "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                    "Width" : 3.5000000000000000E+002' + LineEnding +
'                  }' + LineEnding +
'                ]' + LineEnding +
'              }' + LineEnding +
'            ]' + LineEnding +
'          }' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 9.0196079015731812E-001,' + LineEnding +
'            "Blue" : 7.5294119119644165E-001,' + LineEnding +
'            "Green" : 7.5294119119644165E-001,' + LineEnding +
'            "Red" : 7.5294119119644165E-001' + LineEnding +
'          },' + LineEnding +
'          "Height" : 3.0000000000000000E+001,' + LineEnding +
'          "Name" : "HierarchyBottomControls",' + LineEnding +
'          "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'              "Caption" : "Show Even Internal",' + LineEnding +
'              "Name" : "CheckboxShowEvenInternal"' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        }' + LineEnding +
'      ]' + LineEnding +
'    },' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'      "Border" : {' + LineEnding +
'        "$$ClassName" : "TBorder",' + LineEnding +
'        "AllSides" : 1.0000000000000000E+001' + LineEnding +
'      },' + LineEnding +
'      "ColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 8.9999997615814209E-001' + LineEnding +
'      },' + LineEnding +
'      "HeightFraction" : 7.5000000000000000E-001,' + LineEnding +
'      "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'      "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'      "Name" : "RectProperties",' + LineEnding +
'      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'      "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "ClipChildren" : true,' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'            "Blue" : 7.5294119119644165E-001,' + LineEnding +
'            "Green" : 7.5294119119644165E-001,' + LineEnding +
'            "Red" : 7.5294119119644165E-001' + LineEnding +
'          },' + LineEnding +
'          "Height" : 3.0000000000000000E+001,' + LineEnding +
'          "Name" : "HeaderProperties",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelPropertiesHeader",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "Properties"' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleButton",' + LineEnding +
'              "AutoSize" : false,' + LineEnding +
'              "Caption" : "X",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonPropertiesHide",' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "Width" : 3.0000000000000000E+001' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleScrollView",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Top" : 3.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "FullSize" : true,' + LineEnding +
'          "Name" : "ScrollView4",' + LineEnding +
'          "ScrollArea" : {' + LineEnding +
'            "$$ClassName" : "TInternalCastleScrollArea",' + LineEnding +
'            "AutoSizeToChildren" : true,' + LineEnding +
'            "Name" : "ScrollArea",' + LineEnding +
'            "$Children" : [' + LineEnding +
'              {' + LineEnding +
'                "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                "Name" : "PropertyRowParent",' + LineEnding +
'                "$Children" : [' + LineEnding +
'                  {' + LineEnding +
'                    "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'                    "Border" : {' + LineEnding +
'                      "$$ClassName" : "TBorder",' + LineEnding +
'                      "AllSides" : 2.0000000000000000E+000' + LineEnding +
'                    },' + LineEnding +
'                    "BorderColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                      "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                      "Blue" : 5.5294120311737061E-001,' + LineEnding +
'                      "Green" : 5.5294120311737061E-001,' + LineEnding +
'                      "Red" : 5.6078433990478516E-001' + LineEnding +
'                    },' + LineEnding +
'                    "Height" : 4.0000000000000000E+001,' + LineEnding +
'                    "Name" : "PropertyRowTemplate",' + LineEnding +
'                    "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                    "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                    "Width" : 3.5000000000000000E+002,' + LineEnding +
'                    "$Children" : [' + LineEnding +
'                      {' + LineEnding +
'                        "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                        "Border" : {' + LineEnding +
'                          "$$ClassName" : "TBorder",' + LineEnding +
'                          "Right" : 2.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "BorderColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                          "Blue" : 5.5294120311737061E-001,' + LineEnding +
'                          "Green" : 5.5294120311737061E-001,' + LineEnding +
'                          "Red" : 5.6078433990478516E-001' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Blue" : 7.6078432798385620E-001,' + LineEnding +
'                          "Green" : 7.6078432798385620E-001,' + LineEnding +
'                          "Red" : 7.6078432798385620E-001' + LineEnding +
'                        },' + LineEnding +
'                        "HeightFraction" : 1.0000000000000000E+000,' + LineEnding +
'                        "Name" : "RectangleControl1",' + LineEnding +
'                        "WidthFraction" : 5.0000000000000000E-001,' + LineEnding +
'                        "$Children" : [' + LineEnding +
'                          {' + LineEnding +
'                            "$$ClassName" : "TCastleLabel",' + LineEnding +
'                            "HorizontalAnchorDelta" : 1.0000000000000000E+001,' + LineEnding +
'                            "Name" : "PropName",' + LineEnding +
'                            "Text" : [' + LineEnding +
'                              "Position"' + LineEnding +
'                            ],' + LineEnding +
'                            "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                            "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                          }' + LineEnding +
'                        ]' + LineEnding +
'                      },' + LineEnding +
'                      {' + LineEnding +
'                        "$$ClassName" : "TCastleEdit",' + LineEnding +
'                        "AutoSizeHeight" : false,' + LineEnding +
'                        "CaptureAllInput" : false,' + LineEnding +
'                        "HeightFraction" : 1.0000000000000000E+000,' + LineEnding +
'                        "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                        "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                        "Name" : "PropValue",' + LineEnding +
'                        "Text" : "PropValue",' + LineEnding +
'                        "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                        "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                        "WidthFraction" : 5.0000000000000000E-001' + LineEnding +
'                      }' + LineEnding +
'                    ]' + LineEnding +
'                  }' + LineEnding +
'                ]' + LineEnding +
'              }' + LineEnding +
'            ]' + LineEnding +
'          }' + LineEnding +
'        }' + LineEnding +
'      ]' + LineEnding +
'    },' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'      "Border" : {' + LineEnding +
'        "$$ClassName" : "TBorder",' + LineEnding +
'        "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'        "Left" : 1.0000000000000000E+001,' + LineEnding +
'        "Right" : 1.0000000000000000E+001' + LineEnding +
'      },' + LineEnding +
'      "ColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 8.9999997615814209E-001' + LineEnding +
'      },' + LineEnding +
'      "HeightFraction" : 2.5000000000000000E-001,' + LineEnding +
'      "Name" : "RectOptions",' + LineEnding +
'      "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "AllSides" : 5.0000000000000000E+000,' + LineEnding +
'            "Top" : 3.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "FullSize" : true,' + LineEnding +
'          "Name" : "VerticalGroup5",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Red" : 1.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelInspectorHelp",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "Press F12 to close inspector."' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleFloatSlider",' + LineEnding +
'              "Caption" : "Opacity",' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "SliderOpacity",' + LineEnding +
'              "SmallFont" : false,' + LineEnding +
'              "Value" : 8.9999997615814209E-001,' + LineEnding +
'              "VerticalAnchorDelta" : -2.2000001907348633E+001,' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'              "Name" : "HorizontalGroupShow",' + LineEnding +
'              "VerticalAnchorDelta" : -5.2000000000000000E+001,' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "Exists" : false,' + LineEnding +
'                  "Name" : "Label1",' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Show: "' + LineEnding +
'                  ],' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "Caption" : "Hierarchy",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonHierarchyShow",' + LineEnding +
'                  "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "Caption" : "Properties",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorDelta" : 1.1300000762939453E+002,' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonPropertiesShow",' + LineEnding +
'                  "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "Caption" : "Log",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorDelta" : 2.3000000000000000E+002,' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonLogShow",' + LineEnding +
'                  "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "Caption" : "Profiler",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorDelta" : 2.8200000000000000E+002,' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonProfilerShow",' + LineEnding +
'                  "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'            "Blue" : 7.5294119119644165E-001,' + LineEnding +
'            "Green" : 7.5294119119644165E-001,' + LineEnding +
'            "Red" : 7.5294119119644165E-001' + LineEnding +
'          },' + LineEnding +
'          "Height" : 3.0000000000000000E+001,' + LineEnding +
'          "Name" : "HeaderOptions",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelOptionsHeader",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "Options"' + LineEnding +
'              ],' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        }' + LineEnding +
'      ]' + LineEnding +
'    }' + LineEnding +
'  ]' + LineEnding +
'}' + LineEnding +
''
