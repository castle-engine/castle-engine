{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "inspector_ui.castle-user-interface" }
'{' + LineEnding +
'  "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'  "FullSize" : true,' + LineEnding +
'  "Name" : "InspectorRoot",' + LineEnding +
'  "$Children" : [' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'      "BorderColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 2.5000000000000000E-001,' + LineEnding +
'        "Red" : 1.0000000000000000E+000' + LineEnding +
'      },' + LineEnding +
'      "FullSize" : true,' + LineEnding +
'      "Name" : "SafeBorderContainer",' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'            "Left" : 1.0000000000000000E+001,' + LineEnding +
'            "Right" : 1.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "Name" : "RectOptions",' + LineEnding +
'          "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000,' + LineEnding +
'                "Top" : 3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "Name" : "VerticalGroup5",' + LineEnding +
'              "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                    "Red" : 1.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelInspectorHelp",' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Press XXX to close inspector."' + LineEnding +
'                  ],' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleFloatSlider",' + LineEnding +
'                  "Background" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Opacity",' + LineEnding +
'                  "Name" : "SliderOpacity",' + LineEnding +
'                  "Thumb" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "Y" : -2.6999998092651367E+001' + LineEnding +
'                  },' + LineEnding +
'                  "Value" : 8.9999997615814209E-001,' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Name" : "HorizontalGroup5",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "Y" : -6.2000000000000000E+001' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleLabel",' + LineEnding +
'                      "Name" : "LabelAutoSelect",' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "AutoSelect (F9):"' + LineEnding +
'                      ],' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "Nothing",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonAutoSelectNothing",' + LineEnding +
'                      "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                      "Toggle" : true,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 1.6300000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "UI",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonAutoSelectUi",' + LineEnding +
'                      "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                      "Toggle" : true,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 2.3300000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "Transform",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonAutoSelectTransform",' + LineEnding +
'                      "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                      "Toggle" : true,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 2.5875000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Name" : "HorizontalGroupShow",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "Y" : -1.0125000000000000E+002' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleLabel",' + LineEnding +
'                      "Name" : "Label1",' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "Show: "' + LineEnding +
'                      ],' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "Hierarchy",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonHierarchyShow",' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 6.7000000000000000E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "Properties",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonPropertiesShow",' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 1.5575000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "Log",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonLogShow",' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 2.4750000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Caption" : "Profiler",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonProfilerShow",' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 2.9050000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Caption" : "Monitor and Auto-Reload Data",' + LineEnding +
'                  "Name" : "CheckboxFileMonitorEnabled",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "Y" : -1.4050000000000000E+002' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderOptions",' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder",' + LineEnding +
'                    "AllSides" : 5.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelOptionsHeader",' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Options"' + LineEnding +
'                  ],' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "AutoSizeToChildren" : true,' + LineEnding +
'          "AutoSizeToChildrenPaddingRight" : 5.0000000000000000E+000,' + LineEnding +
'          "AutoSizeToChildrenPaddingTop" : 5.0000000000000000E+000,' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 2.5000000000000000E-001,' + LineEnding +
'            "Blue" : 0.0000000000000000E+000,' + LineEnding +
'            "Green" : 0.0000000000000000E+000,' + LineEnding +
'            "Red" : 0.0000000000000000E+000' + LineEnding +
'          },' + LineEnding +
'          "Name" : "RectStatsMore",' + LineEnding +
'          "TranslationPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'            "X" : 3.5050000000000000E+002,' + LineEnding +
'            "Y" : -1.4000000000000000E+002' + LineEnding +
'          },' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'              "Caption" : "User Interface Batching",' + LineEnding +
'              "CaptionMargin" : 5.0000000000000000E+000,' + LineEnding +
'              "CheckboxColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                "Green" : 1.0000000000000000E+000,' + LineEnding +
'                "Red" : 1.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "CheckboxSize" : 2.0000000000000000E+001,' + LineEnding +
'              "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'              "Name" : "CheckboxUiBatching",' + LineEnding +
'              "TextColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                "Green" : 1.0000000000000000E+000,' + LineEnding +
'                "Red" : 1.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                "X" : 5.0000000000000000E+000,' + LineEnding +
'                "Y" : -5.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleLabel",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                "Green" : 1.0000000000000000E+000,' + LineEnding +
'                "Red" : 1.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'              "Name" : "LabelStatsMore",' + LineEnding +
'              "Text" : [' + LineEnding +
'                "LabelStatsMore"' + LineEnding +
'              ],' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                "X" : 5.0000000000000000E+000,' + LineEnding +
'                "Y" : -3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'            "Top" : 1.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'            "Blue" : 2.6666668057441711E-001,' + LineEnding +
'            "Green" : 2.6666668057441711E-001,' + LineEnding +
'            "Red" : 2.6666668057441711E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 1.5000000596046448E-001,' + LineEnding +
'          "HorizontalAnchorParent" : "hpMiddle",' + LineEnding +
'          "HorizontalAnchorSelf" : "hpMiddle",' + LineEnding +
'          "Name" : "RectProfiler",' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 5.0000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderProfiler",' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder",' + LineEnding +
'                    "AllSides" : 5.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelProfilerHeader",' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Frame Profiler"' + LineEnding +
'                  ],' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "AutoSize" : false,' + LineEnding +
'                  "Caption" : "X",' + LineEnding +
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
'                  "Height" : 3.0000000000000000E+001,' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonProfilerHide",' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "Width" : 3.0000000000000000E+001' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Caption" : "Log Stats",' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "CheckboxProfilerDetailsInLog",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -4.0000000000000000E+001' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Caption" : "More",' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "CheckboxProfilerMore",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -1.8000000000000000E+002' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Exists" : false,' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderProfiler2ndRow",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                "Y" : -3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "Top" : 3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "Name" : "ProfilerGraph",' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                    "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                    "Green" : 1.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                  "Name" : "Label2",' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "FPS = 60.0"' + LineEnding +
'                  ],' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                    "Alpha" : 1.0000000149011612E-001,' + LineEnding +
'                    "Blue" : 0.0000000000000000E+000,' + LineEnding +
'                    "Red" : 0.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "Height" : 2.0000000000000000E+000,' + LineEnding +
'                  "Name" : "RectangleControl5",' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "ProfilerLegend",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -5.0000000000000000E+000,' + LineEnding +
'                    "Y" : -5.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "$Children" : [' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                      "Name" : "HorizontalGroup1",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Blue" : 0.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl2",' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "Name" : "Label3",' + LineEnding +
'                          "Text" : [' + LineEnding +
'                            "Update"' + LineEnding +
'                          ],' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                            "X" : 1.5000000000000000E+001' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                        }' + LineEnding +
'                      ]' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                      "Name" : "HorizontalGroup4",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "Y" : -1.6500000000000000E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Blue" : 0.0000000000000000E+000,' + LineEnding +
'                            "Green" : 4.7843137383460999E-001,' + LineEnding +
'                            "Red" : 4.7843137383460999E-001' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl6",' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "Name" : "Label6",' + LineEnding +
'                          "Text" : [' + LineEnding +
'                            "Physics"' + LineEnding +
'                          ],' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                            "X" : 1.5000000000000000E+001' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                        }' + LineEnding +
'                      ]' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                      "Name" : "HorizontalGroup2",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "Y" : -3.3000000000000000E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Green" : 4.0392157435417175E-001,' + LineEnding +
'                            "Red" : 4.0392157435417175E-001' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl3",' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "Name" : "Label4",' + LineEnding +
'                          "Text" : [' + LineEnding +
'                            "Render"' + LineEnding +
'                          ],' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                            "X" : 1.5000000000000000E+001' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                        }' + LineEnding +
'                      ]' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                      "Name" : "HorizontalGroup3",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "Y" : -4.9500000000000000E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Green" : 6.9411766529083252E-001,' + LineEnding +
'                            "Red" : 6.9411766529083252E-001' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl4",' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "Name" : "Label5",' + LineEnding +
'                          "Text" : [' + LineEnding +
'                            "Swap (V-Sync Wait)"' + LineEnding +
'                          ],' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                            "X" : 1.5000000000000000E+001' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                        }' + LineEnding +
'                      ]' + LineEnding +
'                    }' + LineEnding +
'                  ]' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'            "Right" : 1.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'          "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'          "Name" : "RectLog",' + LineEnding +
'          "WidthFraction" : 7.5000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderLog",' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder",' + LineEnding +
'                    "AllSides" : 5.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelLogHeader",' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Log (10)"' + LineEnding +
'                  ],' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "AutoSize" : false,' + LineEnding +
'                  "Caption" : "X",' + LineEnding +
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
'                  "Height" : 3.0000000000000000E+001,' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonLogHide",' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "Width" : 3.0000000000000000E+001' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "Caption" : "Clear",' + LineEnding +
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
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonLogClear",' + LineEnding +
'                  "PaddingVertical" : 5.0000000000000000E+000,' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -3.2000000000000000E+002' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Caption" : "Auto Scroll To Bottom",' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "CheckboxLogAutoScroll",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -5.0000000000000000E+001' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleScrollView",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "Top" : 3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Name" : "ScrollLogs",' + LineEnding +
'              "ScrollArea" : {' + LineEnding +
'                "$$ClassName" : "TCastleScrollArea",' + LineEnding +
'                "$Children" : [' + LineEnding +
'                  {' + LineEnding +
'                    "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                    "Name" : "LogsVerticalGroup",' + LineEnding +
'                    "Padding" : 2.0000000000000000E+000,' + LineEnding +
'                    "$Children" : [' + LineEnding +
'                      {' + LineEnding +
'                        "$$ClassName" : "TCastleLabel",' + LineEnding +
'                        "Exists" : false,' + LineEnding +
'                        "Name" : "LabelEarlierLogsRemoved",' + LineEnding +
'                        "Text" : [' + LineEnding +
'                          "Earlier log messages have been hidden."' + LineEnding +
'                        ],' + LineEnding +
'                        "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                        "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'                      }' + LineEnding +
'                    ]' + LineEnding +
'                  }' + LineEnding +
'                ]' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarFrame" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarSlider" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
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
