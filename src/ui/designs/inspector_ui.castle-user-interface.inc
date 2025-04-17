{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "inspector_ui.castle-user-interface" }
'{' + LineEnding +
'  "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'  "Border" : {' + LineEnding +
'    "$$ClassName" : "TBorder"' + LineEnding +
'  },' + LineEnding +
'  "BorderColorPersistent" : {' + LineEnding +
'    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'  },' + LineEnding +
'  "FullSize" : true,' + LineEnding +
'  "Name" : "InspectorRoot",' + LineEnding +
'  "TranslationPersistent" : {' + LineEnding +
'    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'  },' + LineEnding +
'  "$Children" : [' + LineEnding +
'    {' + LineEnding +
'      "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'      "Border" : {' + LineEnding +
'        "$$ClassName" : "TBorder"' + LineEnding +
'      },' + LineEnding +
'      "BorderColorPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'        "Alpha" : 2.5000000000000000E-001,' + LineEnding +
'        "Red" : 1.0000000000000000E+000' + LineEnding +
'      },' + LineEnding +
'      "FullSize" : true,' + LineEnding +
'      "Name" : "SafeBorderContainer",' + LineEnding +
'      "TranslationPersistent" : {' + LineEnding +
'        "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'      },' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "Bottom" : 1.0000000000000000E+001,' + LineEnding +
'            "Left" : 1.0000000000000000E+001,' + LineEnding +
'            "Right" : 1.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "BorderColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "Name" : "RectOptions",' + LineEnding +
'          "TranslationPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'          },' + LineEnding +
'          "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "AllSides" : 5.0000000000000000E+000,' + LineEnding +
'                "Top" : 3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "FrameColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "Name" : "VerticalGroup5",' + LineEnding +
'              "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
'              "VerticalAnchorParent" : "vpTop",' + LineEnding +
'              "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                    "Red" : 1.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelInspectorHelp",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Press XXX to close inspector."' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleFloatSlider",' + LineEnding +
'                  "Background" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Opacity",' + LineEnding +
'                  "Name" : "SliderOpacity",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Thumb" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "Y" : -2.7000000000000000E+001' + LineEnding +
'                  },' + LineEnding +
'                  "Value" : 8.9999997615814209E-001,' + LineEnding +
'                  "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                  "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleHorizontalGroup",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FrameColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "LabelAutoSelect",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "AutoSelect (F9):"' + LineEnding +
'                      ],' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "Nothing",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonAutoSelectNothing",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "UI",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonAutoSelectUi",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "Transform",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonAutoSelectTransform",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
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
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "ColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FrameColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "Label1",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Text" : [' + LineEnding +
'                        "Show: "' + LineEnding +
'                      ],' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "Hierarchy",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonHierarchyShow",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 6.7000000000000000E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "Properties",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonPropertiesShow",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 1.5575000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "Log",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonLogShow",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "X" : 2.4750000000000000E+002' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                    },' + LineEnding +
'                    {' + LineEnding +
'                      "$$ClassName" : "TCastleButton",' + LineEnding +
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Caption" : "Profiler",' + LineEnding +
'                      "CustomBackgroundDisabled" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundFocused" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundNormal" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomBackgroundPressed" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomColorPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "CustomTextColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                      "Image" : {' + LineEnding +
'                        "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                        "ClipLinePersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ProtectedSides" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "RegionPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "RotationCenterPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        }' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "ButtonProfilerShow",' + LineEnding +
'                      "OutlineColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "PaddingHorizontal" : 8.0000000000000000E+000,' + LineEnding +
'                      "TintDisabledPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintFocusedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintNormalPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "TintPressedPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
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
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Monitor and Auto-Reload Data",' + LineEnding +
'                  "CheckboxColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "CheckboxFileMonitorEnabled",' + LineEnding +
'                  "TextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
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
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderOptions",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
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
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelOptionsHeader",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Options"' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "AllSides" : 1.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "BorderColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 7.5000000000000000E-001,' + LineEnding +
'          "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'          "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'          "Name" : "RectProperties",' + LineEnding +
'          "TranslationPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'          },' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ClipChildren" : true,' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderProperties",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
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
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelPropertiesHeader",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Properties"' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "AutoSize" : false,' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "X",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomTextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Height" : 3.0000000000000000E+001,' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonPropertiesHide",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "Width" : 3.0000000000000000E+001' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleScrollView",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "Top" : 3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "Name" : "ScrollProperties",' + LineEnding +
'              "ScrollArea" : {' + LineEnding +
'                "$$ClassName" : "TCastleScrollArea",' + LineEnding +
'                "Border" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "BorderColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "TranslationPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                },' + LineEnding +
'                "$Children" : [' + LineEnding +
'                  {' + LineEnding +
'                    "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                    "Border" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "BorderColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "FrameColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "Name" : "PropertyRowParent",' + LineEnding +
'                    "TranslationPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "$Children" : [' + LineEnding +
'                      {' + LineEnding +
'                        "$$ClassName" : "TCastleUserInterface",' + LineEnding +
'                        "Border" : {' + LineEnding +
'                          "$$ClassName" : "TBorder",' + LineEnding +
'                          "AllSides" : 1.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "BorderColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                          "Blue" : 5.5294120311737061E-001,' + LineEnding +
'                          "Green" : 5.5294120311737061E-001,' + LineEnding +
'                          "Red" : 5.6078433990478516E-001' + LineEnding +
'                        },' + LineEnding +
'                        "Height" : 4.0000000000000000E+001,' + LineEnding +
'                        "Name" : "PropertyRowTemplate",' + LineEnding +
'                        "TranslationPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                        "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                        "Width" : 3.5000000000000000E+002,' + LineEnding +
'                        "$Children" : [' + LineEnding +
'                          {' + LineEnding +
'                            "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                            "Border" : {' + LineEnding +
'                              "$$ClassName" : "TBorder"' + LineEnding +
'                            },' + LineEnding +
'                            "BorderColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                              "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                              "Blue" : 5.5294120311737061E-001,' + LineEnding +
'                              "Green" : 5.5294120311737061E-001,' + LineEnding +
'                              "Red" : 5.6078433990478516E-001' + LineEnding +
'                            },' + LineEnding +
'                            "ColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                              "Blue" : 7.6078432798385620E-001,' + LineEnding +
'                              "Green" : 7.6078432798385620E-001,' + LineEnding +
'                              "Red" : 7.6078432798385620E-001' + LineEnding +
'                            },' + LineEnding +
'                            "HeightFraction" : 1.0000000000000000E+000,' + LineEnding +
'                            "Name" : "RectangleControl1",' + LineEnding +
'                            "TranslationPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                            },' + LineEnding +
'                            "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'                            "$Children" : [' + LineEnding +
'                              {' + LineEnding +
'                                "$$ClassName" : "TCastleLabel",' + LineEnding +
'                                "Border" : {' + LineEnding +
'                                  "$$ClassName" : "TBorder"' + LineEnding +
'                                },' + LineEnding +
'                                "BorderColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "ColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "FrameColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "Name" : "PropName",' + LineEnding +
'                                "OutlineColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "Text" : [' + LineEnding +
'                                  "Position"' + LineEnding +
'                                ],' + LineEnding +
'                                "TranslationPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                                  "X" : 1.0000000000000000E+001' + LineEnding +
'                                },' + LineEnding +
'                                "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                                "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                              }' + LineEnding +
'                            ]' + LineEnding +
'                          },' + LineEnding +
'                          {' + LineEnding +
'                            "$$ClassName" : "TCastleEdit",' + LineEnding +
'                            "AutoSizeHeight" : false,' + LineEnding +
'                            "BackgroundColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "Border" : {' + LineEnding +
'                              "$$ClassName" : "TBorder"' + LineEnding +
'                            },' + LineEnding +
'                            "BorderColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "CaptureAllInput" : false,' + LineEnding +
'                            "FocusedColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "HeightFraction" : 1.0000000000000000E+000,' + LineEnding +
'                            "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                            "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                            "Name" : "PropValue",' + LineEnding +
'                            "OutlineColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "PlaceholderColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "Text" : "PropValue",' + LineEnding +
'                            "TranslationPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                            },' + LineEnding +
'                            "UnfocusedColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                            "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                            "WidthFraction" : 5.0000000000000000E-001' + LineEnding +
'                          },' + LineEnding +
'                          {' + LineEnding +
'                            "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                            "Border" : {' + LineEnding +
'                              "$$ClassName" : "TBorder"' + LineEnding +
'                            },' + LineEnding +
'                            "BorderColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                            },' + LineEnding +
'                            "ColorPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                              "Blue" : 7.6078432798385620E-001,' + LineEnding +
'                              "Green" : 7.6078432798385620E-001,' + LineEnding +
'                              "Red" : 7.6078432798385620E-001' + LineEnding +
'                            },' + LineEnding +
'                            "HeightFraction" : 1.0000000000000000E+000,' + LineEnding +
'                            "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                            "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                            "Name" : "PropValueCheckboxParent",' + LineEnding +
'                            "TranslationPersistent" : {' + LineEnding +
'                              "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                            },' + LineEnding +
'                            "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                            "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                            "WidthFraction" : 5.0000000000000000E-001,' + LineEnding +
'                            "$Children" : [' + LineEnding +
'                              {' + LineEnding +
'                                "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                                "Border" : {' + LineEnding +
'                                  "$$ClassName" : "TBorder"' + LineEnding +
'                                },' + LineEnding +
'                                "BorderColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "CheckboxColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "Name" : "PropValueCheckbox",' + LineEnding +
'                                "TextColorPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                                },' + LineEnding +
'                                "TranslationPersistent" : {' + LineEnding +
'                                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                                },' + LineEnding +
'                                "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                                "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                              }' + LineEnding +
'                            ]' + LineEnding +
'                          }' + LineEnding +
'                        ]' + LineEnding +
'                      }' + LineEnding +
'                    ]' + LineEnding +
'                  }' + LineEnding +
'                ]' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarFrame" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                "ClipLinePersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                },' + LineEnding +
'                "ColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "ProtectedSides" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "RegionPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                },' + LineEnding +
'                "RotationCenterPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                }' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarSlider" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                "ClipLinePersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                },' + LineEnding +
'                "ColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "ProtectedSides" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "RegionPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                },' + LineEnding +
'                "RotationCenterPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                }' + LineEnding +
'              },' + LineEnding +
'              "TintScrollBarInactivePersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              }' + LineEnding +
'            }' + LineEnding +
'          ]' + LineEnding +
'        },' + LineEnding +
'        {' + LineEnding +
'          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder",' + LineEnding +
'            "AllSides" : 1.0000000000000000E+001' + LineEnding +
'          },' + LineEnding +
'          "BorderColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 7.5000000000000000E-001,' + LineEnding +
'          "Name" : "RectHierarchy",' + LineEnding +
'          "TranslationPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'          },' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderHierarchy",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
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
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelHierarchyHeader",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Hierarchy"' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "AutoSize" : false,' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "X",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomTextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Height" : 3.0000000000000000E+001,' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonHierarchyHide",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "Width" : 3.0000000000000000E+001' + LineEnding +
'                }' + LineEnding +
'              ]' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleScrollView",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder",' + LineEnding +
'                "Bottom" : 3.0000000000000000E+001,' + LineEnding +
'                "Top" : 3.0000000000000000E+001' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "Name" : "ScrollView3",' + LineEnding +
'              "ScrollArea" : {' + LineEnding +
'                "$$ClassName" : "TCastleScrollArea",' + LineEnding +
'                "Border" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "BorderColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "TranslationPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                },' + LineEnding +
'                "$Children" : [' + LineEnding +
'                  {' + LineEnding +
'                    "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                    "Border" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "BorderColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "FrameColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "Name" : "HierarchyRowParent",' + LineEnding +
'                    "TranslationPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "$Children" : [' + LineEnding +
'                      {' + LineEnding +
'                        "$$ClassName" : "TCastleButton",' + LineEnding +
'                        "Alignment" : "hpLeft",' + LineEnding +
'                        "AutoSize" : false,' + LineEnding +
'                        "Border" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "BorderColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "Caption" : "HierarchyRowTemplate",' + LineEnding +
'                        "CustomBackground" : true,' + LineEnding +
'                        "CustomBackgroundDisabled" : {' + LineEnding +
'                          "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                          "ClipLinePersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ProtectedSides" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "RegionPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "RotationCenterPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          }' + LineEnding +
'                        },' + LineEnding +
'                        "CustomBackgroundFocused" : {' + LineEnding +
'                          "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                          "ClipLinePersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ProtectedSides" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "RegionPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "RotationCenterPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          }' + LineEnding +
'                        },' + LineEnding +
'                        "CustomBackgroundNormal" : {' + LineEnding +
'                          "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                          "ClipLinePersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ProtectedSides" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "RegionPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "RotationCenterPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          }' + LineEnding +
'                        },' + LineEnding +
'                        "CustomBackgroundPressed" : {' + LineEnding +
'                          "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                          "ClipLinePersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ProtectedSides" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "RegionPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "RotationCenterPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          }' + LineEnding +
'                        },' + LineEnding +
'                        "CustomColorDisabledPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                          "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                          "Green" : 1.0000000000000000E+000,' + LineEnding +
'                          "Red" : 1.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "CustomColorFocusedPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                          "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                          "Green" : 1.0000000000000000E+000,' + LineEnding +
'                          "Red" : 1.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "CustomColorNormalPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                          "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                          "Green" : 1.0000000000000000E+000,' + LineEnding +
'                          "Red" : 1.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "CustomColorPressedPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 1.0000000000000000E+000,' + LineEnding +
'                          "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                          "Green" : 1.0000000000000000E+000,' + LineEnding +
'                          "Red" : 1.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "CustomTextColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "EnableParentDragging" : true,' + LineEnding +
'                        "Height" : 3.0000000000000000E+001,' + LineEnding +
'                        "Image" : {' + LineEnding +
'                          "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                          "ClipLinePersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ProtectedSides" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "RegionPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "RotationCenterPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          }' + LineEnding +
'                        },' + LineEnding +
'                        "Name" : "HierarchyRowTemplate",' + LineEnding +
'                        "OutlineColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "PaddingHorizontal" : 5.0000000000000000E+000,' + LineEnding +
'                        "PaddingVertical" : 5.0000000000000000E+000,' + LineEnding +
'                        "TextAlignment" : "hpLeft",' + LineEnding +
'                        "TintDisabledPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "TintFocusedPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Blue" : 4.8235294222831726E-001' + LineEnding +
'                        },' + LineEnding +
'                        "TintNormalPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Alpha" : 0.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "TintPressedPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                          "Blue" : 0.0000000000000000E+000' + LineEnding +
'                        },' + LineEnding +
'                        "Toggle" : true,' + LineEnding +
'                        "TranslationPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                        "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                        "Width" : 3.5000000000000000E+002' + LineEnding +
'                      }' + LineEnding +
'                    ]' + LineEnding +
'                  }' + LineEnding +
'                ]' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarFrame" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                "ClipLinePersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                },' + LineEnding +
'                "ColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "ProtectedSides" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "RegionPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                },' + LineEnding +
'                "RotationCenterPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                }' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarSlider" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                "ClipLinePersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                },' + LineEnding +
'                "ColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "ProtectedSides" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "RegionPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                },' + LineEnding +
'                "RotationCenterPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                }' + LineEnding +
'              },' + LineEnding +
'              "TintScrollBarInactivePersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              }' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 9.0196079015731812E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HierarchyBottomControls",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
'              "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Show Even Internal",' + LineEnding +
'                  "CheckboxColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "CheckboxShowEvenInternal",' + LineEnding +
'                  "TextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  }' + LineEnding +
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
'          "Border" : {' + LineEnding +
'            "$$ClassName" : "TBorder"' + LineEnding +
'          },' + LineEnding +
'          "BorderColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'          },' + LineEnding +
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
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
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
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                "Green" : 1.0000000000000000E+000,' + LineEnding +
'                "Red" : 1.0000000000000000E+000' + LineEnding +
'              },' + LineEnding +
'              "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'              "FrameColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "LabelStatsMore",' + LineEnding +
'              "OutlineColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
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
'          "BorderColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
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
'          "TranslationPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'          },' + LineEnding +
'          "VerticalAnchorParent" : "vpTop",' + LineEnding +
'          "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'          "WidthFraction" : 5.0000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderProfiler",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
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
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelProfilerHeader",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Frame Profiler"' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "AutoSize" : false,' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "X",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomTextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Height" : 3.0000000000000000E+001,' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonProfilerHide",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "Width" : 3.0000000000000000E+001' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Log Stats",' + LineEnding +
'                  "CheckboxColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "CheckboxProfilerDetailsInLog",' + LineEnding +
'                  "TextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -4.0000000000000000E+001' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "More",' + LineEnding +
'                  "CheckboxColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "CheckboxProfilerMore",' + LineEnding +
'                  "TextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
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
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
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
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "Name" : "ProfilerGraph",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
'              "$Children" : [' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleLabel",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                    "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                    "Green" : 1.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "Label2",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "FPS = 60.0"' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                    "Alpha" : 1.0000000149011612E-001,' + LineEnding +
'                    "Blue" : 0.0000000000000000E+000,' + LineEnding +
'                    "Red" : 0.0000000000000000E+000' + LineEnding +
'                  },' + LineEnding +
'                  "Height" : 2.0000000000000000E+000,' + LineEnding +
'                  "Name" : "RectangleControl5",' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "WidthFraction" : 1.0000000000000000E+000' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FrameColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "HorizontalGroup1",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Blue" : 0.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl2",' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "FrameColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "Name" : "Label3",' + LineEnding +
'                          "OutlineColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FrameColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "HorizontalGroup4",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "Y" : -1.6500001907348633E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Blue" : 0.0000000000000000E+000,' + LineEnding +
'                            "Green" : 4.7843137383460999E-001,' + LineEnding +
'                            "Red" : 4.7843137383460999E-001' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl6",' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "FrameColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "Name" : "Label6",' + LineEnding +
'                          "OutlineColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FrameColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "HorizontalGroup2",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "Y" : -3.3000003814697266E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Green" : 4.0392157435417175E-001,' + LineEnding +
'                            "Red" : 4.0392157435417175E-001' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl3",' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "FrameColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "Name" : "Label4",' + LineEnding +
'                          "OutlineColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
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
'                      "Border" : {' + LineEnding +
'                        "$$ClassName" : "TBorder"' + LineEnding +
'                      },' + LineEnding +
'                      "BorderColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "FrameColorPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                      },' + LineEnding +
'                      "Name" : "HorizontalGroup3",' + LineEnding +
'                      "Spacing" : 5.0000000000000000E+000,' + LineEnding +
'                      "TranslationPersistent" : {' + LineEnding +
'                        "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                        "Y" : -4.9500007629394531E+001' + LineEnding +
'                      },' + LineEnding +
'                      "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                      "VerticalAnchorSelf" : "vpTop",' + LineEnding +
'                      "$Children" : [' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Green" : 6.9411766529083252E-001,' + LineEnding +
'                            "Red" : 6.9411766529083252E-001' + LineEnding +
'                          },' + LineEnding +
'                          "Height" : 1.0000000000000000E+001,' + LineEnding +
'                          "Name" : "RectangleControl4",' + LineEnding +
'                          "TranslationPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                          },' + LineEnding +
'                          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                          "Width" : 1.0000000000000000E+001' + LineEnding +
'                        },' + LineEnding +
'                        {' + LineEnding +
'                          "$$ClassName" : "TCastleLabel",' + LineEnding +
'                          "Border" : {' + LineEnding +
'                            "$$ClassName" : "TBorder"' + LineEnding +
'                          },' + LineEnding +
'                          "BorderColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "ColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                            "Alpha" : 5.0000000000000000E-001,' + LineEnding +
'                            "Blue" : 1.0000000000000000E+000,' + LineEnding +
'                            "Green" : 1.0000000000000000E+000,' + LineEnding +
'                            "Red" : 1.0000000000000000E+000' + LineEnding +
'                          },' + LineEnding +
'                          "FontSize" : 1.5000000000000000E+001,' + LineEnding +
'                          "FrameColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
'                          "Name" : "Label5",' + LineEnding +
'                          "OutlineColorPersistent" : {' + LineEnding +
'                            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                          },' + LineEnding +
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
'          "BorderColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'          },' + LineEnding +
'          "ColorPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'            "Alpha" : 8.9999997615814209E-001' + LineEnding +
'          },' + LineEnding +
'          "HeightFraction" : 2.5000000000000000E-001,' + LineEnding +
'          "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'          "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'          "Name" : "RectLog",' + LineEnding +
'          "TranslationPersistent" : {' + LineEnding +
'            "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'          },' + LineEnding +
'          "WidthFraction" : 7.5000000000000000E-001,' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "$$ClassName" : "TCastleRectangleControl",' + LineEnding +
'              "Border" : {' + LineEnding +
'                "$$ClassName" : "TBorder"' + LineEnding +
'              },' + LineEnding +
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "ColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent",' + LineEnding +
'                "Alpha" : 8.9999997615814209E-001,' + LineEnding +
'                "Blue" : 7.5294119119644165E-001,' + LineEnding +
'                "Green" : 7.5294119119644165E-001,' + LineEnding +
'                "Red" : 7.5294119119644165E-001' + LineEnding +
'              },' + LineEnding +
'              "Height" : 3.0000000000000000E+001,' + LineEnding +
'              "Name" : "HeaderLog",' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'              },' + LineEnding +
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
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "ColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "FrameColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "LabelLogHeader",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Text" : [' + LineEnding +
'                    "Log (10)"' + LineEnding +
'                  ],' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "AutoSize" : false,' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "X",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomTextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Height" : 3.0000000000000000E+001,' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonLogHide",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'                  "Width" : 3.0000000000000000E+001' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleButton",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Clear",' + LineEnding +
'                  "CustomBackgroundDisabled" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundFocused" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundNormal" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomBackgroundPressed" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomColorPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "CustomTextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Image" : {' + LineEnding +
'                    "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                    "ClipLinePersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "ProtectedSides" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "RegionPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "RotationCenterPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    }' + LineEnding +
'                  },' + LineEnding +
'                  "Name" : "ButtonLogClear",' + LineEnding +
'                  "OutlineColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "PaddingVertical" : 5.0000000000000000E+000,' + LineEnding +
'                  "TintDisabledPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintFocusedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintNormalPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TintPressedPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "TranslationPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleVector2Persistent",' + LineEnding +
'                    "X" : -3.2000000000000000E+002' + LineEnding +
'                  },' + LineEnding +
'                  "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'                  "VerticalAnchorSelf" : "vpMiddle"' + LineEnding +
'                },' + LineEnding +
'                {' + LineEnding +
'                  "$$ClassName" : "TCastleCheckbox",' + LineEnding +
'                  "Border" : {' + LineEnding +
'                    "$$ClassName" : "TBorder"' + LineEnding +
'                  },' + LineEnding +
'                  "BorderColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "Caption" : "Auto Scroll To Bottom",' + LineEnding +
'                  "CheckboxColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
'                  "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'                  "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'                  "Name" : "CheckboxLogAutoScroll",' + LineEnding +
'                  "TextColorPersistent" : {' + LineEnding +
'                    "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                  },' + LineEnding +
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
'              "BorderColorPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "FullSize" : true,' + LineEnding +
'              "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'              "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'              "Name" : "ScrollLogs",' + LineEnding +
'              "ScrollArea" : {' + LineEnding +
'                "$$ClassName" : "TCastleScrollArea",' + LineEnding +
'                "Border" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "BorderColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "TranslationPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                },' + LineEnding +
'                "$Children" : [' + LineEnding +
'                  {' + LineEnding +
'                    "$$ClassName" : "TCastleVerticalGroup",' + LineEnding +
'                    "Border" : {' + LineEnding +
'                      "$$ClassName" : "TBorder"' + LineEnding +
'                    },' + LineEnding +
'                    "BorderColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "FrameColorPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                    },' + LineEnding +
'                    "Name" : "LogsVerticalGroup",' + LineEnding +
'                    "Padding" : 2.0000000000000000E+000,' + LineEnding +
'                    "TranslationPersistent" : {' + LineEnding +
'                      "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                    },' + LineEnding +
'                    "$Children" : [' + LineEnding +
'                      {' + LineEnding +
'                        "$$ClassName" : "TCastleLabel",' + LineEnding +
'                        "Border" : {' + LineEnding +
'                          "$$ClassName" : "TBorder"' + LineEnding +
'                        },' + LineEnding +
'                        "BorderColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "ColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "Exists" : false,' + LineEnding +
'                        "FrameColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "Name" : "LabelEarlierLogsRemoved",' + LineEnding +
'                        "OutlineColorPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                        },' + LineEnding +
'                        "Text" : [' + LineEnding +
'                          "Earlier log messages have been hidden."' + LineEnding +
'                        ],' + LineEnding +
'                        "TranslationPersistent" : {' + LineEnding +
'                          "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                        },' + LineEnding +
'                        "VerticalAnchorParent" : "vpTop",' + LineEnding +
'                        "VerticalAnchorSelf" : "vpTop"' + LineEnding +
'                      }' + LineEnding +
'                    ]' + LineEnding +
'                  }' + LineEnding +
'                ]' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarFrame" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                "ClipLinePersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                },' + LineEnding +
'                "ColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "ProtectedSides" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "RegionPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                },' + LineEnding +
'                "RotationCenterPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                }' + LineEnding +
'              },' + LineEnding +
'              "ScrollbarSlider" : {' + LineEnding +
'                "$$ClassName" : "TCastleImagePersistent",' + LineEnding +
'                "ClipLinePersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector3Persistent"' + LineEnding +
'                },' + LineEnding +
'                "ColorPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'                },' + LineEnding +
'                "ProtectedSides" : {' + LineEnding +
'                  "$$ClassName" : "TBorder"' + LineEnding +
'                },' + LineEnding +
'                "RegionPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TFloatRectanglePersistent"' + LineEnding +
'                },' + LineEnding +
'                "RotationCenterPersistent" : {' + LineEnding +
'                  "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
'                }' + LineEnding +
'              },' + LineEnding +
'              "TintScrollBarInactivePersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleColorPersistent"' + LineEnding +
'              },' + LineEnding +
'              "TranslationPersistent" : {' + LineEnding +
'                "$$ClassName" : "TCastleVector2Persistent"' + LineEnding +
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
