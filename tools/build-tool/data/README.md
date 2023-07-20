Data of the Castle Game Engine Build Tool.

Some of the files and subdirectories here (like android/integrated/ , ios/xcode_project/) are copied to the final project (e.g. for Android and iOS) using our custom system for templates and macros.

Basic syntax of macros (which you can use in any text template file):

- ${MacroVariableName} - Insert given variable name. See the TCastleProject.ReplaceMacros implementation for a list of possible macros.

    This can be used in a "normal" context (outside of an expression within ${IF xxx}, ${CALCULATE xxx} or similar). Then we simply insert the value without any additional processing.

    This can also be used in CastleScript expressions (inside ${IF xxx}, ${CALCULATE xxx} or similar). In this case we insert the value converted to a CastleScript string constant (with added apostrophes around, quoted apostrophes inside).

- ${CALCULATE xxx}

    Calculate the xxx as a CastleScript expression that should return String.

- ${IF xxx}
  one text to include
  ${ELSE}
  alternative text to include
  ${ENDIF}

    Calculate the xxx as a CastleScript expression that should return Boolean.
    If true, executes 1st part, otherwise executes the 2nd.
    The "${ELSE} .. " block is optional.

    TODO: Nesting ${IF} within ${IF} is not implemented yet.

See https://castle-engine.io/castle_script.php for the documentation of CastleScript.

The core of it is implemented in TCastleProject.ExtractTemplateFile, TCastleProject.ReplaceMacros and ToolMacros unit.