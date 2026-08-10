{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Generate Pascal code with "flat" WebGL API. See README.md for details. }

{$ifndef FPC}
  {$error This program requires FPC.}
{$endif}
{$if defined(VER3_0) or defined(VER3_2)}
  {$error This program requires FPC > 3.2, with WEBIDL support.}
{$endif}

uses SysUtils, Classes, StrUtils,
  WebidlParser, WebidlScanner, WebidlDefs,
  CastleUtils, CastleUriUtils, CastleDownload, CastleClassUtils,
  CastleStringUtils;

{ Only for debug: Writeln all definitions. }
procedure DebugWriteDefinitions(const Definitions: TIDLDefinitionList);
var
  I: Integer;
begin
  Write('Definitions count: ', Definitions.Count, ' ');
  for I := 0 to Definitions.Count - 1 do
  begin
    Writeln('Definition: ', Definitions[I].Name, ': ', Definitions[I].ClassName);
    try
      // AsString crashes with EAccessViolation for some types
      Writeln(Definitions[I].AsString(true) + ';');
    except
      on E: Exception do
        Writeln('  Error when doing AsString: ' + ExceptMessage(E));
    end;
  end;
end;

{ Find WEBIDL interface with the given name, raise exception if not found. }
function FindInterface(const Context: TWebIDLContext;
  const InterfaceName: String): TIDLInterfaceDefinition;
var
  Index: Integer;
begin
  for Index := 0 to Context.Definitions.Count - 1 do
    if (Context.Definitions[Index].Name = InterfaceName) and
       (Context.Definitions[Index] is TIDLInterfaceDefinition)
       //and
       //(not TIDLInterfaceDefinition(Context.Definitions[Index]).IsMixin)
       then
      Exit(TIDLInterfaceDefinition(Context.Definitions[Index]));

  raise Exception.CreateFmt('Interface %s not found', [InterfaceName]);
end;

{ Add to OutputStream Pascal code that exposes constants
  from the given WEBIDL interface. }
procedure ExposeConstants(const Context: TWebIDLContext; const OutputStream: TStream;
  const InterfaceName: String);
var
  Definition: TIDLInterfaceDefinition;
  I: Integer;
  Constant: TIDLConstDefinition;
  MemberName: String;
  AnyOutput: Boolean;
begin
  Writeln('Exposing constants from ', InterfaceName);
  AnyOutput := false;

  Definition := FindInterface(Context, InterfaceName);

  // DebugWriteDefinitions(Definition.Members);
  for I := 0 to Definition.Members.Count - 1 do
  begin
    if Definition.Members[I] is TIDLConstDefinition then
    begin
      Constant := Definition.Members[I] as TIDLConstDefinition;
      MemberName := Constant.Name;
      // add _ suffix, to match what webidl2pas is doing
      if ArrayContainsString(MemberName, ['VIEWPORT', 'REPEAT']) then
        MemberName := MemberName + '_';
      if not AnyOutput then
      begin
        WritelnStr(OutputStream, '');
        WritelnStr(OutputStream, Format('{ Constants from %s }', [InterfaceName]));
        WritelnStr(OutputStream, 'const');
        AnyOutput := true;
      end;
      WritelnStr(OutputStream, Format('  GL_%s = TJS%s.%s;', [
        Constant.Name,
        InterfaceName,
        MemberName
      ]));
    end;
  end;
end;

{ Add to OutputStream Pascal code that exposes functions
  from the given WEBIDL interface. }
procedure ExposeFunctions(const Context: TWebIDLContext;
  const OutputStream, OutputStreamImplementation: TStream;
  const InterfaceName: String; const WebGL2: Boolean);

  function PascalType(const IdlType: TIDLTypeDefDefinition): String;
  begin
    if NameToWebIDLBaseType(IdlType.TypeName) = wibtAny then
      Result := 'Variant'
    else
    if SameText(IdlType.TypeName, 'boolean') then
      Result := 'Boolean'
    else
    if SameText(IdlType.TypeName, 'domstring') then
      Result := 'String' // Note that our flat API converts UnicodeString (TDOMString) <-> AnsiString (String)
    else
    // JS objects
    if IsPrefix('WebGL', IdlType.TypeName) or
       IsPrefix('HTML', IdlType.TypeName) or
       SameText('ArrayBuffer', IdlType.TypeName) or
       SameText('ArrayBufferView', IdlType.TypeName) or
       SameText('VideoFrame', IdlType.TypeName) or
       SameText('ImageData', IdlType.TypeName) or
       SameText('ImageBitmap', IdlType.TypeName) or
       SameText('OffscreenCanvas', IdlType.TypeName) then
      Result := 'IJS' + IdlType.TypeName
    else
    if SameText('Float32List', IdlType.TypeName) then
      Result := 'IJSFloat32Array'
    else
    if SameText('Int32List', IdlType.TypeName) then
      Result := 'IJSInt32Array'
    else
    if SameText('Uint32List', IdlType.TypeName) then
      Result := 'IJSUint32Array'
    else
      Result := 'T' + IdlType.TypeName;
  end;

  { In flat API, we cannot expose functions that take/return some types. }
  function UnsupportedType(const IdlType: TIDLTypeDefDefinition): Boolean;
  begin
    Result :=
      (IdlType is TIDLSequenceTypeDefDefinition) or
      SameText('object', IdlType.TypeName);
  end;

  { Is S a keyword in Pascal.
    Ignores case.
    Implementation copied from FPC TPascalCodeGenerator.IsKeyWord
    (same license as CGE). }
  function IsKeyWord(const S: String): Boolean;
  const
    KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
      'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
      'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
      'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'+
      'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
      'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
      'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
      'private;published;length;setlength;';
  begin
    Result := Pos(';' + LowerCase(S) + ';', KW) <> 0;
  end;

var
  Definition: TIDLInterfaceDefinition;
  I, J: Integer;
  Func: TIDLFunctionDefinition;
  AnyOutput, HasReturnValue, Unsupported: Boolean;
  FlatName, ArgumentsDeclare, ArgumentsCall, ArgumentName: String;
  GLObject, GLVersion, GLObjectCheck: String;
begin
  Writeln('Exposing functions from ', InterfaceName);
  AnyOutput := false;

  Definition := FindInterface(Context, InterfaceName);

  for I := 0 to Definition.Members.Count - 1 do
  begin
    if Definition.Members[I] is TIDLFunctionDefinition then
    begin
      Func := Definition.Members[I] as TIDLFunctionDefinition;
      FlatName := 'gl' + UpCase(Func.Name[1]) + SEnding(Func.Name, 2);
      if not AnyOutput then
      begin
        WritelnStr(OutputStream, '');
        WritelnStr(OutputStream, Format('{ Functions from %s }', [InterfaceName]));
        AnyOutput := true;
      end;

      Unsupported := false;

      // calculate ArgumentsDeclare, ArgumentsCall
      ArgumentsDeclare := '';
      ArgumentsCall := '';
      for J := 0 to Func.Arguments.Count - 1 do
      begin
        Unsupported := Unsupported or
          UnsupportedType(Func.Argument[J].ArgumentType);
        ArgumentName := Func.Argument[J].Name;
        if IsKeyWord(ArgumentName) then
          ArgumentName := ArgumentName + '_';
        ArgumentsDeclare := SAppendPart(ArgumentsDeclare, '; ',
          Format('const %s: %s', [
            ArgumentName,
            PascalType(Func.Argument[J].ArgumentType)
          ]));
        ArgumentsCall := SAppendPart(ArgumentsCall, ', ',
          ArgumentName);
      end;

      HasReturnValue :=
        (Func.ReturnType <> nil) and
        (NameToWebIDLBaseType(Func.ReturnType.TypeName) <> wibtVoid) and
        (NameToWebIDLBaseType(Func.ReturnType.TypeName) <> wibtUndefined);

      Unsupported := Unsupported or
        (HasReturnValue and UnsupportedType(Func.ReturnType));

      if Unsupported then
      begin
        WriteStr(OutputStream, '// Not in auto-generated flat WebGL API: ');
        WritelnStr(OutputStreamImplementation, '{ Not in auto-generated flat WebGL API:');
      end;

      GLObject := IfThen(WebGL2, 'GL2', 'GL');
      GLVersion := IfThen(WebGL2, '2', '1');
      GLObjectCheck := Format('if %s = nil then raise Exception.Create(''Failed to load WebGL context (version %s.0), which is necessary to call %s'');', [
        GLObject,
        GLVersion,
        Func.Name
      ]);

      if HasReturnValue then
      begin
        WritelnStr(OutputStream, Format('function %s(%s): %s;', [
          FlatName,
          ArgumentsDeclare,
          PascalType(Func.ReturnType)
        ]));
        WritelnStr(OutputStreamImplementation, Format(
          'function %s(%s): %s;' + NL +
          'begin' + NL +
          '  %s' + NL +
          '  Result := %s.%s(%s);' + NL +
          'end;' + NL, [
          FlatName,
          ArgumentsDeclare,
          PascalType(Func.ReturnType),
          GLObjectCheck,
          GLObject,
          Func.Name,
          ArgumentsCall
        ]));
      end else
      begin
        WritelnStr(OutputStream, Format('procedure %s(%s);', [
          FlatName,
          ArgumentsDeclare
        ]));
        WritelnStr(OutputStreamImplementation, Format(
          'procedure %s(%s);' + NL +
          'begin' + NL +
          '  %s' + NL +
          '  %s.%s(%s);' + NL +
          'end;' + NL, [
          FlatName,
          ArgumentsDeclare,
          GLObjectCheck,
          GLObject,
          Func.Name,
          ArgumentsCall
        ]));
      end;

      if Unsupported then
        WritelnStr(OutputStreamImplementation, '}');
    end;
  end;
end;

{ main program }

var
  WebidlFileName, OutputFileName: String;
  InputStream, OutputStream: TStream;
  OutputStreamImplementation: TStringStream;
  Scanner: TWebIDLScanner;
  Parser: TWebIDLParser;
  Context: TWebIDLContext;
begin
  WebidlFileName := InclPathDelim(GetEnvironmentVariable('CASTLE_ENGINE_PATH')) +
    'src/base_rendering/web/webidl/castleinternaljobweb.webidl';
  OutputFileName := InclPathDelim(GetEnvironmentVariable('CASTLE_ENGINE_PATH')) +
    'src/base_rendering/web/castleinternalwebgl_flat_api.inc';

  { We know that at program start, all the variables above like Stream are nil,
    so we can safely use one try-finally to free everything at end. }

  try
    OutputStream := UrlSaveStream(FilenameToUriSafe(OutputFileName));
    WritelnStr(OutputStream, '{ This file is automatically generated by generate_webgl_flat_api. }');
    WritelnStr(OutputStream, '{$ifdef read_interface}');

    OutputStreamImplementation := TStringStream.Create('');

    Context := TWebIDLContext.Create;
    InputStream := Download(FilenameToUriSafe(WebidlFileName));
    Scanner := TWebIDLScanner.Create(InputStream);
    Parser := TWebIDLParser.Create(Context, Scanner);
    Parser.Version := v2;

    Writeln('Parsing ', WebidlFileName);
    Parser.Parse;

    // These make sense, but don't seem to actually have any effect in our case
    Writeln('Appending partials to interfaces.');
    Context.AppendPartials;
    Writeln('Appending includes to interfaces.');
    Context.AppendIncludes;

    // DebugWriteDefinitions(Context.Definitions);

    ExposeConstants(Context, OutputStream, 'WebGLRenderingContextBase');
    ExposeConstants(Context, OutputStream, 'WebGL2RenderingContextBase');
    ExposeConstants(Context, OutputStream, 'EXT_texture_filter_anisotropic');
    ExposeFunctions(Context, OutputStream, OutputStreamImplementation,
      'WebGLRenderingContextBase', false);
    ExposeFunctions(Context, OutputStream, OutputStreamImplementation,
      'WebGL2RenderingContextBase', true);

    // write implementation
    WritelnStr(OutputStream,
      '{$endif read_interface}' + NL +
      NL +
      '{$ifdef read_implementation}');
    WriteStr(OutputStream, OutputStreamImplementation.DataString);
    WritelnStr(OutputStream, '{$endif read_implementation}');
  finally
    FreeAndNil(OutputStream);
    FreeAndNil(Parser);
    FreeAndNil(Scanner);
    FreeAndNil(InputStream);
    FreeAndNil(Context);
  end;
end.
