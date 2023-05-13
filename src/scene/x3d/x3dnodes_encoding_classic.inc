{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global routines for parsing classic VRML/X3D encoding. }

{$ifdef read_interface}

const
  { File filters if you want to open a file and then
    pass it to LoadX3DClassic.
    Suitable for TCastleWindow.FileDialog and such. }
  LoadX3DClassic_FileFilters =
  'All files|*|' +
  '*VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz'
  deprecated 'use LoadNode and LoadScene_FileFilters';

type
  { Raised by classic X3D loading routines when the stream is compressed with gzip. }
  EGzipCompressed = CastleInternalX3DLexer.EGzipCompressed;

{ Load VRML/X3D file in classic encoding from stream. Return it's root node.

  This function can't handle compressed data (X3D files are
  sometimes compressed with gzip). You should pass here a stream
  with an already uncompressed text data.

  The BaseUrl must be absolute URL. Use AbsoluteURI to make sure to convert
  eventual relative URLs to absolute.

  @raises(EGzipCompressed If the Stream starts with gzip file header.)

  @exclude }
function LoadX3DClassicInternal(const Stream: TStream;
  const BaseUrl: string): TX3DRootNode; overload;

{ Load VRML/X3D file in classic encoding from a string. Return it's root node.
  @raises(EGzipCompressed If the Stream is compressed with gzip.) }
function LoadX3DClassicFromString(const VRMLContents: string;
  const BaseUrl: string): TX3DRootNode; deprecated 'use LoadNode with TStringStream';

{ Load VRML/X3D file in classic encoding, return it's root node.

  Automatically handles VRML/X3D files compressed with gzip,
  uncompressing them on the fly.

  URL without any protocol is treated as a filename (absolute or relative
  to current dir). We can also load network URLs using CastleDownload.

  If Gzipped then we can be relatively sure that stream is gzipped.
  Otherwise, it can still be gzipped (.wrl files are sometimes gzipped
  without extension indicating it). }
function LoadX3DClassic(const URL: string;
  const Gzipped: boolean): TX3DRootNode; deprecated 'use LoadNode';

{$endif read_interface}

{$ifdef read_implementation}

{ Internal for ParseIgnoreToMatchingSqBracket and
  ParseIgnoreToMatchingCurlyBracket }
procedure ParseIgnoreToMatchingBracket(
  Lexer: TX3DLexer; Reader: TX3DReaderNames;
  LevelSqBracket, LevelCurlyBracket: integer);
begin
  while (LevelSqBracket > 0) or
        (LevelCurlyBracket > 0) do
  begin
    case Lexer.Token of
      vtOpenCurlyBracket: Inc(LevelCurlyBracket);
      vtCloseCurlyBracket: Dec(LevelCurlyBracket);
      vtOpenSqBracket: Inc(LevelSqBracket);
      vtCloseSqBracket: Dec(LevelSqBracket);
      vtEnd: raise EX3DParserError.Create(Lexer, 'Unexpected end of stream');
      else ;
    end;
    Lexer.NextToken;
  end;
end;

(* Assume that we just read "[", and are looking at next character.
   Read everything up to (and including) matching "]".
   This is a hack to omit (not really parse) interface sections
   of prototypes. *)
procedure ParseIgnoreToMatchingSqBracket(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  ParseIgnoreToMatchingBracket(Lexer, Reader, 1, 0);
end;

(* Just like ParseIgnoreToMatchingSqBracket, but here for "{" and "}" brackets.
   This is a hack to omit (not really parse) unknown VRML nodes. *)
procedure ParseIgnoreToMatchingCurlyBracket(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  ParseIgnoreToMatchingBracket(Lexer, Reader, 0, 1);
end;

(*
  Parse VRML node. This parses
  @preformatted([ DEF <nodename> ] <nodetype> { node-content })
  or
  @preformatted(USE <nodename>)

  If we will find USE clause but node name will be unknown, the normal
  behavior (when NilIfUnresolvedUSE = @false, default) is to raise
  EX3DParserError (just like in case of many other errors).
  However, this is a particular parsing error, because we can probably
  pretty safely continue parsing, ignoring this error.
  So if you pass NilIfUnresolvedUSE = @true, this function will do
  WritelnWarning and simply return @nil.

  @raises(EX3DParserError On various parsing errors.)
*)
function ParseNode(const Lexer: TX3DLexer; const Reader: TX3DReaderNames;
  const NilIfUnresolvedUSE: Boolean;
  const AllowNodeCycle: Boolean; var UsingNodeCycle: Boolean): TX3DNode; overload;

  procedure ParseNamedNode(const NodeName: string);
  var
    NodeClass: TX3DNodeClass;
    X3DType: string;
    Proto: TX3DPrototypeBase;
  begin
    Lexer.CheckTokenIs(vtName, 'node type');
    X3DType := Lexer.TokenName;
    Lexer.NextToken;

    { VRML / X3D specifications say it is an error for a PROTO to have
      the same name as built-in node. I didn't found any definite
      statements about EXTERNPROTO names, although I vaguely remember
      some notes that EXTERNPROTO name should take precedence over
      built-in names. And this seems most sensible. Especially for
      nodes that were not available in older VRML / X3D editions and
      were declared (possibly slightly sensible) using EXTERNPROTOs,
      it's a good thing to use EXTERNPROTOs: they were probably most
      current with respect to given VRML file. Examples are CAD
      test files from
      http://www.web3d.org/x3d/content/examples/Basic/CAD/

      So we first search for X3DType among protos.
      Only when this failed, we look at built-in nodes in NodesManager.
    }

    Proto := Reader.Prototypes.Bound(X3DType);
    if Proto <> nil then
    begin
      if (Proto is TX3DExternalPrototype) and
         (TX3DExternalPrototype(Proto).ReferencedClass <> nil) then
        Result := TX3DExternalPrototype(Proto).ReferencedClass.Create(NodeName, Reader.BaseUrl) else
        Result := TX3DPrototypeNode.CreatePrototypeNode(NodeName, Reader.BaseUrl, Proto);
    end else
    begin
      NodeClass := NodesManager.X3DTypeToClass(X3DType, Reader.Version);
      if NodeClass <> nil then
      begin
        Result := NodeClass.Create(NodeName, Reader.BaseUrl);
      end else
      begin
        Result := TX3DUnknownNode.CreateUnknown(NodeName, Reader.BaseUrl, X3DType);
      end;
    end;

    { Register node name early, before it is finalized.
      Reason:
      - Some X3D models use ROUTEs inside the nodes to/from which they lead.
      - Some X3D models may have cycles in node graph (see notes below about Script).
    }
    Reader.Nodes.Bind(Result, false);

    Result.Parse(Lexer, Reader);

    if Result is TX3DPrototypeNode then
    try
      { Note that this changes Result, and the new Result must be send to Nodes.Bind,
        as the "instantiated" node must be registered. }
      Result := TX3DPrototypeNode(Result).Instantiate;
    except
      on E: EX3DPrototypeInstantiateError do
        { Just write E.Message and silence the exception.
          Result will simply remain as TX3DPrototypeNode instance in this case. }
        WritelnWarning('VRML/X3D', E.Message);
    end;

    { Cycles in VRML / X3D graph are possible.

      Problems with them:
      - they can cause our traversing routines to loop infinitely
        (we don't check there to avoid visiting already visited nodes).
      - they can cause memory leaks, since reference
        counting doesn't work when we have loops.

      But we cannot really avoid handling them.
      We need to
        Reader.Nodes.Bind(Result, false)
      before parsing node centents, and
        Reader.Nodes.Bind(Result, true)
      after.

      That is because a Script node's interface declarations may contain
      cyclic references, to Script node itself or it's parents. See
        demo-models/x3d/node_cycle_in_script.x3d
        demo-models/x3d/node_cycle_in_script.x3dv

      Also RenderedTexture.scene may contain a cycle.

      Looking at spec: It prohibits certain special kinds of cycles,
      but not *any* cycles in graph.

      - X3D specification says
        "A children field is not allowed to contain nodes that
        are ancestors of the grouping node"
        (so "children" MFNode field cycles are not allowed).

      - Many other cases cycles are not possible,
        e.g. Appearance cannot create cycles since no Appearance children
        can be of Appearance type.

      SFNode can handle cycles in some cases with AutomaticWeakLink.
      When not AutomaticWeakLink, at least we use NilIfUnresolvedUSE
      to clearly ignore such cycles and continue reading the file. }

    Reader.Nodes.Bind(Result, true);
  end;

var
  NodeName, S: string;
  UsedNodeFinished: boolean;
begin
  Result := nil;
  try
    (* node means :
       DEF <name> <nodetype> { node-content } or
       USE <name> or
       <nodetype> { node-content }
    *)

    case Lexer.Token of
     vtKeyword:
       case Lexer.TokenKeyword of
        vkDEF:
          begin
           Lexer.NextTokenForceVTName;
           NodeName := Lexer.TokenX3DName;
           Lexer.NextToken;
           ParseNamedNode(NodeName);
          end;
        vkUSE:
          begin
           Lexer.NextTokenForceVTName;
           NodeName := Lexer.TokenX3DName;

           {get appropriate node}
           Result := Reader.Bound(NodeName, UsedNodeFinished, true);
           { When AllowNodeCycle, then UsedNodeFinished=false is not a problem.
             Just flip UsingNodeCycle to true, caller can handle a loop in node definition. }
           if (Result <> nil) and (not UsedNodeFinished) and AllowNodeCycle then
           begin
             UsingNodeCycle := true;
             UsedNodeFinished := true;
           end;
           if (Result = nil) or (not UsedNodeFinished) then
           begin
             if Result = nil then
               S := Format('Incorrect USE clause: node name "%s" undefined', [NodeName]) else
             begin
               S := Format('Cycles in VRML/X3D graph: USE clause inside node "%s" refers to the same node', [NodeName]);
               Result := nil; { return nil in case of cycles }
             end;
             if NilIfUnresolvedUSE then
               WritelnWarning('VRML/X3D', S)
             else
               raise EX3DParserError.Create(Lexer, S);
           end;

           Lexer.NextToken;
          end;
        else raise EX3DParserError.Create(Lexer,
               'Expected node type or DEF or USE, got '+Lexer.DescribeToken);
       end;
     vtName: ParseNamedNode('');
     else raise EX3DParserError.Create(Lexer,
            'Expected node type or DEF or USE, got '+Lexer.DescribeToken);
    end;
  except FreeAndNil(Result); raise end;
end;

function ParseNode(const Lexer: TX3DLexer; const Reader: TX3DReaderNames;
  const NilIfUnresolvedUSE: Boolean): TX3DNode; overload;
var
  IgnoredUsingNodeCycle: Boolean;
begin
  IgnoredUsingNodeCycle := false;
  Result := ParseNode(Lexer, Reader, NilIfUnresolvedUSE, false, IgnoredUsingNodeCycle);
end;

{ Parses a sequence of VRML/X3D statements: any number of nodes,
  (external) protypes, routes, import/export declarations.
  This is good to use to parse whole VRML/X3D file (when FileTopLevel = true),
  or a (non-external) prototype content (when FileTopLevel = false). }
function ParseStatements(
  Lexer: TX3DLexer; Reader: TX3DReaderNames;
  const EndToken: TX3DToken;
  FileTopLevel: boolean): TX3DRootNode; overload;
var
  PositionInParent: Integer;

  { Create root group node, appropriate for current VRML version in Lexer. }
  function CreateRootNode: TX3DRootNode;
  begin
    Result := TX3DRootNode.Create('', Reader.BaseUrl);
    Result.HasForceVersion := true;
    Result.ForceVersion := Reader.Version;
  end;

  procedure ParseProfile;
  begin
    if Lexer.TokenIsKeyword(vkPROFILE) then
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtName, 'X3D profile name');
      Result.Profile := Lexer.TokenName;

      Lexer.NextToken;
    end else
      { We allow PROFILE to be omitted, which is not really allowed by
        X3D spec. }
      WritelnWarning('VRML/X3D', 'X3D PROFILE statement missing');
  end;

  procedure ParseComponents;
  var
    Name: string;
    Level: Integer;
  begin
    while Lexer.TokenIsKeyword(vkCOMPONENT) do
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtName, 'X3D component name');
      Name := Lexer.TokenName;

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtColon);

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtInteger, 'X3D component level');
      Level := Lexer.TokenInteger;
      Result.Components[Name] := Level;

      Lexer.NextToken;
    end;
  end;

  procedure ParseUnits;
  var
    Category, Name: string;
    ConversionFactor: Float;
  begin
    while Lexer.TokenIsKeyword(vkUNIT) do
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtName, 'X3D unit category');
      Category := Lexer.TokenName;

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtName, 'X3D unit name');
      Name := Lexer.TokenName;

      Lexer.NextToken;
      Lexer.CheckTokenIs(TokenNumbers, 'X3D unit conversion factor (double-precision float)');
      ConversionFactor := Lexer.TokenFloat;

      Lexer.NextToken;

      Reader.UnitConversion(Category, Name, ConversionFactor);
    end;
    Result.Scale := Reader.LengthConversionFactor;
  end;

  procedure ParseMetas;
  var
    Key, Value: string;
  begin
    if Lexer.TokenIsKeyword(vkMETA) and
       (Reader.Version.Major < 3) then
      WritelnWarning('VRML/X3D', 'META is not allowed in VRML, you should use X3D instead');

    while Lexer.TokenIsKeyword(vkMETA) do
    begin
      Lexer.NextToken;
      Lexer.CheckTokenIs(vtString, 'X3D meta key');
      Key := Lexer.TokenString;

      Lexer.NextToken;
      Lexer.CheckTokenIs(vtString, 'X3D meta value');
      Value := Lexer.TokenString;
      Result.Meta[Key] := Value;

      Lexer.NextToken;
    end;
  end;

  procedure ParseStatement;

    { You can safely assume that current token is PROTO or EXTERNPROTO. }
    procedure ParseProtoStatement;
    var
      Proto: TX3DPrototypeBase;
    begin
      if Lexer.TokenKeyword = vkPROTO then
        Proto := TX3DPrototype.Create else
        Proto := TX3DExternalPrototype.Create;
      try
        Proto.Parse(Lexer, Reader);
        Proto.PositionInParent := PositionInParent;
        Result.AddPrototype(Proto);
      except
        FreeAndNil(Proto); { do not add invalid Proto, free it }
        raise;
      end;
    end;

    procedure ParseRouteStatement;
    var
      Route: TX3DRoute;
    begin
      Route := TX3DRoute.Create;
      try
        Route.Parse(Lexer, Reader);
        Route.PositionInParent := PositionInParent;
        Result.AddRoute(Route);
      except
        FreeAndNil(Route); { do not add invalid Route, free it }
        raise;
      end;
    end;

    procedure ParseImportStatement;
    var
      Import: TX3DImport;
    begin
      Import := TX3DImport.Create;
      try
        Import.Parse(Lexer, Reader);
        Import.PositionInParent := PositionInParent;
        Result.AddImport(Import);
      except
        FreeAndNil(Import); { do not add invalid Import, free it }
        raise;
      end;
    end;

    procedure ParseExportStatement;
    var
      Export: TX3DExport;
    begin
      Export := TX3DExport.Create;
      try
        Export.Parse(Lexer, Reader);
        Export.PositionInParent := PositionInParent;
        Result.AddExport(Export);
      except
        FreeAndNil(Export); { do not add invalid Export, free it }
        raise;
      end;
    end;

    procedure ParseNodeStatement;
    var
      NewNode: TX3DNode;
    begin
      NewNode := ParseNode(Lexer, Reader, false);
      NewNode.PositionInParent := PositionInParent;
      Result.FdChildren.Add(NewNode);
      { for prototypes, do not check NewNode class, as anything is valid. }
      if FileTopLevel then
        Result.TopLevelCheckChild(NewNode);
    end;

  begin
    if (Lexer.Token = vtKeyword) and
       (Lexer.TokenKeyword in [vkPROTO, vkEXTERNPROTO]) then
      ParseProtoStatement else
    if Lexer.TokenIsKeyword(vkROUTE) then
      ParseRouteStatement else
    if Lexer.TokenIsKeyword(vkIMPORT) then
      ParseImportStatement else
    if Lexer.TokenIsKeyword(vkEXPORT) then
      ParseExportStatement else
      ParseNodeStatement;
  end;

begin
  { We used to have more conservative mechanism here when it comes to using
    CreateRootNode: we used to start with Result = nil.
    First ParseNodeInternal simply set the Result to it's contents.
    ParseProtoStatement, ParseRouteInternal and second and subsequent
    ParseNodeInternal calls called CreateRootNode, and added previous
    content as new root node child.

    This was an unnecessarily complicated mechanism. The history is that
    initially, for correct VRML 1.0, the whole concept of RootGroup was
    not needed. It became needed since VRML 2.0, to
    - store multiple root nodes of VRML 2.0 file
    - and many VRML 1.0 files also used (incorrectly) the "multiple root nodes"
      feature
    - also, VRML 2.0 / X3D routes and prototypes declared at global scope had
      to be stored in a root group
    - Finally, ForceVersion mechanism is available for RootGroup to force
      writing of specific VRML/X3D version.
    - And for X3D, TX3DRootNode has to be enhanced
      to keep other top-level X3D data: profile, components, meta statements.

    So now, we simply always create root node by ParseStatements. }

  Result := CreateRootNode;
  try
    { Parse X3D "configuration information": profile, component and meta
      statements here. }
    if FileTopLevel and (Reader.Version.Major >= 3) then
    begin
      ParseProfile;
      ParseComponents;
      ParseUnits;
    end;
    if FileTopLevel then { META is not part of VRML 2.0, but Titania uses it }
      ParseMetas;

    PositionInParent := 0;

    if EndToken <> vtEnd then
    begin
      while Lexer.Token <> EndToken do
      begin
        ParseStatement;
        Inc(PositionInParent);
      end;
    end else
    begin
      { Somewhat more involved version of the loop, the idea is to catch
        EX3DClassicReadError happening in the ParseStatement
        and convert it to WritelnWarning (then merely stop reading VRML file,
        but the file contents read so far are Ok). }
      while Lexer.Token <> EndToken do
      try
        ParseStatement;
        Inc(PositionInParent);
      except
        on E: EX3DClassicReadError do
        begin
          WritelnWarning('VRML/X3D', Format('Error when reading, will skip the rest of X3D file: %s', [E.Message]));
          Break;
        end;
      end;
    end;
  except FreeAndNil(Result); raise end;
end;

function LoadX3DClassicInternal(const Stream: TStream;
  const BaseUrl: string): TX3DRootNode;
var
  PeekStream: TPeekCharStream;
  Lexer: TX3DLexer;
  Reader: TX3DReaderNames;
begin
  PeekStream := TBufferedReadStream.Create(Stream, false);
  try
    Lexer := TX3DLexer.Create(PeekStream, false);
    try
      Reader := TX3DReaderNames.Create(false, BaseUrl, Lexer.Version);
      try
        Result := ParseStatements(Lexer, Reader, vtEnd, true);
        Reader.ExtractNames(Result.FPrototypeNames, Result.FExportedNames, Result.FImportedNames);
      finally FreeAndNil(Reader) end;
    finally FreeAndNil(Lexer) end;
  finally FreeAndNil(PeekStream) end;
end;

function LoadX3DClassic(const URL: string;
  const Gzipped: boolean): TX3DRootNode;

  function DoIt(BaseStream: TStream; const BaseUrl: string): TX3DRootNode;
  begin
    try
      Result := LoadX3DClassicInternal(BaseStream, BaseUrl);
    finally FreeAndNil(BaseStream) end;
  end;

var
  BaseUrl: string;
begin
  BaseUrl := AbsoluteURI(URL);

  if Gzipped then
    Result := DoIt(Download(URL, [soGzip]), BaseUrl) else
  begin
    try
      Result := DoIt(Download(URL, []), BaseUrl);
    except
      { It's somewhat hacky solution to reopen the file, creating new stream.
        But in practice, this works OK, and it allows us to read files
        compressed by gzip but without indicating this by file extension.
        See LoadNode comments. }
      on EGzipCompressed do
        Result := DoIt(Download(URL, [soGzip]), BaseUrl);
    end;
  end;
end;

function LoadX3DClassicFromString(const VRMLContents: string;
  const BaseUrl: string): TX3DRootNode;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(VRMLContents);
  try
    Result := LoadX3DClassicInternal(Stream, BaseUrl);
  finally FreeAndNil(Stream) end;
end;

{ Internal parsing helper. }
function InterfaceDeclarationKeywords(
  const AccessTypes: TX3DAccessTypes): TX3DKeywords;
begin
  Result := [];
  if atInputOnly in AccessTypes then
    Result := Result + [vkEventIn, vkInputOnly];
  if atOutputOnly in AccessTypes then
    Result := Result + [vkEventOut, vkOutputOnly];
  if atInitializeOnly in AccessTypes then
    Result := Result + [vkField, vkInitializeOnly];
  if atInputOutput in AccessTypes then
    Result := Result + [vkExposedField, vkInputOutput];
end;

{$endif read_implementation}
