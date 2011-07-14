{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Save routines, common for both XML and classic encoding. }

{$ifdef read_interface}

{ Which VRML/X3D version should be used to save this 3D model. }
function SaveVRMLVersion(Node: TVRMLNode): TVRMLVersion;

{ Write VRML/X3D model to a file.
  Generates a complete file, with header lines (XML headers,
  or #VRML / #X3D in classic encoding) and everything.

  Generator and Source, if not empty, will be used to set appropriate
  META values of X3D root file node. This way you can indicate inside X3D file
  the generator (your program) name and source (original 3D model file name).
  If this is not an X3D root file node, we will record it inside a comment.

  Encoding = xeXML should be used only with X3D or VRML 2.0,
  that is when Version.Major >= 2.
  In case of VRML 2.0, this will cause conversion VRML 2.0 -> X3D.
  Previous standard versions (VRML 1.0, Inventor) cannot be saved to XML.
  This procedure will actually generate something sensible (you can imagine
  how VRML 1.0 can be encoded in XML, by analogy with X3D),
  but it will only be readable by our engine.

  @param(Version Which VRML/X3D specification version should be used
    to encode. It should be calculated by SaveVRMLVersion(Node).
    You often want to calculate it yourself, before calling SaveVRML,
    this way you can propose a sensible saved model extension for user
    (.wrl for VRML <= 2.0 in classic encoding, .x3dv for X3D in classic encoding).

    Note that @italic(this parameter is @bold(not) a mechanism to convert
    between various VRML/X3D versions). This procedure does not convert VRML/X3D
    nodes/fields inside. For example, you can't just change Version.Major value
    from 1 to 3 to convert VRML 1.0 to X3D.
    It would cause some information to be encoded in X3D style
    (e.g. generated file will have X3D header and PROFILE information),
    but the nodes would stil have VRML 1.0 names and fields.
    Which means that you would not generate correct X3D file.

    So this should really be calculated based on model contents,
    usually by SaveVRMLVersion(Node).)

  @groupBegin }
procedure SaveVRML(Node: TVRMLNode;
  Stream: TStream; const Generator, Source: string;
  const Version: TVRMLVersion; const Encoding: TX3DEncoding); overload;
procedure SaveVRML(Node: TVRMLNode;
  const Filename, Generator, Source: string;
  const Version: TVRMLVersion; const Encoding: TX3DEncoding); overload;
procedure SaveVRML(Node: TVRMLNode;
  Stream: TStream; const Generator, Source: string;
  const Encoding: TX3DEncoding); overload;
procedure SaveVRML(Node: TVRMLNode;
  const Filename, Generator, Source: string;
  const Encoding: TX3DEncoding); overload;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

type
  { TX3DWriter extended with NodeNames. }
  TX3DWriterNames = class(TX3DWriter)
    { Record known node names.
      This allows to write correct DEF / USE when saving. }
    NodeNames: TVRMLNodeNames;

    constructor Create(AStream: TStream;
      const AVersion: TVRMLVersion; const AEncoding: TX3DEncoding);
    destructor Destroy; override;
  end;

constructor TX3DWriterNames.Create(AStream: TStream;
  const AVersion: TVRMLVersion; const AEncoding: TX3DEncoding);
begin
  inherited;
  NodeNames := TVRMLNodeNames.Create(false);
end;

destructor TX3DWriterNames.Destroy;
begin
  FreeAndNil(NodeNames);
  inherited;
end;

const
  { Used when we have to save as X3D, but profile information is missing. }
  DefaultX3DSaveProfile = 'Interchange';

function SaveVRMLVersion(Node: TVRMLNode): TVRMLVersion;
var
  SuggestionPriority: Integer;
begin
  if not Node.SuggestedVRMLVersion(Result, SuggestionPriority) then
  begin
    { If nothing is suggested, we use latest X3D. Reason:
      - For now, SuggestedVRMLVersion doesn't check IsClauseNames.
        But if IsClauseNames is present anywhere, then this must use VRML >= 2.0
        features ("IS" is keyword only in VRML >= 2.0,
        it will not be understood in VRML 1.0).
      - Besides, we should promote newer X3D standard. }

    Result := X3DVersion;
  end;
end;

procedure SaveVRML(Node: TVRMLNode;
  Stream: TStream; const Generator, Source: string;
  const Version: TVRMLVersion;
  const Encoding: TX3DEncoding);
var
  Writer: TX3DWriterNames;

  procedure Classic;

    procedure SaveProfile(Root: TVRMLRootNode);
    begin
      if Root.Profile <> '' then
        Writer.Writeln('PROFILE ' + Root.Profile + NL) else
        Writer.Writeln('PROFILE ' + DefaultX3DSaveProfile + NL);
    end;

    procedure SaveComponents(Root: TVRMLRootNode);
    var
      I: Integer;
    begin
      for I := 0 to Root.Components.Count - 1 do
        Writer.Writeln(Format('COMPONENT %s:%d',
          [ Root.Components.Keys[I],
            Root.Components.Data[I] ]));
      if Root.Components.Count <> 0 then
        Writer.Write(NL);
    end;

    procedure SaveMetas(Root: TVRMLRootNode);
    var
      I: Integer;
    begin
      for I := 0 to Root.Meta.Count - 1 do
        Writer.Writeln(Format('META %s %s',
          [ StringToX3DClassic(Root.Meta.Keys[I]),
            StringToX3DClassic(Root.Meta.Data[I])]));
      if Root.Meta.Count <> 0 then
        Writer.Write(NL);
    end;

  const
    VRML10Header = '#VRML V1.0 ascii';
    VRML20Header = '#VRML V2.0 utf8';
    X3DHeader = '#X3D V%d.%d utf8';
  var
    VRMLHeader, PrecedingComment: string;
  begin
    if Version.Major <= 1 then
      VRMLHeader := VRML10Header else
    if Version.Major = 2 then
      VRMLHeader := VRML20Header else
    if Version.Major >= 3 then
      VRMLHeader := Format(X3DHeader, [Version.Major, Version.Minor]);

    Writer.Writeln(VRMLHeader + NL { yes, one more NL, to look good });

    if (Version.Major >= 3) and (Node is TVRMLRootNode) then
    begin
      if Generator <> '' then
        TVRMLRootNode(Node).Meta.PutPreserve('generator', Generator);
      if Source <> '' then
        TVRMLRootNode(Node).Meta.PutPreserve('source', Source);
      SaveProfile(TVRMLRootNode(Node));
      SaveComponents(TVRMLRootNode(Node));
      SaveMetas(TVRMLRootNode(Node));
    end else
    if (Generator <> '') or (Source <> '') then
    begin
      { write Generator and Source as comment }
      PrecedingComment := '';
      if Generator <> '' then
        PrecedingComment += '# Generated by ' + SReplaceChars(Generator, [#10, #13], ' ') + NL;
      if Source <> '' then
        PrecedingComment += '# Source model: ' + SReplaceChars(Source, [#10, #13], ' ') + NL;
      Writer.Writeln(PrecedingComment);
    end;

    { Node may be TVRMLRootNode here, that's OK,
      TVRMLRootNode.SaveToStream will magically handle this right. }
    Node.SaveToStream(Writer);
  end;

  procedure Xml;

    function Profile: string;
    begin
      if (Node is TVRMLRootNode) and
         (TVRMLRootNode(Node).Profile <> '') then
        Result := TVRMLRootNode(Node).Profile else
        Result := DefaultX3DSaveProfile;
    end;

    procedure SaveComponents(Root: TVRMLRootNode);
    var
      I: Integer;
    begin
      for I := 0 to Root.Components.Count - 1 do
        Writer.WritelnIndent(Format('<component name=%s level="%d" />',
          [ StringToX3DXml(Root.Components.Keys[I]),
            Root.Components.Data[I] ]));
    end;

    procedure SaveMetas(Root: TVRMLRootNode);
    var
      I: Integer;
    begin
      for I := 0 to Root.Meta.Count - 1 do
        Writer.WritelnIndent(Format('<meta name=%s content=%s />',
          [ StringToX3DXml(Root.Meta.Keys[I]),
            StringToX3DXml(Root.Meta.Data[I])]));
    end;

  begin
    Writer.Writeln(Format(
      '<?xml version="1.0" encoding="UTF-8"?>' + NL +
      '<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D %d.%d//EN" "http://www.web3d.org/specifications/x3d-%0:d.%1:d.dtd">' + NL+
      '<X3D profile=%2:s version="%0:d.%1:d"' + NL+
      '     xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance"' + NL+
      '     xsd:noNamespaceSchemaLocation="http://www.web3d.org/specifications/x3d-%0:d.%1:d.xsd">' + NL+
      '<head>',
      [Version.Major, Version.Minor, StringToX3DXml(Profile)]));
    Writer.IncIndent;

    if Node is TVRMLRootNode then
    begin
      if Generator <> '' then
        TVRMLRootNode(Node).Meta.PutPreserve('generator', Generator);
      if Source <> '' then
        TVRMLRootNode(Node).Meta.PutPreserve('source', Source);
      SaveComponents(TVRMLRootNode(Node));
      SaveMetas(TVRMLRootNode(Node));
    end else
    begin
      { write Generator etc. directly, as we don't have TVRMLRootNode.Meta available }
      if Generator <> '' then
        Writer.WritelnIndent(Format('<meta name="generator" content=%s />',
          [StringToX3DXml(Generator)]));
      if Source <> '' then
        Writer.WritelnIndent(Format('<meta name="source" content=%s />',
          [StringToX3DXml(Source)]));
    end;

    Writer.DecIndent;
    Writer.Writeln('</head>' + NL + '<Scene>');

    { Node may be TVRMLRootNode here, that's OK,
      TVRMLRootNode.SaveToStream will magically handle this right. }
    Writer.IncIndent;
    Node.SaveToStream(Writer);
    Writer.DecIndent;

    Writer.Writeln('</Scene>' +NL + '</X3D>');
  end;

begin
  Writer := TX3DWriterNames.Create(Stream, Version, Encoding);
  try
    case Encoding of
      xeClassic: Classic;
      xeXML:     Xml;
      else raise EInternalError.Create('Saving Encoding?');
    end;
  finally FreeAndNil(Writer) end;
end;

procedure SaveVRML(Node: TVRMLNode;
  const Filename, Generator, Source: string; const Version: TVRMLVersion;
  const Encoding: TX3DEncoding);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveVRML(Node, Stream, Generator, Source, Version, Encoding);
  finally Stream.Free end;
end;

procedure SaveVRML(Node: TVRMLNode;
  Stream: TStream; const Generator, Source: string;
  const Encoding: TX3DEncoding);
begin
  SaveVRML(Node, Stream, Generator, Source, SaveVRMLVersion(Node), Encoding);
end;

procedure SaveVRML(Node: TVRMLNode;
  const FileName, Generator, Source: string;
  const Encoding: TX3DEncoding);
begin
  SaveVRML(Node, FileName, Generator, Source, SaveVRMLVersion(Node), Encoding);
end;

{$endif read_implementation}
