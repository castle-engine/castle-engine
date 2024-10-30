{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Save routines, common for both XML and classic encoding. }

{$ifdef read_interface}

{$endif read_interface}

{$ifdef read_implementation}

type
  { TX3DWriter extended with NodeNames. }
  TX3DWriterNames = class(TX3DWriter)
    { Record known node names.
      This allows to write correct DEF / USE when saving. }
    NodeNames: TX3DNodeNames;

    constructor Create(AStream: TStream;
      const AVersion: TX3DVersion; const AEncoding: TX3DEncoding);
    destructor Destroy; override;
  end;

constructor TX3DWriterNames.Create(AStream: TStream;
  const AVersion: TX3DVersion; const AEncoding: TX3DEncoding);
begin
  inherited;
  NodeNames := TX3DNodeNames.Create(false);
end;

destructor TX3DWriterNames.Destroy;
begin
  FreeAndNil(NodeNames);
  inherited;
end;

const
  { Used when we have to save as X3D, but profile information is missing. }
  DefaultX3DSaveProfile = 'Interchange';

{ global routines ------------------------------------------------------------ }

{ Write model to a VRML or X3D file.
  Generates a complete file, with header lines (XML headers,
  or #VRML / #X3D in classic encoding) and everything.

  Generator and Source, if not empty, will be used to set appropriate
  META values of X3D root file node. This way you can indicate inside X3D file
  the generator (your program) name and source (original 3D model file name).
  If this is not an X3D root file node, we will record it inside a comment.

  To force convering to X3D, use @link(TX3DRootNode.ForceSaveAsX3D)
  before calling this. }
procedure InternalSaveVrmlX3D(Node: TX3DNode;
  const Stream: TStream; const Generator, Source: string;
  const Encoding: TX3DEncoding);
var
  Version: TX3DVersion; // Version to save

  { Which VRML/X3D version should be used to save this 3D model. }
  function GetSaveVersion(const Node: TX3DNode): TX3DVersion;
  begin
    if (Node is TX3DRootNode) and
      TX3DRootNode(Node).HasForceVersion then
      Result := TX3DRootNode(Node).ForceVersion
    else
      { Use latest X3D. We should promote latest, best version of VRML/X3D.
        Also, this avoids conversion to X3D when saving it to XML encoding. }
      Result := X3DVersion;
  end;

var
  Writer: TX3DWriterNames;

  procedure Classic;

    procedure SaveProfile(Root: TX3DRootNode);
    begin
      if Root.Profile <> '' then
        Writer.Writeln('PROFILE ' + Root.Profile + NL) else
        Writer.Writeln('PROFILE ' + DefaultX3DSaveProfile + NL);
    end;

    procedure SaveComponents(Root: TX3DRootNode);
    var
      ComponentLevel: {$ifdef FPC}TStringIntegerMap.TDictionaryPair{$else}TPair<string, Integer>{$endif};
    begin
      for ComponentLevel in Root.Components do
        Writer.Writeln(Format('COMPONENT %s:%d',
          [ ComponentLevel.Key,
            ComponentLevel.Value ]));
      if Root.Components.Count <> 0 then
        Writer.Write(NL);
    end;

    procedure SaveMetas(Root: TX3DRootNode);
    var
      MetaPair: {$ifdef FPC}TStringStringMap.TDictionaryPair{$else}TPair<string, string>{$endif};
    begin
      for MetaPair in Root.Meta do
        Writer.Writeln(Format('META %s %s',
          [ StringToX3DClassic(MetaPair.Key),
            StringToX3DClassic(MetaPair.Value)]));
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

    if (Version.Major >= 3) and (Node is TX3DRootNode) then
    begin
      if Generator <> '' then
        TX3DRootNode(Node).Meta.PutPreserve('generator', Generator);
      if Source <> '' then
        TX3DRootNode(Node).Meta.PutPreserve('source', Source);
      SaveProfile(TX3DRootNode(Node));
      SaveComponents(TX3DRootNode(Node));
      SaveMetas(TX3DRootNode(Node));
    end else
    if Version.Major >= 3 then
    begin
      { X3D, but no TX3DRootNode. Use default X3D profile. }
      Writer.Writeln('PROFILE ' + DefaultX3DSaveProfile);
      if Generator <> '' then
        Writer.Writeln(Format('META %s %s',
          [ StringToX3DClassic('generator'),
            StringToX3DClassic(Generator)]));
      if Source <> '' then
        Writer.Writeln(Format('META %s %s',
          [ StringToX3DClassic('source'),
            StringToX3DClassic(Source)]));
      Writer.Writeln;
    end else
    if (Generator <> '') or (Source <> '') then
    begin
      { write Generator and Source as comment }
      PrecedingComment := '';
      if Generator <> '' then
        PrecedingComment := PrecedingComment + ('# Generated by ' + SReplaceChars(Generator, [#10, #13], ' ') + NL);
      if Source <> '' then
        PrecedingComment := PrecedingComment + ('# Source model: ' + SReplaceChars(Source, [#10, #13], ' ') + NL);
      Writer.Writeln(PrecedingComment);
    end;

    { Node may be TX3DRootNode here, that's OK,
      TX3DRootNode.SaveToStream will magically handle this right. }
    Node.SaveToStream(Writer);
  end;

  procedure Xml;

    function Profile: string;
    begin
      if (Node is TX3DRootNode) and
         (TX3DRootNode(Node).Profile <> '') then
        Result := TX3DRootNode(Node).Profile else
        Result := DefaultX3DSaveProfile;
    end;

    procedure SaveComponents(Root: TX3DRootNode);
    var
      ComponentLevel: {$ifdef FPC}TStringIntegerMap.TDictionaryPair{$else}TPair<string, Integer>{$endif};
    begin
      for ComponentLevel in Root.Components do
        Writer.WritelnIndent(Format('<component name=%s level="%d" />',
          [ StringToX3DXml(ComponentLevel.Key),
            ComponentLevel.Value ]));
    end;

    procedure SaveMetas(Root: TX3DRootNode);
    var
      MetaPair: {$ifdef FPC}TStringStringMap.TDictionaryPair{$else}TPair<string, string>{$endif};
    begin
      for MetaPair in Root.Meta do
        Writer.WritelnIndent(Format('<meta name=%s content=%s />',
          [ StringToX3DXml(MetaPair.Key),
            StringToX3DXml(MetaPair.Value)]));
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

    if Node is TX3DRootNode then
    begin
      if Generator <> '' then
        TX3DRootNode(Node).Meta.PutPreserve('generator', Generator);
      if Source <> '' then
        TX3DRootNode(Node).Meta.PutPreserve('source', Source);
      SaveComponents(TX3DRootNode(Node));
      SaveMetas(TX3DRootNode(Node));
    end else
    begin
      { write Generator etc. directly, as we don't have TX3DRootNode.Meta available }
      if Generator <> '' then
        Writer.WritelnIndent(Format('<meta name="generator" content=%s />',
          [StringToX3DXml(Generator)]));
      if Source <> '' then
        Writer.WritelnIndent(Format('<meta name="source" content=%s />',
          [StringToX3DXml(Source)]));
    end;

    Writer.DecIndent;
    Writer.Writeln('</head>' + NL + '<Scene>');

    { Node may be TX3DRootNode here, that's OK,
      TX3DRootNode.SaveToStream will magically handle this right. }
    Writer.IncIndent;
    Node.SaveToStream(Writer);
    Writer.DecIndent;

    Writer.Writeln('</Scene>' +NL + '</X3D>');
  end;

begin
  Version := GetSaveVersion(Node);

  Writer := TX3DWriterNames.Create(Stream, Version, Encoding);
  try
    case Encoding of
      xeClassic: Classic;
      xeXML:     Xml;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Saving Encoding?');
      {$endif}
    end;
  finally FreeAndNil(Writer) end;
end;

procedure SaveX3DXml(const Node: TX3DRootNode;
  const Stream: TStream; const Generator, Source: string);
begin
  // optionally call ForceSaveToX3D, when converting VRML (<= 2) -> X3D (>= 3)
  if Node.HasForceVersion and
     (Node.ForceVersion.Major <= 2) then
    Node.ForceSaveAsX3D;

  InternalSaveVrmlX3D(Node, Stream, Generator, Source, xeXML);
end;

{ Saves classic encoding, suitable to save all VRML and X3D versions. }
procedure SaveX3DClassic(const Node: TX3DRootNode;
  const Stream: TStream; const Generator, Source: string);
begin
  InternalSaveVrmlX3D(Node, Stream, Generator, Source, xeClassic);
end;

{ Like SaveX3DClassic,
  but optionally calls ForceSaveToX3D if we convert from VRML <= 2,
  thus it will upgrade VRML <= 2 to X3D >= 3. }
procedure SaveX3DClassicForceX3D(const Node: TX3DRootNode;
  const Stream: TStream; const Generator, Source: string);
begin
  // optionally call ForceSaveToX3D, when converting VRML (<= 2) -> X3D (>= 3)
  if Node.HasForceVersion and
     (Node.ForceVersion.Major <= 2) then
    Node.ForceSaveAsX3D;
  SaveX3DClassic(Node, Stream, Generator, Source);
end;

{$endif read_implementation}
