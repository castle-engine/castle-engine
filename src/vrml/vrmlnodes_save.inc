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

  Generator and Source, if not empty, will be used to set appropriate
  META values of X3D root file node. This way you can indicate inside X3D file
  the generator (your program) name and source (original 3D model file name).
  If this is not an X3D root file node, we will record it inside a comment.

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
begin
  case Encoding of
    xeClassic: SaveVRMLClassic(Node, Stream, Generator, Source, Version);
    xeXML:     SaveX3DXml     (Node, Stream, Generator, Source, Version);
    else raise EInternalError.Create('Saving Encoding?');
  end;
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
