{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for converting other 3D model formats into VRML/X3D. }
unit X3DLoadInternalUtils;

{$I castleconf.inc}

interface

uses CastleVectors, X3DNodes;

const
  NiceCreaseAngle = DefaultVRML1CreaseAngle;

{ Store X3D name in file, in format that allows to store @italic(any) possible value
  (regardless of X3D node name limitations).
  Names that are already valid in X3D are stored as-is,
  special names have a prefix CastleEncoded_ added and are encoded (will be decoded later
  with DecodeX3DName).

  This allows to set as TX3DNode.X3DName anything, and it will work OK (and survive
  model save+load to file). This is useful to us, e.g. to preserve animation names
  from Spine or glTF in X3D TouchSensor node names. }
function EncodeX3DName(const S: String): String;
function DecodeX3DName(const S: String): String;

{ Calculate best possible ambientIntensity. This is a float that tries to
  satisfy the equation AmbientColor = AmbientIntensity * DiffuseColor.
  Suitable for VRML 2.0/X3D Material.ambientIntensity (as there's no
  Material.ambientColor in VRML 2.0/X3D). }
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector3): Single; overload;
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector4): Single; overload;

{ Search harder for file named Base inside directory of BaseUrl.
  BaseUrl must be an absolute URL, we will extract path from it.
  Returns URL relative to BaseUrl.

  Automatically uses FixRelativeUrl on Base, fixing backslashes to slashes,
  so assuming that this is an old 3D format and backslash should be interpreted
  as directory separator (like on Windows).

  We prefer to return just Base, if it exists, or when no alternative exists.
  When Base doesn't exist but some likely alternative exists (e.g. with
  different case), we return it. }
function SearchTextureFile(const BaseUrl: string; Base: string): string;

{ Convert backslashes to slashes. Use for formats where this interpretation
  of backslash (instead of %-encoding actual backslash, see
  https://sourceforge.net/p/castle-engine/tickets/21/ ) seems more common. }
function FixRelativeUrl(const URL: string): string;

var
  { Use CGE X3D extensions when importing the model.
    Set to false to force creating a valid X3D model (but maybe with less functionality). }
  CastleX3dExtensions: Boolean = true;

implementation

uses SysUtils, Math, URIParser, StrUtils,
  CastleStringUtils, CastleFindFiles, CastleLog, CastleURIUtils, CastleUnicode;

const
  EncodedPrefix = 'CastleEncoded_';

function EncodeX3DName(const S: String): String;
const
  { Similar to most conservative version of VRMLNameChars,
    which follow VRML and X3D specifications. }
  NonAllowedChars = [#0..#$1f, ' ', '''', '"', '#', ',', '.', '[', ']', '\', '{', '}', '(', ')', '|', ':'];
  // first character cannot be digit etc.
  NonAllowedFirstChars = NonAllowedChars + ['0'..'9', '-', '+'];

  function SafeX3DName(const S: String): Boolean;
  begin
    Result := not (
      SCharIs(S, 1, NonAllowedFirstChars) or
      (CharsPos(NonAllowedChars, S) <> 0));
  end;

  function DoEncodeUTF8(const S: String): String;
  var
    C: TUnicodeChar;
    {$ifdef FPC}
    TextPtr: PChar;
    CharLen: Integer;
    {$else}
    TextIndex: Integer;
    NextTextIndex: Integer;
    TextLength: Integer;
    {$endif}

  begin
    Result := '';

    {$ifdef FPC}
    TextPtr := PChar(S);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    while (C > 0) and (CharLen > 0) do
    {$else}
    TextIndex := 1;
    TextLength := Length(S);
    while (TextIndex <= TextLength) do
    {$endif}
    begin
      {$ifdef FPC}
      Inc(TextPtr, CharLen);
      {$else}
      C := UnicodeStringNextChar(S, TextIndex, NextTextIndex);
      TextIndex := NextTextIndex;
      {$endif}

      { We use $ to mark encoded chars, so we need to encode it too.
        Note that we don't worry NonAllowedFirstChars,
        since we add prefix EncodedPrefix anyway, so S[1]
        is not the first character in the final encoded X3D name. }
      if (C < 128) and (Chr(C) <> '$') and (not CharInSet(Chr(C), NonAllowedChars)) then
        Result := Result + Chr(C)
      else
        Result := Result + '$' + IntToStr(C) + '$';

      {$ifdef FPC}
      C := UTF8CharacterToUnicode(TextPtr, CharLen);
      {$endif}
    end;
  end;

begin
  if IsPrefix(EncodedPrefix, S, false) then
    WritelnWarning('Encoding X3D name "%s", it seems like you encode it twice (EncodeX3DName)', [S]);

  if SafeX3DName(S) then
    Result := S
  else
    Result := EncodedPrefix + DoEncodeUTF8(S);
end;

function DecodeX3DName(const S: String): String;

  function DoDecodeUTF8(const S: String): String;
  var
    I, EndingDollar: Integer;
    CharCode: Integer;
  begin
    Result := '';
    I := 1;
    while I <= Length(S) do
    begin
      if S[I] = '$' then
      begin
        EndingDollar := PosEx('$', S, I + 1);
        if EndingDollar = 0 then
        begin
          WritelnWarning('No matching $ found in encoded X3D name %s. Invalid encoding of X3D name, assuming not encoded at all.', [S]);
          // resulting name will be just like S, only with EncodedPrefix removed
          Exit(S);
        end;

        if not TryStrToInt(Copy(S, I + 1, EndingDollar - I - 1), CharCode) then
        begin
          WritelnWarning('Invalid UTF-8 code in encoded X3D name %s. Invalid encoding of X3D name, assuming not encoded at all.', [S]);
          Exit(S);
        end;

        {$ifdef FPC}
        Result += UnicodeToUTF8(CharCode);
        {$else}
        Result := Result + Chr(CharCode);
        {$endif}
        I := EndingDollar + 1;
      end else
      begin
        Result := Result + S[I];
        Inc(I);
      end;
    end;
  end;

begin
  if IsPrefix(EncodedPrefix, S, false) then
    Result := DoDecodeUTF8(PrefixRemove(EncodedPrefix, S, false))
  else
    Result := S;
end;

function AmbientIntensity(const AmbientColor, DiffuseColor: TVector3): Single;
begin
  Result := 0;
  if not IsZero(DiffuseColor[0]) then
    Result := Result + (AmbientColor[0] / DiffuseColor[0]);
  if not IsZero(DiffuseColor[1]) then
    Result := Result + (AmbientColor[1] / DiffuseColor[1]);
  if not IsZero(DiffuseColor[2]) then
    Result := Result + (AmbientColor[2] / DiffuseColor[2]);
  Result := Result / 3;
end;

function AmbientIntensity(const AmbientColor, DiffuseColor: TVector4): Single;
begin
  Result := AmbientIntensity(
    AmbientColor.XYZ,
    DiffuseColor.XYZ);
end;

function SearchTextureFile(const BaseUrl: string; Base: string): string;
var
  SomePathDelim: Integer;
  BaseShort, Path: string;
begin
  Path := ExtractURIPath(BaseUrl);
  Base := FixRelativeUrl(Base);

  try
    if SearchFileHard(Path, Base, Result) then
      Exit;

    { According to https://sourceforge.net/tracker/index.php?func=detail&aid=3305661&group_id=200653&atid=974391
      some archives expect search within textures/ subdirectory.
      Example on http://www.gfx-3d-model.com/2008/06/house-07/#more-445
      for Wavefront OBJ. }
    if SearchFileHard(Path + 'textures/', Base, Result) then
    begin
      Result := 'textures/' + Result;
      Exit;
    end;
    if SearchFileHard(Path + 'Textures/', Base, Result) then
    begin
      Result := 'Textures/' + Result;
      Exit;
    end;

    { Some invalid models place full (absolute) path inside texture filename.
      Try to handle it, by stripping path part (from any OS), and trying
      to match new name. }
    SomePathDelim := BackCharsPos(['/', '\'], Base);
    if SomePathDelim <> 0  then
    begin
      BaseShort := SEnding(Base, SomePathDelim + 1);

      if SearchFileHard(Path, BaseShort, Result) then
        Exit;
      if SearchFileHard(Path + 'textures/', BaseShort, Result) then
      begin
        Result := 'textures/' + Result;
        Exit;
      end;
      if SearchFileHard(Path + 'Textures/', BaseShort, Result) then
      begin
        Result := 'Textures/' + Result;
        Exit;
      end;
    end;

  finally
    if Result <> Base then
      { Texture file found, but not under original name }
      WritelnWarning('Texture', Format('Exact texture URL "%s" not found, using instead "%s"',
        [Base, Result]));
  end;

  { default result if nowhere found }
  Result := Base;
end;

function FixRelativeUrl(const URL: string): string;
begin
  Result := SReplaceChars(URL, '\', '/');
end;

end.
