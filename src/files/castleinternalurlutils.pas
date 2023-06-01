{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Internal URL utilities. }
unit CastleInternalUrlUtils;

{$I castleconf.inc}

interface

{ Turn URL into a nice component name.
  May return '' if not possible. }
function GetBaseNameFromUrl(const Url: String): String;

{ Get parent directory name from URL, sans Sketchfab model id.
  E.g. from
    'castle-data:/sketchfab/cat-123abc/scene.gltf'
  get
    'cat'
  Returns '' if no useful parent directory.
}
function GetUrlParentName(const Url: String): String;

implementation

uses UriParser, SysUtils,
  CastleStringUtils, CastleUriUtils;

function IsModelId(const Id: String): Boolean;
begin
  Result := CharsPos(AllChars - ['0'..'9', 'a'..'f', 'A'..'F'], Id) = 0;
end;

function GetUrlParentName(const Url: String): String;
var
  U: TUri;
  PosIdDelimiter: Integer;
begin
  U := ParseUri(Url);
  Result := ExtractUriName(UriExcludeSlash(U.Path));

  // strip Sketchfab model id
  PosIdDelimiter := BackPos('-', Result);
  if (PosIdDelimiter <> 0) and
     (IsModelId(SEnding(Result, PosIdDelimiter + 1)))  then
    Result := Copy(Result, 1, PosIdDelimiter - 1);

  if Result = '/' then
    Result := '';
end;

function GetBaseNameFromUrl(const Url: String): String;
var
  UrlBase: String;
  SB: TStringBuilder;
  I: Integer;
  C: Char;
  NextUpCase: Boolean;
  UrlParentName: String;
begin
  UrlBase := ExtractUriName(Url);
  UrlBase := DeleteUriExt(UrlBase);

  { This case happens esp. for Sketchfab-imported models.
    Use parent directory name then. }
  if SameText(UrlBase, 'scene') then
  begin
    UrlParentName := GetUrlParentName(Url);
    if UrlParentName <> '' then
      UrlBase := UrlParentName;
  end;

  SB := TStringBuilder.Create;
  try
    NextUpCase := true;
    for I := 1 to Length(UrlBase) do
    begin
      C := UrlBase[I];
      if C in ['a'..'z', 'A'..'Z', '0'..'9'] then
      begin
        if NextUpCase then
        begin
          SB.Append(UpCase(C));
          NextUpCase := C in ['0'..'9'];
        end else
          SB.Append(C);
      end else
        NextUpCase := true;
    end;
    Result := SB.ToString;
  finally FreeAndNil(SB) end;
end;

end.