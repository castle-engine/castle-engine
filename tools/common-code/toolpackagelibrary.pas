{
  Copyright 2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download and parse the Castle packages library. }
unit ToolPackageLibrary;

{$I castleconf.inc}

interface

type
  { Link attached to a package in the library. }
  TPackageLink = record
    { Display name of the link. }
    Name: String;
    { Destination URL. }
    Url: String;
  end;

  { Component information from the packages library. }
  TPackageInfo = record
    { Stable package identifier. }
    Id: String;
    { Package title. }
    Name: String;
    { Package description, which may contain HTML. }
    Description: String;
    { Image filename relative to the image base URLs below. }
    ImageUrl: String;
    { Package repository or project page URL. }
    RepoUrl: String;
    { Package author. }
    Author: String;
    { Additional links associated with the package. }
    Links: array of TPackageLink;
  end;

  { Array of package information returned by @link(LoadPackageLibrary). }
  TPackageInfoArray = array of TPackageInfo;

const
  { Fallback source for the packages library, used when the JSON source fails. }
  LibraryFallbackUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/additional_components.php';

  { Base URL for regular package images. }
  ImageBaseUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/images/original_size/';

  { Base URL for gallery-sized package images. }
  GalleryImageBaseUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/images/gallery_size/';

{ Parse packages from a JSON library document. }
function ParsePackageLibraryJson(const Content: String): TPackageInfoArray;

{ Parse packages from the legacy PHP library document. }
function ParsePackageLibraryFallback(const Content: String): TPackageInfoArray;

{ Download the JSON packages library and parse it, falling back to the legacy
  PHP library if downloading or parsing the JSON library fails. }
function LoadPackageLibrary(const Url: String): TPackageInfoArray;

implementation

uses
  Classes, SysUtils, RegExpr, fpjson, jsonparser,
  CastleDownload, CastleLog;

function ParseComponentBlock(const Block: String): TPackageInfo;
var
  ArgPosition: Integer;

  procedure SkipWhitespaceAndComments;
  begin
    while ArgPosition <= Length(Block) do
    begin
      if Block[ArgPosition] <= ' ' then
        Inc(ArgPosition)
      else
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '/') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '*') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition < Length(Block)) and
          ((Block[ArgPosition] <> '*') or (Block[ArgPosition + 1] <> '/')) do
          Inc(ArgPosition);
        if ArgPosition < Length(Block) then
          Inc(ArgPosition, 2);
      end else
      if Block[ArgPosition] = '#' then
      begin
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
        Exit;
    end;
  end;

  function ReadStringArgument(const ArgumentName: String): String;
  var
    QuoteChar: Char;
    ParenthesisDepth: Integer;
    IsTopLevelString: Boolean;
    ClosedString: Boolean;
    SawTopLevelString: Boolean;
  begin
    SkipWhitespaceAndComments;
    if ArgPosition > Length(Block) then
      raise Exception.Create('Missing ' + ArgumentName + ' argument in gallery_link');
    Result := '';
    ParenthesisDepth := 0;
    SawTopLevelString := false;
    while ArgPosition <= Length(Block) do
    begin
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '/') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '*') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition < Length(Block)) and
          ((Block[ArgPosition] <> '*') or (Block[ArgPosition + 1] <> '/')) do
          Inc(ArgPosition);
        if ArgPosition < Length(Block) then
          Inc(ArgPosition, 2);
      end else
      if Block[ArgPosition] = '#' then
      begin
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = '''') or (Block[ArgPosition] = '"') then
      begin
        QuoteChar := Block[ArgPosition];
        IsTopLevelString := ParenthesisDepth = 0;
        ClosedString := false;
        if IsTopLevelString then
          SawTopLevelString := true;
        Inc(ArgPosition);
        while ArgPosition <= Length(Block) do
        begin
          if Block[ArgPosition] = '\' then
          begin
            if ArgPosition < Length(Block) then
            begin
              Inc(ArgPosition);
              if (Block[ArgPosition] = QuoteChar) or
                (Block[ArgPosition] = '\') then
              begin
                if IsTopLevelString then
                  Result += Block[ArgPosition];
              end else
              begin
                if IsTopLevelString then
                begin
                  Result += '\';
                  Result += Block[ArgPosition];
                end;
              end;
              Inc(ArgPosition);
            end else
            begin
              if IsTopLevelString then
                Result += '\';
              Inc(ArgPosition);
            end;
          end else
          if Block[ArgPosition] = QuoteChar then
          begin
            Inc(ArgPosition);
            ClosedString := true;
            Break;
          end else
          begin
            if IsTopLevelString then
              Result += Block[ArgPosition];
            Inc(ArgPosition);
          end;
        end;
        if not ClosedString then
          raise Exception.Create('Unterminated ' + ArgumentName +
            ' string in gallery_link');
      end else
      if Block[ArgPosition] = '(' then
      begin
        Inc(ParenthesisDepth);
        Inc(ArgPosition);
      end else
      if Block[ArgPosition] = ')' then
      begin
        if ParenthesisDepth = 0 then
          Break;
        Dec(ParenthesisDepth);
        Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = ',') and (ParenthesisDepth = 0) then
        Break
      else
        Inc(ArgPosition);
    end;
    if not SawTopLevelString then
      raise Exception.Create('No string value found for ' + ArgumentName +
        ' argument in gallery_link');
  end;

  procedure RequireComma;
  begin
    SkipWhitespaceAndComments;
    if (ArgPosition > Length(Block)) or (Block[ArgPosition] <> ',') then
      raise Exception.Create('Expected comma between gallery_link arguments');
    Inc(ArgPosition);
  end;

begin
  ArgPosition := 1;
  Result.Id := '';
  Result.Author := '';
  Result.Links := nil;
  Result.Name := Trim(ReadStringArgument('title'));
  RequireComma;
  Result.Description := Trim(ReadStringArgument('description'));
  RequireComma;
  Result.ImageUrl := Trim(ReadStringArgument('image'));
  RequireComma;
  Result.RepoUrl := Trim(ReadStringArgument('page'));
end;

function ParsePackageLibraryJson(const Content: String): TPackageInfoArray;
var
  JSONData: TJSONData;
  LibraryPackages: TJSONArray;
  PackageData: TJSONObject;
  PackageLinks: TJSONArray;
  LinkData: TJSONObject;
  Field: TJSONData;
  PackageInfo: TPackageInfo;
  I, J: Integer;

  function RequiredString(const ObjectData: TJSONObject;
    const FieldName: String): String;
  var
    FieldData: TJSONData;
  begin
    FieldData := ObjectData.Find(FieldName);
    if (FieldData = nil) or (FieldData.JSONType <> jtString) then
      raise Exception.Create('Invalid package field "' + FieldName + '"');
    Result := FieldData.AsString;
  end;

begin
  Result := nil;
  JSONData := GetJSON(Content);
  try
    if JSONData.JSONType <> jtObject then
      raise Exception.Create('Library JSON root must be an object');

    Field := TJSONObject(JSONData).Find('packages');
    if (Field = nil) or (Field.JSONType <> jtArray) then
      raise Exception.Create('Library JSON must contain a packages array');
    LibraryPackages := TJSONArray(Field);

    SetLength(Result, LibraryPackages.Count);
    for I := 0 to LibraryPackages.Count - 1 do
    begin
      if LibraryPackages[I].JSONType <> jtObject then
        raise Exception.Create('Library package must be an object');

      PackageData := TJSONObject(LibraryPackages[I]);
      PackageInfo.Id := RequiredString(PackageData, 'id');
      PackageInfo.Name := RequiredString(PackageData, 'title');
      PackageInfo.Description := RequiredString(PackageData, 'description');
      PackageInfo.ImageUrl := RequiredString(PackageData, 'image');
      PackageInfo.RepoUrl := RequiredString(PackageData, 'page');
      PackageInfo.Author := RequiredString(PackageData, 'author');

      Field := PackageData.Find('links');
      if (Field = nil) or (Field.JSONType <> jtArray) then
        raise Exception.Create('Invalid package field "links"');
      PackageLinks := TJSONArray(Field);
      SetLength(PackageInfo.Links, PackageLinks.Count);
      for J := 0 to PackageLinks.Count - 1 do
      begin
        if PackageLinks[J].JSONType <> jtObject then
          raise Exception.Create('Package link must be an object');
        LinkData := TJSONObject(PackageLinks[J]);
        PackageInfo.Links[J].Name := RequiredString(LinkData, 'name');
        PackageInfo.Links[J].Url := RequiredString(LinkData, 'url');
      end;

      Result[I] := PackageInfo;
      WritelnLog('PackageLibrary', 'Found component: ' + PackageInfo.Name);
    end;
  finally
    JSONData.Free;
  end;
end;

function ParsePackageLibraryFallback(const Content: String): TPackageInfoArray;
var
  Regex: TRegExpr;
  BlockStart, BlockEnd: Integer;
  Block: String;
  PackageInfo: TPackageInfo;
begin
  Result := nil;
  Regex := TRegExpr.Create;
  try
    Regex.ModifierM := true;
    Regex.ModifierG := true;
    Regex.Expression := '<\?php\s+gallery_link\s*\(';

    if Regex.Exec(Content) then
    begin
      repeat
        BlockStart := Regex.MatchPos[0] + Regex.MatchLen[0];

        { Find the end of the PHP block }
        BlockEnd := BlockStart;
        while (BlockEnd < Length(Content)) and
          (Copy(Content, BlockEnd, 2) <> '?>') do
          Inc(BlockEnd);

        if BlockEnd < Length(Content) then
        begin
          Block := Copy(Content, BlockStart, BlockEnd - BlockStart);
          PackageInfo := ParseComponentBlock(Block);

          if (PackageInfo.Name <> '') and (PackageInfo.ImageUrl <> '') then
          begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := PackageInfo;
            WritelnLog('PackageLibrary', 'Found component: ' + PackageInfo.Name);
          end;
        end;
      until not Regex.ExecNext;
    end;
    if Length(Result) = 0 then
      raise Exception.Create('No gallery_link components found in fallback library');
  finally
    Regex.Free;
  end;
end;

function DownloadContent(const Url: String): String;
var
  DownloadStream: TStream;
begin
  DownloadStream := Download(Url);
  try
    SetLength(Result, DownloadStream.Size);
    if DownloadStream.Size > 0 then
      DownloadStream.ReadBuffer(Result[1], DownloadStream.Size);
  finally
    DownloadStream.Free;
  end;
end;

function LoadPackageLibrary(const Url: String): TPackageInfoArray;
var
  Content: String;
  PreviousEnableBlockingDownloads: Boolean;
begin
  PreviousEnableBlockingDownloads := EnableBlockingDownloads;
  EnableBlockingDownloads := true;
  try
    try
      WritelnLog('PackageLibrary', 'Downloading library from: ' + Url);
      Content := DownloadContent(Url);
      WritelnLog('PackageLibrary', 'Downloaded ' + IntToStr(Length(Content)) + ' bytes');
      Result := ParsePackageLibraryJson(Content);
    except
      on E: Exception do
      begin
        WritelnWarning('PackageLibrary', 'Failed to download/parse JSON library: ' +
          E.Message + '; trying fallback URL');
        try
          WritelnLog('PackageLibrary', 'Downloading fallback library from: ' +
            LibraryFallbackUrl);
          Content := DownloadContent(LibraryFallbackUrl);
          WritelnLog('PackageLibrary', 'Downloaded ' + IntToStr(Length(Content)) + ' bytes');
          Result := ParsePackageLibraryFallback(Content);
        except
          on FallbackException: Exception do
          begin
            WritelnWarning('PackageLibrary', 'Failed to download/parse fallback library: ' +
              FallbackException.Message);
            raise Exception.Create('JSON library failed: ' + E.Message +
              '; fallback library failed: ' + FallbackException.Message);
          end;
        end;
      end;
    end;
  finally
    EnableBlockingDownloads := PreviousEnableBlockingDownloads;
  end;
  WritelnLog('PackageLibrary', 'Parsed ' + IntToStr(Length(Result)) + ' components');
end;

end.
