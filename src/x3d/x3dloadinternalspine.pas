{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine 2D animations loader. }
unit X3DLoadInternalSpine;

interface

uses X3DNodes;

function LoadSpine(const URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, FGL, FpJson, JSONParser,
  CastleVectors, CastleUtils, CastleLog, CastleURIUtils, CastleDownload,
  CastleStringUtils, CastleClassUtils;

type
  ESpineReadError = class(Exception);

  TAttachment = class
  public
    Name: string;
    Rotate: boolean;
    XY, Size, Orig, Offset: TVector2Integer;
    Index: Integer;
  end;

  TAttachmentList = specialize TFPGObjectList<TAttachment>;

  TAtlas = class
  public
    TextureURL: string;
    Format: string;
    Filter: string; //< a value allowed by TextureProperties.MinificationFilter and MagnificationFilter
    IsRepeat: boolean;
    Attachments: TAttachmentList;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TAtlas.Create;
begin
  inherited;
  Attachments := TAttachmentList.Create;
end;

destructor TAtlas.Destroy;
begin
  FreeAndNil(Attachments);
  inherited;
end;

function LoadSpine(const URL: string): TX3DRootNode;

  { Read .atlas file as produced by Spine, in format of libgdx, see
    https://github.com/libgdx/libgdx/wiki/Texture-packer }
  function ReadAtlas(const AtlasURL: string): TAtlas;

    { Split a Line divided by character Separator into two strings.
      Assumes that whitespace doesn't matter (so we trim it),
      and Name must not be empty. }
    function Split(const Line: string; const Separator: char;
      out Name, Value: string): boolean;
    var
      Index: Integer;
    begin
      Result := false;
      Index := Pos(Separator, Line);
      if Index <> 0 then
      begin
        Name := Trim(Copy(Line, 1, Index - 1));
        Value := Trim(SEnding(Line, Index + 1));
        if Name <> '' then
          Result := true;
      end;
    end;

    function IsNameValueString(const Line, Name: string; out Value: string): boolean;
    var
      N, V: string;
    begin
      Result := Split(Line, ':', N, V) and (N = Name);
      if Result then
        Value := V;
    end;

    function IsNameValueBoolean(const Line, Name: string; out Value: boolean): boolean;
    var
      ValueStr: string;
    begin
      Result := IsNameValueString(Line, Name, ValueStr);
      if Result then
      begin
        if ValueStr = 'false' then
          Value := false else
        if ValueStr = 'true' then
          Value := true else
          raise ESpineReadError.CreateFmt('Invalid boolean value "%s"', [ValueStr]);
      end;
    end;

    function IsNameValueInteger(const Line, Name: string; out Value: Integer): boolean;
    var
      ValueStr: string;
    begin
      Result := IsNameValueString(Line, Name, ValueStr);
      if Result then
      begin
        try
          Value := StrToInt(ValueStr);
        except
          on E: EConvertError do
            raise ESpineReadError.CreateFmt('Invalid integer value "%s": %s', [ValueStr, E.Message]);
        end;
      end;
    end;

    function IsNameValueVector2Integer(const Line, Name: string; out Vector: TVector2Integer): boolean;
    var
      ValueStr, ValueStr0, ValueStr1: string;
    begin
      Result := IsNameValueString(Line, Name, ValueStr);
      if Result then
      begin
        if Split(ValueStr, ',', ValueStr0, ValueStr1) then
        try
          Vector[0] := StrToInt(ValueStr0);
          Vector[1] := StrToInt(ValueStr1);
        except
          on E: EConvertError do
            raise ESpineReadError.CreateFmt('Invalid integer value in vector of 2 integers "%s": %s', [ValueStr, E.Message]);
        end else
          raise ESpineReadError.CreateFmt('Cannot split a vector of 2 integers "%s" by a comma', [ValueStr]);
      end;
    end;

    function IsNameValueFilter(const Line, Name: string; out Filter: string): boolean;
    var
      ValueStr: string;
    begin
      Result := IsNameValueString(Line, Name, ValueStr);
      if Result then
      begin
        if ValueStr = 'Linear,Linear' then
          Filter := 'AVG_PIXEL' else
        if ValueStr = 'Nearest,Nearest' then
          Filter := 'NEAREST_PIXEL' else
          raise ESpineReadError.CreateFmt('Unsupported filter mode "%s"', [ValueStr]);
      end;
    end;

    function IsNameValueRepeat(const Line, Name: string; out IsRepeat: boolean): boolean;
    var
      ValueStr: string;
    begin
      Result := IsNameValueString(Line, Name, ValueStr);
      if Result then
      begin
        if ValueStr = 'none' then
          IsRepeat := false else
          { is there anything else allowed for repeat: field ? }
          raise ESpineReadError.CreateFmt('Unsupported repeat mode "%s"', [ValueStr]);
      end;
    end;

  var
    AtlasReader: TTextReader;
    Attachment: TAttachment;
    Line: string;
  begin
    AtlasReader := TTextReader.Create(AtlasURL);
    try
      Result := TAtlas.Create;
      try
        { TODO: support > 1 texture in atlas, separated by newline.
          No need to omit empty lines, they should not occur,
          except to separate texture in atlas. }
        repeat
          Result.TextureURL := AtlasReader.Readln;
        until AtlasReader.Eof or (Trim(Result.TextureURL) <> '');

        if AtlasReader.Eof then
          raise ESpineReadError.Create('Unexpected end of atlas file');

        while not AtlasReader.Eof do
        begin
          Line := AtlasReader.Readln;
          if Trim(Line) <> '' then
          begin
            if Line[1] <> ' ' then
            begin
              Attachment := nil;
              if IsNameValueString(Line, 'format', Result.Format) then else
              if IsNameValueFilter(Line, 'filter', Result.Filter) then else
              if IsNameValueRepeat(Line, 'repeat', Result.IsRepeat) then else
              if Pos(':', Line) <> 0 then
                raise ESpineReadError.CreateFmt('Unhandled name:value pair "%s"', [Line]) else
              begin
                Attachment := TAttachment.Create;
                Attachment.Name := Line;
                Result.Attachments.Add(Attachment);
                WritelnLog('Spine', 'Added attachment ' + Attachment.Name);
              end;
            end else
            if Attachment <> nil then
            begin
              if IsNameValueBoolean(Line, 'rotate', Attachment.Rotate) then else
              if IsNameValueVector2Integer(Line, 'xy', Attachment.XY) then else
              if IsNameValueVector2Integer(Line, 'size', Attachment.Size) then else
              if IsNameValueVector2Integer(Line, 'orig', Attachment.Orig) then else
              if IsNameValueVector2Integer(Line, 'offset', Attachment.Offset) then else
              if IsNameValueInteger(Line, 'index', Attachment.Index) then else
                raise ESpineReadError.CreateFmt('Unhandled name:value pair "%s"', [Line]);
            end else
              raise ESpineReadError.Create('Atlas file contains indented line, but no attachment name specified');
          end;
        end;
      except FreeAndNil(Result); raise end;
    finally FreeAndNil(AtlasReader) end;
  end;

var
  Json: TJSONData;
  P: TJSONParser;
  S: TStream;
  AtlasURL: string;
begin
  S := Download(URL);
  try
    P := TJSONParser.Create(S);
    try
      Json := P.Parse;
      try
        Result := TX3DRootNode.Create('', URL);
        try
          if Assigned(Json) then
            WritelnLogMultiline('Spine', 'Returned JSON structure: ' +
              Json.ClassName + NL + Json.AsJSON);

          AtlasURL := ChangeURIExt(URL, '.atlas');
          if URIFileExists(AtlasURL) then
            ReadAtlas(AtlasURL);
        except FreeAndNil(Result); raise end;
      finally FreeAndNil(Json) end;
    finally FreeAndNil(P) end;
  finally FreeAndNil(S) end;
end;

end.

