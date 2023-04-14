{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine atlas. }

type
  TAtlasRegion = class
  public
    Name: String;
    Rotate: Boolean;
    { Left-bottom corner of the place in atlas. In pixels of the atlas image. }
    XY: TVector2Integer;
    { Size of the place in atlas. In pixels of the atlas image. }
    Size: TVector2Integer;
    { Original region size, before whitespace stripping was done.
      May be equal Size, or just be left 0, if no whitespace stripping was done. }
    Orig: TVector2Integer;
    { Offset is non-zero only it whitespace stripping was done,
      i.e. the complete texture area is not in the atlas file, as it was stripped. }
    Offset: TVector2Integer;
    TexCoord: TQuadTexCoord;
    { When Spine atlas creator uses whitespace compression for texture,
      the actual 3D points need to be squeezed a little to show smaller
      part of the texture. Without compression, is starts at (0,0)
      and ends at (1,1), with compression it is a little smaller. }
    TexRect: TQuadTexRect;
    Index: Integer;
    procedure CalculateTexCoord(const ImageWidth, ImageHeight: Integer);
  end;

  TAtlasRegionList = {$ifdef FPC}specialize{$endif} TObjectList<TAtlasRegion>;

  TAtlasPage = class
  public
    TextureURL: string;
    PMA: String;
    Format: string;
    Filter: string; //< a value allowed by TextureProperties.MinificationFilter and MagnificationFilter
    IsRepeat: boolean;
    Size: TVector2Integer; //< atlas may (but does not have to) contain this
    Scale: Single; //< scale used at atlas generation; we don't actually need this for anything
    Regions: TAtlasRegionList;
    Node: TImageTextureNode;
    NodeUsedAsChild: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure BuildNodes(const BaseUrl: string);
  end;

  TAtlasPageList = {$ifdef FPC}specialize{$endif} TObjectList<TAtlasPage>;

  TAtlas = class(TTextureLoader)
    Pages: TAtlasPageList;
    constructor Create;
    destructor Destroy; override;
    { Read .atlas file as produced by Spine, in format of libgdx, see
      https://github.com/libgdx/libgdx/wiki/Texture-packer }
    procedure Parse(const URL: string);
    procedure BuildNodes(const BaseUrl: string);
    { Find atlas page and region corresponding to given region name.
      Returns @false if it does not exist (the out vars are undefined in
      this case). }
    function Find(const RegionName: string;
      out Page: TAtlasPage; out Region: TAtlasRegion): boolean;
    function UseNode(const RegionName: string;
      out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect;
      out TexRotated: boolean): TImageTextureNode; override;
  end;

procedure TAtlasRegion.CalculateTexCoord(const ImageWidth, ImageHeight: Integer);
var
  TextureXY, TextureSize: TVector2;
  I: Integer;
  OriginalSize: TVector2Integer;
begin
  TextureXY := Vector2(
    XY.X / ImageWidth,
    XY.Y / ImageHeight);

  OriginalSize := Orig;
  { if not specified, assume Orig is equal Size. }
  if OriginalSize.X = 0 then
    OriginalSize.X := Size.X;
  if OriginalSize.Y = 0 then
    OriginalSize.Y := Size.Y;

  TexRect[0].X := Offset.X / OriginalSize.X;
  TexRect[0].Y := Offset.Y / OriginalSize.Y;
  TexRect[1].X := (Offset.X + Size.X) / OriginalSize.X;
  TexRect[1].Y := (Offset.Y + Size.Y) / OriginalSize.Y;
  if Rotate then
  begin
    TextureSize := Vector2(
      Size.X / ImageHeight,
      Size.Y / ImageWidth);
    TextureXY.Y :=
      { flip top-bottom }
      1 - TextureXY.Y
      { move corner to bottom }
      - TextureSize.X;
  end else
  begin
    TextureSize := Vector2(
      Size.X / ImageWidth,
      Size.Y / ImageHeight);
    TextureXY.Y :=
      { flip top-bottom }
      1 - TextureXY.Y
      { move corner to bottom }
      - TextureSize.Y;
  end;

  if Rotate then
  begin
    TexCoord[0] := Vector2(TextureSize.Y,             0);
    TexCoord[1] := Vector2(TextureSize.Y, TextureSize.X);
    TexCoord[2] := Vector2(            0, TextureSize.X);
    TexCoord[3] := Vector2(            0,             0);
  end else
  begin
    TexCoord[0] := Vector2(            0,             0);
    TexCoord[1] := Vector2(TextureSize.X,             0);
    TexCoord[2] := Vector2(TextureSize.X, TextureSize.Y);
    TexCoord[3] := Vector2(            0, TextureSize.Y);
  end;

  for I := 0 to 3 do
    TexCoord[I] := TexCoord[I] + TextureXY;
end;

constructor TAtlasPage.Create;
begin
  inherited;
  Regions := TAtlasRegionList.Create;
  Scale := 1;
end;

destructor TAtlasPage.Destroy;
begin
  FreeAndNil(Regions);
  if NodeUsedAsChild then
    { in case NodeUsedAsChild, don't even try FreeIfUnusedAndNil,
      as the check "is it unused" may already cause access violation
      since it may be already freed by freeing parent. }
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

procedure TAtlasPage.BuildNodes(const BaseUrl: string);
var
  I, ReferenceImageWidth, ReferenceImageHeight: Integer;
begin
  Node := TImageTextureNode.Create('Page_' + TextureURL, BaseUrl);
  Node.SetUrl([TextureURL]);
  AddSpineTextureProperties(Node, IsRepeat);

  ReferenceImageWidth := 0;
  ReferenceImageHeight := 0;
  if (Size.X > 0) and (Size.Y > 0) then
  begin
    { It's best to read reference sizes from .atlas file.
      This way you can scale the image underneath at any time, without changing
      the .atlas file, and it will render Ok. }
    ReferenceImageWidth := Size.X;
    ReferenceImageHeight := Size.Y;
  end else
  begin
    { Then the only way to calculate tex coords is to actually load the texture.
      We use the texture Node for this, this way loaded texture will be reused for
      actual model rendering. }
    Node.IsTextureLoaded := true;
    if Node.IsTextureImage then
    begin
      ReferenceImageWidth := Node.TextureImage.Width;
      ReferenceImageHeight := Node.TextureImage.Height;
    end;
  end;

  if (ReferenceImageWidth > 0) and
     (ReferenceImageHeight > 0) then
  begin
    for I := 0 to Regions.Count - 1 do
      Regions[I].CalculateTexCoord(ReferenceImageWidth, ReferenceImageHeight);
  end else
    WritelnWarning('Spine', SysUtils.Format('Cannot load texture "%s", texture coordinates cannot be correctly calculated based on Spine atlas information',
      [TextureURL]));
end;

constructor TAtlas.Create;
begin
  inherited;
  Pages := TAtlasPageList.Create;
end;

destructor TAtlas.Destroy;
begin
  FreeAndNil(Pages);
  inherited;
end;

procedure TAtlas.Parse(const URL: string);

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

  { All the IsNameValueXxx functions return @true and change Value
    only if the line contains (corrent) specification of attribute
    Name. Otherwise, they return @false and leave Value unmodified. }

  function IsNameValueString(const Line, Name: string; var Value: string): boolean;
  var
    N, V: string;
  begin
    Result := Split(Line, ':', N, V) and (N = Name);
    if Result then
      Value := V;
  end;

  (* Not useful now
  function IsNameValueBoolean(const Line, Name: string; var Value: boolean): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'false' then
        Value := false
      else
      if ValueStr = 'true' then
        Value := true
      else
        raise ESpineReadError.CreateFmt('Invalid boolean value "%s"', [ValueStr]);
    end;
  end;
  *)

  { Read "rotate" type, which is either boolean or integer (changed somewhere around Spine 4.0 release). }
  function IsNameValueRotate(const Line, Name: string; var Value: Boolean): Boolean;
  var
    ValueStr: String;
    ValueInt: Integer;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'false' then
        Value := false
      else
      if ValueStr = 'true' then
        Value := true
      else
      if TryStrToInt(ValueStr, ValueInt) then
      begin
        case ValueInt of
          0: Value := false;
          90: Value := true;
          else
            begin
              WritelnWarning('Unsupported "rotate" integer value in atlas "%d"', [ValueInt]);
              Exit(false);
            end;
        end;
      end else
      begin
        WritelnWarning('Unsupported "rotate" value in atlas "%s"', [ValueStr]);
        Exit(false);
      end;
    end;
  end;

  function IsNameValueInteger(const Line, Name: string; var Value: Integer): boolean;
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

  function IsNameValueSingle(const Line, Name: string; var Value: Single): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if not TryStrToFloatDot(ValueStr, Value) then
      begin
        WritelnWarning('Invalid float value in atlas "%s"', [ValueStr]);
        Exit(false);
      end;
    end;
  end;

  function IsNameValueVector2Integer(const Line, Name: string; var Vector: TVector2Integer): boolean;
  var
    ValueStr, ValueStr0, ValueStr1: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if Split(ValueStr, ',', ValueStr0, ValueStr1) then
      try
        Vector.X := StrToInt(ValueStr0);
        Vector.Y := StrToInt(ValueStr1);
      except
        on E: EConvertError do
          raise ESpineReadError.CreateFmt('Invalid integer value in vector of 2 integers "%s": %s', [ValueStr, E.Message]);
      end else
        raise ESpineReadError.CreateFmt('Cannot split a vector of 2 integers "%s" by a comma', [ValueStr]);
    end;
  end;

  function IsNameValueVector4Integer(const Line, Name: string; var Vector: TVector4Integer): Boolean;
  var
    ValueStr: String;
    Tokens: TCastleStringList;
    I: Integer;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      Tokens := CreateTokens(ValueStr, WhiteSpaces + [',']);
      try
        if Tokens.Count <> 4 then
        begin
          WritelnWarning('Expected 4 values for "%s", got %d values', [Name, Tokens.Count]);
          Exit(false);
        end;
        try
          for I := 0 to 3 do
            Vector.Data[I] := StrToInt(Tokens[I]);
        except
          on E: EConvertError do
          begin
            WritelnWarning('Invalid integer value in vector of 4 integers "%s": %s', [ValueStr, E.Message]);
            Exit(false);
          end;
        end;
      finally FreeAndNil(Tokens) end;
    end;
  end;

  function IsNameValueTwoVectors2Integer(const Line, Name: string; var V1, V2: TVector2Integer): Boolean;
  var
    Bounds: TVector4Integer;
  begin
    Result := IsNameValueVector4Integer(Line, Name, Bounds);
    if Result then
    begin
      V1.X := Bounds[0];
      V1.Y := Bounds[1];
      V2.X := Bounds[2];
      V2.Y := Bounds[3];
    end;
  end;

  function IsNameValueFilter(const Line, Name: string; var Filter: string): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'Linear,Linear' then
        Filter := 'AVG_PIXEL'
      else
      if ValueStr = 'Nearest,Nearest' then
        Filter := 'NEAREST_PIXEL'
      else
        raise ESpineReadError.CreateFmt('Unsupported filter mode "%s"', [ValueStr]);
    end;
  end;

  function IsNameValueRepeat(const Line, Name: string; var IsRepeat: boolean): boolean;
  var
    ValueStr: string;
  begin
    Result := IsNameValueString(Line, Name, ValueStr);
    if Result then
    begin
      if ValueStr = 'none' then
        IsRepeat := false
      else
        { is there anything else allowed for repeat: field ? }
        raise ESpineReadError.CreateFmt('Unsupported repeat mode "%s"', [ValueStr]);
    end;
  end;

var
  Reader: TTextReader;
  Page: TAtlasPage;
  Region: TAtlasRegion;
  Line: string;
begin
  Page := nil;
  Region := nil;
  Reader := TTextReader.Create(URL);
  try
    while not Reader.Eof do
    begin
      Line := Reader.Readln;
      if Page = nil then
      begin
        { start atlas page }
        if Trim(Line) <> '' then
        begin
          Page := TAtlasPage.Create;
          Page.TextureURL := Trim(Line);
          Pages.Add(Page);
          Region := nil;
        end;
      end else
      if Trim(Line) = '' then
      begin
        { end atlas page }
        Page := nil;
        Region := nil;
      end else
      if Region = nil then
      begin
        { read per-page (but not per-region) info }
        if IsNameValueString(Line, 'format', Page.Format) then else
        if IsNameValueFilter(Line, 'filter', Page.Filter) then else
        if IsNameValueRepeat(Line, 'repeat', Page.IsRepeat) then else
        if IsNameValueSingle(Line, 'scale', Page.Scale) then else
        if IsNameValueString(Line, 'pma', Page.PMA) then else // We ignore pma
        if IsNameValueVector2Integer(Line, 'size', Page.Size) then else
        if Pos(':', Line) <> 0 then
          raise ESpineReadError.CreateFmt('Unhandled name:value pair (outside region) "%s"', [Line]) else
        begin
          { new region }
          Region := TAtlasRegion.Create;
          Region.Name := Line;
          Page.Regions.Add(Region);
        end;
      end else
      begin
        { read per-region info }
        if IsNameValueRotate(Line, 'rotate', Region.Rotate) then else
        if IsNameValueVector2Integer(Line, 'xy', Region.XY) then else
        if IsNameValueVector2Integer(Line, 'size', Region.Size) then else
        if IsNameValueVector2Integer(Line, 'orig', Region.Orig) then else
        if IsNameValueVector2Integer(Line, 'offset', Region.Offset) then else
        if IsNameValueInteger(Line, 'index', Region.Index) then else
        if IsNameValueTwoVectors2Integer(Line, 'bounds', Region.XY, Region.Size) then else
        if IsNameValueTwoVectors2Integer(Line, 'offsets', Region.Offset, Region.Orig) then else
        if Pos(':', Line) <> 0 then
          raise ESpineReadError.CreateFmt('Unhandled name:value pair (inside region) "%s"', [Line]) else
        begin
          { new region }
          Region := TAtlasRegion.Create;
          Region.Name := Line;
          Page.Regions.Add(Region);
        end;
      end;
    end;
  finally FreeAndNil(Reader) end;

  //WritelnLog('Spine', Format('Atlas parsed, pages: %d', [Pages.Count]));
end;

procedure TAtlas.BuildNodes(const BaseUrl: string);
var
  I: Integer;
begin
  for I := 0 to Pages.Count - 1 do
    Pages[I].BuildNodes(BaseUrl);
end;

function TAtlas.Find(const RegionName: string;
  out Page: TAtlasPage; out Region: TAtlasRegion): boolean;
var
  I, J: Integer;
begin
  for I := 0 to Pages.Count - 1 do
  begin
    Page := Pages[I];
    for J := 0 to Page.Regions.Count - 1 do
    begin
      Region := Page.Regions[J];
      if Region.Name = RegionName then
        Exit(true);
    end;
  end;

  WritelnWarning('Spine', Format('Region name "%s" not found in atlas', [RegionName]));
  Result := false;
end;

function TAtlas.UseNode(const RegionName: string;
  out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect;
  out TexRotated: boolean): TImageTextureNode;
var
  AtlasPage: TAtlasPage;
  AtlasRegion: TAtlasRegion;
begin
  if not Find(RegionName, AtlasPage, AtlasRegion) then Exit(nil);
  Result := AtlasPage.Node;
  AtlasPage.NodeUsedAsChild := true;
  TexRect := AtlasRegion.TexRect;
  TexCoord := AtlasRegion.TexCoord;
  TexRotated := AtlasRegion.Rotate;
end;
