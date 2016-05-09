unit SandBoxMap;

interface

uses Classes, CastleVectors, CastleImages, CastleGLUtils, CastleGLImages;

type
  TTile = class
  public
    GLImage: TGLImage;
    { Relative URL vs tiles directory.
      This is read and written from/to a map file. }
    RelativeURL: string;
    { This is an absolute URL constructed from RelativeURL. }
    function FullURL: string;
  public
    CharCode: char;
    procedure LoadFromFile; virtual; abstract;
    destructor Destroy; override;
  end;

  { Base ground tile. When loading from file, this will always be resized to
    BaseWidth x BaseHeight.

    It must have alpha channel set properly,
    so that corners are not drawn. If the image doesn't have alpha channel,
    alpha channel will be added and colors equal to lower-left corner
    will be set transparent. It's @link(Image) is always TRGBAlphaImage. }
  TBaseTile = class(TTile)
    procedure LoadFromFile; override;
  end;

  TBonusTile = class(TTile)
    procedure LoadFromFile; override;
  end;

  TMapTile = class
    { Base ground tile. }
    BaseTile: TBaseTile;
    { Object drawn on top of base. Usually has alpha such that
      base is visible beside it. May be large, covering other tiles.
      May be nil to indicate no object. }
    BonusTile: TBonusTile;
  end;

  TMap = class
  private
    procedure CommonCreate;
    { Call this after Width and Height are set, this will initialize Items. }
    procedure CreateItems;
  public
    BaseTiles: array[char] of TBaseTile;
    BonusTiles: array[char] of TBonusTile;
    BaseTilesList: TList;
    BonusTilesList: TList;
    Items: array of array of TMapTile;
    { Must be > 0 always }
    Width, Height: Cardinal;
    PlayerStartX, PlayerStartY: Cardinal;
    constructor Create(AWidth, AHeight: Cardinal);
    constructor CreateFromFile(const AURL: string);
    destructor Destroy; override;
    procedure SaveToFile(const AURL: string);
  end;

implementation

uses SysUtils, CastleFilesUtils, CastleUtils, SandBoxGame, CastleStringUtils,
  CastleClassUtils, CastleDownload;

{ TTile ---------------------------------------------------------------------- }

destructor TTile.Destroy;
begin
  FreeAndNil(GLImage);
  inherited;
end;

function TTile.FullURL: string;
begin
  Result := ApplicationData('tiles/' + RelativeURL);
end;

{ TBaseTile ------------------------------------------------------------------ }

procedure TBaseTile.LoadFromFile;
var
  Image: TCastleImage;
  NewImage: TRGBAlphaImage;
begin
  Image := LoadImage(FullURL, [TRGBImage, TRGBAlphaImage], BaseWidth, BaseHeight);
  if not (Image is TRGBAlphaImage) then
  begin
    NewImage := (Image as TRGBImage).ToRGBAlphaImage;
    NewImage.AlphaDecide(
      Vector3Byte(
        TRGBAlphaImage(Image).AlphaPixels[0][0],
        TRGBAlphaImage(Image).AlphaPixels[1][0],
        TRGBAlphaImage(Image).AlphaPixels[2][0]),
      0, 0, 255);
    Writeln('Alpha added to "', RelativeURL, '" while loading');
    FreeAndNil(Image);
    Image := NewImage;
    { This will automatically fix such images, assuming that URL
      extension is PNG.
    SaveImage(Image, FullURL); }
  end;

  GLImage := TGLImage.Create(Image, false, true);
end;

{ TBonusTile ----------------------------------------------------------------- }

procedure TBonusTile.LoadFromFile;
var
  Image: TCastleImage;
begin
  Image := LoadImage(FullURL, PixelsImageClasses);
  GLImage := TGLImage.Create(Image, false, true);
end;

{ TMap ----------------------------------------------------------------------- }

procedure TMap.CommonCreate;
begin
  inherited Create;
  BaseTilesList := TList.Create;
  BonusTilesList := TList.Create;
end;

constructor TMap.Create(AWidth, AHeight: Cardinal);
begin
  CommonCreate;
  Width := AWidth;
  Height := AHeight;
  CreateItems;
end;

constructor TMap.CreateFromFile(const AURL: string);

  procedure ReadlnTileLine(const F: TTextReader;
    var C: char; var RelativeURL: string);
  var
    CStr: string;
  begin
    CStr := F.Read;
    if Length(CStr) <> 1 then
      raise Exception.Create('Not a single 1st character');
    C := CStr[1];

    RelativeURL := F.Read;
    if RelativeURL = '' then
      raise Exception.CreateFmt('Empty URL after character "%s"', [C]);

    F.Readln;
  end;

var
  F: TTextReader;
  BaseTilesCount, BonusTilesCount: Cardinal;
  C: char;
  S: string;
  I: Integer;
  X, Y: Cardinal;
begin
  CommonCreate;

  F := TTextReader.Create(AURL);
  try
    Width := F.ReadInteger;
    Height := F.ReadInteger;
    PlayerStartX := F.ReadInteger;
    PlayerStartY := F.ReadInteger;
    BaseTilesCount := F.ReadInteger;
    BonusTilesCount := F.ReadInteger;

    if (Width = 0) or (Height = 0) then
      raise Exception.Create('Map width and height must be > 0');

    for I := 0 to Integer(BaseTilesCount) - 1 do
    begin
      ReadlnTileLine(F, C, S);
      BaseTiles[C] := TBaseTile.Create;
      BaseTiles[C].CharCode := C;
      BaseTiles[C].RelativeURL := S;
      BaseTiles[C].LoadFromFile;
      BaseTilesList.Add(BaseTiles[C]);
    end;

    for I := 0 to Integer(BonusTilesCount) - 1 do
    begin
      ReadlnTileLine(F, C, S);
      if C = '_' then
        raise Exception.Create('Bonus tile character cannot be "_"');
      BonusTiles[C] := TBonusTile.Create;
      BonusTiles[C].CharCode := C;
      BonusTiles[C].RelativeURL := S;
      BonusTiles[C].LoadFromFile;
      BonusTilesList.Add(BonusTiles[C]);
    end;

    CreateItems;

    for Y := Height - 1 downto 0 do
    begin
      S := F.Readln;
      if Cardinal(Length(S)) <> Width * 2  then
        raise Exception.CreateFmt('Map line %d has wrong length (%d instead of %d)',
          [Y, Cardinal(Length(S)), Width * 2]);
      for X := 0 to Width - 1 do
      begin
        C := S[X*2 + 1];
        Items[X, Y].BaseTile := BaseTiles[C];
        if Items[X, Y].BaseTile = nil then
          raise Exception.CreateFmt('Base tile character "%s" not initialized, ' +
            'but used on map position (%d, %d)', [C, X, Y]);
        C := S[X*2 + 2];
        if C <> '_' then
        begin
          Items[X, Y].BonusTile := BonusTiles[C];
          if Items[X, Y].BonusTile = nil then
            raise Exception.CreateFmt('Bonus tile character "%s" not initialized, ' +
              'but used on map position (%d, %d)', [C, X, Y]);
        end else
          Items[X, Y].BonusTile := nil;
      end;
    end;
  finally FreeAndNil(F) end;
end;

procedure TMap.CreateItems;
var
  X, Y: Cardinal;
begin
  SetLength(Items, Width, Height);
  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
      Items[X, Y] := TMapTile.Create;
end;

destructor TMap.Destroy;
var
  X, Y: Integer;
  C: char;
begin
  { We can't assume that Width and Height are > 0 here,
    they are possibly not initialized. That's why X, Y must be Integer,
    not Cardinal. }
  for X := 0 to Length(Items) - 1 do
    for Y := 0 to Length(Items[X]) - 1 do
      FreeAndNil(Items[X, Y]);

  for C := Low(C) to High(C) do
  begin
    FreeAndNil(BaseTiles[C]);
    FreeAndNil(BonusTiles[C]);
  end;

  FreeAndNil(BaseTilesList);
  FreeAndNil(BonusTilesList);

  inherited;
end;

procedure TMap.SaveToFile(const AURL: string);
var
  F: TStream;
  S: string;
  I: Integer;
  X, Y: Cardinal;
begin
  F := URLSaveStream(AURL);
  try
    WritelnStr(F, Format('%d %d', [Width, Height]));
    WritelnStr(F, Format('%d %d', [PlayerStartX, PlayerStartY]));
    WritelnStr(F, Format('%d %d', [BaseTilesList.Count, BonusTilesList.Count]));

    for I := 0 to BaseTilesList.Count - 1 do
      WritelnStr(F, Format('%s %s', [
        TBaseTile(BaseTilesList[I]).CharCode,
        TBaseTile(BaseTilesList[I]).RelativeURL]));

    for I := 0 to BonusTilesList.Count - 1 do
      WritelnStr(F, Format('%s %s', [
        TBonusTile(BonusTilesList[I]).CharCode,
        TBonusTile(BonusTilesList[I]).RelativeURL]));

    for Y := Height - 1 downto 0 do
    begin
      SetLength(S, Width * 2);
      for X := 0 to Width - 1 do
      begin
        S[X*2 + 1] := Items[X, Y].BaseTile.CharCode;
        if Items[X, Y].BonusTile <> nil then
          S[X*2 + 2] := Items[X, Y].BonusTile.CharCode else
          S[X*2 + 2] := '_';
      end;
      WritelnStr(F, S);
    end;
  finally FreeAndNil(F) end;
end;

end.
