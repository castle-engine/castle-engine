{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}
unit Videos;

interface

uses Images, ImagesCache;

type
  { Simple video loader. For now, the only "movie format" it can load
    is just a sequence of image files. }
  TVideo = class
  private
    FItems: array of TImage;
    function GetItems(Index: Integer): TImage;
    FCache: TImagesCache;
    FLoaded: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Loaded video properties.
      Use these only when @link(Loaded) is @true.
      @groupBegin }
    function Count: Integer;
    property Items [Index: Integer]: TImage read GetItems;
    function IndexFromTime(const Time: Single): Integer;
    function ImageFromTime(const Time: Single): TImage;
    { @groupEnd }

    { Loads video from file.

      For now, this can load only a sequence of images with a filename
      like image%d.png.

      More precisely: we use FormatIndexedName to
      recognize filename with %d pattern. If it contains %d pattern,
      then we try to load image sequence starting from counter 1.
      If not, we just load a single image (and treat it as a movie with only
      one frame).

      We load images using the @link(Cache), you must assign it. }
    procedure LoadFromFile(const FileName: string);

    { This releases all resources allocared by Load (or LoadFromFile).
      @link(Loaded) property changes to @false after calling this.

      It's safe to call this even if @link(Loaded) is already @false --- then
      this will do nothing. }
    procedure Close;

    property Loaded: boolean read FLoaded;

    { Cache used to load images. You @bold(must) assign the cache
      before loading anything, and the Cache instance must remain constant
      between loading and closing the video file. }
    property Cache: TImagesCache read FCache write FCache;
  end;

implementation

uses SysUtils, Math, KambiStringUtils;

const
  FramesPerSecond = 25.0;

constructor TVideo.Create;
begin
  inherited;
  FLoaded := false;
end;

destructor TVideo.Destroy;
begin
  Close;
  inherited;
end;

function TVideo.Count: Integer;
begin
  Assert(Loaded);
  Result := High(FItems) + 1;
end;

function TVideo.GetItems(Index: Integer): TImage;
begin
  Assert(Loaded);
  Result := FItems[Index];
end;

function TVideo.IndexFromTime(const Time: Single): Integer;
begin
  Assert(Loaded);
  if Time < 0 then
    Result := 0 else
  begin
    Result := Floor(Time * FramesPerSecond);
    if Result >= Count then
      Result := Count - 1;
  end;
end;

function TVideo.ImageFromTime(const Time: Single): TImage;
begin
  Result := FItems[IndexFromTime(Time)];
end;

procedure TVideo.Close;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    Cache.LoadImage_DecReference(FItems[I]);
  SetLength(FItems, 0);
  FLoaded := false;
end;

procedure TVideo.LoadFromFile(const FileName: string);
var
  Index, ReplacementsDone: Cardinal;
  S: string;
  NewItem: TImage;
begin
  Close;

  FormatIndexedName(FileName, 0, ReplacementsDone);
  if ReplacementsDone > 0 then
  begin
    Index := 1;
    S := FormatIndexedName(FileName, Index);
    while FileExists(S) do
    begin
      { Remember that Cache.LoadImage_IncReference may raise an exception
        for invalid / not existing / not readable image filenames.
        So don't increase FItems before NewItem is successfully loaded. }
      NewItem := Cache.LoadImage_IncReference(
        { Cache.LoadImage_IncReference requires absolute expanded filename }
        ExpandFileName(S));
      SetLength(FItems, Length(FItems) + 1);
      FItems[High(FItems)] := NewItem;

      Inc(Index);
      S := FormatIndexedName(FileName, Index);
    end;
    if Length(FItems) = 0 then
      raise Exception.CreateFmt('First video image "%s" not found, cannot load the video',
        [S]);
  end else
  begin
    NewItem := Cache.LoadImage_IncReference(
      { Cache.LoadImage_IncReference requires absolute expanded filename }
      ExpandFileName(FileName));
    SetLength(FItems, 1);
    FItems[0] := NewItem;
  end;

  FLoaded := true;
end;

end.
