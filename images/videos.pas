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
    is just a sequence of image files.

    Note that some properties and methods of this class may look familiar
    to properties of TVRMLGLAnimation. And indeed they are familiar
    and work in an analogous way. Since both of these classes implement
    some sort of animation loading and playing, it's most sensible that
    some ideas find use in both of them. For example the idea of
    LoadFromFile / checking Loaded / Close. For example the idea that
    the programmer interface is mainly through ImageFromTime
    (like TVRMLGLAnimation.SceneFromTime). For example the way
    TimeLoop and TimeBackwards properties work. }
  TVideo = class
  private
    FItems: array of TImage;
    function GetItems(Index: Integer): TImage;
    FCache: TImagesCache;
    FLoaded: boolean;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
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

    { @abstract(Should the video be played in a loop?)

      If yes, then IndexFromTime and ImageFromTime will return information
      that causes the video to be played in an infinite loop.
      This cooperates with TimeBackwards:
      If TimeBackwards is also @true, then each loop step will play
      video once forward, and once backward --- infinitely.

      It's Ok to change the value of this property at any time
      (even when video is not yet loaded), since this doesn't really
      cause any internal recalculation. It only affects what
      *FromTime methods return. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop default false;

    { @abstract(Should the video be played backwards after playing forward?)

      This cooperates with TimeLoop. If this is @true and TimeLoop = @false,
      video will be played once forward, once backward, and then stop.
      If this is @true and TimeLoop = also @true, then the video
      will be played forward, then backward, infinitely.

      It's Ok to change the value of this property at any time
      (even when video is not yet loaded), since this doesn't really
      cause any internal recalculation. It only affects what
      *FromTime methods return. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards default false;
  end;

implementation

uses KambiUtils, SysUtils, Math, KambiStringUtils;

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
var
  DivResult: SmallInt;
  ModResult: Word;
begin
  Assert(Loaded);

  Result := Floor(Time * FramesPerSecond);

  DivUnsignedMod(Result, Count, DivResult, ModResult);

  if TimeLoop then
  begin
    if TimeBackwards and Odd(DivResult) then
      Result := High(FItems) - ModResult else
      Result := ModResult;
  end else
  begin
    if TimeBackwards then
    begin
      if (DivResult < 0) or (DivResult > 1) then
        Result := 0 else
      if DivResult = 1 then
        Result := High(FItems) - ModResult;
        { else DivResult = 0, so Result is already correct }
    end else
    begin
      if DivResult < 0 then
        Result := 0 else
      if DivResult > 0 then
        Result := High(FItems);
    end;
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
