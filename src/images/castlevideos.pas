{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Video (movie) data (TVideo and helpers). }
unit CastleVideos;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections,
  CastleImages, CastleTimeUtils;

type
  EInvalidFadeFrames = class(Exception);

  { Video.

    It can load movie from a sequence of image files.
    With the help of ffmpeg, it can also load any normal movie file format,
    like avi (compressed by anything ffmpeg can handle), OggTheora and
    many others. ffmpeg really handles probably everything you will
    ever want.

    Video is stored simply as a sequence of uncompressed images in memory.
    This is not good for real movies, so don't even try to open
    any real, 2 hour, full dvd resolution movie --- loading will
    take hours and you will clog your memory. This class is @italic(not)
    supposed to be used in a real movie player.

    However, this simple storage is perfect to load a short movie
    for some effect in a game, for example a simple movie with flames
    or smoke to be shown as texture in your 3D game.
    Memory and loading time is acceptable then,
    and you want to prepare all your textures before the game starts anyway
    (for a 3D game, there's no time to decode some movie format while
    playing...).

    See example program
    @code(castle_game_engine/examples/images_videos/simple_video_editor.lpr)
    in our engine for example of a simple movie player/editor implemented on top
    of this class. }
  TVideo = class
  private
    FItems: array of TCastleImage;
    function GetItems(Index: Integer): TCastleImage;
  private
    FLoaded: boolean;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FFramesPerSecond: Single;
  public
    constructor Create;
    destructor Destroy; override;

    { Loaded video properties.
      Use these only when @link(Loaded) is @true.

      Time is supposed to be in seconds. (Actually, we don't care;
      but when the movie file says to play "25 frames per second", then
      we will make 25 frames per this Time unit. So you probably
      want to count the Time in seconds too.)

      @link(Count) is always >= 1 when the video is loaded.
      That is, we don't allow videos with zero frames (I don't
      know if any movie format allows this.)

      @groupBegin }
    function Count: Integer;
    property Items [Index: Integer]: TCastleImage read GetItems;
    function Width: Cardinal;
    function Height: Cardinal;
    function IndexFromTime(const Time: TFloatTime): Integer;
    function ImageFromTime(const Time: TFloatTime): TCastleImage;
    { @groupEnd }

    { Duration of the video. In seconds (or, more precisely, in the
      same time units as for ImageFromTime and other methods).

      Note that this doesn't count the "backwards" running time,
      so it ignores TimeBackwards and TimeLoop values.

      Use this only when @link(Loaded). }
    function TimeDuration: Single;

    { Number of frames per second for this video.

      For formats with varying frame-rate (although we do not support
      any such format right now), this will be just average frame-rate.
      The idea is that this property is just to show the user, so (s)he
      can judge the quality of the video. }
    property FramesPerSecond: Single read FFramesPerSecond;

    { Loads video from file or URL.

      URL is downloaded using CastleDownload unit.
      As always, if you all you care about is loading normal files, then just pass
      a normal filename (absolute or relative to the current directory)
      as the URL parameter.

      Supported video formats:

      @unorderedList(

        @item(We recognize video URLs by extension, and then try
          to load them through ffmpeg. So ffmpeg must be available
          on $PATH for this. See view3dscene docs for some links.

          Internally, for now we just use ffmpeg to decompose the video to
          single images, and then proceed to load this image sequence.
          So this takes some time and drains memory for longer movies,
          and it's not supposed to be really used to load longer movies.

          Of course, this may get improved (and some simple video formats
          may be handled without ffmpeg dependency) in the future,
          if the need arises. For now, it's enough for me --- all I want
          is to load some simple texture movies (like smoke or fire
          or dense fog animations) to use as billboards in games.
          For such short movies, loading time and memory use are acceptable.)

        @item(We can also load a sequence of images with a URL
          like image@@counter(1).png.

          We use FormatNameCounter to recognize URLs with
          @code(@@counter(*)) macro. If it contains @code(@@counter(*)) macro,
          then we try to load image sequence starting from counter 1.
          If not, we just load a single image (and treat it as a movie with only
          one frame).

          Note that this allows you to have alpha channel for the video.
          Since we just load the sequence of images, they can have
          alpha channel just like any still image (e.g. PNG) can have it.
          So your animated texture of fire / smoke
          etc. will be rendered will all the features of alpha channel
          (blending or alha testing, depending on alpha channel type).
          I don't know if any video file format supports any kind of
          alpha channel, so using image sequence may be actually your
          only choice for videos with alpha channel.)
      )

      Parameters ResizeToX, ResizeToY allow you to resize video frames.
      If one of them is non-zero, the appropriate video size (width and/or
      height) will be resized. Resizing quality is controlled by
      Interpolation parameter. }
    procedure LoadFromFile(const URL: string;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riBilinear);

    { Save video to file (or image sequence).

      Handled formats: just like LoadFromFile. Also, just like LoadFromFile,
      we need ffmpeg on $PATH to save to any single-file movie format. }
    procedure SaveToFile(const URL: string);

    procedure Resize(const ResizeToX, ResizeToY: Cardinal;
      const Interpolation: TResizeInterpolation = riBilinear);

    { Release all resources allocated by @link(LoadFromFile).
      @link(Loaded) property changes to @false after calling this.

      It's safe to call this even if @link(Loaded) is already @false --- then
      this will do nothing. }
    procedure Close;

    property Loaded: boolean read FLoaded;

    { Play the video in a never-ending loop.

      If @true then IndexFromTime and ImageFromTime will return information
      that causes the video to be played in an infinite loop.
      This cooperates with TimeBackwards:
      If TimeBackwards is also @true, then each loop step will play
      video once forward, and once backward --- infinitely.

      It's Ok to change the value of this property at any time
      (even when video is not yet loaded), since this doesn't really
      cause any internal recalculation. It only affects what
      *FromTime methods return. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop default false;

    { Play the video backwards after playing it forward.

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

    { Mix the video with itself played backwards,
      to force the video to play seamless in a loop.

      This edits the loaded video, such that every frame becomes a mix
      between itself and the corresponding frame from the second half
      of the video. This forces any
      movie to become seamless when played in a loop.

      After this, the movie frames count is halved and TimeBackwards
      is set to @true. This is to conserve memory (and running time of
      this function), since there's no point in keeping both halves of the
      movie anymore --- they are identical (except reversed in time)
      after such operation.

      @italic(Unfortunately, this doesn't look perfect, human eye can easily
      notice the extreme time points (in the middle and at the end),
      when objects in the video reverse their directions etc.
      So the video is seamless, and it's a better trick than just setting
      TimeBackwards := true, but it still doesn't really look good.)

      If ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from CastleProgress to indicate progress of operation. }
    procedure MixWithSelfBackwards(const ProgressTitle: string);

    { Edit the video beginning to fade with the video ending,
      thus forcing the video to play seamless in a loop.

      The idea is to take out last FadeFrames from the video
      (FadeFrames must be >= 0 and <= Count div 2). And then mix
      the beginning of the video, such that the beginning FadeFrames
      frames gradually fade from the original end of video to original
      begin of the video.

      Video must be loaded when using this.

      If ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from CastleProgress to indicate progress of operation.

      @raises(EInvalidFadeFrames When FadeFrames is wrong.) }
    procedure FadeWithSelf(FadeFrames: Cardinal;
      const ProgressTitle: string);

    { Similar to IndexFromTime, this is a class method that calculates
      frame index using the same algorithm. (Actually, IndexFromTime
      just calls this, but this is implementation detail...).

      The idea is that you can reuse the same algorithm in cases when
      you somehow have the video data but do not have an instance
      of TVideo class.

      In particular, this public for TGLVideo.IndexFromTime,
      TGLVideo.GLTextureFromTime methods. }
    class function FrameIndexFromTime(const Time: TFloatTime;
      const ACount: Integer;
      const AFramesPerSecond: Single;
      const ATimeLoop, ATimeBackwards: boolean): Integer;

    { Alpha channel type of loaded video.
      See TCastleImage.AlphaChannel for precise meaning of this.
      Currently based on the first video image, so it's fast although
      in some cases possibly inaccurate. }
    function AlphaChannel(
      const AlphaTolerance: Byte = DefaultAlphaTolerance): TAlphaChannel;

    procedure FlipHorizontal;
    procedure FlipVertical;
  end;

  { A cache of loaded videos.

    The idea is that instead of creating TVideo instance and calling
    TVideo.LoadFromFile, you instead call
    @code(Video := Cache.Video_IncReference(...)).
    Later, instead of freeing this video, call
    @code(Video_DecReference(Video)). From your point of view, things
    will work the same. But if you expect to load many videos from the
    same URL, then you will get a great speed and memory saving,
    because video will only be actually loaded once. This may happen
    e.g. if you have a VRML / X3D file with lots of MovieTexture nodes
    with the same urls.

    Notes:

    @unorderedList(
      @item(All passed here URLs must be absolute.)

      @item(Note that in case of problems with loading,
        Video_IncReference may raise an exception, just like normal
        TVideo.LoadFromFile. In this case it's guaranteed that no reference will
        be incremented, of course. If Video_IncReference returns
        in a normal way, then it will return something non-@nil for sure.)

      @item(Video_DecReference alwas sets Video to @nil, like FreeAndNil.)

      @item(All videos handled here are always loaded.
        So Video_IncReference always returns TVideo with TVideo.Loaded = @true.
        And you are forbidden from closing this video by TVideo.Close
        yourself.)
    )

    Note that before destroying this object you must free all videos,
    i.e. call Video_DecReference for all videos allocated by
    Video_IncReference. @italic(This class is not a lousy way
    of avoiding memory leaks) --- it would be a bad idea, because it would
    cause sloppy programming, where memory is unnecessarily allocated for
    a long time. In fact, this class asserts in destructor that no videos
    are in cache anymore, so if you compiled with assertions enabled,
    this class does the job of memory-leak detector. }
  TVideosCache = class
  private
    type
      TCachedVideo = class
        References: Cardinal;
        URL: string;
        Video: TVideo;
        AlphaChannel: TAlphaChannel;
      end;
      TCachedVideoList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TCachedVideo>;
    var
      CachedVideos: TCachedVideoList;
      FOnEmpty: TProcedure;
  protected
    { If cache is empty, calls OnEmpty. Note that OnEmpty may destroy current
      instance, so call CheckEmpty only when you finished processing
      --- Self may be invalid afterwards. }
    procedure CheckEmpty;
  public
    constructor Create;
    destructor Destroy; override;

    function Video_IncReference(const URL: string; out AlphaChannel: TAlphaChannel): TVideo;
    procedure Video_DecReference(var Video: TVideo);

    function Empty: boolean; virtual;

    { Called when cache becomes empty. This is only for internal usage
      by X3DNodes unit for now. }
    property OnEmpty: TProcedure read FOnEmpty write FOnEmpty;
  end;

{ Does given MIME type looks like a video file extension
  that can be handled (encoded or decoded) by ffmpeg.

  FfmpegOutput = @true means that you want to encode this video
  (use this as output of ffmpeg). FfmpegOutput = @false means you
  want to decode the video, that is use this as an input to ffmpeg. }
function FfmpegVideoMimeType(const MimeType: string;
  const FfmpegOutput: boolean): boolean;

{ Returns full path to ffmpeg-compatible executable.

  When not found:
  @unorderedList(
    @item If ExceptionOnError then we raise an exception.
    @item If not ExceptionOnError then we simply return ''.
  ) }
function FfmpegExecutable(const ExceptionOnError: boolean): string;

{ Execute ffmpeg. 1st parameter must not be ''.
  It should usually be calculated by FfmpegExecutable. }
procedure FfmpegExecute(const Executable: string;
  const Parameters: array of string);

var
  { When @true, then we will load animated GIFs using ffmpeg.
    Otherwise, we load GIF using TCastleImage as a single (not animated) image.

    TODO: When the TFPReaderGif from FPC will support reading all GIF frames,
    then this variable will be ignored, and we will always read all GIF frames
    inside @link(TVideo), @link(TGLVideo2D), @link(TGLVideo3D),
    and ffmpeg will not be necessary to read GIF animations.

    Use @link(TCastleImage) if you want to always load GIF as static image
    (first frame, if case of animated GIF).
  }
  LoadAnimatedGifs: boolean = false;

  { Maximum number of video frames to read, for TVideo.LoadFromFile.

    This prevents using up all the memory by accidentaly trying to read
    a long movie. Remember that our current implementation is @bold(not)
    suited for long movies, it will load very slowly and consume a lot of memory.
    See https://castle-engine.io/x3d_implementation_texturing.php
    notes about MovieTexture.

    By default this is equal to 1 minute, for 25 frames-per-second movie.
    Even this can eat 2 GB for 640 x 350 resolution. }
  MaximumVideoLength: Cardinal = 1 * 60 * 25;

  { Log video cache events. Allows to see how the cache performs,
    and also how long it takes to load videos (useful, as some videos may
    be already lengthy).

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogVideosCache: boolean = false;

const
  DefaultFramesPerSecond = 25.0;

implementation

uses Classes, CastleClassUtils, CastleUtils, Math, CastleStringUtils,
  CastleLog, CastleFilesUtils, CastleProgress, CastleTextureImages,
  CastleDownload, CastleURIUtils, CastleFindFiles;

{ TVideo --------------------------------------------------------------------- }

constructor TVideo.Create;
begin
  inherited;
  FLoaded := false;
  { This is just constant for now }
  FFramesPerSecond := DefaultFramesPerSecond;
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

function TVideo.GetItems(Index: Integer): TCastleImage;
begin
  Assert(Loaded);
  Result := FItems[Index];
end;

class function TVideo.FrameIndexFromTime(const Time: TFloatTime;
  const ACount: Integer;
  const AFramesPerSecond: Single;
  const ATimeLoop, ATimeBackwards: boolean): Integer;
var
  DivResult: SmallInt;
  ModResult: Word;
begin
  Result := Floor(Time * AFramesPerSecond);

  DivUnsignedMod(Result, ACount, DivResult, ModResult);

  if ATimeLoop then
  begin
    if ATimeBackwards and Odd(DivResult) then
      Result := ACount - 1 - ModResult else
      Result := ModResult;
  end else
  begin
    if ATimeBackwards then
    begin
      if (DivResult < 0) or (DivResult > 1) then
        Result := 0 else
      if DivResult = 1 then
        Result := ACount - 1 - ModResult;
        { else DivResult = 0, so Result is already correct }
    end else
    begin
      if DivResult < 0 then
        Result := 0 else
      if DivResult > 0 then
        Result := ACount - 1;
    end;
  end;
end;

function TVideo.IndexFromTime(const Time: TFloatTime): Integer;
begin
  Assert(Loaded);
  Result := FrameIndexFromTime(Time, Count, FramesPerSecond,
    TimeLoop, TimeBackwards);
end;

function TVideo.ImageFromTime(const Time: TFloatTime): TCastleImage;
begin
  Result := FItems[IndexFromTime(Time)];
end;

function TVideo.TimeDuration: Single;
begin
  { Use Count, not High(FItems) (Count = High(FItems) + 1).
    Reason: we want the last frame to be treated as well as the others. }
  Result := Count / FramesPerSecond;
end;

procedure TVideo.Close;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FreeAndNil(FItems[I]);
  SetLength(FItems, 0);
  FLoaded := false;
end;

procedure TVideo.Resize(const ResizeToX, ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation = riBilinear);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Resize(ResizeToX, ResizeToY, Interpolation);
end;

procedure TVideo.LoadFromFile(const URL: string;
  const ResizeToX: Cardinal = 0;
  const ResizeToY: Cardinal = 0;
  const Interpolation: TResizeInterpolation = riBilinear);

  function LoadSingleImage(const URL: string): TCastleImage;
  begin
    Result := LoadImage(URL, TextureImageClasses,
      ResizeToX, ResizeToY, Interpolation);
  end;

  { Load from an image sequence (possibly just a single image).
    When RemoveLoadedTempImages, we will remove the loaded image files,
    in this case the URL @italic(must) be a filename. }
  procedure LoadFromImages(const URL: string;
    RemoveLoadedTempImages: boolean);

    { Load movie frame number Index. Returns if success. }
    function LoadFrame(const Index: Cardinal): boolean;
    var
      URLComplete: string;
      NewItem: TCastleImage;
    begin
      URLComplete := FormatNameCounter(URL, Index, false);

      try
        { LoadImage will raise an exception
          for invalid / not existing / not readable image file / url.
          Don't increase FItems before NewItem is successfully loaded. }
        NewItem := LoadSingleImage(URLComplete);
      except
        Exit(false);
      end;

      SetLength(FItems, Length(FItems) + 1);
      FItems[High(FItems)] := NewItem;

      if RemoveLoadedTempImages then
        CheckDeleteFile(URLComplete, true);

      Result := true;
    end;

  var
    Index, ReplacementsDone: Cardinal;
    SingleFrame: TCastleImage;
  begin
    FormatNameCounter(URL, 0, false, ReplacementsDone);
    if ReplacementsDone > 0 then
    begin
      { Try to load frame 0. Ignore failure (it means movie starts at frame 1). }
      LoadFrame(0);

      Index := 1;
      while LoadFrame(Index) and
            (Length(FItems) < MaximumVideoLength) do
        Inc(Index);

      if Length(FItems) = 0 then
        raise Exception.CreateFmt('First video image ("%s" or "%s") cannot be loaded (not found, or not supported image format --- in case of png make sure libpng is installed). Cannot load the video',
          [FormatNameCounter(URL, 0, false),
           FormatNameCounter(URL, 1, false)]);
    end else
    begin
      SingleFrame := LoadSingleImage(URL);
      SetLength(FItems, 1);
      FItems[0] := SingleFrame;

      { when RemoveLoadedTempImages, we know URL is a filename and can be safely
        passed to CheckDeleteFile }
      if RemoveLoadedTempImages then
        CheckDeleteFile(URL, true);
    end;
  end;

  { Load a single video file from an URL. }
  procedure LoadFromFfmpeg(const URL: string);
  var
    MovieFileName: string;
    MovieFileNameTemporary: boolean;
    TemporaryImagesPrefix, FfmpegTemporaryImagesPattern, OurTemporaryImagesPattern: string;
    Executable: string;
    S: TStream;
  begin
    Executable := FfmpegExecutable(true);

    { initialize TemporaryImagesPrefix, TemporaryImagesPattern }
    TemporaryImagesPrefix := GetTempFileNamePrefix;
    FfmpegTemporaryImagesPattern := TemporaryImagesPrefix + '%d.png';
    OurTemporaryImagesPattern := TemporaryImagesPrefix + '@counter(1).png';

    MovieFileName := URIToFilenameSafe(URL);
    MovieFileNameTemporary := false;

    { If URL isn't a local file (maybe it's http, maybe data URI) then
      download it ourselves to a temporary file.

      We don't let ffmpeg to download video. Although ffmpeg can handle
      http URLs, but then progress of download is not visible using
      our progress (OTOH, it is streamed and converted during downloading).
      Most important: ffmpeg can't handle data URI (we could not even
      pass such long strings on the command-line). }
    if MovieFileName = '' then
    begin
      MovieFileName := GetTempFileNameCheck;
      MovieFileNameTemporary := true;
      S := Download(URL);
      try
        StreamSaveToFile(S, MovieFileName);
      finally FreeAndNil(S) end;
    end;

    try
      FfmpegExecute(Executable, [ '-i', MovieFileName, '-y', '-qscale', '1',
        '-vframes', IntToStr(MaximumVideoLength),
        '-f', 'image2', FfmpegTemporaryImagesPattern ]);
    finally
      if MovieFileNameTemporary then
        CheckDeleteFile(MovieFileName, true);
    end;

    LoadFromImages(OurTemporaryImagesPattern, true);
  end;

begin
  Close;

  if FfmpegVideoMimeType(URIMimeType(URL), true) then
    LoadFromFfmpeg(URL) else
    LoadFromImages(URL, false);

  FLoaded := true;
end;

procedure TVideo.SaveToFile(const URL: string);

  procedure SaveToImages(const URL: string);
  var
    Index, ReplacementsDone: Cardinal;
    S: string;
  begin
    FormatNameCounter(URL, 0, true, ReplacementsDone);
    if ReplacementsDone > 0 then
    begin
      { Note that Index is 1-based, that's how FormatNameCounter
        works for LoadFromFile and ffmpeg, so also here. }
      for Index := 1 to Count do
      begin
        S := FormatNameCounter(URL, Index, true);
        SaveImage(FItems[Index - 1], S);
      end;
    end else
    begin
      { single image file --- suitable only if video has exactly one frame }
      if Count <> 1 then
        raise Exception.CreateFmt('Single image URL detected ("%s"), but saved video doesn''t have exactly one frame (it has %d frames). Cannot save',
          [URIDisplay(URL), Count]);

      SaveImage(FItems[0], URL);
    end;
  end;

  procedure RemoveTemporaryImages(const FileName: string);
  var
    Index, ReplacementsDone: Cardinal;
    S: string;
  begin
    WritelnLog('Removing temporary image files "' + FileName + '" ...');
    FormatNameCounter(FileName, 0, true, ReplacementsDone);
    if ReplacementsDone > 0 then
    begin
      for Index := 1 to Count do
      begin
        S := FormatNameCounter(FileName, Index, true);
        if not DeleteFile(S) then
          WritelnWarning('Video', Format('Cannot delete temporary file "%s"', [S]));
      end;
    end else
    begin
      if not DeleteFile(FileName) then
        WritelnWarning('Video', Format('Cannot delete temporary file "%s"', [FileName]));
    end;
    WritelnLog('Done removing temporary image files.');
  end;

  procedure SaveToFfmpeg(const FileName: string);
  var
    TemporaryImagesPrefix, TemporaryImagesPattern: string;
    Executable: string;
  begin
    Executable := FindExe('ffmpeg');

    if Executable = '' then
    begin
      raise Exception.Create('You must have "ffmpeg" program from ' +
        '[http://ffmpeg.mplayerhq.hu/] installed and available on $PATH to be able to ' +
        'save movie files');
    end else
    begin
      { initialize TemporaryImagesPrefix, TemporaryImagesPattern }
      TemporaryImagesPrefix := GetTempFileNamePrefix;
      TemporaryImagesPattern := TemporaryImagesPrefix + '%d.png';

      SaveToImages(TemporaryImagesPattern);

      { ffmpeg call will output some things on stdout anyway.
        So it's Ok that we also output something, stdout is required
        by ffmpeg anyway... }

      WritelnLog('FFMpeg found, executing...');
      WritelnLog(Executable + ' -f image2 -i "' + TemporaryImagesPattern +
        '" -y -qscale 1 "' + FileName + '"');

      ExecuteProcess(Executable,
        ['-f', 'image2', '-i', TemporaryImagesPattern, '-y', '-qscale', '1', FileName]);

      RemoveTemporaryImages(TemporaryImagesPattern);
    end;
  end;

begin
  Assert(Loaded);

  if FfmpegVideoMimeType(URIMimeType(URL), false) then
    SaveToFfmpeg(URL) else
    SaveToImages(URIToFilenameSafe(URL));
end;

function TVideo.Width: Cardinal;
begin
  Assert(Loaded);
  Result := Items[0].Width;
end;

function TVideo.Height: Cardinal;
begin
  Assert(Loaded);
  Result := Items[0].Height;
end;

procedure TVideo.MixWithSelfBackwards(const ProgressTitle: string);
var
  I, NewCount: Integer;
begin
  Assert(Loaded);

  if ProgressTitle <> '' then
    Progress.Init((Count div 2) * 2, ProgressTitle);
  try

    for I := 0 to Count div 2 do
    begin
      Items[I].LerpWith(0.5, Items[Count - 1 - I]);
      if ProgressTitle <> '' then Progress.Step;
    end;

    { shrink count to half }
    NewCount := (Count div 2) + (Count mod 2);
    for I := NewCount to Count - 1 do
    begin
      FreeAndNil(FItems[I]);
      if ProgressTitle <> '' then Progress.Step;
    end;
    SetLength(FItems, NewCount);

    TimeBackwards := true;
  finally
    if ProgressTitle <> '' then Progress.Fini;
  end;
end;

procedure TVideo.FadeWithSelf(FadeFrames: Cardinal;
  const ProgressTitle: string);
var
  I, NewCount: Integer;
begin
  Assert(Loaded);

  if FadeFrames > Cardinal(Count) div 2 then
    raise EInvalidFadeFrames.CreateFmt('FadeFrames for FadeWithSelf too large: are %d, but frames count div 2 = %s',
      [FadeFrames, Count div 2]);

  if ProgressTitle <> '' then
    Progress.Init(FadeFrames * 2, ProgressTitle);
  try
    for I := 0 to FadeFrames - 1 do
    begin
      Items[I].LerpWith(1 - I / FadeFrames, Items[Count - FadeFrames + I]);
      if ProgressTitle <> '' then Progress.Step;
    end;

    NewCount := Count - FadeFrames;
    for I := NewCount to Count - 1 do
    begin
      FreeAndNil(FItems[I]);
      if ProgressTitle <> '' then Progress.Step;
    end;
    SetLength(FItems, NewCount);

  finally
    if ProgressTitle <> '' then Progress.Fini;
  end;
end;

function TVideo.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
begin
  Assert(Loaded);
  Result := Items[0].AlphaChannel(AlphaTolerance);
end;

procedure TVideo.FlipHorizontal;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FlipHorizontal;
end;

procedure TVideo.FlipVertical;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FlipVertical;
end;

{ TVideosCache --------------------------------------------------------------- }

constructor TVideosCache.Create;
begin
  inherited;
  CachedVideos := TCachedVideoList.Create;
end;

destructor TVideosCache.Destroy;
begin
  if CachedVideos <> nil then
  begin
    Assert(CachedVideos.Count = 0, ' Some references to videos still exist ' +
      'when freeing TVideosCache');
    FreeAndNil(CachedVideos);
  end;
  inherited;
end;

function TVideosCache.Video_IncReference(const URL: string;
  out AlphaChannel: TAlphaChannel): TVideo;
var
  I: Integer;
  C: TCachedVideo;
  Start: TProcessTimerResult;
  S: string;
begin
  for I := 0 to CachedVideos.Count - 1 do
  begin
    C := CachedVideos[I];
    if C.URL = URL then
    begin
      Inc(C.References);
      AlphaChannel := C.AlphaChannel;

      if LogVideosCache and Log then
        WritelnLog('++', 'Video %s : %d', [URIDisplay(URL), C.References]);

      Exit(C.Video);
    end;
  end;

  { Initialize Result first, before calling CachedVideos.Add.
    That's because in case TVideo.LoadFromFile raises exception,
    we don't want to add video to cache (because caller would have
    no way to call Video_DecReference later). }

  if LogVideosCache and Log then
    Start := ProcessTimer;

  Result := TVideo.Create;
  try
    Result.LoadFromFile(URL);
  except
    FreeAndNil(Result);
    raise;
  end;

  C := TCachedVideo.Create;
  CachedVideos.Add(C);
  C.References := 1;
  C.URL := URL;
  C.Video := Result;
  C.AlphaChannel := Result.AlphaChannel;
  AlphaChannel := C.AlphaChannel;

  if LogVideosCache and Log then
  begin
    S := Format('Video %s : 1. Loading time: %f',
      [URIDisplay(URL), ProcessTimerSeconds(ProcessTimer, Start)]);
    if AlphaChannel <> acNone then
      S := S + '. Detected as simple yes/no ("test") alpha channel: ' +
           BoolToStr(AlphaChannel = acTest, true);
    WritelnLog('++', S);
  end;
end;

procedure TVideosCache.Video_DecReference(var Video: TVideo);
var
  I: Integer;
  C: TCachedVideo;
begin
  for I := 0 to CachedVideos.Count - 1 do
  begin
    C := CachedVideos[I];
    if C.Video = Video then
    begin
      if LogVideosCache and Log then
        WritelnLog('--', 'Video %s : %d', [C.URL, C.References - 1]);

      Video := nil;

      if C.References = 1 then
      begin
        FreeAndNil(C.Video);
        CachedVideos.Delete(I);
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVideosCache.Video_DecReference: no reference found for video %s',
    [PointerToStr(Video)]);
end;

function TVideosCache.Empty: boolean;
begin
  Result := (CachedVideos.Count = 0);
end;

procedure TVideosCache.CheckEmpty;
begin
  { Check Assigned(OnEmpty) first, as it's usually not assigned. }
  if Assigned(OnEmpty) and Empty then
    OnEmpty();
end;

{ non-object routines -------------------------------------------------------- }

function FfmpegVideoMimeType(const MimeType: string;
  const FfmpegOutput: boolean): boolean;
begin
  { For now we ignore FfmpegOutput, all formats below are good
    for both input and output. (Actually, MIME (always from file extension now)
    specifies only  "container" data type, not actual encoding,
    so there's no guarentee, but potentially it's all possible.)
    See "ffmpeg -formats". }
  Result :=
    (LoadAnimatedGifs and (MimeType = 'image/gif')) or
    (MimeType = 'video/x-msvideo') or
    (MimeType = 'video/mpeg') or
    (MimeType = 'video/ogg') or
    (MimeType = 'video/quicktime') or
    (MimeType = 'video/x-flv') or
    (MimeType = 'application/x-shockwave-flash') or
    (MimeType = 'video/mp4');
end;

function FfmpegExecutable(const ExceptionOnError: boolean): string;
const
  SFfmpegNotFound = 'You must have "ffmpeg" from [http://www.ffmpeg.org/] ' +
    '(or "avconv" from [http://www.libav.org/]) ' +
    'installed and available on $PATH to be able to load movie files';
begin
  Result := FindExe('ffmpeg');
  if Result = '' then
    Result := FindExe('avconv');
  if (Result = '') and ExceptionOnError then
    raise Exception.Create(SFfmpegNotFound);
end;

procedure FfmpegExecute(const Executable: string;
  const Parameters: array of string);
var
  S, Parameter: string;
begin
  WritelnLog('FFMpeg found, executing...');
  { Only for the sake of logging we glue Parameters together.
    For actual execution, we pass Parameters as an array, which is *much*
    safer (no need to worry whether Parameter contains " inside etc.) }
  S := Executable;
  for Parameter in Parameters do
  begin
    S := S + ' ';
    if Pos(' ', Parameter) <> 0 then
      S := S + '"' + Parameter + '"'
    else
      S := S + Parameter;
  end;
  WritelnLog(S);
  ExecuteProcess(Executable, Parameters);
end;

end.
