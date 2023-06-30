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

{ Searching, downloading models from Sketchfab.
  This does most of the actual work and is used by FormImportSketchfab. }
unit CastleSketchfab;

interface

uses
  { Using CThreads, as threads are required by TCastleDownload asynchronous version. }
  {$ifdef UNIX} CThreads, {$endif}
  SysUtils, Classes, Generics.Collections, FpJson, JsonParser, Zipper,
  { Enable https downloads. }
  {$ifndef VER3_0} OpenSslSockets, {$endif}
  { CGE units }
  CastleFilesUtils, CastleDownload, CastleStringUtils, CastleURIUtils, CastleLog,
  CastleUtils, CastleClassUtils, CastleApplicationProperties, CastleImages;

const
  { Sketchfab API returns thumbnails in various sizes,
    we will pick the closest one to this. }
  BestThumbnailSize = 256;

  { The thumbnails in TSketchfabModel.ThumbnailImage are resized to this size.
    This allows to display them nicely in LCL TListView (which requires
    constant size for all images, otherwise it stretched them in ugly way).

    The aspect ratio of ThumbnailOptimalWidth / ThumbnailOptimalHeight
    follows the aspect ratio of most (but not all)
    Sketchfab thumbnails, so they typically fill the desired area nicely
    (without excessive white space around). }
  ThumbnailOptimalWidth = BestThumbnailSize;
  ThumbnailOptimalHeight = 144;

type
  TSketchfabModel = class;

  TSketchfabModelList = {$ifdef FPC}specialize{$endif} TObjectList<TSketchfabModel>;

  { Notification when thumbnail is downloaded for given model.
    When this is called, Sender.ThumbnailImage is non-nil for sure. }
  TThumbnailDownloadedEvent = procedure (const Sender: TSketchfabModel) of object;

  TSketchfabModel = class
  private
    DownloadURL: String;
    ThumbnailDownload: TCastleDownload;
    FThumbnailImage: TCastleImage;
    FThumbnailImageError: Boolean;
    procedure ThumbnailDownloadFinish(const Sender: TCastleDownload; var FreeSender: Boolean);
  public
    ModelId: String;

    { Various additional info about model, set by @link(Search). }
    Name, Description: String;
    FaceCount: UInt64;
    ThumbnailUrl, ViewerUrl: String;
    License: String;

    { Model name with all characters potentially unsafe for filename
      removed/replaced + added model id as a suffix.
      The idea is that this is unique, just like ModelId,
      but it is also more recognizable for humans.

      Sketchfab URLs are actually similar,
      https://sketchfab.com/3d-models/anything-70a23788ef984a7a9a1c9a9fe6d5a651
      redirects to
      https://sketchfab.com/3d-models/cat-70a23788ef984a7a9a1c9a9fe6d5a651
      The end is just model id,
      the initial "cat" is only for humans. }
    ModelPrettyId: String;

    { Set to be notified when ThumbnailImage is downloaded successfully. }
    OnThumbnailDownloaded: TThumbnailDownloadedEvent;

    { When thumbnail image is downloaded, it is placed here right
      before calling @link(OnThumbnailDownload). }
    property ThumbnailImage: TCastleImage read FThumbnailImage;

    { If thumbnail download was attempted but failed. }
    property ThumbnailImageError: Boolean read FThumbnailImageError;

    { Search Sketchfab for Query, return list of model ids. }
    class function Search(const Query: String;
      const AnimatedOnly: Boolean): TSketchfabModelList;

    destructor Destroy; override;

    { Set Download* fields based on ModelId. }
    procedure StartDownload(const ApiToken: String);
    { Use Download* fields to get model zip. }
    procedure DownloadZip(const ZipFileName: String);
    { Extract zip to a subdirectory in ExtractBasePath. }
    procedure ExtractZip(const ZipFileName, ZipUnpackDir: String);

    { Start downloading thumbnail,
      to set @link(ThumbnailImage) and call @link(OnThumbnailDownloaded).
      Does nothing if download is already in progress.
      Calls @link(OnThumbnailDownloaded) immediately if thumbnail is already
      downloaded. }
    procedure StartThumbnailDownload;

    { If the thumbnail download is in progress, abort it.
      Sets ThumbnailDownloading to @false. }
    procedure AbortThumbnailDownload;

    { Are we currently downloading thumbnail.

      This is set to @true by StartThumbnailDownload.

      This changes to @false by

      - AbortThumbnailDownload

      - or when thumbnail is downloaded with success
        (then we set ThumbnailImage to non-nil and call OnThumbnailDownloaded)

      - or when thumbnail is downloaded with failure
        (then we set ThumbnailImageError to @true). }
    function ThumbnailDownloading: Boolean;
  end;

implementation

uses FormProgress,
  CastleVectors;

{ TSketchfabModel (class methods) --------------------------------------------- }

class function TSketchfabModel.Search(const Query: String;
  const AnimatedOnly: Boolean): TSketchfabModelList;

  { Find thumbnail URL best matching given Size, in a JSON array
    as returned by Sketchfab search query.
    Returns '' if no thumbnail found. }
  function SearchThumbnails(const Thumbnails: TJSONArray; const DesiredSize: Integer): String;
  var
    I: Integer;
    Thumbnail: TJSONObject;
    ThumbnailSize: Integer;
    BestThumbnail: TJSONObject;
    BestThumbnailSize: Integer;
  begin
    BestThumbnail := nil;
    BestThumbnailSize := 0;
    for I := 0 to Thumbnails.Count - 1 do
    begin
      Thumbnail := Thumbnails[I] as TJSONObject;
      // average width and height
      ThumbnailSize := (Thumbnail.Integers['width'] + Thumbnail.Integers['height']) div 2;
      if (BestThumbnail = nil) or
         ( Abs(ThumbnailSize - DesiredSize) <
           Abs(BestThumbnailSize - DesiredSize) ) then
      begin
        BestThumbnail := Thumbnail;
        BestThumbnailSize := ThumbnailSize;
      end;
    end;
    if BestThumbnail <> nil then
    begin
      Result := BestThumbnail.Strings['url'];
      // WritelnLog('Best thumbnail dimensions: %d x %d, url: %s', [
      //   BestThumbnail.Integers['width'],
      //   BestThumbnail.Integers['height'],
      //   Result
      // ]);
    end else
      Result := '';
  end;

  function URICleanFilename(const S: String): String;
  const
    ValidFilenameChars = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.'];
  begin
    Result := LowerCase(S);
    Result := SReplaceChars(Result, AllChars - ValidFilenameChars, '-');
  end;

  function SRemoveConsecuitive(const S: String; const C: Char): String;
  var
    SB: TStringBuilder;
    I: Integer;
  begin
    SB := TStringBuilder.Create;
    try
      SB.Append(S[1]);
      for I := 2 to Length(S) do
        if (S[I] <> C) or (S[I - 1] <> C) then
          SB.Append(S[I]);
      Result := SB.ToString;
    finally FreeAndNil(SB) end;
  end;

var
  Download: TCastleDownload;
  Response: String;
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
  Model: TSketchfabModel;
  SearchUrl: String;
begin
  Result := TSketchfabModelList.Create(true);
  Download := TCastleDownload.Create(nil);
  try
    { See https://docs.sketchfab.com/data-api/v3/index.html#!/search/get_v3_search_type_models
      for documentation of this API.
      Notes:

      - The default "sort_by" is by relevance, which is just like in Sketchfab UI
        on the website. No need to tweak this default.

      - There is "file_format" API parameter but it is unclear should I specify
        there glTF somehow.
        It seems it allows to filter by source formats (like .blend, obj)
        not by output formats. https://sketchfab.com/developers/download-api
        says """Models are available in glTF, GLB, and USDZ formats.
        Models are not currently available in their source formats such
        as FBX and OBJ through the API.""

        In all practical experiments, models we found have glTF versions.

      - There is "pbr_type" and it is tempting to use it, as CGE doesn't
        support fully Specular-Glossiness PBR workflow yet.
        (And we're reluctant to support it, as it is deprecated in glTF 2.0.)
        However, it seems we cannot tell there "unlit or metallic-roughness".
    }
    SearchUrl := 'https://api.sketchfab.com/v3/search?type=models&downloadable=true';
    if AnimatedOnly then
      SearchUrl += '&animated=true';
    SearchUrl += '&q=' + InternalUriEscape(Query);
    Download.Url := SearchUrl;

    Download.Start;
    TProgressForm.WaitFor('Searching Sketchfab', Download);
    Response := StreamToString(Download.Contents);

    JSONData := GetJSON(Response);

    //WritelnLog('Got response, storing in response-search.json (for debug)');
    //StringToFile('response-search.json', Response);

    if JSONData.JSONType = jtObject then
    begin
      JSONArray := TJSONObject(JSONData).Arrays['results'];
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONObject := JSONArray.Objects[I];

        // sanity check
        if not JSONObject.Booleans['isDownloadable'] then
        begin
          WritelnWarning('Model %s not downloadable, even though we requested only downloadable');
          Continue;
        end;

        Model := TSketchfabModel.Create;
        Model.ModelId := JSONObject.Strings['uid'];
        Model.Name := JSONObject.Strings['name'];
        Model.Description := JSONObject.Strings['description'];
        Model.FaceCount := JSONObject.QWords['faceCount'];
        Model.ThumbnailUrl := SearchThumbnails(JSONObject.Objects['thumbnails'].Arrays['images'], BestThumbnailSize);
        Model.License := JSONObject.Objects['license'].Strings['label'];
        Model.ViewerUrl := JSONObject.Strings['viewerUrl'];
        Model.ModelPrettyId := SRemoveConsecuitive(
          URICleanFilename(Model.Name) + '-' + Model.ModelId, '-');
        Result.Add(Model);
      end;
    end else
      raise Exception.Create('Unexpected JSON response: ' + Response);
  finally
    FreeAndNil(Download);
  end;
end;

{ TSketchfabModel (instance methods) ----------------------------------------- }

destructor TSketchfabModel.Destroy;
begin
  FreeAndNil(ThumbnailDownload);
  FreeAndNil(FThumbnailImage);
  inherited;
end;

procedure TSketchfabModel.StartDownload(const ApiToken: String);
const
  ApiUrl = 'https://api.sketchfab.com/v3/models';
var
  Download: TCastleDownload;
  Response: String;
  JSONData: TJSONData;
  DownloadSize: Int64;
begin
  Download := TCastleDownload.Create(nil);
  try
    WritelnLog('Starting download of model id ' + ModelId);
    Download.HttpHeader('Authorization', 'Token ' + ApiToken);
    Download.Url := ApiUrl + '/' + ModelId + '/download';
    Download.Start;
    TProgressForm.WaitFor('Starting download of model', Download);
    Response := StreamToString(Download.Contents);
  finally
    FreeAndNil(Download);
  end;

  //WritelnLog('Got response, storing in response-download.json (for debug)');
  //StringToFile('response-download.json', Response);

  JSONData := GetJSON(Response);
  try
    DownloadURL := JSONData.FindPath('gltf.url').AsString;
    DownloadSize := JSONData.FindPath('gltf.size').AsInt64;
  finally
    FreeAndNil(JSONData);
  end;

  WritelnLog('Downloading model from: '+ DownloadURL);
  WritelnLog('Download size: ' + SizeToStr(DownloadSize));
end;

procedure TSketchfabModel.DownloadZip(const ZipFileName: String);
var
  Download: TCastleDownload;
begin
  Download := TCastleDownload.Create(nil);
  try
    Download.Url := DownloadURL;
    Download.Start;
    TProgressForm.WaitFor('Downloading model', Download);
    StreamSaveToFile(Download.Contents, ZipFileName);
    WritelnLog('Model downloaded to: %s, file size: %s', [
      ZipFileName,
      SizeToStr(Download.Contents.Size)
    ]);
  finally
    FreeAndNil(Download);
  end;
end;

procedure TSketchfabModel.ExtractZip(const ZipFileName, ZipUnpackDir: String);
var
  Zip: TUnZipper;
begin
  { Unzip to given directory. }
  if DirectoryExists(ZipUnpackDir) then
    RemoveNonEmptyDir(ZipUnpackDir, true);
  ForceDirectories(ZipUnpackDir);

  Zip := TUnZipper.Create;
  try
    Zip.FileName := ZipFileName;
    Zip.OutputPath := ZipUnpackDir;
    Zip.Examine;
    Zip.UnZipAllFiles;
  finally
    FreeAndNil(Zip);
  end;

  WritelnLog('Model extracted to: ' + ZipUnpackDir);
end;

procedure TSketchfabModel.StartThumbnailDownload;
begin
  if not ThumbnailDownloading then
  begin
    if ThumbnailImage <> nil then
      OnThumbnailDownloaded(Self)
    else
    begin
      ThumbnailDownload := TCastleDownload.Create(nil);
      ThumbnailDownload.Url := ThumbnailUrl;
      ThumbnailDownload.OnFinish := @ThumbnailDownloadFinish;
      ThumbnailDownload.Start;
    end;
  end;
end;

procedure TSketchfabModel.AbortThumbnailDownload;
begin
  FreeAndNil(ThumbnailDownload);
end;

function TSketchfabModel.ThumbnailDownloading: Boolean;
begin
  Result := ThumbnailDownload <> nil;
end;

{ Make Image size exactly W x H.

  Reason: Sketchfab thumbnails have various aspect ratios (some are square,
  some are 16:9...),
  also they are not guaranteed to match BestThumbnailSize even in one dimension.
  And we need them to be exactly ThumbnailOptimalWidth x ThumbnailOptimalHeight,
  as LCL TListView rendering will assume that, otherwise images are stretched
  in ugly way. }
procedure FixImageSize(var Image: TCastleImage; const W, H: Integer);
var
  NewW, NewH: Integer;
  NewImage: TRGBImage;
begin
  { Make at least one side equal to desired W or H. }
  if Image.Width / Image.Height > W / H then
  begin
    NewW := W;
    NewH := Round(W * Image.Height / Image.Width);
  end else
  begin
    NewW := Round(H * Image.Width / Image.Height);
    NewH := H;
  end;

  { Note: TCastleImage.Resize does nothing when NewW/H match current size.
    So we don't check it here. }
  Image.Resize(NewW, NewH);

  { Too small? Then place in the center of white square. }
  if (NewW < W) or (NewH < H) then
  begin
    NewImage := TRGBImage.Create(W, H);
    NewImage.Clear(Vector4Byte(255, 255, 255, 255));
    NewImage.DrawFrom(Image, (W - NewW) div 2, (H - NewH) div 2, dmOverwrite);
    FreeAndNil(Image);
    Image := NewImage;
  end;
end;

procedure TSketchfabModel.ThumbnailDownloadFinish(
  const Sender: TCastleDownload; var FreeSender: Boolean);
begin
  Assert(FThumbnailImage = nil);
  Assert(Sender = ThumbnailDownload);
  ThumbnailDownload := nil;

  if Sender.Status = dsSuccess then
  begin
    FThumbnailImage := LoadImage(Sender.Contents, Sender.MimeType, []);
    FixImageSize(FThumbnailImage, ThumbnailOptimalWidth, ThumbnailOptimalHeight);
    if Assigned(OnThumbnailDownloaded) then
      OnThumbnailDownloaded(Self);
  end else
  begin
    FThumbnailImageError := true;
    WritelnWarning('Failed to download thumbnail for "%s" from Sketchfab: "%s"', [
      Name,
      Sender.ErrorMessage
    ]);
  end;
  FreeSender := true;
end;

end.
