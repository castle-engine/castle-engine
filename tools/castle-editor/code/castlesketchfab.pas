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
  CastleUtils, CastleClassUtils, CastleApplicationProperties;

type
  TSketchfabModel = class;

  TSketchfabModelList = {$ifdef FPC}specialize{$endif} TObjectList<TSketchfabModel>;

  TSketchfabModel = class
  private
    DownloadURL: String;
  public
    ModelId: String;

    { Various additional info about model,
      set by @link(Search) and @link(SearchGetFirst).
      Not used in this application, but will be useful for UI in CGE. }
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

    { Search Sketchfab for Query, return list of model ids. }
    class function Search(const Query: String): TSketchfabModelList;

    { Set Download* fields based on ModelId. }
    procedure StartDownload(const ApiToken: String);
    { Use Download* fields to get model zip. }
    procedure DownloadZip(const ZipFileName: String);
    { Extract zip to a subdirectory in ExtractBasePath. }
    procedure ExtractZip(const ZipFileName, ZipUnpackDir: String);
  end;

implementation

uses FormProgress;

const
  { Sketchfab API returns thumbnails in various sizes,
    we will pick the closest one to this. }
  BestThumbnailSize = 256;

{ TSketchfabModel (class methods) --------------------------------------------- }

class function TSketchfabModel.Search(const Query: String): TSketchfabModelList;

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

  const
    ValidFilenameChars = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.'];
  begin
    Result := LowerCase(S);
    Result := SReplaceChars(Result, AllChars - ValidFilenameChars, '_');
    Result := SRemoveConsecuitive(Result, '_');
  end;

var
  Download: TCastleDownload;
  Response: String;
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
  Model: TSketchfabModel;
begin
  Result := TSketchfabModelList.Create(true);
  Download := TCastleDownload.Create(nil);
  try
    Download.Url := 'https://api.sketchfab.com/v3/search?type=models&downloadable=true&q=' + InternalUriEscape(Query);
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
        Model.ModelPrettyId := URICleanFilename(Model.Name) + '-' + Model.ModelId;
        Result.Add(Model);
      end;
    end else
      raise Exception.Create('Unexpected JSON response: ' + Response);
  finally
    FreeAndNil(Download);
  end;
end;

{ TSketchfabModel (instance methods) ----------------------------------------- }

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

end.
