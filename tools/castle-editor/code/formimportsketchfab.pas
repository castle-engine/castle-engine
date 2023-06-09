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

{ Form to search and download models from Sketchfab to add them to application data,
  and optionally to the viewport in current design. }
unit FormImportSketchfab;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons,
  FrameDesign,
  CastleSketchfab, CastleImages;

type
  TCanAddImported = function (const AddUrl: String): Boolean of object;
  TAddImported = procedure (const AddUrl: String) of object;

  { Form to import Sketchfab models. }
  TImportSketchfabForm = class(TForm)
    ButtonDownloadAndAddViewport: TButton;
    ButtonDownloadOnly: TButton;
    ButtonGrid: TSpeedButton;
    ButtonList: TSpeedButton;
    ButtonSearch: TButton;
    ButtonTokenUrl: TButton;
    ButtonViewSketchfab: TButton;
    ButtonClose: TButton;
    CheckBoxAnimated: TCheckBox;
    EditApiToken: TEdit;
    EditQuery: TEdit;
    Label1: TLabel;
    LabelDocs: TLabel;
    LabelQuery: TLabel;
    ListModels: TListView;
    Timer1: TTimer;
    ImageListModelThumbnails: TImageList;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDownloadAndAddViewportClick(Sender: TObject);
    procedure ButtonDownloadOnlyClick(Sender: TObject);
    procedure ButtonGridClick(Sender: TObject);
    procedure ButtonListClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonTokenUrlClick(Sender: TObject);
    procedure ButtonViewSketchfabClick(Sender: TObject);
    procedure CheckBoxAnimatedChange(Sender: TObject);
    procedure EditQueryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelDocsClick(Sender: TObject);
    procedure ListModelsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    Models: TSketchfabModelList;

    { Download currently selected model, raise exception if cannot.
      Returns URL on disk of the downloaded model. }
    function DownloadSelectedModel: String;

    { Update the displayed thumbnail of the given model.
      This is called when thumbnail is downloaded. }
    procedure ThumbnailDownloaded(const Model: TSketchfabModel);
  public
    ProjectPath: String;
    OnCanAddImported: TCanAddImported;
    OnAddImported: TAddImported;
    OnRefreshFiles: TNotifyEvent;
    procedure UpdateEnabled;
  end;

var
  ImportSketchfabForm: TImportSketchfabForm;

implementation

uses LCLType, IntfGraphics, GraphType, FpImage,
  CastleOpenDocument, CastleStringUtils, CastleConfig, CastleUtils,
  CastleURIUtils, CastleLog, CastleApplicationProperties,
  EditorUtils;

{$R *.lfm}

{ TImportSketchfabForm ------------------------------------------------------- }

procedure TImportSketchfabForm.ButtonTokenUrlClick(Sender: TObject);
begin
  OpenUrl('https://sketchfab.com/settings/password');
end;

procedure TImportSketchfabForm.ButtonViewSketchfabClick(Sender: TObject);
var
  Model: TSketchfabModel;
begin
  Model := TObject(ListModels.Selected.Data) as TSketchfabModel;
  OpenUrl(Model.ViewerUrl);
end;

procedure TImportSketchfabForm.CheckBoxAnimatedChange(Sender: TObject);
begin
  { None of the conditions below seem perfect.
    Just refresh always, as Sketchfab website also does it like this. }
  //if (ListModels.Items.Count <> 0) and (EditQuery.Text <> '') then
    ButtonSearchClick(ButtonSearch);
end;

procedure TImportSketchfabForm.EditQueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ButtonSearchClick(ButtonSearch);
    Key := 0; // handled
  end;
end;

procedure TImportSketchfabForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  { Do not free at Close anymore, keep data (like query and last search results).
    This is more comfortable.
  CloseAction := caFree;
  ImportSketchfabForm := nil; // do not leave dangling pointer
  }
end;

procedure TImportSketchfabForm.FormCreate(Sender: TObject);
begin
  ImageListModelThumbnails.Width := ThumbnailOptimalWidth;
  ImageListModelThumbnails.Height := ThumbnailOptimalHeight;
end;

procedure TImportSketchfabForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Models);
end;

procedure TImportSketchfabForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
  EditApiToken.Text := UserConfig.GetValue('sketchfab/api_token', '');
  UpdateEnabled;

  { Make ListModels fill as much as it can.
    Otherwise bottom things (EditApiToken and lower) are anchored to form bottom,
    rest is anchored to form top, and the gap between
    ListModels and "bottom things" can get larger on different font scaling values
    on user's desktop. }
  ListModels.Height := EditApiToken.Top - 8 - ListModels.Top;
end;

procedure TImportSketchfabForm.LabelDocsClick(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io/sketchfab');
end;

procedure TImportSketchfabForm.FormHide(Sender: TObject);
var
  M: TSketchfabModel;
begin
  UserConfig.SetDeleteValue('sketchfab/api_token', EditApiToken.Text, '');
  Timer1.Enabled := false;
  // do not use resources to download thumbnails, when window not visible
  if Models <> nil then
    for M in Models do
      M.AbortThumbnailDownload;
end;

procedure TImportSketchfabForm.ListModelsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateEnabled;
end;

procedure TImportSketchfabForm.Timer1Timer(Sender: TObject);
const
  MaxThumbnailDownloads = 3;
var
  M: TSketchfabModel;
  ThumbnailDownloadsCount: Integer;
begin
  { Keep calling UpdateEnabled as OnCanAddImported can change when user
    closes / opens design. }
  UpdateEnabled;

  if Models <> nil then
  begin
    // to process downloading thumbnails
    ApplicationProperties._Update;

    { Calculate ThumbnailDownloadsCount.
      It's simplest to let TSketchfabModel manage downloading internally,
      and only monitor it this way, so we don't manage here any list
      of "current downloads", instead we just iterate over all models
      in each timer call. }
    ThumbnailDownloadsCount := 0;
    for M in Models do
      if M.ThumbnailDownloading then
        Inc(ThumbnailDownloadsCount);

    if ThumbnailDownloadsCount < MaxThumbnailDownloads then
      for M in Models do
      begin
        if (M.ThumbnailImage = nil) and
           (not M.ThumbnailDownloading) and
           (not M.ThumbnailImageError) then
        begin
          WritelnLog('Sketchfab', 'Starting downloading thumbnail for model: ' + M.Name);
          M.OnThumbnailDownloaded := @ThumbnailDownloaded;
          M.StartThumbnailDownload;
          // add one download in one timer call
          Exit;
        end;
      end;
  end;
end;

procedure TImportSketchfabForm.ThumbnailDownloaded(const Model: TSketchfabModel);

  { Add given image to ImageListModelThumbnails and return the index. }
  function AddImageToList(const Image: TCastleImage): Integer;
  var
    IntfImage: TLazIntfImage;
    RawImage: TRawImage;
    CustomFPImage: TFPCustomImage;
    Bitmap: TBitmap;
  begin
    RawImage.Init;
    { TListView on WIN32 widgetset need RGB image }
    {$ifdef LCLWIN32}
    RawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(Image.Width, Image.Height);
    {$else}
    RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Image.Width, Image.Height);
    {$endif}
    RawImage.CreateData(True);

    IntfImage := TLazIntfImage.Create(0, 0);
    IntfImage.SetRawImage(RawImage);
    CustomFPImage := Image.ToFpImage;
    try
      IntfImage.CopyPixels(CustomFPImage);
    finally
      FreeAndNil(CustomFPImage);
    end;

    Bitmap := TBitmap.Create;
    Bitmap.LoadFromIntfImage(IntfImage);
    Result := ImageListModelThumbnails.Add(Bitmap, nil);
  end;

var
  ModelIndex: Integer;
begin
  ModelIndex := Models.IndexOf(Model);
  if ModelIndex = -1 then
  begin
    WritelnWarning('Downloaded thumbnail but model instance no longer on list');
    Exit;
  end;

  Assert(Model.ThumbnailImage <> nil);
  WritelnLog('Sketchfab', Format('Downloaded thumbnail for model "%s"', [Model.Name]));
  ListModels.Items[ModelIndex].ImageIndex := AddImageToList(Model.ThumbnailImage);

  { Show new images in the list. }
  if ListModels.ViewStyle = vsIcon then
  begin
    //ListModels.Refresh;

    // a bit of a hack, but it's the only way I found to refresh images
    ListModels.BeginUpdate;
    ListModels.ViewStyle := vsReport;
    ListModels.ViewStyle := vsIcon;
    ListModels.EndUpdate;
  end;
end;

procedure TImportSketchfabForm.UpdateEnabled;
begin
  ButtonViewSketchfab.Enabled := ListModels.Selected <> nil;
  ButtonDownloadOnly.Enabled := ListModels.Selected <> nil;
  ButtonDownloadAndAddViewport.Enabled := (ListModels.Selected <> nil) and
    Assigned(OnAddImported) and
    Assigned(OnCanAddImported) and
    OnCanAddImported('.gltf');
end;

procedure TImportSketchfabForm.ButtonSearchClick(Sender: TObject);

  function DescriptionCut(const OriginalDescription: String): String;
  const
    MaxLen = 80;
  var
    NewlinePos: Integer;
  begin
    Result := OriginalDescription;
    NewlinePos := CharsPos([#10, #13], Result);
    if NewlinePos <> 0 then
      Result := Copy(Result, 1, NewlinePos - 1);
  end;

var
  M: TSketchfabModel;
  ListItem: TListItem;
begin
  ListModels.Items.Clear;
  FreeAndNil(Models);
  ImageListModelThumbnails.Clear;

  Models := TSketchfabModel.Search(EditQuery.Text, CheckBoxAnimated.Checked);
  for M in Models do
  begin
    ListItem := ListModels.Items.Add;
    ListItem.Data := M;
    ListItem.Caption := M.Name;
    ListItem.SubItems.Append(IntToStr(M.FaceCount));
    ListItem.SubItems.Append(M.License);
    ListItem.SubItems.Append(DescriptionCut(M.Description));
  end;

  UpdateEnabled;
end;

function TImportSketchfabForm.DownloadSelectedModel: String;
var
  Model: TSketchfabModel;
  BasePath, ZipFileName, ZipUnpackDir, ModelFileName: String;
begin
  if Trim(EditApiToken.Text) = '' then
    raise Exception.Create('You must provide Sketchfab API token to download');

  Model := TObject(ListModels.Selected.Data) as TSketchfabModel;
  Model.StartDownload(EditApiToken.Text);

  BasePath := ProjectPath + 'data' + PathDelim + 'sketchfab' + PathDelim;
  ForceDirectories(BasePath);
  ZipFileName := BasePath + Model.ModelPrettyId + '.zip';
  ZipUnpackDir := BasePath + Model.ModelPrettyId;
  Model.DownloadZip(ZipFileName);
  Model.ExtractZip(ZipFileName, ZipUnpackDir);

  OnRefreshFiles(Self);

  ModelFileName := InclPathDelim(ZipUnpackDir) + 'scene.gltf';
  Result := MaybeUseDataProtocol(FilenameToURISafe(ModelFileName));
end;

procedure TImportSketchfabForm.ButtonDownloadOnlyClick(Sender: TObject);
var
  DownloadedUrl: String;
begin
  DownloadedUrl := DownloadSelectedModel;
  InfoBox('Model downloaded to:' + NL + NL + URIDisplay(DownloadedUrl));
  Close;
end;

procedure TImportSketchfabForm.ButtonGridClick(Sender: TObject);
begin
  ListModels.ViewStyle := vsIcon;
  ButtonGrid.Down := true; // ButtonList.Down will be set to false automatically
end;

procedure TImportSketchfabForm.ButtonListClick(Sender: TObject);
begin
  //try
    ListModels.ViewStyle := vsReport;
  (*
  // On LCL GTK2, switching between list/grid repeatedly unfortunately may crash.
  // Debugging, it is in GTK2 code.
  // We tried workarounding it as below, but this is not effective,
  // there will be more crashes.

  except
    on EAccessViolation do
      WritelnWarning('Hiding crash as ListModels.ViewStyle switch, possible with LCL on GTK2 if you switch between grid/list repeatedly');
  end;
  *)
  ButtonList.Down := true; // ButtonGrid.Down will be set to false automatically
end;

procedure TImportSketchfabForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TImportSketchfabForm.ButtonDownloadAndAddViewportClick(Sender: TObject);
var
  DownloadedUrl: String;
begin
  DownloadedUrl := DownloadSelectedModel;
  OnAddImported(DownloadedUrl);
  Close;
end;

end.

