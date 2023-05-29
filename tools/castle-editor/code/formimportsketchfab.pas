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
  ComCtrls, FrameDesign, CastleSketchfab;

type
  TCanAddImported = function (const AddUrl: String): Boolean of object;
  TAddImported = procedure (const AddUrl: String) of object;

  TImportSketchfabForm = class(TForm)
    ButtonDownloadAndAddViewport: TButton;
    ButtonDownloadOnly: TButton;
    ButtonTokenUrl: TButton;
    ButtonViewSketchfab: TButton;
    ButtonClose: TButton;
    ButtonSearch: TButton;
    EditApiToken: TLabeledEdit;
    EditQuery: TLabeledEdit;
    ListModels: TListView;
    PanelList: TPanel;
    PanelSearch: TPanel;
    Timer1: TTimer;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDownloadAndAddViewportClick(Sender: TObject);
    procedure ButtonDownloadOnlyClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonTokenUrlClick(Sender: TObject);
    procedure ButtonViewSketchfabClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListModelsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    Models: TSketchfabModelList;
    { Download currently selected model, raise exception if cannot.
      Returns URL on disk of the downloaded model. }
    function DownloadSelectedModel: String;
  public
    ProjectPath: String;
    OnCanAddImported: TCanAddImported;
    OnAddImported: TAddImported;
    procedure UpdateEnabled;
  end;

var
  ImportSketchfabForm: TImportSketchfabForm;

implementation

uses CastleOpenDocument, CastleStringUtils, CastleConfig, CastleUtils,
  CastleURIUtils;

{$R *.lfm}

{ TImportSketchfabForm }

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

procedure TImportSketchfabForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  ImportSketchfabForm := nil; // do not leave dangling pointer
end;

procedure TImportSketchfabForm.FormShow(Sender: TObject);
begin
  EditApiToken.Text := UserConfig.GetValue('sketchfab/api_token', '');
  UpdateEnabled;
end;

procedure TImportSketchfabForm.FormHide(Sender: TObject);
begin
  FreeAndNil(Models);
  ListModels.Items.Clear;
  UserConfig.SetDeleteValue('sketchfab/api_token', EditApiToken.Text, '');
end;

procedure TImportSketchfabForm.ListModelsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateEnabled;
end;

procedure TImportSketchfabForm.Timer1Timer(Sender: TObject);
begin
  { Keep calling UpdateEnabled as OnCanAddImported can change when user
    closes / opens design. }
  UpdateEnabled;
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

  Models := TSketchfabModel.Search(EditQuery.Text);
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

  ModelFileName := InclPathDelim(ZipUnpackDir) + 'scene.gltf';
  Result := MaybeUseDataProtocol(FilenameToURISafe(ModelFileName));
end;

procedure TImportSketchfabForm.ButtonDownloadOnlyClick(Sender: TObject);
begin
  DownloadSelectedModel;
  Close;
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

