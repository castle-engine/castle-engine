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
  private
    Models: TSketchfabModelList;
  public
    ProjectPath: String;
    procedure UpdateEnabled;
  end;

var
  ImportSketchfabForm: TImportSketchfabForm;

implementation

uses CastleOpenDocument, CastleStringUtils, CastleConfig;

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

procedure TImportSketchfabForm.UpdateEnabled;
begin
  ButtonViewSketchfab.Enabled := ListModels.Selected <> nil;
  ButtonDownloadAndAddViewport.Enabled := ListModels.Selected <> nil;
  ButtonDownloadOnly.Enabled := ListModels.Selected <> nil;
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

procedure TImportSketchfabForm.ButtonDownloadOnlyClick(Sender: TObject);
var
  Model: TSketchfabModel;
  BasePath, ZipFileName: String;
begin
  if Trim(EditApiToken.Text) = '' then
    raise Exception.Create('You must provide Sketchfab API token to download');

  Model := TObject(ListModels.Selected.Data) as TSketchfabModel;
  Model.StartDownload(EditApiToken.Text);

  BasePath := ProjectPath + 'data' + PathDelim + 'sketchfab' + PathDelim;
  ForceDirectories(BasePath);
  ZipFileName := BasePath + Model.ModelId + '.zip';
  Model.DownloadZip(ZipFileName);
  Model.ExtractZip(ZipFileName, BasePath);

  Close;
end;

procedure TImportSketchfabForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TImportSketchfabForm.ButtonDownloadAndAddViewportClick(Sender: TObject);
begin
  ButtonDownloadOnlyClick(ButtonDownloadOnly);
  // TODO
end;

end.

