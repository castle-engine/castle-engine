unit FormSpriteSheetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, ActnList, StdCtrls, Spin, Menus, CastleSpriteSheet, CastleDialogs,
  DataModuleIcons;

type
  TSpriteSheetEditorForm = class(TForm)
    ActionSaveSpriteSheetAs: TAction;
    ActionSaveSpriteSheet: TAction;
    ActionNewSpriteSheet: TAction;
    ActionOpenSpriteSheet: TAction;
    ActionListSpriteSheet: TActionList;
    ImageListFrames: TImageList;
    OpenDialog: TCastleOpenDialog;
    SaveDialog: TCastleSaveDialog;
    FloatSpinEditFPS: TFloatSpinEdit;
    LabelPreview: TLabel;
    LabelFrames: TLabel;
    LabelAnimations: TLabel;
    LabelFPS: TLabel;
    ListBoxAnimations: TListBox;
    ListViewFrames: TListView;
    MainMenu: TMainMenu;
    MenuItemNew: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemFile: TMenuItem;
    PanelFPS: TPanel;
    PanelRight: TPanel;
    PanelMiddle: TPanel;
    PanelTop: TPanel;
    PanelLeft: TPanel;
    SpeedButtonOpenSpriteSheet: TSpeedButton;
    SpeedButtonNewSpriteSheet: TSpeedButton;
    SpeedButtonAddAnimation: TSpeedButton;
    SpeedButtonSaveSpriteSheet: TSpeedButton;
    SpeedButtonRemoveAnimation: TSpeedButton;
    SplitterRight: TSplitter;
    SplitterLeft: TSplitter;
    procedure ActionNewSpriteSheetExecute(Sender: TObject);
    procedure ActionOpenSpriteSheetExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxAnimationsSelectionChange(Sender: TObject; User: boolean);
  private
    FSpriteSheet: TCastleSpriteSheet;

    procedure CloseSpriteSheet;
    procedure ClearAnimations;
    procedure LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
    procedure ClearFrames;
    procedure LoadFrames(const Animation: TCastleSpriteSheetAnimation);
  public
    procedure OpenSpriteSheet(const URL: String);
  end;

var
  SpriteSheetEditorForm: TSpriteSheetEditorForm;

implementation

{$R *.lfm}

uses EditorUtils;

{ TSpriteSheetEditorForm }

procedure TSpriteSheetEditorForm.ActionNewSpriteSheetExecute(Sender: TObject);
begin
  ShowMessage('test');
end;

procedure TSpriteSheetEditorForm.ActionOpenSpriteSheetExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenSpriteSheet(OpenDialog.URL);
end;

procedure TSpriteSheetEditorForm.FormCreate(Sender: TObject);
begin
  FSpriteSheet := nil;
end;

procedure TSpriteSheetEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpriteSheet);
end;

procedure TSpriteSheetEditorForm.ListBoxAnimationsSelectionChange(
  Sender: TObject; User: boolean);
begin
  ClearFrames;

  if ListBoxAnimations.ItemIndex > -1 then
  begin
    LoadFrames(ListBoxAnimations.Items.Objects[ListBoxAnimations.ItemIndex] as TCastleSpriteSheetAnimation);
  end;
end;

procedure TSpriteSheetEditorForm.CloseSpriteSheet;
begin
  // TODO: ask for save
  ClearFrames;
  ClearAnimations;
  FreeAndNil(FSpriteSheet);
end;

procedure TSpriteSheetEditorForm.LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
var
  I: Integer;
  Animation: TCastleSpriteSheetAnimation;
begin
  for I := 0 to SpriteSheet.AnimationCount - 1 do
  begin
    Animation := SpriteSheet.AnimationByIndex(I);
    ListBoxAnimations.Items.AddObject(Animation.Name, Animation);
  end;

  if SpriteSheet.AnimationCount > 0 then
    ListBoxAnimations.ItemIndex := 0;
end;

procedure TSpriteSheetEditorForm.ClearFrames;
begin
  ListViewFrames.Items.Clear;
end;

procedure TSpriteSheetEditorForm.LoadFrames(const Animation: TCastleSpriteSheetAnimation);
var
  ListItem: TListItem;
  I: Integer;
  Frame: TCastleSpriteSheetFrame;
begin
  for I := 0 to Animation.FrameCount - 1 do
  begin
    Frame := Animation.Frame(I);
    ListItem := ListViewFrames.Items.Add;
    ListItem.Caption := IntToStr(I) + ' - ' + IntToStr(Frame.FrameWidth) +
      'x' + IntToStr(Frame.FrameHeight);
    ListItem.Data := Frame;
    // TODO: read frame image
    ListItem.ImageIndex := 0;
  end;
end;

procedure TSpriteSheetEditorForm.ClearAnimations;
begin
  ListBoxAnimations.Items.Clear;
end;

procedure TSpriteSheetEditorForm.OpenSpriteSheet(const URL: String);
begin
  try
    CloseSpriteSheet;
    FSpriteSheet :=  TCastleSpriteSheet.Create;
    FSpriteSheet.Load(URL);
    LoadAnimations(FSpriteSheet);
  except
    on E:Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

end.

