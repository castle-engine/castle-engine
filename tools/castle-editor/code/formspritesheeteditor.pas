unit FormSpriteSheetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, ActnList, StdCtrls, Spin, Menus,
  CastleControl, CastleDialogs, CastleScene, CastleSpriteSheet, CastleViewport,
  DataModuleIcons;

type
  TSpriteSheetEditorForm = class(TForm)
    ActionSaveSpriteSheetAs: TAction;
    ActionSaveSpriteSheet: TAction;
    ActionNewSpriteSheet: TAction;
    ActionOpenSpriteSheet: TAction;
    ActionListSpriteSheet: TActionList;
    CastleControlPreview: TCastleControlBase;
    ImageListFrames: TImageList;
    LabelNoFrameToShow: TLabel;
    OpenDialog: TCastleOpenDialog;
    PanelPreviewHead: TPanel;
    RadioAnimation: TRadioButton;
    RadioFrame: TRadioButton;
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
    procedure ListViewFramesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RadioFrameChange(Sender: TObject);
  private
    FSpriteSheet: TCastleSpriteSheet;
    FPreviewScene: TCastleScene;
    FViewport: TCastleViewport;

    procedure CloseSpriteSheet;

    procedure ClearAnimations;
    procedure LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
    procedure LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
    procedure LoadAnimationInPreview(const Animation: TCastleSpriteSheetAnimation);
    function GetCurrentAnimation: TCastleSpriteSheetAnimation;

    procedure ClearFrames;
    procedure LoadFrames(const Animation: TCastleSpriteSheetAnimation);
    procedure LoadFrameInPreview(const Frame: TCastleSpriteSheetFrame);
    function GetSelectedFrame: TCastleSpriteSheetFrame;

    procedure ShowPreviewControl(const MakeVisible: Boolean);
    { Creates viewport and scene for preview }
    procedure CreatePreviewUIIfNeeded;
    { Loads current animation (with file regeneration when
      ReloadSpriteSheetFile = true), or frame in preview. }
    procedure UpdatePreview(const ReloadSpriteSheetFile: Boolean);
    { Regenerates and load animation temp file }
    procedure RegenerateAnimationPreviewFile;
    { Regenerates and load frame temp file }
    procedure RegenerateFramePreviewFile(const Frame: TCastleSpriteSheetFrame);
  public
    procedure OpenSpriteSheet(const URL: String);
  end;

var
  SpriteSheetEditorForm: TSpriteSheetEditorForm;

implementation

{$R *.lfm}

uses GraphType, IntfGraphics,
  CastleImages, CastleLog, CastleVectors, CastleURIUtils,
  EditorUtils,
  FormProject;

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
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;

  if Animation <> nil then
    LoadAnimation(Animation);
end;

procedure TSpriteSheetEditorForm.ListViewFramesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdatePreview(false);
end;

procedure TSpriteSheetEditorForm.RadioFrameChange(Sender: TObject);
begin
  UpdatePreview(true);
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

procedure TSpriteSheetEditorForm.LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
begin
  FloatSpinEditFPS.Value := Animation.FramesPerSecond;
  ClearFrames;
  LoadFrames(Animation);
  UpdatePreview(false);
end;

procedure TSpriteSheetEditorForm.ClearFrames;
begin
  ListViewFrames.Items.Clear;
end;

procedure TSpriteSheetEditorForm.LoadFrames(const Animation: TCastleSpriteSheetAnimation);
const
  MaxFrameSize = 256;
  DefaultFrameSize = 128;
var
  ListItem: TListItem;
  I: Integer;
  Frame: TCastleSpriteSheetFrame;
  ResizedFrameImage: TCastleImage;
  Bitmap: TBitmap;
  IntfImage: TLazIntfImage;
  ImageIndex: Integer;
  FrameIconSize: TVector2Integer;

  procedure PrepareImageList;
  begin
    ImageListFrames.Clear;

    { ListView can have only one size of images so we need decide about size. }

    FrameIconSize := Animation.GetBigestFrameSize(MaxFrameSize, MaxFrameSize);

    if (FrameIconSize.X = 0) or (FrameIconSize.Y = 0) then
    begin
      FrameIconSize.X := DefaultFrameSize;
      FrameIconSize.Y := DefaultFrameSize;
    end;

    ImageListFrames.Width := FrameIconSize.X;
    ImageListFrames.Height := FrameIconSize.Y;
  end;

  function FrameImageToLazImage(const FrameImage: TCastleImage;
    const Width, Height: Integer): TLazIntfImage;
  var
    RawImage: TRawImage;
    Y: Integer;
    RowSize: Integer;
    SourceRow: PByte;
    DestRow: PByte;
  begin
    // https://wiki.freepascal.org/Developing_with_Graphics#Working_with_TLazIntfImage.2C_TRawImage_and_TLazCanvas

    // TODO: Support other image format than RGBA
    RawImage.Init;
    RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Width, Height);
    RawImage.CreateData(True);
    RowSize := Frame.FrameWidth * 4;

    // go to last row
    SourceRow := PByte(FrameImage.RawPixels) + RowSize * Height;
    DestRow := RawImage.Data;

    for Y := Height - 1 downto 0 do
    begin
      Dec(SourceRow, RowSize);
      Move(SourceRow^, DestRow^, RowSize);
      Inc(DestRow, RowSize);
    end;

    Result := TLazIntfImage.Create(0, 0);
    Result.SetRawImage(RawImage);
  end;

begin
  PrepareImageList;

  for I := 0 to Animation.FrameCount - 1 do
  begin
    Frame := Animation.Frame[I];

    ResizedFrameImage := nil;
    IntfImage := nil;
    Bitmap := nil;
    try
      // TODO:  better scaling alghoritm
      ResizedFrameImage := Frame.FrameImage.MakeResized(FrameIconSize.X,
        FrameIconSize.Y);

      IntfImage := FrameImageToLazImage(ResizedFrameImage, FrameIconSize.X,
        FrameIconSize.Y);

      Bitmap := TBitmap.Create;
      Bitmap.LoadFromIntfImage(IntfImage);
      ImageIndex := ImageListFrames.Add(Bitmap, nil);
    finally
      FreeAndNil(ResizedFrameImage);
      FreeAndNil(IntfImage);
      FreeAndNil(Bitmap);
    end;

    ListItem := ListViewFrames.Items.Add;
    ListItem.Caption := IntToStr(I) + ' - ' + IntToStr(Frame.FrameWidth) +
      'x' + IntToStr(Frame.FrameHeight);
    ListItem.Data := Frame;
    ListItem.ImageIndex := ImageIndex;
  end;
end;

procedure TSpriteSheetEditorForm.LoadFrameInPreview(
  const Frame: TCastleSpriteSheetFrame);
begin
  if Frame = nil then
  begin
    ShowPreviewControl(false);
    Exit;
  end;
  RegenerateFramePreviewFile(Frame);
  ShowPreviewControl(true);
end;

function TSpriteSheetEditorForm.GetSelectedFrame: TCastleSpriteSheetFrame;
begin
  if ListViewFrames.ItemIndex < 0 then
    Exit(nil);

  { TODO: add frame button - add condition here!!! }

  Result := TCastleSpriteSheetFrame(ListViewFrames.Items[ListViewFrames.ItemIndex].Data);
end;

procedure TSpriteSheetEditorForm.ShowPreviewControl(const MakeVisible: Boolean);
begin
  CastleControlPreview.Visible := MakeVisible;
  LabelNoFrameToShow.Visible := not MakeVisible;
end;

procedure TSpriteSheetEditorForm.CreatePreviewUIIfNeeded;
begin
  if FPreviewScene = nil then
  begin
    FViewport := TCastleViewport.Create(Application);
    FViewport.FullSize := true;
    FViewport.AutoCamera := true;
    FViewport.AutoNavigation := true;
    CastleControlPreview.Controls.InsertFront(FViewport);


    FPreviewScene := TCastleScene.Create(FViewport);

    FViewport.Items.Add(FPreviewScene);
    FViewport.Items.MainScene := FPreviewScene;
  end;
end;

procedure TSpriteSheetEditorForm.UpdatePreview(
  const ReloadSpriteSheetFile: Boolean);
begin
  if RadioAnimation.Checked then
  begin
    { only when we know file changed }
    if ReloadSpriteSheetFile then
      RegenerateAnimationPreviewFile;
    LoadAnimationInPreview(GetCurrentAnimation);
  end else
  begin
    LoadFrameInPreview(GetSelectedFrame);
  end;
end;

procedure TSpriteSheetEditorForm.LoadAnimationInPreview(const Animation: TCastleSpriteSheetAnimation);
begin
  ShowPreviewControl(true);
  if FPreviewScene = nil then
    RegenerateAnimationPreviewFile;

  if Animation = nil then
    FPreviewScene.StopAnimation
  else
    FPreviewScene.PlayAnimation(Animation.Name, true, true);
end;

function TSpriteSheetEditorForm.GetCurrentAnimation: TCastleSpriteSheetAnimation;
begin
  if ListBoxAnimations.ItemIndex < 0 then
    Exit(nil);

  Result := ListBoxAnimations.Items.Objects[ListBoxAnimations.ItemIndex] as TCastleSpriteSheetAnimation;
end;

procedure TSpriteSheetEditorForm.RegenerateAnimationPreviewFile;
var
  TempURL: String;
begin
  TempURL := URIIncludeSlash(ProjectForm.ProjectPathUrl) + 'temp/preview.castle-sprite-sheet';
  ForceDirectories(ExtractFilePath(URIToFilenameSafe(TempURL)));
  FSpriteSheet.Save(TempURL);

  CreatePreviewUIIfNeeded;

  FPreviewScene.Load(TempURL);
end;

procedure TSpriteSheetEditorForm.RegenerateFramePreviewFile(const Frame: TCastleSpriteSheetFrame);
var
  TempURL: String;
begin
  TempURL := URIIncludeSlash(ProjectForm.ProjectPathUrl) + 'temp/frame_preview.png';
  ForceDirectories(ExtractFilePath(URIToFilenameSafe(TempURL)));
  SaveImage(Frame.FrameImage, TempURL);

  CreatePreviewUIIfNeeded;

  FPreviewScene.Load(TempURL);
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

