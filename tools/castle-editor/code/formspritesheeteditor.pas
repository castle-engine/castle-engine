unit FormSpriteSheetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, ActnList, StdCtrls, Spin, Menus,
  CastleControl, CastleDialogs, CastleScene, CastleSpriteSheet, CastleVectors,
  CastleViewport,
  DataModuleIcons;

type
  TSpriteSheetEditorForm = class(TForm)
    ActionAddFrame: TAction;
    ActionRenameAnimation: TAction;
    ActionRemoveAnimation: TAction;
    ActionRemoveFrame: TAction;
    ActionSaveSpriteSheetAs: TAction;
    ActionSaveSpriteSheet: TAction;
    ActionNewSpriteSheet: TAction;
    ActionOpenSpriteSheet: TAction;
    ActionListSpriteSheet: TActionList;
    CastleControlPreview: TCastleControlBase;
    CastleOpenImageDialog: TCastleOpenImageDialog;
    ImageListFrames: TImageList;
    LabelNoFrameToShow: TLabel;
    ListViewAnimations: TListView;
    MenuItemAddFrame: TMenuItem;
    MenuItemRename: TMenuItem;
    MenuItemRemoveAnimation: TMenuItem;
    MenuItemRemoveFrame: TMenuItem;
    OpenDialog: TCastleOpenDialog;
    PanelPreviewHead: TPanel;
    PopupMenuAnimations: TPopupMenu;
    PopupMenuFrames: TPopupMenu;
    RadioAnimation: TRadioButton;
    RadioFrame: TRadioButton;
    SaveDialog: TCastleSaveDialog;
    FloatSpinEditFPS: TFloatSpinEdit;
    LabelPreview: TLabel;
    LabelFrames: TLabel;
    LabelAnimations: TLabel;
    LabelFPS: TLabel;
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
    procedure ActionAddFrameExecute(Sender: TObject);
    procedure ActionAddFrameUpdate(Sender: TObject);
    procedure ActionNewSpriteSheetExecute(Sender: TObject);
    procedure ActionOpenSpriteSheetExecute(Sender: TObject);
    procedure ActionRemoveAnimationExecute(Sender: TObject);
    procedure ActionRemoveAnimationUpdate(Sender: TObject);
    procedure ActionRemoveFrameExecute(Sender: TObject);
    procedure ActionRemoveFrameUpdate(Sender: TObject);
    procedure ActionRenameAnimationExecute(Sender: TObject);
    procedure ActionRenameAnimationUpdate(Sender: TObject);
    procedure FloatSpinEditFPSChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewAnimationsEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure ListViewAnimationsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewFramesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RadioFrameChange(Sender: TObject);
  private
    type
      TPreviewMode = (pmAnimation, pmFrame);
      { Enum just for readability }
      TForceFileRegen = (ffgDoForceFileRegen, ffgDontForceFileRegen);

    const
      MaxFrameSize = 256;
      DefaultFrameSize = 128;

    var
      FSpriteSheet: TCastleSpriteSheet;
      FPreviewScene: TCastleScene;
      FViewport: TCastleViewport;
      FWindowTitle: String;
      FrameIconSize: TVector2Integer; // current frame size in list view

    procedure CloseSpriteSheet;

    procedure ClearAnimations;
    procedure LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
    procedure LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
    function GetCurrentAnimation: TCastleSpriteSheetAnimation;

    procedure ClearFrames;
    function AddFrameToListView(const Frame: TCastleSpriteSheetFrame;
      const FrameNo: Integer): TListItem;
    procedure LoadFrames(const Animation: TCastleSpriteSheetAnimation);
    function GetSelectedFrame: TCastleSpriteSheetFrame;

    { Returns current preview mode }
    function GetCurrentPreviewMode: TPreviewMode;
    { Shows or hides some preview controls. Just to have this switch in
      one place. }
    procedure ShowPreviewControl(const MakeVisible: Boolean);
    { Creates viewport and scene for preview }
    procedure CreatePreviewUIIfNeeded;
    { Updates current preview. If PreviewModesToUpdate is pmAnimation
      value of ForcePreviewFileRegen controls sprite sheet file should be forced
      to regenrate/reload. Note that it will be regenerated even if you don't
      set it force when it's obvoius (eg. for frames). }
    procedure UpdatePreview(const PreviewModesToUpdate: TPreviewMode;
      const ForcePreviewFileRegen: TForceFileRegen);
    { Regenerates and load animation temp file }
    procedure RegenerateAnimationPreviewFile;
    { Regenerates and load frame temp file }
    procedure RegenerateFramePreviewFile(const Frame: TCastleSpriteSheetFrame);

    procedure UpdateWindowCaption;

    // events:

    procedure ModifiedStateChanged(Sender: TObject);
    procedure BeforeAnimationRemoved(AnimationToRemove: TCastleSpriteSheetAnimation);
    procedure BeforeAnimationFrameRemoved(FrameToRemove: TCastleSpriteSheetFrame);
    procedure FrameAdded(NewFrame: TCastleSpriteSheetFrame);

  public
    procedure OpenSpriteSheet(const URL: String);
  end;

var
  SpriteSheetEditorForm: TSpriteSheetEditorForm;

implementation

{$R *.lfm}

uses GraphType, IntfGraphics,
  CastleImages, CastleLog, CastleURIUtils,
  EditorUtils,
  FormProject;

{ TSpriteSheetEditorForm }

procedure TSpriteSheetEditorForm.ActionNewSpriteSheetExecute(Sender: TObject);
begin
  ShowMessage('test');
end;

procedure TSpriteSheetEditorForm.ActionAddFrameUpdate(Sender: TObject);
begin
  ActionAddFrame.Enabled := GetCurrentAnimation <> nil;
end;

procedure TSpriteSheetEditorForm.ActionAddFrameExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  if CastleOpenImageDialog.Execute then
  begin
    Animation.AddFrame(CastleOpenImageDialog.URL);
  end;
end;

procedure TSpriteSheetEditorForm.ActionOpenSpriteSheetExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenSpriteSheet(OpenDialog.URL);
end;

procedure TSpriteSheetEditorForm.ActionRemoveAnimationExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSpriteSheet.RemoveAnimation(Animation);
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionRemoveAnimationUpdate(Sender: TObject);
begin
  ActionRemoveAnimation.Enabled := (GetCurrentAnimation <> nil);
end;

procedure TSpriteSheetEditorForm.ActionRemoveFrameExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  Frame: TCastleSpriteSheetFrame;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  Frame := GetSelectedFrame;
  Animation.RemoveFrame(Frame);
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionRemoveFrameUpdate(Sender: TObject);
begin
  ActionRemoveFrame.Enabled := (GetSelectedFrame <> nil);
end;

procedure TSpriteSheetEditorForm.ActionRenameAnimationExecute(Sender: TObject);
begin
  ListViewAnimations.Selected.EditCaption;
end;

procedure TSpriteSheetEditorForm.ActionRenameAnimationUpdate(Sender: TObject);
begin
  ActionRenameAnimation.Enabled := (GetCurrentAnimation <> nil);
end;

procedure TSpriteSheetEditorForm.FloatSpinEditFPSChange(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  Assert(Animation <> nil,
    'Animation should never be nil when SpinEditFPS is enabled');
  Animation.FramesPerSecond := FloatSpinEditFPS.Value;
  { To change frames per second file must be regenerated. }
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.FormCreate(Sender: TObject);
begin
  FSpriteSheet := nil;
  FWindowTitle := SpriteSheetEditorForm.Caption;
end;

procedure TSpriteSheetEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpriteSheet);
end;

procedure TSpriteSheetEditorForm.ListViewAnimationsEdited(Sender: TObject;
  Item: TListItem; var AValue: string);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := TCastleSpriteSheetAnimation(Item.Data);

  if Animation.Name = AValue then
    Exit;

  if FSpriteSheet.HasAnimation(AValue) then
  begin
    EditorUtils.ErrorBox('Animation "' + AValue + '" already exist.');
    AValue := Animation.Name;
    Exit;
  end;

  Animation.Name := AValue;
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ListViewAnimationsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
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
  { In case of changing frames, animation preview should be ok
    (frame preview is always regenerated). }
  UpdatePreview(GetCurrentPreviewMode, ffgDontForceFileRegen);
end;

procedure TSpriteSheetEditorForm.RadioFrameChange(Sender: TObject);
begin
  { In case of changing from frame preview to animation preview,
    animation preview can be outdated so we force file regeneration }
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
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
  ListItem: TListItem;
begin
  for I := 0 to SpriteSheet.AnimationCount - 1 do
  begin
    Animation := SpriteSheet.AnimationByIndex(I);
    ListItem := ListViewAnimations.Items.Add;
    ListItem.Caption := Animation.Name;
    ListItem.Data := Animation;
  end;

  if SpriteSheet.AnimationCount > 0 then
    ListViewAnimations.ItemIndex := 0;
end;

procedure TSpriteSheetEditorForm.LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
begin
  FloatSpinEditFPS.Value := Animation.FramesPerSecond;
  ClearFrames;
  LoadFrames(Animation);
  UpdatePreview(GetCurrentPreviewMode, ffgDontForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ClearFrames;
begin
  ListViewFrames.Items.Clear;
end;

function TSpriteSheetEditorForm.AddFrameToListView(
  const Frame: TCastleSpriteSheetFrame; const FrameNo: Integer): TListItem;
var
  ListItem: TListItem;
  ResizedFrameImage: TCastleImage;
  Bitmap: TBitmap;
  IntfImage: TLazIntfImage;
  ImageIndex: Integer;

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
    RowSize := FrameImage.Width * 4;

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
  ResizedFrameImage := nil;
  IntfImage := nil;
  Bitmap := nil;
  try
    // TODO:  better scaling alghoritm
    ResizedFrameImage := Frame.MakeResized(FrameIconSize.X, FrameIconSize.Y);

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
  ListItem.Caption := IntToStr(FrameNo) + ' - ' + IntToStr(Frame.FrameWidth) +
    'x' + IntToStr(Frame.FrameHeight);
  ListItem.Data := Frame;
  ListItem.ImageIndex := ImageIndex;
  Result := ListItem;
end;

procedure TSpriteSheetEditorForm.LoadFrames(const Animation: TCastleSpriteSheetAnimation);
var
  I: Integer;

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

begin
  PrepareImageList;

  for I := 0 to Animation.FrameCount - 1 do
    AddFrameToListView(Animation.Frame[I], I);
end;

function TSpriteSheetEditorForm.GetSelectedFrame: TCastleSpriteSheetFrame;
begin
  if ListViewFrames.ItemIndex < 0 then
    Exit(nil);

  { TODO: add frame button - add condition here!!! }

  Result := TCastleSpriteSheetFrame(ListViewFrames.Items[ListViewFrames.ItemIndex].Data);
end;

function TSpriteSheetEditorForm.GetCurrentPreviewMode: TPreviewMode;
begin
  if RadioAnimation.Checked then
    Result:= pmAnimation
  else
    Result:= pmFrame;
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
  const PreviewModesToUpdate: TPreviewMode;
  const ForcePreviewFileRegen: TForceFileRegen);

  procedure LoadFrameInPreview(
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

  procedure LoadAnimationInPreview(
    const Animation: TCastleSpriteSheetAnimation);
  begin
    ShowPreviewControl(true);
    if FPreviewScene = nil then
      RegenerateAnimationPreviewFile;

    if (Animation = nil) or (Animation.FrameCount = 0) then
    begin
      FPreviewScene.Exists := false;
      FPreviewScene.StopAnimation
    end else
    begin
      FPreviewScene.Exists := true;
      FPreviewScene.PlayAnimation(Animation.Name, true, true);
    end;
  end;

begin
  case PreviewModesToUpdate of
    pmAnimation:
      begin
        { only when we know file should change }
        if ForcePreviewFileRegen = ffgDoForceFileRegen then
          RegenerateAnimationPreviewFile;
        LoadAnimationInPreview(GetCurrentAnimation);
      end;
    pmFrame:
      LoadFrameInPreview(GetSelectedFrame);
  end;
end;

function TSpriteSheetEditorForm.GetCurrentAnimation: TCastleSpriteSheetAnimation;
begin
  if ListViewAnimations.ItemIndex < 0 then
    Exit(nil);

  Result := TCastleSpriteSheetAnimation(ListViewAnimations.Items[ListViewAnimations.ItemIndex].Data);
end;

procedure TSpriteSheetEditorForm.RegenerateAnimationPreviewFile;
var
  TempURL: String;
begin
  TempURL := URIIncludeSlash(ProjectForm.ProjectPathUrl) + 'temp/preview.castle-sprite-sheet';
  ForceDirectories(ExtractFilePath(URIToFilenameSafe(TempURL)));
  FSpriteSheet.Save(TempURL, true);

  CreatePreviewUIIfNeeded;

  FPreviewScene.Load(TempURL);
end;

procedure TSpriteSheetEditorForm.RegenerateFramePreviewFile(const Frame: TCastleSpriteSheetFrame);
var
  TempURL: String;
begin
  TempURL := URIIncludeSlash(ProjectForm.ProjectPathUrl) + 'temp/frame_preview.png';
  ForceDirectories(ExtractFilePath(URIToFilenameSafe(TempURL)));
  Frame.SaveFrameImage(TempURL);

  CreatePreviewUIIfNeeded;

  FPreviewScene.Load(TempURL);
end;

procedure TSpriteSheetEditorForm.UpdateWindowCaption;
var
  ModifiedMark: String;
begin
  if FSpriteSheet = nil then
  begin
    SpriteSheetEditorForm.Caption := FWindowTitle;
    Exit;
  end;

  if FSpriteSheet.IsModified then
    ModifiedMark := '*'
  else
    ModifiedMark := '';

  SpriteSheetEditorForm.Caption := FWindowTitle + ' - ' + ModifiedMark +
    FSpriteSheet.URL;
end;

procedure TSpriteSheetEditorForm.ModifiedStateChanged(Sender: TObject);
begin
  UpdateWindowCaption;
end;

procedure TSpriteSheetEditorForm.BeforeAnimationRemoved(
  AnimationToRemove: TCastleSpriteSheetAnimation);
var
  I: Integer;
  ItemIndex: Integer;
begin
  if AnimationToRemove = GetCurrentAnimation then
  begin
    ClearFrames;
    ItemIndex := ListViewAnimations.ItemIndex;
    ListViewAnimations.Items.Delete(ItemIndex);

    { Select next animation }
    if ListViewAnimations.Items.Count > 0 then
    begin
      if ListViewAnimations.Items.Count > ItemIndex then
        ListViewAnimations.ItemIndex := ItemIndex
      else
        ListViewAnimations.ItemIndex := ListViewAnimations.Items.Count - 1;
    end;

    Exit;
  end;

  { When not current animation was removed }
  for I := ListViewAnimations.Items.Count - 1 downto 0 do
  begin
    if TObject(ListViewAnimations.Items[i].Data) = AnimationToRemove then
    begin
      ListViewAnimations.Items.Delete(I);
      Exit;
    end;
  end;
end;

procedure TSpriteSheetEditorForm.BeforeAnimationFrameRemoved(
  FrameToRemove: TCastleSpriteSheetFrame);
var
  I: Integer;
begin
  if FrameToRemove.Animation <> GetCurrentAnimation then
    Exit;

  for I := ListViewFrames.Items.Count - 1 downto 0 do
  begin
    if TCastleSpriteSheetFrame(ListViewFrames.Items[I].Data) = FrameToRemove then
      ListViewFrames.Items.Delete(I);
  end;
end;

procedure TSpriteSheetEditorForm.FrameAdded(NewFrame: TCastleSpriteSheetFrame);
begin
  if NewFrame.Animation <> GetCurrentAnimation then
    Exit;

  { Add frame on last position }
  AddFrameToListView(NewFrame, NewFrame.Animation.FrameCount);
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ClearAnimations;
begin
  ListViewAnimations.Items.Clear;
end;

procedure TSpriteSheetEditorForm.OpenSpriteSheet(const URL: String);
begin
  try
    CloseSpriteSheet;
    FSpriteSheet :=  TCastleSpriteSheet.Create;
    FSpriteSheet.OnModifiedStateChanged := @ModifiedStateChanged;
    FSpriteSheet.Load(URL);
    UpdateWindowCaption;
    LoadAnimations(FSpriteSheet);
    FSpriteSheet.BeforeAnimationRemoved := @BeforeAnimationRemoved;
    FSpriteSheet.BeforeFrameRemoved := @BeforeAnimationFrameRemoved;
    FSpriteSheet.OnFrameAdded := @FrameAdded;
  except
    on E:Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

end.

