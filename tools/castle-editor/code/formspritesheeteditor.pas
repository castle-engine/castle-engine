unit FormSpriteSheetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Buttons, ActnList, StdCtrls, Spin, Menus,
  CastleControl, CastleDialogs, CastleScene, CastleSpriteSheet, CastleVectors,
  CastleViewport,
  DataModuleIcons;

type
  TSpriteSheetEditorForm = class(TForm)
    ActionCreateNewAnimationFromSelection: TAction;
    ActionMoveAnimationEnd: TAction;
    ActionMoveAnimationTop: TAction;
    ActionMoveAnimationDown: TAction;
    ActionMoveAnimationUp: TAction;
    ActionMoveFrameEnd: TAction;
    ActionMoveFrameTop: TAction;
    ActionMoveFrameRight: TAction;
    ActionMoveFrameLeft: TAction;
    ActionAddAnimation: TAction;
    ActionAddFrame: TAction;
    ActionRenameAnimation: TAction;
    ActionDeleteAnimation: TAction;
    ActionDeleteFrame: TAction;
    ActionSaveSpriteSheetAs: TAction;
    ActionSaveSpriteSheet: TAction;
    ActionNewSpriteSheet: TAction;
    ActionOpenSpriteSheet: TAction;
    ActionListSpriteSheet: TActionList;
    CastleControlPreview: TCastleControlBase;
    CastleOpenImageDialog: TCastleOpenImageDialog;
    ImageAtlasSizeWarning: TImage;
    ImageListFrames: TImageList;
    LabelAtlasWarning: TLabel;
    LabelAtlasSizeError: TLabel;
    LabelMaximumAtlasSize: TLabel;
    LabelNoFrameToShow: TLabel;
    ListViewAnimations: TListView;
    MainMenuItemAddAnimation: TMenuItem;
    MainMenuItemDeleteAnimation: TMenuItem;
    MainMenuItemMoveFrameUp: TMenuItem;
    MainMenuItemMoveFrameDown: TMenuItem;
    MainMenuItemMoveFrameTop: TMenuItem;
    MainMenuItemMoveFrameEnd: TMenuItem;
    MainMenuItemAddFrame: TMenuItem;
    MainMenuItemDeleteFrame: TMenuItem;
    MainMenuItemCreateAnimFromSelection: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuItemCreateNewAnimationFromSelection: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MenuItemRenameAnimation: TMenuItem;
    MenuItemFrameMenu: TMenuItem;
    MenuItemAnimationMenu: TMenuItem;
    MenuItemAnimationEnd: TMenuItem;
    MenuItemAnimationTop: TMenuItem;
    MenuItemAnimationDown: TMenuItem;
    MenuItemMoveAnimationUp: TMenuItem;
    MenuItemMoveEnd: TMenuItem;
    MenuItemMoveFrameToTop: TMenuItem;
    MenuItemMoveFrameDown: TMenuItem;
    MenuItemMoveFrameUp: TMenuItem;
    MenuItemAddAnimation: TMenuItem;
    MenuItemAddFrame: TMenuItem;
    MenuItemRename: TMenuItem;
    MenuItemDeleteAnimation: TMenuItem;
    MenuItemDeleteFrame: TMenuItem;
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
    SpeedButtonSaveSpriteSheetAs: TSpeedButton;
    SpinEditMaxAtlasSize: TSpinEdit;
    SplitterRight: TSplitter;
    SplitterLeft: TSplitter;
    procedure ActionAddAnimationExecute(Sender: TObject);
    procedure ActionAddAnimationUpdate(Sender: TObject);
    procedure ActionAddFrameExecute(Sender: TObject);
    procedure ActionAddFrameUpdate(Sender: TObject);
    procedure ActionCreateNewAnimationFromSelectionExecute(Sender: TObject);
    procedure ActionCreateNewAnimationFromSelectionUpdate(Sender: TObject);
    procedure ActionMoveAnimationDownExecute(Sender: TObject);
    procedure ActionMoveAnimationDownUpdate(Sender: TObject);
    procedure ActionMoveAnimationEndExecute(Sender: TObject);
    procedure ActionMoveAnimationEndUpdate(Sender: TObject);
    procedure ActionMoveAnimationTopExecute(Sender: TObject);
    procedure ActionMoveAnimationTopUpdate(Sender: TObject);
    procedure ActionMoveAnimationUpExecute(Sender: TObject);
    procedure ActionMoveAnimationUpUpdate(Sender: TObject);
    procedure ActionMoveFrameRightExecute(Sender: TObject);
    procedure ActionMoveFrameRightUpdate(Sender: TObject);
    procedure ActionMoveFrameEndExecute(Sender: TObject);
    procedure ActionMoveFrameEndUpdate(Sender: TObject);
    procedure ActionMoveFrameTopExecute(Sender: TObject);
    procedure ActionMoveFrameTopUpdate(Sender: TObject);
    procedure ActionMoveFrameLeftExecute(Sender: TObject);
    procedure ActionMoveFrameLeftUpdate(Sender: TObject);
    procedure ActionNewSpriteSheetExecute(Sender: TObject);
    procedure ActionOpenSpriteSheetExecute(Sender: TObject);
    procedure ActionDeleteAnimationExecute(Sender: TObject);
    procedure ActionDeleteAnimationUpdate(Sender: TObject);
    procedure ActionDeleteFrameExecute(Sender: TObject);
    procedure ActionDeleteFrameUpdate(Sender: TObject);
    procedure ActionRenameAnimationExecute(Sender: TObject);
    procedure ActionRenameAnimationUpdate(Sender: TObject);
    procedure FloatSpinEditFPSChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewAnimationsEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure ListViewAnimationsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewFramesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RadioFrameChange(Sender: TObject);
    procedure SpinEditMaxAtlasSizeChange(Sender: TObject);
    procedure SpinEditMaxAtlasSizeEditingDone(Sender: TObject);
  private
    type
      TPreviewMode = (pmAnimation, pmFrame);
      { Enum just for readability }
      TForceFileRegen = (ffgDoForceFileRegen, ffgDontForceFileRegen);

      TSelectedFrames = class
        strict private
          type
            TListOfSelectedFrames = specialize TList<TCastleSpriteSheetFrame>;
          var
            FSelectedFrames: TListOfSelectedFrames;
            FrameListView: TListView;
        public
          constructor Create(const FrameListViewToMonitor: TListView);
          destructor Destroy; override;

          procedure GetCurrentSelection;
          procedure SetSelection;
          procedure Clear;
          function GetFrameByIndex(const Index: Integer): TCastleSpriteSheetFrame;
          function FrameCount: Integer;

          { This function is preparation for adding "+" button to ListViewFrames }
          class function IsFrameListItem(const ItemToCheck: TListItem): Boolean;

          property Frame[Index: Integer]: TCastleSpriteSheetFrame read GetFrameByIndex;
      end;

    const
      MaxFrameIconSize = 256;
      DefaultFrameIconSize = 128;

    var
      FSpriteSheet: TCastleSpriteSheet;
      FPreviewScene: TCastleScene;
      FViewport: TCastleViewport;
      FWindowTitle: String;
      CurrentFrameIconSize: TVector2Integer; // current frame size in list view
      FSelectedFrames: TSelectedFrames;
      { should we select added animation (not always desirable) }
      FSelectNewAnimation: Boolean;

    // Returns true if sprite sheet is closed
    function CloseSpriteSheet: Boolean;
    procedure AssignEventsToSpriteSheet;

    procedure ClearAnimations;
    procedure LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
    function AddAnimationToListView(const Animation: TCastleSpriteSheetAnimation):
      TListItem;
    procedure LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
    function GetCurrentAnimation: TCastleSpriteSheetAnimation;

    procedure ClearFrames;
    function AddFrameToListView(const Frame: TCastleSpriteSheetFrame;
      const FrameNo: Integer): TListItem;
    procedure LoadFrames(const Animation: TCastleSpriteSheetAnimation);
    function GetFirstSelectedFrame: TCastleSpriteSheetFrame;
    function GetLastSelectedFrame: TCastleSpriteSheetFrame;
    function FrameTitle(const FrameNo: Integer;
      const Frame: TCastleSpriteSheetFrame): string;
    procedure UpdateFrameTitles;

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
    procedure SetAtlasError(const Message: String);
    procedure SetAtlasWarning(const Message: String);

    { Check atlas size }
    function CheckAtlasMinSize: Boolean;

    // events:

    procedure ModifiedStateChanged(Sender: TObject);

    procedure AnimationAdded(NewAnimation: TCastleSpriteSheetAnimation);
    procedure BeforeAnimationRemoved(AnimationToRemove: TCastleSpriteSheetAnimation);
    procedure AnimationMoved(const Animation: TCastleSpriteSheetAnimation;
      const OldIndex, NewIndex: Integer);

    procedure FrameAdded(NewFrame: TCastleSpriteSheetFrame);
    procedure BeforeAnimationFrameRemoved(FrameToRemove: TCastleSpriteSheetFrame);
    procedure FrameMoved(const Frame: TCastleSpriteSheetFrame;
      const OldIndex, NewIndex: Integer);

    procedure MaxAtlasSizeChanged(const MaxWidth, MaxHeight: Integer);

  public
    procedure OpenSpriteSheet(const URL: String);
    procedure NewSpriteSheet;
  end;

var
  SpriteSheetEditorForm: TSpriteSheetEditorForm;

implementation

{$R *.lfm}

uses GraphType, IntfGraphics, Math,
  CastleImages, CastleLog, CastleUtils, CastleURIUtils,
  EditorUtils,
  FormProject
  {$ifdef LCLGTK2},Gtk2Globals{$endif};

{ TSpriteSheetEditorForm.TSelectedFrames }

constructor TSpriteSheetEditorForm.TSelectedFrames.Create(
  const FrameListViewToMonitor: TListView);
begin
  FrameListView := FrameListViewToMonitor;
  FSelectedFrames := TListOfSelectedFrames.Create;
end;

destructor TSpriteSheetEditorForm.TSelectedFrames.Destroy;
begin
  FreeAndNil(FSelectedFrames);
  inherited Destroy;
end;

procedure TSpriteSheetEditorForm.TSelectedFrames.GetCurrentSelection;
var
  Item: TListItem;
begin
  Clear;
  Item := FrameListView.Selected;

  while Item <> nil do
  begin
    if IsFrameListItem(Item) then
      FSelectedFrames.Add(TCastleSpriteSheetFrame(Item.Data));

    Item := FrameListView.GetNextItem(Item, sdBelow, [lisSelected]);
  end;
end;

procedure TSpriteSheetEditorForm.TSelectedFrames.SetSelection;
var
  Item: TListItem;
begin
  for Item in FrameListView.Items do
  begin
    if IsFrameListItem(Item) then
    begin
      Item.Selected := FSelectedFrames.Contains(TCastleSpriteSheetFrame(Item.Data));
    end;
  end;
end;

procedure TSpriteSheetEditorForm.TSelectedFrames.Clear;
begin
  FSelectedFrames.Clear;
end;

function TSpriteSheetEditorForm.TSelectedFrames.GetFrameByIndex(
  const Index: Integer): TCastleSpriteSheetFrame;
begin
  Result := FSelectedFrames[Index];
end;

function TSpriteSheetEditorForm.TSelectedFrames.FrameCount: Integer;
begin
  Result := FSelectedFrames.Count;
end;

class function TSpriteSheetEditorForm.TSelectedFrames.IsFrameListItem(
  const ItemToCheck: TListItem): Boolean;
begin
  Result := ItemToCheck.Data <> nil;
end;

{ TSpriteSheetEditorForm }

procedure TSpriteSheetEditorForm.ActionNewSpriteSheetExecute(Sender: TObject);
begin
  NewSpriteSheet;
end;

procedure TSpriteSheetEditorForm.ActionAddFrameUpdate(Sender: TObject);
begin
  ActionAddFrame.Enabled := GetCurrentAnimation <> nil;
end;

procedure TSpriteSheetEditorForm.ActionCreateNewAnimationFromSelectionExecute(
  Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  NewAnimation: TCastleSpriteSheetAnimation;
  I: Integer;
  OldSelectNewAnimation: Boolean;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSelectedFrames.GetCurrentSelection;

  if FSelectedFrames.FrameCount = 0 then
    Exit;

  OldSelectNewAnimation := FSelectNewAnimation;
  try
    { We don't want to jump to new animation in this case }
    FSelectNewAnimation := false;
    NewAnimation := FSpriteSheet.AddAnimation(FSpriteSheet.ProposeAnimationName);
  finally
    FSelectNewAnimation := OldSelectNewAnimation;
  end;

  for I := 0 to FSelectedFrames.FrameCount -1 do
  begin
    NewAnimation.AddFrameCopy(FSelectedFrames.Frame[I]);
  end;
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionCreateNewAnimationFromSelectionUpdate(
  Sender: TObject);
begin
  ActionCreateNewAnimationFromSelection.Enabled := (GetFirstSelectedFrame <> nil);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationDownExecute(Sender: TObject
  );
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSpriteSheet.MoveAnimationDown(Animation);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationDownUpdate(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  ActionMoveAnimationDown.Enabled := (Animation <> nil)
    and (FSpriteSheet.AnimationIndex(Animation) <> FSpriteSheet.AnimationCount - 1);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationEndExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSpriteSheet.MoveAnimationToEnd(Animation);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationEndUpdate(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  ActionMoveAnimationEnd.Enabled := (Animation <> nil)
    and (FSpriteSheet.AnimationIndex(Animation) <> FSpriteSheet.AnimationCount - 1);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationTopExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSpriteSheet.MoveAnimationToTop(Animation);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationTopUpdate(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  ActionMoveAnimationTop.Enabled := (Animation <> nil)
    and (FSpriteSheet.AnimationIndex(Animation) <> 0);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationUpExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSpriteSheet.MoveAnimationUp(Animation);
end;

procedure TSpriteSheetEditorForm.ActionMoveAnimationUpUpdate(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  ActionMoveAnimationUp.Enabled := (Animation <> nil)
    and (FSpriteSheet.AnimationIndex(Animation) <> 0);
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameRightExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  I: Integer;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSelectedFrames.GetCurrentSelection;
  for I := FSelectedFrames.FrameCount - 1 downto 0 do
  begin
    Animation.MoveFrameRight(FSelectedFrames.Frame[I]);
  end;
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameRightUpdate(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
begin
  Frame := GetFirstSelectedFrame;
  ActionMoveFrameRight.Enabled := (Frame <> nil)
    and (Frame.Animation.FrameIndex(Frame) < Frame.Animation.FrameCount - 1);
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameEndExecute(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  Frame := GetFirstSelectedFrame;

  if (Animation = nil) or (Frame = nil) then
    Exit;

  Animation.MoveFrameToEnd(Frame);
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameEndUpdate(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
begin
  Frame := GetFirstSelectedFrame;
  ActionMoveFrameEnd.Enabled := (Frame <> nil) and (ListViewFrames.SelCount = 1)
    and (Frame.Animation.FrameIndex(Frame) < Frame.Animation.FrameCount - 1);
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameTopExecute(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  Frame := GetFirstSelectedFrame;

  if (Animation = nil) or (Frame = nil) then
    Exit;

  Animation.MoveFrameToTop(Frame);
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameTopUpdate(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
begin
  Frame := GetFirstSelectedFrame;
  ActionMoveFrameTop.Enabled := (Frame <> nil) and (ListViewFrames.SelCount = 1)
    and (Frame.Animation.FrameIndex(Frame) > 0);
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameLeftExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  I: Integer;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSelectedFrames.GetCurrentSelection;
  for I := 0 to FSelectedFrames.FrameCount -1 do
  begin
    Animation.MoveFrameLeft(FSelectedFrames.Frame[I]);
  end;
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameLeftUpdate(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
begin
  Frame := GetFirstSelectedFrame;
  ActionMoveFrameLeft.Enabled := (Frame <> nil)
    and (Frame.Animation.FrameIndex(Frame) > 0);
end;

procedure TSpriteSheetEditorForm.ActionAddFrameExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  I: Integer;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  if CastleOpenImageDialog.Execute then
  begin
    for I := 0 to CastleOpenImageDialog.URLCount - 1 do
      Animation.AddFrame(CastleOpenImageDialog.URLs[I]);
  end;
end;

procedure TSpriteSheetEditorForm.ActionAddAnimationUpdate(Sender: TObject);
begin
  ActionAddAnimation.Enabled := FSpriteSheet <> nil;
end;

procedure TSpriteSheetEditorForm.ActionAddAnimationExecute(Sender: TObject);
var
  OldSelectNewAnimation: Boolean;
begin
  OldSelectNewAnimation := FSelectNewAnimation;
  try
    FSelectNewAnimation := true;
    FSpriteSheet.AddAnimation(FSpriteSheet.ProposeAnimationName);
  finally
    FSelectNewAnimation := OldSelectNewAnimation;
  end;
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionOpenSpriteSheetExecute(Sender: TObject);
begin
  if not CloseSpriteSheet then
    Exit;
  if OpenDialog.Execute then
    OpenSpriteSheet(OpenDialog.URL);
end;

procedure TSpriteSheetEditorForm.ActionDeleteAnimationExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSpriteSheet.RemoveAnimation(Animation);
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionDeleteAnimationUpdate(Sender: TObject);
begin
  ActionDeleteAnimation.Enabled := (GetCurrentAnimation <> nil);
end;

procedure TSpriteSheetEditorForm.ActionDeleteFrameExecute(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  I: Integer;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  FSelectedFrames.GetCurrentSelection;
  for I := 0 to FSelectedFrames.FrameCount -1 do
  begin
    Animation.RemoveFrame(FSelectedFrames.Frame[I]);
  end;
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionDeleteFrameUpdate(Sender: TObject);
begin
  ActionDeleteFrame.Enabled := (GetFirstSelectedFrame <> nil);
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
  FSelectNewAnimation := true;
  FSpriteSheet := nil;
  FSelectedFrames := TSelectedFrames.Create(ListViewFrames);
  FWindowTitle := Caption;
  SetAtlasError('');
  SetAtlasWarning('');
  NewSpriteSheet;
end;

procedure TSpriteSheetEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSelectedFrames);
  FreeAndNil(FSpriteSheet);
end;

procedure TSpriteSheetEditorForm.FormShow(Sender: TObject);
begin
  {$ifdef LCLGTK2}
  { On GTK2 actions in SpeedButtons are not updated after window show
    See: https://bugs.freepascal.org/view.php?id=38345 }
  LastMouse.Button := 0;
  LastMouse.ClickCount := 0;
  LastMouse.Down := False;
  LastMouse.MousePos := Point(0, 0);
  LastMouse.Time := 0;
  LastMouse.WinControl := nil;

  { Update actions state after FormShow - I think this is next GTK2 bug. }
  UpdateActions;
  {$endif}
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
  if GetCurrentPreviewMode = pmFrame then
    UpdatePreview(GetCurrentPreviewMode, ffgDontForceFileRegen);
end;

procedure TSpriteSheetEditorForm.RadioFrameChange(Sender: TObject);
begin
  { In case of changing from frame preview to animation preview,
    animation preview can be outdated so we force file regeneration }
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.SpinEditMaxAtlasSizeChange(Sender: TObject);
begin
end;

procedure TSpriteSheetEditorForm.SpinEditMaxAtlasSizeEditingDone(Sender: TObject
  );
begin
  FSpriteSheet.SetMaxAtlasSize(SpinEditMaxAtlasSize.Value,
    SpinEditMaxAtlasSize.Value);
end;

function TSpriteSheetEditorForm.CloseSpriteSheet: Boolean;
begin
  // TODO: ask for save
  ClearFrames;
  ClearAnimations;
  FreeAndNil(FSpriteSheet);
  Result := True;
end;

procedure TSpriteSheetEditorForm.AssignEventsToSpriteSheet;
begin
  FSpriteSheet.OnModifiedStateChanged := @ModifiedStateChanged;
  FSpriteSheet.OnAnimationAdded := @AnimationAdded;
  FSpriteSheet.OnAnimationMoved := @AnimationMoved;
  FSpriteSheet.BeforeAnimationRemoved := @BeforeAnimationRemoved;
  FSpriteSheet.OnFrameAdded := @FrameAdded;
  FSpriteSheet.OnFrameMoved := @FrameMoved;
  FSpriteSheet.BeforeFrameRemoved := @BeforeAnimationFrameRemoved;
  FSpriteSheet.OnMaxAtlasSizeChanged := @MaxAtlasSizeChanged;
end;

procedure TSpriteSheetEditorForm.LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
var
  I: Integer;
begin
  for I := 0 to SpriteSheet.AnimationCount - 1 do
    AddAnimationToListView(SpriteSheet.AnimationByIndex(I));

  if SpriteSheet.AnimationCount > 0 then
    ListViewAnimations.ItemIndex := 0;
end;

function TSpriteSheetEditorForm.AddAnimationToListView(
  const Animation: TCastleSpriteSheetAnimation): TListItem;
var
  ListItem: TListItem;
begin
  ListItem := ListViewAnimations.Items.Add;
  ListItem.Caption := Animation.Name;
  ListItem.Data := Animation;
  Result := ListItem;
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
    { When frame is smaller than CurrentFrameIconSize do not resize frame }
    if (Frame.FrameWidth = CurrentFrameIconSize.X) and
      (Frame.FrameHeight = CurrentFrameIconSize.Y) then
    begin
      ResizedFrameImage := Frame.MakeImageCopy;
    end else
    if (Frame.FrameWidth < CurrentFrameIconSize.X) and
      (Frame.FrameHeight < CurrentFrameIconSize.Y) then
    begin
      ResizedFrameImage := Frame.CenterOnBiggerImage(CurrentFrameIconSize.X,
        CurrentFrameIconSize.Y);
    end else
      ResizedFrameImage := Frame.MakeResized(CurrentFrameIconSize.X,
        CurrentFrameIconSize.Y);

    IntfImage := FrameImageToLazImage(ResizedFrameImage, CurrentFrameIconSize.X,
      CurrentFrameIconSize.Y);

    Bitmap := TBitmap.Create;
    Bitmap.LoadFromIntfImage(IntfImage);
    ImageIndex := ImageListFrames.Add(Bitmap, nil);
  finally
    FreeAndNil(ResizedFrameImage);
    FreeAndNil(IntfImage);
    FreeAndNil(Bitmap);
  end;

  ListItem := ListViewFrames.Items.Add;
  ListItem.Caption := FrameTitle(FrameNo, Frame);
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

    CurrentFrameIconSize := Animation.GetBigestFrameSize(MaxFrameIconSize, MaxFrameIconSize);

    if (CurrentFrameIconSize.X = 0) or (CurrentFrameIconSize.Y = 0) then
    begin
      CurrentFrameIconSize.X := DefaultFrameIconSize;
      CurrentFrameIconSize.Y := DefaultFrameIconSize;
    end;

    ImageListFrames.Width := CurrentFrameIconSize.X;
    ImageListFrames.Height := CurrentFrameIconSize.Y;
  end;

begin
  PrepareImageList;

  for I := 0 to Animation.FrameCount - 1 do
    AddFrameToListView(Animation.Frame[I], I);
end;

function TSpriteSheetEditorForm.GetFirstSelectedFrame: TCastleSpriteSheetFrame;
var
  Item: TListItem;
begin
  Item := ListViewFrames.Selected;
  if (Item <> nil) and TSelectedFrames.IsFrameListItem(Item) then
    Result := TCastleSpriteSheetFrame(Item.Data)
  else
    Result := nil;
end;


function TSpriteSheetEditorForm.GetLastSelectedFrame: TCastleSpriteSheetFrame;
var
  Item: TListItem;
begin
  Item := ListViewFrames.LastSelected;
  if (Item <> nil) and TSelectedFrames.IsFrameListItem(Item) then
    Result := TCastleSpriteSheetFrame(Item.Data)
  else
    Result := nil;
end;

function TSpriteSheetEditorForm.FrameTitle(const FrameNo: Integer;
  const Frame: TCastleSpriteSheetFrame): string;
begin
  Result := IntToStr(FrameNo) + ' - ' + IntToStr(Frame.FrameWidth) +
     'x' + IntToStr(Frame.FrameHeight);
end;

procedure TSpriteSheetEditorForm.UpdateFrameTitles;
var
  I: Integer;
  AListItem: TListItem;
begin
  for I := 0 to ListViewFrames.Items.Count - 1 do
  begin
    AListItem := ListViewFrames.Items[I];
    if TSelectedFrames.IsFrameListItem(AListItem) then
    begin
      AListItem.Caption := FrameTitle(I + 1,
        TCastleSpriteSheetFrame(AListItem.Data));
    end;
  end;
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
    FViewport.Setup2D;
    FViewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
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
    FPreviewScene.Exists := true;
    ShowPreviewControl(true);
  end;

  procedure LoadAnimationInPreview(
    const Animation: TCastleSpriteSheetAnimation);
  begin
    ShowPreviewControl(true);
    if FPreviewScene = nil then
      RegenerateAnimationPreviewFile;

    if (Animation = nil) or (Animation.FrameCount = 0) or
      (not CheckAtlasMinSize) then
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
  WritelnLog('Update Preview');
  case PreviewModesToUpdate of
    pmAnimation:
      begin
        { only when we know file should change }
        if ForcePreviewFileRegen = ffgDoForceFileRegen then
          RegenerateAnimationPreviewFile;
        LoadAnimationInPreview(GetCurrentAnimation);
      end;
    pmFrame:
      LoadFrameInPreview(GetFirstSelectedFrame);
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
  try
    CreatePreviewUIIfNeeded;
    if not CheckAtlasMinSize then
      Exit;

    TempURL := URIIncludeSlash(ProjectForm.ProjectPathUrl) + 'temp/preview.castle-sprite-sheet';
    ForceDirectories(ExtractFilePath(URIToFilenameSafe(TempURL)));

    FSpriteSheet.Save(TempURL, true);

    FPreviewScene.Scale := Vector3(1.0, 1.0, 1.0);
    FPreviewScene.Load(TempURL);
    if not FPreviewScene.LocalBoundingBox.IsEmpty then
      FViewport.Camera.Orthographic.Width := FPreviewScene.LocalBoundingBox.MaxSize
    else
      FViewport.Camera.Orthographic.Width := DefaultFrameIconSize;
  except
    on E: Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

procedure TSpriteSheetEditorForm.RegenerateFramePreviewFile(const Frame: TCastleSpriteSheetFrame);
var
  TempURL: String;
begin
  TempURL := URIIncludeSlash(ProjectForm.ProjectPathUrl) + 'temp/frame_preview.png';
  ForceDirectories(ExtractFilePath(URIToFilenameSafe(TempURL)));
  Frame.SaveFrameImage(TempURL);

  CreatePreviewUIIfNeeded;

  FPreviewScene.Scale := Vector3(1.0, 1.0, 1.0);
  FPreviewScene.Load(TempURL);

  { Always set to the width of the first frame because we want to see the
    size difference }
  FViewport.Camera.Orthographic.Width := Frame.Animation.Frame[0].FrameWidth
end;

procedure TSpriteSheetEditorForm.UpdateWindowCaption;
var
  ModifiedMark: String;
  FileName: String;
begin
  if FSpriteSheet = nil then
  begin
    Caption := FWindowTitle;
    Exit;
  end;

  if FSpriteSheet.IsModified then
    ModifiedMark := '*'
  else
    ModifiedMark := '';

  if FSpriteSheet.URL = '' then
    FileName := 'unsaved sprite sheet'
  else
    FileName := FSpriteSheet.URL;

  Caption := ModifiedMark + FileName + ' | ' + FWindowTitle;
end;

procedure TSpriteSheetEditorForm.SetAtlasError(const Message: String);
begin
  LabelAtlasSizeError.Caption := Message;
end;

procedure TSpriteSheetEditorForm.SetAtlasWarning(const Message: String);
begin
  ImageAtlasSizeWarning.Visible := (Message <> '');
  LabelAtlasWarning.Caption := Message;
end;

function TSpriteSheetEditorForm.CheckAtlasMinSize: Boolean;
var
  MinAtlasWidth, MinAtlasHeight: Integer;
begin
  Result := true;
  FSpriteSheet.GetMinAtlasSize(MinAtlasWidth, MinAtlasHeight);

  if (MinAtlasWidth > FSpriteSheet.MaxAtlasWidth) or
    (MinAtlasHeight > FSpriteSheet.MaxAtlasHeight) then
  begin
    SetAtlasError(Format(
      'Max atlas size to small to fit all frames %d needed.',
      [Max(MinAtlasWidth, MinAtlasHeight)])
    );
    Result := false;
  end else
    SetAtlasError('');

  { check power of two }
  if not IsPowerOf2(Max(FSpriteSheet.MaxAtlasHeight,
    FSpriteSheet.MaxAtlasWidth)) then
    SetAtlasWarning('We adwise using power of 2 size.')
  else
    SetAtlasWarning('');
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
  { Is changed Animation the current one? }
  if FrameToRemove.Animation <> GetCurrentAnimation then
    Exit;

  { Remove frames from ListViewFrames }
  for I := ListViewFrames.Items.Count - 1 downto 0 do
  begin
    if TCastleSpriteSheetFrame(ListViewFrames.Items[I].Data) = FrameToRemove then
      ListViewFrames.Items.Delete(I);
  end;

  UpdateFrameTitles;
  { No preview update here, becouse this is "Before" event so update preview do
    nothing. Preview update is in action Execute function. }
end;

procedure TSpriteSheetEditorForm.FrameMoved(
  const Frame: TCastleSpriteSheetFrame; const OldIndex, NewIndex: Integer);
var
  Selection: TSelectedFrames;
begin
  { Is changed Animation the current one? }
  if Frame.Animation = GetCurrentAnimation then
  begin
    { I know this looks weird, but after rearranging the order of items
      (at least on GTK2) the selection is completely broken. So here I need
      remember selection and restore it after item rearranging. }
    Selection := TSelectedFrames.Create(ListViewFrames);
    try
      Selection.GetCurrentSelection;
      ListViewFrames.ClearSelection;

      ListViewFrames.Items.Move(OldIndex, NewIndex);
      UpdateFrameTitles;
    finally
      Selection.SetSelection;
      FreeAndNil(Selection);
    end;
  end;

  { Preview update must be always called here (make preview always correct). }
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.AnimationMoved(
  const Animation: TCastleSpriteSheetAnimation; const OldIndex,
  NewIndex: Integer);
begin
  ListViewAnimations.Items.Move(OldIndex, NewIndex);
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.MaxAtlasSizeChanged(const MaxWidth,
  MaxHeight: Integer);
begin
  SpinEditMaxAtlasSize.Value := Max(MaxWidth, MaxHeight);
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.AnimationAdded(
  NewAnimation: TCastleSpriteSheetAnimation);
var
  NewAnimationItem: TListItem;
begin
  NewAnimationItem := AddAnimationToListView(NewAnimation);
  if FSelectNewAnimation then
  begin
    ListViewAnimations.Selected := NewAnimationItem;
    ListViewAnimations.Selected.MakeVisible(false);
  end;
end;

procedure TSpriteSheetEditorForm.FrameAdded(NewFrame: TCastleSpriteSheetFrame);
begin
  { Is changed Animation the current one? }
  if NewFrame.Animation <> GetCurrentAnimation then
    Exit;

  try
    { Cases when we need reload all frames:
      - When added frame is bigger than CurrentFrameIconSize but smaller than
        MaxFrameIconSize
      - When there are no frames we need use LoadFrames to prepare ListView,
        maybe that should be changed to more obvious solution }
    if (NewFrame.Animation.FrameCount = 1) or (
      ((NewFrame.FrameWidth < MaxFrameIconSize) and
      (NewFrame.FrameWidth > CurrentFrameIconSize.X)) or
      ((NewFrame.FrameHeight < MaxFrameIconSize) and
      (NewFrame.FrameHeight > CurrentFrameIconSize.Y))) then
    begin
      ClearFrames;
      LoadFrames(NewFrame.Animation);
      Exit;
    end;

    { Just add frame on last position }
    AddFrameToListView(NewFrame, NewFrame.Animation.FrameCount);
  finally
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
  end;
end;

procedure TSpriteSheetEditorForm.ClearAnimations;
begin
  ListViewAnimations.Items.Clear;
end;

procedure TSpriteSheetEditorForm.OpenSpriteSheet(const URL: String);
begin
  try
    if not CloseSpriteSheet then
      Exit;
    FSpriteSheet :=  TCastleSpriteSheet.Create;
    FSpriteSheet.OnModifiedStateChanged := @ModifiedStateChanged;
    FSpriteSheet.Load(URL);
    UpdateWindowCaption;
    LoadAnimations(FSpriteSheet);
    AssignEventsToSpriteSheet;
  except
    on E:Exception do
    begin
      ErrorBox(E.Message);
      FreeAndNil(FSpriteSheet);
      NewSpriteSheet;
    end;
  end;
end;

procedure TSpriteSheetEditorForm.NewSpriteSheet;
begin
  try
    if not CloseSpriteSheet then
      Exit;
    FSpriteSheet :=  TCastleSpriteSheet.Create;
    UpdateWindowCaption;
    LoadAnimations(FSpriteSheet);
    AssignEventsToSpriteSheet;
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
  except
    on E:Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

end.

