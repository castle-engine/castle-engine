{
  Copyright 2020-2021 Andrzej Kilijanski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit FormSpriteSheetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Buttons, ActnList, StdCtrls, Spin, Menus,
  CastleControl, CastleDialogs, CastleScene, CastleInternalSpriteSheet, CastleVectors,
  CastleViewport, CastleCameras,
  DataModuleIcons;

type
  TSpriteSheetEditorForm = class(TForm)
    ActionCloseSpriteSheetEditor: TAction;
    ActionImportAtlas: TAction;
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
    ButtonCloseWindow: TButton;
    CastleControlPreview: TCastleControl;
    CastleOpenImageDialog: TCastleOpenImageDialog;
    CastleImportAtlasDialog: TCastleOpenImageDialog;
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
    MainMenuItemImportAtlas: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemImportAtlas: TMenuItem;
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
    procedure ActionCloseSpriteSheetEditorExecute(Sender: TObject);
    procedure ActionCreateNewAnimationFromSelectionExecute(Sender: TObject);
    procedure ActionCreateNewAnimationFromSelectionUpdate(Sender: TObject);
    procedure ActionImportAtlasExecute(Sender: TObject);
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
    procedure ActionSaveSpriteSheetAsExecute(Sender: TObject);
    procedure ActionSaveSpriteSheetExecute(Sender: TObject);
    procedure FloatSpinEditFPSChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewAnimationsDragDrop(Sender, Source: TObject; X, Y: Integer
      );
    procedure ListViewAnimationsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListViewAnimationsEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure ListViewAnimationsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewFramesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PopupMenuCloseGTK2Fix(Sender: TObject);
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
      { Max size for frame image in list view }
      MaxFrameIconSize = 256;
      { Default size for frame image in list view }
      DefaultFrameIconSize = 128;
      { Preview margin }
      PreviewMargin = 1;
    var
      FSpriteSheet: TCastleSpriteSheet;
      FPreviewScene: TCastleScene;
      FViewport: TCastleViewport;
      FNavigation: TCastle2DNavigation;
      FWindowTitle: String;
      CurrentFrameIconSize: TVector2Integer; // current frame size in list view
      { Should we select added animation (not always desirable) }
      FSelectNewAnimation: Boolean;
      { RC for LockUpdatePreview/UnlockUpdatePreview }
      FLockUpdatePReviewRC: Integer;

    function ProposeSaveSpriteSheet: Boolean;
    { Returns true if sprite sheet was closed }
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
    { Regenerates and load animation preview }
    procedure RegenerateAnimationPreview;
    { Regenerates and load frame preview }
    procedure RegenerateFramePreview(const Frame: TCastleSpriteSheetFrame);
    { Way to lock update preview, useful for multi frames functions - just
      increases FLockUpdatePReviewRC }
    procedure LockUpdatePreview;
    { Decreases FLockUpdatePReviewRC and calls UpdatePreview() when
      FLockUpdatePReviewRC < 1 }
    procedure UnlockUpdatePreview;

    procedure UpdateWindowCaption;
    procedure SetAtlasError(const Message: String);
    procedure SetAtlasWarning(const Message: String);

    { Check atlas size }
    function CheckAtlasMinSize: Boolean;
    procedure LoadAtlasSize;

    // events:
    procedure URLChanged(Sender: TObject);
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
    procedure OpenSpriteSheet(const URL: String; const ProposeSave: Boolean);
    procedure NewSpriteSheet;
  end;

var
  SpriteSheetEditorForm: TSpriteSheetEditorForm;

implementation

{$R *.lfm}

uses GraphType, IntfGraphics, Math, LCLIntf, LCLType, FPImage,
  CastleImages, CastleLog, CastleUtils, CastleURIUtils, CastleFilesUtils,
  X3DNodes,
  EditorUtils,
  FormProject, FormImportAtlas, FormImportStarling
  {$ifdef LCLGTK2},Gtk2Globals, LCLVersion{$endif};

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

    { About sdAll:

      In theory, sdBelow should be OK.
      FrameListView.Selected returns the first selected item,
      and iterating with sdBelow should return the rest.
      However LCL WinAPI seems to have a weird bug, with sdBelow we
      - may not detect all selected
      - may have infinite loop when only one item is selected.

      The sdAll behaves correctly.
    }
    Item := FrameListView.GetNextItem(Item, sdAll, [lisSelected]);
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

procedure TSpriteSheetEditorForm.ActionCloseSpriteSheetEditorExecute(
  Sender: TObject);
begin
  Close;
end;

procedure TSpriteSheetEditorForm.ActionCreateNewAnimationFromSelectionExecute(
  Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
  NewAnimation: TCastleSpriteSheetAnimation;
  I: Integer;
  OldSelectNewAnimation: Boolean;
  SelectedFrames: TSelectedFrames;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  SelectedFrames := TSelectedFrames.Create(ListViewFrames);
  try
    SelectedFrames.GetCurrentSelection;

    if SelectedFrames.FrameCount = 0 then
      Exit;

    OldSelectNewAnimation := FSelectNewAnimation;
    try
      { We don't want to jump to new animation in this case }
      FSelectNewAnimation := false;
      NewAnimation := FSpriteSheet.AddAnimation(FSpriteSheet.ProposeAnimationName);
    finally
      FSelectNewAnimation := OldSelectNewAnimation;
    end;

    for I := 0 to SelectedFrames.FrameCount -1 do
    begin
      NewAnimation.AddFrameCopy(SelectedFrames.Frame[I]);
    end;
  finally
    FreeAndNil(SelectedFrames);
  end;
  UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
end;

procedure TSpriteSheetEditorForm.ActionCreateNewAnimationFromSelectionUpdate(
  Sender: TObject);
begin
  ActionCreateNewAnimationFromSelection.Enabled := (GetFirstSelectedFrame <> nil);
end;

procedure TSpriteSheetEditorForm.ActionImportAtlasExecute(Sender: TObject);
begin
  if CastleImportAtlasDialog.Execute then
  begin
    ImportAtlasForm.Initialize(FSpriteSheet, CastleImportAtlasDialog.URL);
    ImportAtlasForm.ShowModal;
  end;
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
  SelectedFrames: TSelectedFrames;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  SelectedFrames := TSelectedFrames.Create(ListViewFrames);
  try
    SelectedFrames.GetCurrentSelection;
    LockUpdatePreview;
    try
      for I := SelectedFrames.FrameCount - 1 downto 0 do
        Animation.MoveFrameRight(SelectedFrames.Frame[I]);
    finally
      UnlockUpdatePreview;
    end;
  finally
    FreeAndNil(SelectedFrames);
  end;
end;

procedure TSpriteSheetEditorForm.ActionMoveFrameRightUpdate(Sender: TObject);
var
  Frame: TCastleSpriteSheetFrame;
begin
  Frame := GetLastSelectedFrame;
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
  SelectedFrames: TSelectedFrames;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  SelectedFrames := TSelectedFrames.Create(ListViewFrames);
  try
    SelectedFrames.GetCurrentSelection;
    LockUpdatePreview;
    try
      for I := 0 to SelectedFrames.FrameCount - 1 do
        Animation.MoveFrameLeft(SelectedFrames.Frame[I]);
    finally
      UnlockUpdatePreview;
    end;
  finally
    FreeAndNil(SelectedFrames);
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
    LockUpdatePreview;
    try
      for I := 0 to CastleOpenImageDialog.URLCount - 1 do
        Animation.AddFrame(CastleOpenImageDialog.URLs[I]);
    finally
      UnlockUpdatePreview;
    end;
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
var
  URLAnchor: String;
  URLToOpen, MimeType: String;
begin
  if not ProposeSaveSpriteSheet then
    Exit;
  if OpenDialog.Execute then
  begin
    URLToOpen := OpenDialog.URL;

    { Check: if file is Starling }
    MimeType := URIMimeType(URLToOpen);
    if (MimeType = 'application/x-starling-sprite-sheet') or
       (MimeType = 'application/xml') then
    begin
      { If file has anchors don't show import dialog }
      URIGetAnchor(URLToOpen, URLAnchor, true);
      if URLAnchor = '' then
      begin
        ImportStarlingForm.Initialize(URLToOpen);
        case ImportStarlingForm.ShowModal of
          mrCancel:
            Exit;
          mrOK:
            URLToOpen := ImportStarlingForm.URL;
        end;
      end;

    end;
    OpenSpriteSheet(URLToOpen, false);
  end;
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
  SelectedFrames: TSelectedFrames;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit;

  SelectedFrames := TSelectedFrames.Create(ListViewFrames);
  try
    SelectedFrames.GetCurrentSelection;
    LockUpdatePreview;
    try
      for I := 0 to SelectedFrames.FrameCount - 1 do
        Animation.RemoveFrame(SelectedFrames.Frame[I]);
    finally
      UnlockUpdatePreview;
    end;
  finally
    FreeAndNil(SelectedFrames);
  end;
  { UpdatePreview is not needed here because UnlockUpdatePreview calls that }
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

procedure TSpriteSheetEditorForm.ActionSaveSpriteSheetAsExecute(Sender: TObject
  );
begin
  SaveDialog.DefaultExt := 'castle-sprite-sheet';
  { In case FSpriteSheet.URL = '' (not yet saved), the save dialog shows correct UI. }
  SaveDialog.URL := FSpriteSheet.URL;
  if SaveDialog.Execute then
  begin
    try
      FSpriteSheet.Save(SaveDialog.URL, false);
    except
      on E: Exception do
        ErrorBox(E.Message);
    end;
  end;
end;

procedure TSpriteSheetEditorForm.ActionSaveSpriteSheetExecute(Sender: TObject);
begin
  if (FSpriteSheet.URL = '') or
     (URIMimeType(FSpriteSheet.URL) <> 'application/x-castle-sprite-sheet') then
    ActionSaveSpriteSheetAsExecute(Sender)
  else
    FSpriteSheet.Save(FSpriteSheet.URL, false);
end;

procedure TSpriteSheetEditorForm.FloatSpinEditFPSChange(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := GetCurrentAnimation;
  if Animation = nil then
    Exit; // ignore if no animation (TODO: SpinEditFPS should be disabled then)
  if not SameValue(Animation.FramesPerSecond, FloatSpinEditFPS.Value) then
  begin
    Animation.FramesPerSecond := FloatSpinEditFPS.Value;
    { To change frames per second file must be regenerated. }
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
  end;
end;

procedure TSpriteSheetEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := ProposeSaveSpriteSheet;

  if CanClose then
  begin
    CloseSpriteSheet;
    NewSpriteSheet;
  end;
end;

procedure TSpriteSheetEditorForm.FormCreate(Sender: TObject);

  {$ifdef LCLCocoa}
  procedure FixCocoa;
  var
    FirstCol: TListColumn;
  begin
    // on Cocoa, image list crashes TListView usage
    ListViewFrames.LargeImages := nil;

    // on Cocoa, list always behaves like ViewStyle = vsReport and shows nothing when no columns
    FirstCol := ListViewFrames.Columns.Add;
    FirstCol.Caption := 'Frame name';
    FirstCol.Width := 200;
  end;
  {$endif}


begin
  {$ifdef LCLCocoa}
  FixCocoa;
  {$endif}
  FSelectNewAnimation := true;
  FSpriteSheet := nil;
  FWindowTitle := Caption;
  SetAtlasError('');
  SetAtlasWarning('');
  NewSpriteSheet;
end;

procedure TSpriteSheetEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpriteSheet);
end;

procedure TSpriteSheetEditorForm.FormShow(Sender: TObject);
begin
  {$ifdef LCLGTK2}
    {$if (LCL_FULLVERSION >= 1080000) and (LCL_FULLVERSION < 2001200)}
    { On GTK2 SpeedButtons are frozen after window show from popup menu
      See: https://bugs.freepascal.org/view.php?id=38345 }
    LastMouse.Button := 0;
    LastMouse.ClickCount := 0;
    LastMouse.Down := False;
    LastMouse.MousePos := Point(0, 0);
    LastMouse.Time := 0;
    LastMouse.WinControl := nil;
    {$endif}
    { Update actions state after FormShow - I think this is next GTK2 bug. }
    UpdateActions;
  {$endif}

  { Adjust InitialDir values to make open/save dialogs natural, and clear URL.
    Do this in FormShow, as one instance of TSpriteSheetEditorForm may exist across
    many CGE projects being open. }
  OpenDialog.InitialDir := URIToFilenameSafe('castle-data:/');
  SaveDialog.InitialDir := URIToFilenameSafe('castle-data:/');
  CastleOpenImageDialog.InitialDir := URIToFilenameSafe('castle-data:/');
  CastleImportAtlasDialog.InitialDir := URIToFilenameSafe('castle-data:/');
  OpenDialog.URL := '';
  SaveDialog.URL := '';
  CastleOpenImageDialog.URL := '';
  CastleImportAtlasDialog.URL := '';
end;

procedure TSpriteSheetEditorForm.ListViewAnimationsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  SelectedFrames: TSelectedFrames;
  AnimationItem: TListItem;
  DestAnimation: TCastleSpriteSheetAnimation;
  SrcAnimation: TCastleSpriteSheetAnimation;
  I: Integer;
  CtrlPressed: Boolean;
begin
  if Source = ListViewFrames then
  begin
    SelectedFrames := TSelectedFrames.Create(ListViewFrames);
    try
      SelectedFrames.GetCurrentSelection;

      AnimationItem := ListViewAnimations.GetItemAt(X,Y);
      if AnimationItem = nil then
        Exit;

      DestAnimation := TCastleSpriteSheetAnimation(AnimationItem.Data);

      LockUpdatePreview;
      try

        { Copy frame }
        for I := 0 to SelectedFrames.FrameCount - 1 do
          DestAnimation.AddFrameCopy(SelectedFrames.Frame[I]);

        { Delete frames from current animation if ctrl not pressed
          https://forum.lazarus.freepascal.org/index.php?topic=39663.0 }
        {$ifdef darwin}
          CtrlPressed := (GetKeyState(VK_LWIN) < 0) or (GetKeyState(VK_RWIN) < 0);
        {$else}
          CtrlPressed := GetKeyState(VK_CONTROL) < 0;
        {$endif}
        SrcAnimation := GetCurrentAnimation;
        if (SrcAnimation <> nil) and (not CtrlPressed) then
        begin
          for I := 0 to SelectedFrames.FrameCount - 1 do
            SrcAnimation.RemoveFrame(SelectedFrames.Frame[I]);
        end;

      finally
        UnlockUpdatePreview;
      end;
    finally
      FreeAndNil(SelectedFrames);
    end;
  end;
end;

procedure TSpriteSheetEditorForm.ListViewAnimationsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = ListViewFrames)
    and (ListViewAnimations.GetItemAt(X,Y) <> nil);
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
    ErrorBox('Animation "' + AValue + '" already exist.');
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
  if not Selected then
    Exit;

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

procedure TSpriteSheetEditorForm.PopupMenuCloseGTK2Fix(Sender: TObject);
begin
  {$if defined(LCLGTK2) and (LCL_FULLVERSION >= 1080000) and (LCL_FULLVERSION < 2001200)}
  { On GTK2 SpeedButtons are frozen after close popup menu
    See: https://bugs.freepascal.org/view.php?id=38401 }
  LastMouse.Button := 0;
  LastMouse.ClickCount := 0;
  LastMouse.Down := False;
  LastMouse.MousePos := Point(0, 0);
  LastMouse.Time := 0;
  LastMouse.WinControl := nil;
  {$endif}
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

function TSpriteSheetEditorForm.ProposeSaveSpriteSheet: Boolean;
var
  Mr: TModalResult;
  SpriteSheetName: String;
begin
  Result := true;

  if FSpriteSheet = nil then
    Exit;

  if FSpriteSheet.IsModified then
  begin
    if FSpriteSheet.URL <> '' then
      SpriteSheetName := ' "' + FSpriteSheet.URL + '"'
    else
      SpriteSheetName := '';
    Mr := MessageDlg('Save sprite sheet',
      'Sprite sheet' + SpriteSheetName +
      ' was modified but not saved yet. Save it now?',
      mtConfirmation, mbYesNoCancel, 0);
    case Mr of
      mrYes: ActionSaveSpriteSheetExecute(Self);
      mrCancel: Result := false;
    end;
  end;
end;

function TSpriteSheetEditorForm.CloseSpriteSheet: Boolean;
begin
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
  FSpriteSheet.OnURLChanged := @URLChanged;
end;

procedure TSpriteSheetEditorForm.LoadAnimations(const SpriteSheet: TCastleSpriteSheet);
var
  I: Integer;
begin
  for I := 0 to SpriteSheet.AnimationCount - 1 do
    AddAnimationToListView(SpriteSheet.AnimationByIndex(I));

  if SpriteSheet.AnimationCount > 0 then
    ListViewAnimations.ItemIndex := 0;

  { We need update actions manually here, because some controls is not updated
    automatically }
  UpdateActions;
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
    CustomFPImage: TFPCustomImage;
  begin
    Result := TLazIntfImage.Create(0, 0);
    RawImage.Init;
    { TListView on WIN32 widgetset need RGB image }
    {$ifdef LCLWIN32}
    RawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(Width, Height);
    {$else}
    RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Width, Height);
    {$endif}
    RawImage.CreateData(True);
    Result.SetRawImage(RawImage);
    CustomFPImage := FrameImage.ToFpImage;
    try
      Result.CopyPixels(CustomFPImage);
    finally
      FreeAndNil(CustomFPImage);
    end;
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
      { On win32 widgetset image with transparency looks ugly, so we
        blend frame on white background }
      {$ifdef LCLWIN32}
      ResizedFrameImage := Frame.MakeImageCopyWithBg(Vector4(1, 1, 1, 1));
      {$else}
      ResizedFrameImage := Frame.MakeImageCopy;
      {$endif}
    end else
    if (Frame.FrameWidth < CurrentFrameIconSize.X) and
      (Frame.FrameHeight < CurrentFrameIconSize.Y) then
    begin
      { On win32 widgetset image with transparency looks ugly, so we
        blend frame on white background }
      {$ifdef LCLWIN32}
      ResizedFrameImage := Frame.CenterOnBiggerImage(CurrentFrameIconSize.X,
        CurrentFrameIconSize.Y, Vector4(1, 1, 1, 1));
      {$else}
      ResizedFrameImage := Frame.CenterOnBiggerImage(CurrentFrameIconSize.X,
        CurrentFrameIconSize.Y, Vector4(0, 0, 0, 0));
      {$endif}
    end else
    begin
      { On win32 widgetset image with transparency looks ugly, so we
        blend frame on white background }
      {$ifdef LCLWIN32}
      ResizedFrameImage := Frame.MakeResizedWithBg(CurrentFrameIconSize.X,
        CurrentFrameIconSize.Y, Vector4(1, 1, 1, 1));
      {$else}
      ResizedFrameImage := Frame.MakeResized(CurrentFrameIconSize.X,
        CurrentFrameIconSize.Y);
      {$endif}
    end;

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
  SelectedFrames: TSelectedFrames;
begin
  if ListViewFrames.Selected = nil then
    Exit(nil);

  SelectedFrames := TSelectedFrames.Create(ListViewFrames);
  try
    SelectedFrames.GetCurrentSelection;
    Result := SelectedFrames.Frame[SelectedFrames.FrameCount - 1];
  finally
    FreeAndNil(SelectedFrames);
  end;
end;

function TSpriteSheetEditorForm.FrameTitle(const FrameNo: Integer;
  const Frame: TCastleSpriteSheetFrame): string;
begin
  Result := IntToStr(FrameNo + 1) + ' - ' + IntToStr(Frame.FrameWidth) +
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
      AListItem.Caption := FrameTitle(I,
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
    FNavigation := TCastle2DNavigation.Create(Self);

    FViewport := TCastleViewport.InternalCreateNonDesign(Self);
    Assert(FViewport.Camera <> nil);
    FViewport.FullSize := true;
    FViewport.InsertFront(FNavigation);
    FViewport.Setup2D;
    FViewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
    CastleControlPreview.Controls.InsertFront(FViewport);

    FPreviewScene := TCastleScene.Create(Self);

    FViewport.Items.Add(FPreviewScene);
    FViewport.Items.MainScene := FPreviewScene; // TODO: removing MainScene assignment makes camera not OK, testcase: tentacles
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
    RegenerateFramePreview(Frame);
    FPreviewScene.Exists := true;
    ShowPreviewControl(true);
  end;

  procedure LoadAnimationInPreview(
    const Animation: TCastleSpriteSheetAnimation);
  begin
    ShowPreviewControl(true);
    if FPreviewScene = nil then
      RegenerateAnimationPreview;

    if (Animation = nil) or (Animation.FrameCount = 0) or
      (not CheckAtlasMinSize) then
    begin
      FPreviewScene.Exists := false;
      FPreviewScene.StopAnimation
    end else
    begin
      FPreviewScene.Exists := true;
      FViewport.AssignDefaultCamera;
      // set this after AssignDefaultCamera, as AssignDefaultCamera resets it
      FViewport.Camera.Orthographic.Width := Animation.Frame[0].FrameWidth + PreviewMargin * 2;
      FPreviewScene.PlayAnimation(Animation.Name, true, true);
    end;
  end;

begin
  if FLockUpdatePReviewRC > 0 then
    Exit;

  case PreviewModesToUpdate of
    pmAnimation:
      begin
        { only when we know file should change }
        if ForcePreviewFileRegen = ffgDoForceFileRegen then
          RegenerateAnimationPreview;
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

procedure TSpriteSheetEditorForm.RegenerateAnimationPreview;
begin
  try
    CreatePreviewUIIfNeeded;
    if not CheckAtlasMinSize then
      Exit;

    FPreviewScene.Scale := Vector3(1.0, 1.0, 1.0);
    FPreviewScene.Load(FSpriteSheet.ToX3D, true);
    FViewport.AssignDefaultCamera;
    // set this after AssignDefaultCamera, as AssignDefaultCamera resets it
    FViewport.Camera.Orthographic.Width := DefaultFrameIconSize + PreviewMargin * 2;
  except
    on E: Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

procedure TSpriteSheetEditorForm.RegenerateFramePreview(const Frame: TCastleSpriteSheetFrame);

  function Generate3XDWithImage: TX3DRootNode;
  var
    Material: TUnlitMaterialNode;
    Appearance: TAppearanceNode;
    Shape: TShapeNode;
    Tri: TTriangleSetNode;
    Tex: TPixelTextureNode;
    TexProperties: TTexturePropertiesNode;
    HalfFrameWidth: Single;
    HalfFrameHeight: Single;
    ShapeCoord: TCoordinateNode;
    ShapeTexCoord : TTextureCoordinateNode;
  begin
    Result := TX3DRootNode.Create;

    Material := TUnlitMaterialNode.Create;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    Shape := TShapeNode.Create;
    Shape.Appearance := Appearance;

    Tex := TPixelTextureNode.Create;
    Tex.FdImage.Value := Frame.MakeImageCopy;
    { No point in adjusting RepeatS/T: TextureProperties override it.
    Tex.RepeatS := false;
    Tex.RepeatT := false;
    }
    Appearance.Texture := Tex;

    TexProperties := TTexturePropertiesNode.Create;
    TexProperties.BoundaryModeS := bmClampToEdge;
    TexProperties.BoundaryModeT := bmClampToEdge;
    { Do not force "power of 2" size, which may prevent mipmaps.
      This seems like a better default (otherwise the resizing underneath
      may cause longer loading time, and loss of quality, if not expected).
      Consistent with X3DLoadInternalImage and sprite sheet loaders. }
    TexProperties.GuiTexture := true;
    Tex.TextureProperties := TexProperties;

    Tri := TTriangleSetNode.Create;
    Tri.Solid := false;

    HalfFrameWidth := Frame.FrameWidth * 0.5;
    HalfFrameHeight := Frame.FrameHeight * 0.5;

    ShapeCoord := TCoordinateNode.Create('coord');
    ShapeCoord.SetPoint([
        Vector3(-HalfFrameWidth, -HalfFrameHeight, 0),
        Vector3(HalfFrameWidth, -HalfFrameHeight, 0),
        Vector3(HalfFrameWidth, HalfFrameHeight, 0),
        Vector3(-HalfFrameWidth, -HalfFrameHeight, 0),
        Vector3(HalfFrameWidth, HalfFrameHeight, 0),
        Vector3(-HalfFrameWidth, HalfFrameHeight, 0)]);

    ShapeTexCoord := TTextureCoordinateNode.Create('texcoord');
    ShapeTexCoord.SetPoint([
         Vector2(0, 0),
         Vector2(1, 0),
         Vector2(1, 1),
         Vector2(0, 0),
         Vector2(1, 1),
         Vector2(0, 1)]);

    Tri.Coord := ShapeCoord;
    Tri.TexCoord := ShapeTexCoord;
    Shape.Geometry := Tri;

    Result.AddChildren(Shape);
  end;

begin
  CreatePreviewUIIfNeeded;

  FPreviewScene.Scale := Vector3(1.0, 1.0, 1.0);
  FPreviewScene.Load(Generate3XDWithImage, true);

  FViewport.AssignDefaultCamera;
  { Always set to the width of the first frame because we want to see the
    size difference }
  // set this after AssignDefaultCamera, as AssignDefaultCamera resets it
  FViewport.Camera.Orthographic.Width := Frame.Animation.Frame[0].FrameWidth;
end;

procedure TSpriteSheetEditorForm.LockUpdatePreview;
begin
  Inc(FLockUpdatePReviewRC);
end;

procedure TSpriteSheetEditorForm.UnlockUpdatePreview;
begin
  Dec(FLockUpdatePReviewRC);
  if FLockUpdatePReviewRC < 1 then
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
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
    FileName := 'New Sprite Sheet'
  else
    FileName := ExtractURIName(FSpriteSheet.URL);

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

  if (MinAtlasWidth  > FSpriteSheet.MaxAtlasWidth) or
     (MinAtlasHeight > FSpriteSheet.MaxAtlasHeight) then
  begin
    SetAtlasError(Format(
      'Maximum atlas size too small to fit all the frames. Increase it to at least %d.',
      [Max(MinAtlasWidth, MinAtlasHeight)])
    );
    Result := false;
  end else
    SetAtlasError('');

  { check power of two }
  if not IsPowerOf2(Max(FSpriteSheet.MaxAtlasHeight,
    FSpriteSheet.MaxAtlasWidth)) then
    SetAtlasWarning('We advise using power of 2 size.')
  else
    SetAtlasWarning('');
end;

procedure TSpriteSheetEditorForm.LoadAtlasSize;
begin
  if FSpriteSheet = nil then
    SpinEditMaxAtlasSize.Value := 1024
  else
    SpinEditMaxAtlasSize.Value := Max(FSpriteSheet.MaxAtlasHeight,
      FSpriteSheet.MaxAtlasWidth);
end;

procedure TSpriteSheetEditorForm.URLChanged(Sender: TObject);
begin
  UpdateWindowCaption;
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
  { Moving the animations on the list does not change the display,
    so it doesn't need a preview update }
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
  { On GTK2, actions at this point are not automatically updated }
  UpdateActions;
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
    AddFrameToListView(NewFrame, NewFrame.Animation.FrameCount - 1);
  finally
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
  end;
end;

procedure TSpriteSheetEditorForm.ClearAnimations;
begin
  ListViewAnimations.Items.Clear;
end;

procedure TSpriteSheetEditorForm.OpenSpriteSheet(const URL: String;
  const ProposeSave: Boolean);
begin
  try
    if ProposeSave and (not ProposeSaveSpriteSheet) then
      Exit;
    CloseSpriteSheet;
    FSpriteSheet :=  TCastleSpriteSheet.Create(true); // edit mode
    FSpriteSheet.OnModifiedStateChanged := @ModifiedStateChanged;
    FSpriteSheet.Load(URL);
    LoadAtlasSize;
    UpdateWindowCaption;
    LoadAnimations(FSpriteSheet);
    AssignEventsToSpriteSheet;
    FLockUpdatePReviewRC := 0;
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
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
    if not ProposeSaveSpriteSheet then
      Exit;
    CloseSpriteSheet;
    FSpriteSheet :=  TCastleSpriteSheet.Create(true);
    LoadAtlasSize;
    UpdateWindowCaption;
    LoadAnimations(FSpriteSheet);
    AssignEventsToSpriteSheet;
    FLockUpdatePReviewRC := 0;
    UpdatePreview(GetCurrentPreviewMode, ffgDoForceFileRegen);
  except
    on E:Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

end.
