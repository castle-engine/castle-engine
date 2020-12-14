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
    procedure LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
    procedure ClearFrames;
    procedure LoadFrames(const Animation: TCastleSpriteSheetAnimation);
  public
    procedure OpenSpriteSheet(const URL: String);
  end;

var
  SpriteSheetEditorForm: TSpriteSheetEditorForm;

implementation

{$R *.lfm}

uses GraphType, IntfGraphics,
  CastleImages, CastleLog, CastleVectors,
  EditorUtils;

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
  if ListBoxAnimations.ItemIndex > -1 then
  begin
    LoadAnimation(ListBoxAnimations.Items.Objects[ListBoxAnimations.ItemIndex] as TCastleSpriteSheetAnimation);
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

procedure TSpriteSheetEditorForm.LoadAnimation(const Animation: TCastleSpriteSheetAnimation);
begin
  FloatSpinEditFPS.Value := Animation.FramesPerSecond;
  ClearFrames;
  LoadFrames(Animation);
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

