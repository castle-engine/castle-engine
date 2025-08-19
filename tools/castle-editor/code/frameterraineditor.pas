unit FrameTerrainEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ActnList, StdCtrls, Spin,
  Buttons,
  CastleTerrain, CastleViewport, CastleUiControls;

type
  TTerrainEditMode = (
    temRaise,
    temLower,
    temFlatten
  );

  TTerrainEditorFrame = class(TFrame)
    ActionChangeHeightMapSize: TAction;
    ActionChooseFlattenTerrainTool: TAction;
    ActionChooseLowerTerrainTool: TAction;
    ActionChooseRaiseTerrainTool: TAction;
    ActionListDesign: TActionList;
    ActionSaveTerrain: TAction;
    ActionSaveTerrainAs: TAction;
    ButtonHeightMapChangeSize: TButton;
    ButtonSaveTerrain: TButton;
    ButtonSaveTerrainAs: TButton;
    FloatSpinLevelRingThickness: TFloatSpinEdit;
    FloatSpinLowerRingThickness: TFloatSpinEdit;
    FloatSpinRaiseRingThickness: TFloatSpinEdit;
    GroupBoxFlattenTerrainSettings: TGroupBox;
    GroupBoxHeightMapSize: TGroupBox;
    GroupBoxLowerTerrainSettings: TGroupBox;
    GroupBoxRaiseTerrainSettings: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelLowerRingThickness: TLabel;
    LabelMaxHeight: TLabel;
    LabelMaxHeight1: TLabel;
    LabelRaiseRingThickness: TLabel;
    LabelRaiseRingThickness1: TLabel;
    LabelStrength: TLabel;
    LabelStrength1: TLabel;
    LabelStrength2: TLabel;
    LabelStrength3: TLabel;
    LabelStrength4: TLabel;
    LabelStrength5: TLabel;
    PanelEditTerrain: TPanel;
    PanelEditTerrainSave: TPanel;
    PanelExtraTools: TPanel;
    SpeedButtonFlattenTerrain: TSpeedButton;
    SpeedButtonLevelCircle: TSpeedButton;
    SpeedButtonLevelCone: TSpeedButton;
    SpeedButtonLevelPyramid: TSpeedButton;
    SpeedButtonLevelSquare: TSpeedButton;
    SpeedButtonLowerCircle: TSpeedButton;
    SpeedButtonLowerCone: TSpeedButton;
    SpeedButtonLowerCylinder: TSpeedButton;
    SpeedButtonLowerFixedSquare: TSpeedButton;
    SpeedButtonLowerPyramid: TSpeedButton;
    SpeedButtonLowerRing: TSpeedButton;
    SpeedButtonLowerSquare: TSpeedButton;
    SpeedButtonLowerTerrain: TSpeedButton;
    SpeedButtonRaiseCircle: TSpeedButton;
    SpeedButtonRaiseCone: TSpeedButton;
    SpeedButtonRaiseCylinder: TSpeedButton;
    SpeedButtonRaiseFixedSquare: TSpeedButton;
    SpeedButtonRaisePyramid: TSpeedButton;
    SpeedButtonRaiseRing: TSpeedButton;
    SpeedButtonRaiseSquare: TSpeedButton;
    SpeedButtonRaiseTerrain: TSpeedButton;
    SpinEditHeightMapHeight: TSpinEdit;
    SpinEditHeightMapWidth: TSpinEdit;
    SpinEditLevelBrushRotation: TSpinEdit;
    SpinEditLevelBrushSize: TSpinEdit;
    SpinEditLevelHeight: TSpinEdit;
    SpinEditLevelStrength: TSpinEdit;
    SpinEditLowerBrushRotation: TSpinEdit;
    SpinEditLowerBrushSize: TSpinEdit;
    SpinEditLowerStrength: TSpinEdit;
    SpinEditRaiseBrushRotation: TSpinEdit;
    SpinEditRaiseBrushSize: TSpinEdit;
    SpinEditRaiseMaxHeight: TSpinEdit;
    SpinEditRaiseStrength: TSpinEdit;
    procedure ActionChangeHeightMapSizeExecute(Sender: TObject);
    procedure ActionChangeHeightMapSizeUpdate(Sender: TObject);
    procedure ActionChooseFlattenTerrainToolExecute(Sender: TObject);
    procedure ActionChooseLowerTerrainToolExecute(Sender: TObject);
    procedure ActionChooseRaiseTerrainToolExecute(Sender: TObject);
    procedure ActionSaveTerrainAsExecute(Sender: TObject);
    procedure ActionSaveTerrainExecute(Sender: TObject);
    procedure SpinEditHeightMapWidthChange(Sender: TObject);
  private
    { All these are non-nil only during editing. }
    FEditingTerrain: TCastleTerrain;
    FTerrainViewport: TCastleViewport;
    FTerrainContainer: TCastleContainer;
    { Edit mode, meaningful only if FEditingTerrain <> nil. }
    FEditMode: TTerrainEditMode;
    FTerrainLevelHeight: Byte;
    FIsFirstTerrainLevelFrame: Boolean;
    FWasTerrainUrlUpdate: Boolean;
    procedure CommonAfterEditingStartStop;
    procedure UpdateChoosenTerrainTool;
  public
    constructor Create(AOwner: TComponent); override;

    { Currently edited terrain, or @nil if editing. }
    property EditingTerrain: TCastleTerrain read FEditingTerrain;

    { Just a shortcut for EditingTerrain <> nil check. }
    function IsEditing: Boolean;

    procedure StartEditing(const Terrain: TCastleTerrain;
      const ATerrainViewport: TCastleViewport;
      const ATerrainContainer: TCastleContainer);
    procedure StopEditing;

    { Keep calling every frame.
      Does nothing if not in editing mode. }
    procedure UpdateEditing;
  end;

implementation

uses StrUtils, Math,
  CastleUtils, CastleTransform, CastleKeysMouse, CastleVectors,
  CastleDialogs, CastleUriUtils,
  EditorUtils;

{$R *.lfm}

{ TTerrainEditorFrame }

constructor TTerrainEditorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // We will make Self visible/enabled in StartEditing
  SetEnabledVisible(Self, false);
  Align := alClient;
end;

procedure TTerrainEditorFrame.StartEditing(const Terrain: TCastleTerrain;
  const ATerrainViewport: TCastleViewport;
  const ATerrainContainer: TCastleContainer);
begin
  FEditingTerrain := Terrain;
  FEditingTerrain.Mode := ctmShader;
  FTerrainViewport := ATerrainViewport;
  FTerrainContainer := ATerrainContainer;

  SpinEditHeightMapHeight.Value := Terrain.Editor.GetHeightMapSize.Y;
  SpinEditHeightMapWidth.Value := Terrain.Editor.GetHeightMapSize.X;
  ActionChangeHeightMapSizeUpdate(ActionChangeHeightMapSize);
  FIsFirstTerrainLevelFrame := true;
  FWasTerrainUrlUpdate := false;

  CommonAfterEditingStartStop;
end;

procedure TTerrainEditorFrame.CommonAfterEditingStartStop;
begin
  SetEnabledVisible(Self, IsEditing);
end;

procedure TTerrainEditorFrame.StopEditing;
var
  UrlBuf: String;
begin
  if FEditingTerrain <> nil then
    FEditingTerrain.Mode := ctmMesh;
  FEditingTerrain := nil;
  FTerrainViewport := nil;
  FTerrainContainer := nil;

  // TODO: better way to update TCastleTerrainImage
  if FWasTerrainUrlUpdate then
  begin
    if (FEditingTerrain.Data <> nil) and
       (FEditingTerrain.Data is TCastleTerrainImage) then
    begin
      UrlBuf := TCastleTerrainImage(FEditingTerrain.Data).Url;
      TCastleTerrainImage(FEditingTerrain.Data).Url := '';
      TCastleTerrainImage(FEditingTerrain.Data).Url := UrlBuf;
    end;
  end;

  CommonAfterEditingStartStop;
end;

procedure TTerrainEditorFrame.ActionChangeHeightMapSizeUpdate(Sender: TObject);
begin
  ActionChangeHeightMapSize.Enabled :=
    IsEditing and
    ( (SpinEditHeightMapHeight.Value <> EditingTerrain.Editor.GetHeightMapSize.Y) or
      (SpinEditHeightMapWidth.Value <> EditingTerrain.Editor.GetHeightMapSize.X)
    );
end;

procedure TTerrainEditorFrame.ActionChangeHeightMapSizeExecute(Sender: TObject);
begin
  FEditingTerrain.Editor.SetHeightMapSize(Vector2Integer(
    SpinEditHeightMapWidth.Value,
    SpinEditHeightMapHeight.Value));
end;

procedure TTerrainEditorFrame.ActionChooseFlattenTerrainToolExecute(Sender: TObject);
begin
  FEditMode := temFlatten;
  UpdateChoosenTerrainTool;
end;

procedure TTerrainEditorFrame.ActionChooseLowerTerrainToolExecute(Sender: TObject);
begin
  FEditMode := temLower;
  UpdateChoosenTerrainTool;
end;

procedure TTerrainEditorFrame.ActionChooseRaiseTerrainToolExecute(Sender: TObject);
begin
  FEditMode := temRaise;
  UpdateChoosenTerrainTool;
end;

procedure TTerrainEditorFrame.ActionSaveTerrainAsExecute(Sender: TObject);
var
  SaveHeightImageDialog: TCastleSaveImageDialog;
begin
  Assert(FEditingTerrain <> nil);

  SaveHeightImageDialog := TCastleSaveImageDialog.Create(nil);
  try
    SaveHeightImageDialog.AdviceDataDirectory := true;
    SaveHeightImageDialog.InitialDir := UriToFilenameSafe('castle-data:/');
    if SaveHeightImageDialog.Execute then
    begin
      try
        FEditingTerrain.Editor.SaveHeightMap(SaveHeightImageDialog.Url);
        // update current image also in case the user selected
        // the same file that is loaded
        FWasTerrainUrlUpdate := true;
      except
        on E: Exception do
          ErrorBox(E.Message);
      end;
    end;
  finally
    FreeAndNil(SaveHeightImageDialog);
  end;
end;

procedure TTerrainEditorFrame.ActionSaveTerrainExecute(Sender: TObject);
var
  Terrain: TCastleTerrain;
  TerrainUrl: String;

  function GetTerrainUrl: String;
  begin
    if (FEditingTerrain.Data <> nil) and
       (FEditingTerrain.Data is TCastleTerrainImage) then
      Exit(TCastleTerrainImage(Terrain.Data).Url);

    Result := '';
  end;

begin
  Assert(FEditingTerrain <> nil);

  TerrainUrl := GetTerrainUrl;
  if TerrainUrl <> '' then
  begin
    FEditingTerrain.Editor.SaveHeightMap(TerrainUrl);
    FWasTerrainUrlUpdate := true;
  end
  else
    ActionSaveTerrainAsExecute(Sender);
end;

procedure TTerrainEditorFrame.SpinEditHeightMapWidthChange(Sender: TObject);
begin
  ActionChangeHeightMapSizeUpdate(ActionChangeHeightMapSize);
end;

procedure TTerrainEditorFrame.UpdateEditing;

  function GetTerrainBrush: TCastleTerrainBrush;
  begin
    case FEditMode of
      temRaise:
        begin
          if SpeedButtonRaiseFixedSquare.Down then
            Exit(ctbFixedSquare);

          if SpeedButtonRaiseSquare.Down then
            Exit(ctbSquare);

          if SpeedButtonRaisePyramid.Down then
            Exit(ctbPyramid);

          if SpeedButtonRaiseCircle.Down then
            Exit(ctbCircle);

          if SpeedButtonRaiseCone.Down then
            Exit(ctbCone);

          if SpeedButtonRaiseRing.Down then
            Exit(ctbRing);

          if SpeedButtonRaiseCylinder.Down then
            Exit(ctbLyingCylinder);
        end;
      temLower:
        begin
          if SpeedButtonLowerFixedSquare.Down then
            Exit(ctbFixedSquare);

          if SpeedButtonLowerSquare.Down then
            Exit(ctbSquare);

          if SpeedButtonLowerPyramid.Down then
            Exit(ctbPyramid);

          if SpeedButtonLowerCircle.Down then
            Exit(ctbCircle);

          if SpeedButtonLowerCone.Down then
            Exit(ctbCone);

          if SpeedButtonLowerRing.Down then
            Exit(ctbRing);

          if SpeedButtonLowerCylinder.Down then
            Exit(ctbLyingCylinder);
        end;
      temFlatten:
        begin
          if SpeedButtonLevelSquare.Down then
            Exit(ctbSquare);

          if SpeedButtonLevelPyramid.Down then
            Exit(ctbPyramid);

          if SpeedButtonLevelCircle.Down then
            Exit(ctbCircle);

          if SpeedButtonLevelCone.Down then
            Exit(ctbCone);
        end;
      {$ifndef CASE_ANALYSIS}
      else raise EInternalError.Create('GetTerrainBrush: FEditMode?');
      {$endif}
    end;
  end;

  function GetTerrainBrushSize: Integer;
  begin
    case FEditMode of
      temRaise: Exit(SpinEditRaiseBrushSize.Value);
      temLower: Exit(SpinEditLowerBrushSize.Value);
      temFlatten: Exit(SpinEditLevelBrushSize.Value);
      {$ifndef CASE_ANALYSIS}
      else raise EInternalError.Create('GetTerrainBrushSize: FEditMode?');
      {$endif}
    end;
  end;

  function GetTerrainToolStrength: Byte;
  begin
    case FEditMode of
      temRaise: Exit(SpinEditRaiseStrength.Value);
      temLower: Exit(SpinEditLowerStrength.Value);
      temFlatten: Exit(SpinEditLevelStrength.Value);
      {$ifndef CASE_ANALYSIS}
      else raise EInternalError.Create('GetTerrainToolStrength: FEditMode?');
      {$endif}
    end;
  end;

  function GetTerrainBrushRotation: Single;
  begin
    case FEditMode of
      temRaise: Exit(SpinEditRaiseBrushRotation.Value);
      temLower: Exit(SpinEditLowerBrushRotation.Value);
      temFlatten: Exit(SpinEditLevelBrushRotation.Value);
      {$ifndef CASE_ANALYSIS}
      else raise EInternalError.Create('GetTerrainBrushRotation: FEditMode?');
      {$endif}
    end;
  end;

  function GetTerrainMaxHeight: Byte;
  begin
    case FEditMode of
      temRaise: Exit(SpinEditRaiseMaxHeight.Value);
      temLower: Exit(0);
      temFlatten: Exit(FTerrainLevelHeight);
      {$ifndef CASE_ANALYSIS}
      else raise EInternalError.Create('GetTerrainMaxHeight: FEditMode?');
      {$endif}
    end;
  end;

  function GetTerrainRingThickness: Single;
  begin
    case FEditMode of
      temRaise: Exit(FloatSpinRaiseRingThickness.Value);
      temLower: Exit(FloatSpinLowerRingThickness.Value);
      temFlatten: Exit(FloatSpinLevelRingThickness.Value);
      {$ifndef CASE_ANALYSIS}
      else raise EInternalError.Create('GetTerrainRingThickness: FEditMode?');
      {$endif}
    end;
  end;

var
  RayCollision: TRayCollision;
  HitInfo: TRayCollisionNode;
begin
  if FEditingTerrain = nil then
    Exit;

  if FTerrainContainer.MousePressed = [buttonLeft] then
  begin
    RayCollision := FTerrainViewport.MouseRayHit;
    if (RayCollision <> nil) and RayCollision.Info(HitInfo) then
    begin
      if FIsFirstTerrainLevelFrame and ActionChooseFlattenTerrainTool.Checked then
      begin
        FTerrainLevelHeight := FEditingTerrain.Editor.TerrainHeight(HitInfo.Point);
        SpinEditLevelHeight.Value := FTerrainLevelHeight;
      end;

      FEditingTerrain.Editor.AlterTerrain(FTerrainContainer, HitInfo.Point, GetTerrainBrush,
        GetTerrainBrushSize, GetTerrainToolStrength, DegToRad(GetTerrainBrushRotation),
        GetTerrainMaxHeight, GetTerrainRingThickness);

      FIsFirstTerrainLevelFrame := false;
    end;
  end else
  begin
    FIsFirstTerrainLevelFrame := true;

    // terrain flattening: change level value based on mouse position
    if ActionChooseFlattenTerrainTool.Checked and (FTerrainContainer.MousePressed = []) then
    begin
      RayCollision := FTerrainViewport.MouseRayHit;
      if (RayCollision <> nil) and RayCollision.Info(HitInfo) then
      begin
        FTerrainLevelHeight := FEditingTerrain.Editor.TerrainHeight(HitInfo.Point);
        SpinEditLevelHeight.Value := FTerrainLevelHeight;
      end;
    end;
  end;
end;

function TTerrainEditorFrame.IsEditing: Boolean;
begin
  Result := FEditingTerrain <> nil;
end;

procedure TTerrainEditorFrame.UpdateChoosenTerrainTool;
var
  VisibleGroup: TGroupBox;
begin
  GroupBoxRaiseTerrainSettings.Visible := FEditMode = temRaise;
  GroupBoxLowerTerrainSettings.Visible := FEditMode = temLower;
  GroupBoxFlattenTerrainSettings.Visible := FEditMode = temFlatten;

  case FEditMode of
    temRaise: VisibleGroup := GroupBoxRaiseTerrainSettings;
    temLower: VisibleGroup := GroupBoxLowerTerrainSettings;
    temFlatten: VisibleGroup := GroupBoxFlattenTerrainSettings;
    {$ifndef CASE_ANALYSIS}
    else raise EInternalError.Create('UpdateChoosenTerrainTool: FEditMode?');
    {$endif}
  end;

  PanelEditTerrainSave.AnchorSideTop.Control := VisibleGroup;
end;

end.
