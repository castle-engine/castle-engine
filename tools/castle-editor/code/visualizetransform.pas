{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualize TCastleTransform selection and dragging to transform (TVisualizeTransform). }
unit VisualizeTransform;

interface

uses Classes, SysUtils,
  CastleColors, CastleTransform, CastleDebugTransform, CastleScene;

type
  TVisualizeOperation = (voSelect, voTranslate, voRotate, voScale);

  { Visualize TCastleTransform selection and dragging to transform. }
  TVisualizeTransform = class(TComponent)
  private
    FHover: Boolean;
    FOperation: TVisualizeOperation;
    FParent: TCastleTransform;
    Box: TDebugTransformBox;
    Gizmo: array [TVisualizeOperation] of TCastleScene;
    procedure SetOperation(const AValue: TVisualizeOperation);
    procedure SetParent(const AValue: TCastleTransform);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent; const AHover: Boolean); reintroduce;
    destructor Destroy; override;
    { Currently visualized TCastleTransform instance.
      @nil to not visualize anything. }
    property Parent: TCastleTransform read FParent write SetParent;
    property Operation: TVisualizeOperation read FOperation write SetOperation
      default voSelect;
  end;

var
  ColorHover, ColorSelected, ColorHoverAndSelected: TCastleColor;

implementation

uses ProjectUtils;

{ TVisualizeTransform ------------------------------------------------------ }

constructor TVisualizeTransform.Create(AOwner: TComponent; const AHover: Boolean);

  function CreateGizmoScene: TCastleScene;
  begin
    Result := TCastleScene.Create(Self);
    Result.Collides := false;
    //Result.Pickable := false;
    Result.CastShadowVolumes := false;
    Result.ExcludeFromStatistics := true;
    Result.InternalExcludeFromParentBoundingVolume := true;
  end;

begin
  inherited Create(AOwner);
  FHover := AHover;

  Box := TDebugTransformBox.Create(Self);
  if FHover then
    Box.BoxColor := ColorOpacity(ColorHover, 0.75)
  else
    Box.BoxColor := ColorOpacity(ColorSelected, 0.75);
  Box.Exists := true;

  // Gizmo[voSelect] remains nil
  Gizmo[voTranslate] := CreateGizmoScene;
  Gizmo[voTranslate].Load(EditorApplicationData + 'translate.glb');
  Gizmo[voRotate] := CreateGizmoScene;
  Gizmo[voRotate].Load(EditorApplicationData + 'rotate.glb');
  Gizmo[voScale] := CreateGizmoScene;
  Gizmo[voScale].Load(EditorApplicationData + 'scale.glb');
end;

destructor TVisualizeTransform.Destroy;
begin
  { set to nil by SetParent, to detach free notification }
  Parent := nil;
  inherited;
end;

procedure TVisualizeTransform.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParent) then
    { set to nil by SetParent to clean all connections nicely }
    Parent := nil;
end;

procedure TVisualizeTransform.SetParent(const AValue: TCastleTransform);
begin
  if FParent = AValue then Exit;

  if FParent <> nil then
  begin
    FParent.RemoveFreeNotification(Self);
    Box.Parent := nil;
    if Gizmo[Operation] <> nil then
      FParent.Remove(Gizmo[Operation]);
  end;

  FParent := AValue;

  if FParent <> nil then
  begin
    Box.Parent := FParent;
    if Gizmo[Operation] <> nil then
      FParent.Add(Gizmo[Operation]);
    FParent.FreeNotification(Self);
  end;
end;

procedure TVisualizeTransform.SetOperation(const AValue: TVisualizeOperation);
begin
  if FOperation = AValue then Exit;

  if (FParent <> nil) and (Gizmo[Operation] <> nil) then
    FParent.Remove(Gizmo[Operation]);

  FOperation := AValue;

  if (FParent <> nil) and (Gizmo[Operation] <> nil) then
    FParent.Add(Gizmo[Operation]);
end;

initialization
  ColorHover := HexToColor('fffba0'); // desaturated yellow
  ColorSelected := White;
  ColorHoverAndSelected := Yellow;
end.

