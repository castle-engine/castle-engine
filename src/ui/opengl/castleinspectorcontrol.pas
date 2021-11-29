{
  Copyright 2015-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Inspect Castle Game Engine state at runtime (TCastleInspectorControl).
  Invoke this automatically in debug builds by F12 (see @link(TCastleContainer.InspectorKey)). }
unit CastleInspectorControl;

{$I castleconf.inc}

interface

uses Classes,
  CastleControls, CastleUIControls, CastleColors, CastleRectangles,
  CastleVectors, CastleKeysMouse;

type
  { Inspect Castle Game Engine state.
    Show log, current UI and viewports state.
    Invoke this automatically in debug builds by F12 (see @link(TCastleContainer.InspectorKey)). }
  TCastleInspectorControl = class(TCastleUserInterfaceFont)
  strict private
    Background: TCastleRectangleControl;
    LabelAll: TCastleLabel;
    LabelFocus: TCastleLabel;
    TransparencySlider: TCastleFloatSlider;
    CheckboxShowEvenNotExisting: TCastleCheckbox;
    CheckboxShowEvenInternal: TCastleCheckbox;
    CheckboxShowSize: TCastleCheckbox;
    function ControlColor(const C: TCastleUserInterface): TCastleColor;
    function ControlDescription(const C: TCastleUserInterface): string;
    procedure TransparencyChange(Sender: TObject);
    procedure UpdateDisplay(Sender: TObject);
    function ControlShow(const C: TCastleUserInterface): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderOverChildren; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  published
    property KeepInFront stored false;
    property FullSize stored false;
  end;

implementation

uses SysUtils, StrUtils,
  CastleStringUtils, CastleGLUtils;

const
  Margin = 10;

constructor TCastleInspectorControl.Create(AOwner: TComponent);
var
  UpperControls: TCastleVerticalGroup;
begin
  inherited;

  // adjust inherited published properties
  FullSize := true;
  KeepInFront := true;

  Background := TCastleRectangleControl.Create(Self);
  Background.SetTransient;
  Background.FullSize := true;
  // partially-transparent gray
  Background.Color := Vector4(0.25, 0.25, 0.25, 0.9);
  InsertFront(Background);

  UpperControls := TCastleVerticalGroup.Create(Self);
  UpperControls.SetTransient;
  UpperControls.Anchor(hpRight, -Margin);
  UpperControls.Anchor(vpTop, -Margin);
  Background.InsertFront(UpperControls);

  TransparencySlider := TCastleFloatSlider.Create(Self);
  TransparencySlider.SetTransient;
  TransparencySlider.Caption := 'Overlay Transparency';
  TransparencySlider.Value := 1 - Background.Color.W;
  TransparencySlider.OnChange := @TransparencyChange;
  TransparencySlider.SmallFont := false;
  TransparencySlider.Width := 300;
  TransparencySlider.Height := 30;
  UpperControls.InsertFront(TransparencySlider);

  CheckboxShowEvenNotExisting := TCastleCheckbox.Create(Self);
  CheckboxShowEvenNotExisting.SetTransient;
  CheckboxShowEvenNotExisting.Caption := 'Show Even Not Existing';
  CheckboxShowEvenNotExisting.Checked := false;
  CheckboxShowEvenNotExisting.OnChange := @UpdateDisplay;
  CheckboxShowEvenNotExisting.TextColor := White;
  CheckboxShowEvenNotExisting.CheckboxColor := White;
  UpperControls.InsertFront(CheckboxShowEvenNotExisting);

  CheckboxShowEvenInternal := TCastleCheckbox.Create(Self);
  CheckboxShowEvenInternal.SetTransient;
  CheckboxShowEvenInternal.Caption := 'Show Even Internal';
  CheckboxShowEvenInternal.Checked := false;
  CheckboxShowEvenInternal.OnChange := @UpdateDisplay;
  CheckboxShowEvenInternal.TextColor := White;
  CheckboxShowEvenInternal.CheckboxColor := White;
  UpperControls.InsertFront(CheckboxShowEvenInternal);

  CheckboxShowSize := TCastleCheckbox.Create(Self);
  CheckboxShowSize.SetTransient;
  CheckboxShowSize.Caption := 'Show Size';
  CheckboxShowSize.Checked := false;
  CheckboxShowSize.OnChange := @UpdateDisplay;
  CheckboxShowSize.TextColor := White;
  CheckboxShowSize.CheckboxColor := White;
  UpperControls.InsertFront(CheckboxShowSize);

  LabelAll := TCastleLabel.Create(Self);
  LabelAll.SetTransient;
  LabelAll.Anchor(hpLeft, Margin);
  LabelAll.Anchor(vpBottom, Margin);
  LabelAll.Html := true;
  Background.InsertFront(LabelAll);

  LabelFocus := TCastleLabel.Create(Self);
  LabelFocus.SetTransient;
  LabelFocus.Anchor(hpRight, -Margin);
  LabelFocus.Anchor(vpBottom, Margin);
  LabelFocus.Html := true;
  Background.InsertFront(LabelFocus);
end;

destructor TCastleInspectorControl.Destroy;
begin
  inherited;
end;

function TCastleInspectorControl.ControlColor(const C: TCastleUserInterface): TCastleColor;
const
  Transparency = 0.1;
var
  Hash: PtrUInt;
begin
  { simple hash from https://gist.github.com/badboy/6267743
    Note that the default GetHashCode in FPC simply returns PtrInt(C),
    so is not really distributed in any interesting way. }
  {$I norqcheckbegin.inc}
  Hash := PtrUInt(Pointer(C));
  Hash := (not Hash) + (Hash shl 15);
  Hash := Hash xor (Hash shr 12);
  Hash := Hash + (Hash shl 2);
  Hash := Hash xor (Hash shr 4);
  Hash := Hash * 2057;
  Hash := Hash xor (Hash shr 16);
  {$I norqcheckend.inc}

  Result := Vector4(
    0.5 + (Hash and $FF) / 128,
    0.5 + ((Hash shr 8) and $FF) / 128,
    0.5 + ((Hash shr 16) and $FF) / 128,
    1);
end;

function TCastleInspectorControl.ControlDescription(const C: TCastleUserInterface): string;
begin
  if C.Name <> '' then
    Result := C.Name + ':' + C.ClassName
  else
    Result := C.ClassName;
end;

function TCastleInspectorControl.ControlShow(const C: TCastleUserInterface): Boolean;
var
  IsInternal: Boolean;
begin
  IsInternal := (C = Self) or (csTransient in C.ComponentStyle);
  Result :=
    (C.GetExists or CheckboxShowEvenNotExisting.Checked) and
    ((not IsInternal) or CheckboxShowEvenInternal.Checked);
end;

procedure TCastleInspectorControl.RenderOverChildren;

  function InvertColorRGB(const C: TCastleColor): TCastleColor;
  begin
    Result := C;
    Result[0] := 1 - Result[0];
    Result[1] := 1 - Result[1];
    Result[2] := 1 - Result[2];
  end;

var
  RR: TFloatRectangle;
  UiCol: TCastleColor;
  Ui: TCastleUserInterface;
begin
  inherited;

  // exit if turned off by F12
  if not Background.Exists then Exit;

  RR := RenderRect;
  for Ui in Container.Focus do
    if ControlShow(Ui) then
    begin
      RR := Ui.RenderRect;
      if RR.IsEmpty then Continue;
      UiCol := ControlColor(Ui);
      Theme.Draw(RR, tiActiveFrame, UIScale, UiCol);
      Font.Print(RR.Left, RR.Top - Font.RowHeight, UiCol, ControlDescription(Ui));
    end;
end;

function TCastleInspectorControl.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyF12) then
    Background.Exists := not Background.Exists;
    // do not set event as handled, to allow game to handle it too
end;

function TCastleInspectorControl.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;

  // exit if turned off by F12
  if not Background.Exists then Exit;

  UpdateDisplay(nil);
end;

procedure TCastleInspectorControl.UpdateDisplay(Sender: TObject);
var
  NewText: TCastleStringList;

  function ControlColorizeHtml(const C: TCastleUserInterface; const S: String): String;
  begin
    Result := '<font color="#' + ColorToHex(ControlColor(C)) + '">' + S + '</font>';
  end;

  procedure AddControlDescription(const C: TCastleUserInterface; const Level: Integer);
  var
    I: Integer;
    S: string;
  begin
    if ControlShow(C) then
    begin
      S := ControlDescription(C);
      if CheckboxShowSize.Checked then
        S := S + ' ' + C.RenderRect.ToString;
      if not C.GetExists then
        S := S + ' (hidden)';
      if C.Focused then
        S := S + ' (focused)';
      S := DupeString('- ', Level) + S;
      S := ControlColorizeHtml(C, S);
      NewText.Add(S);

      for I := 0 to C.ControlsCount - 1 do
        AddControlDescription(C.Controls[I], Level + 1);
    end;
  end;

var
  I: Integer;
  C: TCastleUserInterface;
  S: String;
begin
  NewText := TCastleStringList.Create;
  try
    NewText.Add('All controls:');
    for I := 0 to Container.Controls.Count - 1 do
      AddControlDescription(Container.Controls[I], 0);
    LabelAll.Text.Assign(NewText);
  finally FreeAndNil(NewText) end;

  NewText := TCastleStringList.Create;
  try
    if Container.Focus.Count <> 0 then
    begin
      NewText.Add('Container.Focus (controls that can receive input):');
      for C in Container.Focus do
        if ControlShow(C) then
        begin
          S := ControlDescription(C);
          S := ControlColorizeHtml(C, S);
          NewText.Add('  ' + S);
        end;
    end;
    LabelFocus.Text.Assign(NewText);
  finally FreeAndNil(NewText) end;
end;

procedure TCastleInspectorControl.TransparencyChange(Sender: TObject);
begin
  Background.Color := Vector4(Background.Color.XYZ, 1 - TransparencySlider.Value);
end;

end.
