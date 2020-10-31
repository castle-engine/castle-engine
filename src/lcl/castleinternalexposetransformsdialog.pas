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

{ Dialog for editing TCastleTranform.ExposeTransforms, used by CastlePropEdits. }
unit CastleInternalExposeTransformsDialog;

{$I castleconf.inc}

interface

uses
  Generics.Collections, Contnrs,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls;

type
  TExposeTransformSelectionItem = class
    Name: String;
    Selected: Boolean;
    ExistsInScene: Boolean;
  end;

  TExposeTransformSelection = class(specialize TObjectList<TExposeTransformSelectionItem>)
    { Find TExposeTransformSelectionItem with given Name, or @nil if not found. }
    function FindName(const S: String): TExposeTransformSelectionItem;
    { Convert to TStrings. Called is responsible for freeing it. }
    function ToList: TStrings;
  end;

  TExposeTransformsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckTransforms: TScrollBox;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    Checkboxes: TObjectList;
  public
    Selection: TExposeTransformSelection;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSelectionUi;
  end;

implementation

uses CastleLclUtils;

{$R *.lfm}

{ TExposeTransformSelection -------------------------------------------------- }

function TExposeTransformSelection.FindName(const S: String): TExposeTransformSelectionItem;
begin
  for Result in Self do
    if Result.Name = S then
      Exit;
  Result := nil;
end;

function TExposeTransformSelection.ToList: TStrings;
var
  I: TExposeTransformSelectionItem;
begin
  Result := TStringList.Create;
  for I in Self do
    if I.Selected then
      Result.Add(I.Name);
end;

{ TExposeTransformsDialog ---------------------------------------------------- }

constructor TExposeTransformsDialog.Create(AOwner: TComponent);
begin
  inherited;
  Selection := TExposeTransformSelection.Create(true);
  Checkboxes := TObjectList.Create(false);
end;

destructor TExposeTransformsDialog.Destroy;
begin
  FreeAndNil(Selection);
  FreeAndNil(Checkboxes);
  inherited;
end;

procedure TExposeTransformsDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
begin
  if ModalResult = mrOK then
  begin
    Assert(Selection.Count = Checkboxes.Count);
    for I := 0 to Selection.Count - 1 do
    begin
      Selection[I].Selected := TCheckbox(Checkboxes[I]).Checked;
    end;
  end;
end;

procedure TExposeTransformsDialog.UpdateSelectionUi;
var
  SelItem: TExposeTransformSelectionItem;
  S: String;
  Checkbox: TCheckBox;
begin
  Checkboxes.Clear;
  for SelItem in Selection do
  begin
    S := SelItem.Name;
    if not SelItem.ExistsInScene then
      S += ' (not detected in current scene)';
    Checkbox := TCheckBox.Create(Self);
    Checkbox.Caption := S;
    Checkbox.Checked := SelItem.Selected;
    Checkbox.Parent := CheckTransforms;
    Checkboxes.Add(Checkbox);
  end;
end;

end.

