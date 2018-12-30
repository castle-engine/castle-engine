{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unit (soldier) on a map. }
unit GameUnit;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleComponentSerialize;

type
  TUnitKind = (utAlien1, utAlien2, utHuman1, utHuman2);

  TUnit = class(TComponent)
  private
    class var
      UiTemplate: TSerializedComponent;
    var
      ImageIcon: TCastleImageControl;
      LabelAttack: TCastleLabel;
      LabelDefense: TCastleLabel;
      LabelMovement: TCastleLabel;
  public
    Ui: TCastleUserInterface;
    constructor Create(AOwner: TComponent); override;
    procedure Initialize(const AKind: TUnitKind; const Attack, Defense, Movement: Cardinal);
  end;

implementation

uses SysUtils;

constructor TUnit.Create(AOwner: TComponent);
begin
  inherited;
  if UiTemplate = nil then
    UiTemplate := TSerializedComponent.Create('castle-data:/unit.castle-user-interface');
  Ui := UiTemplate.UserInterfaceLoad(Self);

  ImageIcon := FindRequiredComponent('ImageIcon') as TCastleImageControl;
  LabelAttack := FindRequiredComponent('LabelAttack') as TCastleLabel;
  LabelDefense := FindRequiredComponent('LabelDefense') as TCastleLabel;
  LabelMovement := FindRequiredComponent('LabelMovement') as TCastleLabel;
end;

procedure TUnit.Initialize(const AKind: TUnitKind; const Attack, Defense, Movement: Cardinal);
const
  UnitIconUrls: array [TUnitKind] of string  =
  ( 'castle-data:/units/alien1.png',
    'castle-data:/units/alien2.png',
    'castle-data:/units/human1.png',
    'castle-data:/units/human2.png'
  );
begin
  ImageIcon.URL := UnitIconUrls[AKind];
  LabelAttack.Caption := IntToStr(Attack);
  LabelDefense.Caption := IntToStr(Defense);
  LabelMovement.Caption := IntToStr(Movement);
end;

finalization
  FreeAndNil(TUnit.UiTemplate);
end.
