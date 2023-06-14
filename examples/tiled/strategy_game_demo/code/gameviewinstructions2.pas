{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display instructions, part 2 - about units and map. }
unit GameViewInstructions2;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewInstructions2 = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonClose: TCastleButton;
  strict private
    procedure ClickClose(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewInstructions2: TViewInstructions2;

implementation

constructor TViewInstructions2.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewinstructions2.castle-user-interface';
end;

procedure TViewInstructions2.Start;
begin
  inherited;

  ButtonClose.OnClick := {$ifdef FPC}@{$endif}ClickClose;

  { do not pass clicks to view underneath }
  InterceptInput := true;
end;

procedure TViewInstructions2.ClickClose(Sender: TObject);
begin
  Container.PopView(Self);
end;

end.
