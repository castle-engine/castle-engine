{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display instructions. }
unit GameViewInstructions;

interface

uses Classes,
  CastleControls, CastleWindow, CastleUIControls;

type
  TViewInstructions = class(TCastleView)
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
  ViewInstructions: TViewInstructions;

implementation

uses SysUtils,
  CastleComponentSerialize,
  GameViewPlay;

constructor TViewInstructions.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewinstructions.castle-user-interface';
end;

procedure TViewInstructions.Start;
begin
  inherited;

  ButtonClose.OnClick := {$ifdef FPC}@{$endif}ClickClose;

  { do not pass clicks to view underneath }
  InterceptInput := true;
end;

procedure TViewInstructions.ClickClose(Sender: TObject);
begin
  Container.PopView(Self);
end;

end.
