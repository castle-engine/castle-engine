{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, handling CGE controls. }
unit GameViewMain;

interface

uses SysUtils, Classes,
  CastleUIControls, CastleControls, CastleOnScreenMenu;

type
  TViewMain = class(TCastleView)
  strict private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    OnScreenMenu1: TCastleOnScreenMenu;
    MainButton: TCastleButton;

    procedure MenuClick(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
  public
    OnMenuClick: TNotifyEvent;
    OnMainButtonClick: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

implementation

{ TViewMain ------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Add additional user interface elements created manually,
    by code, just to show that we can. }
  procedure AddUserInterfaceFromCode;
  var
    Button: TCastleButton;
  begin
    Button := TCastleButton.Create(FreeAtStop);
    Button.Caption := 'Button added from code';
    Button.Anchor(hpRight, -10);
    Button.Anchor(vpBottom, 120);
    InsertFront(Button);

    OnScreenMenu1 := TCastleOnScreenMenu.Create(FreeAtStop);
    OnScreenMenu1.Bottom := 140;
    OnScreenMenu1.Left := 400;
    OnScreenMenu1.Add('one', @MenuClick);
    OnScreenMenu1.Add('two', @MenuClick);
    OnScreenMenu1.Add('three', @MenuClick);
    InsertFront(OnScreenMenu1);
  end;

begin
  inherited;

  MainButton := DesignedComponent('MainButton') as TCastleButton;
  MainButton.OnClick := @MainButtonClick;

  AddUserInterfaceFromCode;
end;

procedure TViewMain.MenuClick(Sender: TObject);
begin
  if Assigned(OnMenuClick) then
    OnMenuClick(Sender); // pass original Sender to TForm1 event
end;

procedure TViewMain.MainButtonClick(Sender: TObject);
begin
  if Assigned(OnMainButtonClick) then
    OnMainButtonClick(Sender); // pass original Sender to TForm1 event
end;

end.
