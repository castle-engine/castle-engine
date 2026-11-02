{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View to show what some NPC says, also allow to open URL (TViewTalk). }
unit GameViewTalk;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Show what some NPC says, also allow to open URL. }
  TViewTalk = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonOK: TCastleButton;
    LabelSpeaker: TCastleLabel;
    LabelMessage: TCastleLabel;
    ButtonOpenUrl: TCastleButton;
  private
    procedure ClickOK(Sender: TObject);
    procedure ClickUrl(Sender: TObject);
  public
    // Set before Start.
    Speaker, Message, Url: String;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewTalk: TViewTalk;

implementation

uses CastleOpenDocument;

constructor TViewTalk.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewtalk.castle-user-interface';
  InterceptInput := true;
  // preload, to make 1st dialog appear instantly
  DesignPreload := true;
end;

procedure TViewTalk.Start;
begin
  inherited;
  { Executed once when view starts. }
  ButtonOK.OnClick := {$ifdef FPC}@{$endif} ClickOK;
  ButtonOpenUrl.OnClick := {$ifdef FPC}@{$endif} ClickUrl;

  LabelSpeaker.Caption := Speaker;
  LabelMessage.Caption := Message;
  ButtonOpenUrl.Exists := Url <> '';
end;

function TViewTalk.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  // because of InterceptInput, inherited always returns true, so don't do below:
  // if Result then Exit;

  if Event.IsKey(keyEscape) or
     Event.IsController(gbSouth) then
  begin
    ClickOK(nil);
    Exit(true);
  end;

  if Event.IsKey('?') or
     Event.IsController(gbNorth) then
  begin
    ClickUrl(nil);
    Exit(true);
  end;
end;

procedure TViewTalk.ClickOK(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TViewTalk.ClickUrl(Sender: TObject);
begin
  OpenUrl(Url);
end;

end.
