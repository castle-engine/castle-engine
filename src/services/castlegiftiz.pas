{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Giftiz (http://www.giftiz.com/) integration (TGiftiz). }
unit CastleGiftiz;

{$I castleconf.inc}

interface

uses Classes,
  CastleImages, CastleControls, CastleStringUtils;

type
  { Giftiz (http://www.giftiz.com/) integration.
    Only on Android (will simply do nothing on other platforms).

    Usage:

    @orderedList(
      @item(Call @link(TGiftiz.MissionComplete) to notify Giftiz about mission completion.)
      @item(Use @link(TGiftizButton) to show on UI the Giftiz button.
        It's updated/hidden automatically based on Giftiz state.)
      @item(To include the necessary integration code in your Android project,
        declare your Android project type as "integrated" with
        the "giftiz" component inside CastleEngineManifest.xml.
        See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .)
    ) }
  TGiftiz = class(TComponent)
  public
    class procedure MissionComplete;
  end;

  TGiftizButton = class(TCastleButton)
  private
    const
      BaseButtonWidth = 160;
      BaseButtonHeight = 200;
    var
    ImageNaked, ImageBadge, ImageWarning: TCastleImage;
    GiftizStatusVisible: boolean;
    function MessageReceived(const Received: TCastleStringList): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetExists: boolean; override;
    procedure DoClick; override;
    procedure Resize; override;
  published
    property CustomBackground stored false;
    property OwnsCustomBackgroundNormal stored false;
    property CustomBackgroundNormal stored false;
    property AutoSize stored false;
    property Width stored false;
    property Height stored false;
    property KeepInFront stored false;
  end;

implementation

uses SysUtils,
  CastleColors, CastleUtils, CastleMessaging, CastleFilesUtils, CastleLog,
  CastleUIControls;

{ TGiftiz -------------------------------------------------------------------- }

class procedure TGiftiz.MissionComplete;
begin
  Messaging.Send(['giftiz-mission-complete']);
end;

{ TGiftizButton -------------------------------------------------------------- }

constructor TGiftizButton.Create(AOwner: TComponent);
begin
  inherited;
  Messaging.OnReceive.Add(@MessageReceived);

  CustomBackground := true;
  OwnsCustomBackgroundNormal := false;
  TintPressed := Silver;
  AutoSize := false;
  Width  := BaseButtonWidth;
  Height := BaseButtonHeight;
  KeepInFront := true;

  ImageNaked := LoadImage(ApplicationData('giftiz/giftiz_logo.png'));
  ImageBadge := LoadImage(ApplicationData('giftiz/giftiz_logo_badge.png'));
  ImageWarning := LoadImage(ApplicationData('giftiz/giftiz_logo_warning.png'));
  GiftizStatusVisible := false;

  // this is only for testing:
  // GiftizStatusVisible := true;
  // CustomBackgroundNormal := ImageWarning;
end;

destructor TGiftizButton.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  CustomBackgroundNormal := nil;
  FreeAndNil(ImageNaked);
  FreeAndNil(ImageBadge);
  FreeAndNil(ImageWarning);
  inherited;
end;

function TGiftizButton.MessageReceived(const Received: TCastleStringList): boolean;
begin
  Result := false;

  if (Received.Count = 2) and
     (Received[0] = 'giftiz-button-status') then
  begin
    Result := true;
    if Received[1] = 'invisible' then
    begin
      GiftizStatusVisible := false;
    end else
    if Received[1] = 'naked' then
    begin
      GiftizStatusVisible := true;
      CustomBackgroundNormal := ImageNaked;
    end else
    if Received[1] = 'badge' then
    begin
      GiftizStatusVisible := true;
      CustomBackgroundNormal := ImageBadge;
    end else
    if Received[1] = 'warning' then
    begin
      GiftizStatusVisible := true;
      CustomBackgroundNormal := ImageWarning;
    end else
      WritelnWarning('Giftiz', 'Invalid button state ' + Received[1]);
    VisibleChange;
  end;
end;

function TGiftizButton.GetExists: boolean;
begin
  Result := (inherited GetExists) and GiftizStatusVisible;
end;

procedure TGiftizButton.DoClick;
begin
  inherited;
  Messaging.Send(['giftiz-button-clicked']);
end;

procedure TGiftizButton.Resize;
const
  ReferenceWidth = 1600;
var
  Scale: Single;
begin
  inherited;
  if Container.UIScaling = usNone then
  begin
    // manually scale size, if UIScaling not in effect
    Scale := ContainerWidth / ReferenceWidth;
    Width  := Round(BaseButtonWidth  * Scale);
    Height := Round(BaseButtonHeight * Scale);
  end;
end;

end.
