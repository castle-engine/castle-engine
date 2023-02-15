{
  Copyright 2021-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Tenjin integration (TCastleTenjin). }
unit CastleTenjin;

{$I castleconf.inc}

interface

uses Classes;

type
  { Tenjin ( https://www.tenjin.com/ ) integration.
    On Android ( https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/tenjin/README.md )
    and on iOS ( https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/tenjin/README.md ).

    Usage:

    @orderedList(
      @item(Create an instance of this class (only a single instance allowed).)
      @item(Call @link(Initialize).)
      @item(As necessary, use @link(SendEvent) to report custom events.)
    )

    Note that Tenjin is not part of TAnalytics, as you may want to send less events
    to Tenjin than to other analytics services -- as Tenjin's main strength is in
    the installation attribution, and custom events are paid above a certain count.
  }
  TCastleTenjin = class(TComponent)
  strict private
    FLastApiKey: string;
    procedure ReinitializeJavaActivity(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Initialize Tenjin analytics. }
    procedure Initialize(const ApiKey: String);
    { Send a custom event name to Tenjin. }
    procedure SendEvent(const EventName: String);
  end;

implementation

uses CastleApplicationProperties, CastleMessaging;

constructor TCastleTenjin.Create(AOwner: TComponent);
begin
  inherited;
  ApplicationProperties.OnInitializeJavaActivity.Add({$ifdef FPC}@{$endif} ReinitializeJavaActivity);
end;

destructor TCastleTenjin.Destroy;
begin
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnInitializeJavaActivity.Remove({$ifdef FPC}@{$endif} ReinitializeJavaActivity);
  inherited;
end;

procedure TCastleTenjin.ReinitializeJavaActivity(Sender: TObject);
begin
  { in case Java activity got killed and is created again, reinitialize services }
  if FLastApiKey <> '' then
    Initialize(FLastApiKey);
end;

procedure TCastleTenjin.Initialize(const ApiKey: String);
begin
  FLastApiKey := ApiKey;
  Messaging.Send(['tenjin-initialize', ApiKey]);
end;

procedure TCastleTenjin.SendEvent(const EventName: String);
begin
  Messaging.Send(['tenjin-send-event', EventName]);
end;

end.
