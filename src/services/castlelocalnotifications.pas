{
  Copyright 2026-2026 Hamid reza Kabiri.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Local notifications, shown by the system at a chosen time
  (TLocalNotifications). }
unit CastleLocalNotifications;

{$I castleconf.inc}

interface

uses Classes;

type
  { Show a notification at a chosen time, even when the application
    is no longer running.

    This is for telling the player about something that happens in your game
    while they are not looking: a building that is finished, a merchant that
    arrives, a daily reward that is ready. Tapping the notification opens
    the application.

    These are @italic(local) notifications: your application schedules them
    itself, nothing is sent over the network and no server is involved.
    Push notifications (sent from your server) are a different mechanism,
    not implemented by this unit.

    Using this requires the @code(local_notifications) service on Android,
    see https://castle-engine.io/android_services .
    On other platforms, the methods here do nothing (and this is deliberately
    not an error: a game can schedule notifications unconditionally). }
  TLocalNotifications = class(TComponent)
  public
    { Show a notification with given Title and Text after DelaySeconds.

      Id names this notification, so that it can be replaced or canceled
      later. Scheduling again with the same Id replaces the notification
      scheduled before, it does not show two of them.

      The system may show the notification somewhat later than requested,
      to save battery. Do not rely on the exact moment.

      The notification survives closing the application, and a device reboot.
      It does not survive uninstalling the application, and it is gone once
      it was shown or tapped. }
    class procedure Schedule(const Id: String; const DelaySeconds: Cardinal;
      const Title, Text: String);

    { Cancel a notification scheduled by Schedule with the same Id.
      Canceling an Id that is not scheduled (never was, or was already shown)
      is not an error and does nothing. }
    class procedure Cancel(const Id: String);
  end;

implementation

uses SysUtils,
  CastleMessaging;

class procedure TLocalNotifications.Schedule(const Id: String;
  const DelaySeconds: Cardinal; const Title, Text: String);
begin
  Messaging.Send(['local-notification-schedule', Id, IntToStr(DelaySeconds),
    Title, Text]);
end;

class procedure TLocalNotifications.Cancel(const Id: String);
begin
  Messaging.Send(['local-notification-cancel', Id]);
end;

end.
