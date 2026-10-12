{
  Copyright 2026-2026 Hamid reza Kabiri, Michalis Kamburelis.

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
      scheduled before (also when it is already displayed),
      it does not show two of them.

      The system may show the notification somewhat later than requested,
      to save battery. Do not rely on the exact moment.

      The notification survives closing the application, and a device reboot.
      When the device is turned off at the moment when the notification
      should be shown, the notification is shown after the device is
      turned on again.

      It does not survive uninstalling the application (unless Android
      restores the application data from a backup, when the application
      is installed again), and it is gone once it was shown or tapped.

      On Android 13 and newer, the user is asked for the permission to show
      notifications when you call this for the first time. If the user
      refuses, the notifications are not shown, without any error.

      On Android, the request is processed only while the application
      is displayed and nothing covers it.
      This is almost always true from the Pascal code perspective.
      But beware of edge-cases: A request made from
      @link(TCastleApplicationProperties.OnPause ApplicationProperties.OnPause)
      (when the user leaves the application), or while a system dialog covers
      the application (like a question about a permission), is processed only once
      the application is displayed again. }
    class procedure Schedule(const Id: String; const DelaySeconds: Cardinal;
      const Title, Text: String);

    { Cancel a notification scheduled by Schedule with the same Id.
      If the notification was already shown and is still displayed
      (the user did not dismiss it), it is removed.
      Canceling an Id that is neither scheduled nor displayed
      is not an error and does nothing.

      Like @link(Schedule):
      On Android, a request made when the application is not active
      is processed only when the application is active again. }
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
