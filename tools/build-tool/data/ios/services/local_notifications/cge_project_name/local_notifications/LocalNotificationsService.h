/*
  Copyright 2026-2026 Hamid reza Kabiri.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/* Local notifications on iOS, integration with Castle Game Engine https://castle-engine.io/ .
   Handles the same messages as the Android local_notifications service,
   so the Pascal CastleLocalNotifications unit works the same on both. */

#import <UserNotifications/UserNotifications.h>

#import "../ServiceAbstract.h"

@interface LocalNotificationsService : ServiceAbstract <UNUserNotificationCenterDelegate>
{
}

@end
