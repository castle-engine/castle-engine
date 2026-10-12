/* -*- tab-width: 4 -*- */

/*
  Copyright 2026-2026 Hamid reza Kabiri, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package io.castleengine;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

/**
 * Schedules again the pending local notifications after a device reboot,
 * and after the application was updated.
 *
 * Android forgets all alarms when the device restarts, so without this
 * a notification scheduled for tomorrow would silently never arrive.
 * Some Android versions forget the alarms also when the application
 * is updated.
 */
public class LocalNotificationBootReceiver extends BroadcastReceiver
{
    @Override
    public void onReceive(Context context, Intent intent)
    {
        if (intent == null) {
            return;
        }
        String action = intent.getAction();
        if (!Intent.ACTION_BOOT_COMPLETED.equals(action) &&
            !Intent.ACTION_MY_PACKAGE_REPLACED.equals(action)) {
            return;
        }
        ServiceLocalNotifications.rescheduleRemembered(context);
    }
}
