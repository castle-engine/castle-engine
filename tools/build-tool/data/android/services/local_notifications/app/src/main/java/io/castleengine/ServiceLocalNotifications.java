/* -*- tab-width: 4 -*- */

/*
  Copyright 2026-2026 Hamid reza Kabiri.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package io.castleengine;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;

import java.util.Map;

/**
 * Local notifications, called from Castle Game Engine.
 *
 * A notification scheduled here is shown by Android at the requested time,
 * regardless of whether our application still runs then. This is for things
 * like "your building is finished", "the merchant has arrived" -- events
 * the player should learn about without keeping the game open.
 *
 * Nothing here talks to any server: these are local notifications,
 * scheduled by the application itself. Push notifications (sent from a server)
 * are a different thing, not implemented here.
 */
public class ServiceLocalNotifications extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceLocalNotifications";

    /* Whether we already asked for the POST_NOTIFICATIONS permission
       (Android 13+). We ask once, at the first scheduled notification,
       not at startup: asking makes sense only when we actually have
       something to show. */
    private boolean permissionAsked;

    public ServiceLocalNotifications(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "local_notifications";
    }

    /* Ask for the permission to post notifications, necessary since Android 13.
       Nothing is done with the answer: if the user refuses, the notification
       is scheduled but Android will not show it. */
    private void requestNotificationPermission()
    {
        if (permissionAsked) {
            return;
        }
        permissionAsked = true;
        if (Build.VERSION.SDK_INT >= 33) { // Build.VERSION_CODES.TIRAMISU
            getActivity().requestPermission("android.permission.POST_NOTIFICATIONS");
        }
    }

    private AlarmManager alarmManager()
    {
        return (AlarmManager) getActivity().getSystemService(Context.ALARM_SERVICE);
    }

    /* PendingIntent that shows the notification with this id.
       The request code is derived from the id, so that scheduling the same id
       again replaces the previous alarm, and canceling finds it. */
    private PendingIntent notificationIntent(String id, String title, String text)
    {
        Intent intent = new Intent(getActivity(), LocalNotificationReceiver.class);
        intent.setAction(LocalNotificationReceiver.ACTION_SHOW);
        intent.putExtra(LocalNotificationReceiver.EXTRA_ID, id);
        intent.putExtra(LocalNotificationReceiver.EXTRA_TITLE, title);
        intent.putExtra(LocalNotificationReceiver.EXTRA_TEXT, text);

        int flags = PendingIntent.FLAG_UPDATE_CURRENT;
        if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
            flags = flags | PendingIntent.FLAG_IMMUTABLE;
        }
        return PendingIntent.getBroadcast(getActivity(), id.hashCode(), intent, flags);
    }

    /* Remember the notification, to schedule it again after a device reboot
       (a reboot forgets all alarms). Stored as "<when>|<title>|<text>",
       where <when> is absolute, System.currentTimeMillis() based. */
    private void remember(String id, long when, String title, String text)
    {
        SharedPreferences.Editor editor = preferences().edit();
        editor.putString(id, when + "|" + title + "|" + text);
        editor.apply();
    }

    private void forget(String id)
    {
        preferences().edit().remove(id).apply();
    }

    private SharedPreferences preferences()
    {
        return getActivity().getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
    }

    public static final String PREFERENCES = "io.castleengine.local_notifications";

    private void schedule(String id, long delaySeconds, String title, String text)
    {
        requestNotificationPermission();

        long when = System.currentTimeMillis() + delaySeconds * 1000;
        scheduleAt(getActivity(), alarmManager(), notificationIntent(id, title, text), when);
        remember(id, when, title, text);
        logInfo(CATEGORY, "Scheduled notification \"" + id + "\" in " + delaySeconds + " seconds");
    }

    /* Schedule one alarm. Shared with LocalNotificationBootReceiver.
       We use setAndAllowWhileIdle: it survives the device dozing, and unlike
       the "exact" alarms it requires no special permission (which Android 12+
       grants sparingly). The cost is that Android may delay the notification
       by a few minutes, which is acceptable for a game event reminder. */
    public static void scheduleAt(Context context, AlarmManager alarms,
        PendingIntent intent, long when)
    {
        if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
            alarms.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, when, intent);
        } else {
            alarms.set(AlarmManager.RTC_WAKEUP, when, intent);
        }
    }

    private void cancel(String id)
    {
        /* Canceling an unknown id is not an error: the game may cancel
           a notification it never scheduled, or one that already fired. */
        alarmManager().cancel(notificationIntent(id, "", ""));
        LocalNotificationReceiver.hide(getActivity(), id);
        forget(id);
        logInfo(CATEGORY, "Canceled notification \"" + id + "\"");
    }

    /* Schedule again everything that was scheduled before a reboot,
       dropping what should have already been shown.
       Called from LocalNotificationBootReceiver, which has no activity,
       so this is static and takes a Context. */
    public static void rescheduleAfterBoot(Context context)
    {
        AlarmManager alarms = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        SharedPreferences preferences =
            context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = preferences.edit();

        for (Map.Entry<String, ?> entry : preferences.getAll().entrySet()) {
            String id = entry.getKey();
            String[] parts = entry.getValue().toString().split("\\|", 3);
            if (parts.length != 3) {
                editor.remove(id);
                continue;
            }

            long when;
            try {
                when = Long.parseLong(parts[0]);
            } catch (NumberFormatException e) {
                editor.remove(id);
                continue;
            }

            if (when <= System.currentTimeMillis()) {
                /* Its moment passed while the device was off. We do not show it
                   now: a reminder that arrives after the event it announces is
                   worse than no reminder. */
                editor.remove(id);
                continue;
            }

            Intent intent = new Intent(context, LocalNotificationReceiver.class);
            intent.setAction(LocalNotificationReceiver.ACTION_SHOW);
            intent.putExtra(LocalNotificationReceiver.EXTRA_ID, id);
            intent.putExtra(LocalNotificationReceiver.EXTRA_TITLE, parts[1]);
            intent.putExtra(LocalNotificationReceiver.EXTRA_TEXT, parts[2]);

            int flags = PendingIntent.FLAG_UPDATE_CURRENT;
            if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
                flags = flags | PendingIntent.FLAG_IMMUTABLE;
            }
            scheduleAt(context, alarms,
                PendingIntent.getBroadcast(context, id.hashCode(), intent, flags), when);
        }

        editor.apply();
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 5 && parts[0].equals("local-notification-schedule")) {
            long delaySeconds;
            try {
                delaySeconds = Long.parseLong(parts[2]);
            } catch (NumberFormatException e) {
                logError(CATEGORY, "Invalid delay in local-notification-schedule: " + parts[2]);
                return true;
            }
            schedule(parts[1], delaySeconds, parts[3], parts[4]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("local-notification-cancel")) {
            cancel(parts[1]);
            return true;
        } else {
            return false;
        }
    }
}
