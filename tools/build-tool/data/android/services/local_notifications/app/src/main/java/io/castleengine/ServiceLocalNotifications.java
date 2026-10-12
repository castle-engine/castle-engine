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
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;

import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;

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

    /* When did we ask for the POST_NOTIFICATIONS permission
       (System.currentTimeMillis() based), if the user possibly did not
       answer yet. 0 if we are not waiting for the answer. See onResume. */
    private volatile long permissionRequestTime;

    /* We never show a notification later than this (in milliseconds) after
       its time. A reminder that arrives long after the event it announces
       is worse than no reminder. */
    private static final long MAX_LATE = 10 * 60 * 1000;

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
            if (!LocalNotificationReceiver.notificationsEnabled(getActivity())) {
                permissionRequestTime = System.currentTimeMillis();
            }
            getActivity().requestPermission("android.permission.POST_NOTIFICATIONS");
        }
    }

    /* Schedule again the remembered notifications when the application starts.
       Android forgets the alarms not only at a device reboot (handled by
       LocalNotificationBootReceiver) but also e.g. when the user uses
       "Force stop" on the application. */
    @Override
    public void onCreate()
    {
        rescheduleRemembered(getActivity(), MAX_LATE);
    }

    /* A notification scheduled with a short delay (like "now") comes while
       the user still looks at the dialog asking for the permission.
       LocalNotificationReceiver cannot show it then, and leaves it remembered.
       The dialog pauses our activity, and resumes it when the user answers.
       So show such notifications now, if the user agreed.

       Nothing else is done here. In particular we do not show the notifications
       that should have been shown before we asked for the permission. */
    @Override
    public void onResume()
    {
        long since = permissionRequestTime;
        if (since == 0 || !LocalNotificationReceiver.notificationsEnabled(getActivity())) {
            return;
        }
        permissionRequestTime = 0;

        Context context = getActivity();
        SharedPreferences preferences = preferences();
        SharedPreferences.Editor editor = preferences.edit();
        long now = System.currentTimeMillis();

        for (Map.Entry<String, ?> entry : preferences.getAll().entrySet()) {
            String id = entry.getKey();
            Remembered remembered = Remembered.parse(entry.getValue());
            if (remembered == null ||
                remembered.when < since ||
                remembered.when > now ||
                remembered.when < now - MAX_LATE) {
                continue;
            }

            /* Cancel the alarm, in case Android did not deliver it yet
               (alarms are not exact), to not show the notification twice. */
            alarmManager().cancel(notificationIntent(context, id, "", ""));
            editor.remove(id);
            LocalNotificationReceiver.show(context, id, remembered.title, remembered.text);
        }

        editor.apply();
        updateBootReceiverEnabled(context);
    }

    private AlarmManager alarmManager()
    {
        return (AlarmManager) getActivity().getSystemService(Context.ALARM_SERVICE);
    }

    /* PendingIntent that shows the notification with this id.

       The id is placed in the intent data (as URI), because Android tells
       PendingIntents apart by the intent action, data, class (not by extras).
       So scheduling the same id again replaces the previous alarm, canceling
       finds it, and different ids never affect each other.

       Static and takes a Context, as it is used also after a reboot,
       when there is no activity. */
    private static PendingIntent notificationIntent(Context context,
        String id, String title, String text)
    {
        Intent intent = new Intent(context, LocalNotificationReceiver.class);
        intent.setAction(LocalNotificationReceiver.ACTION_SHOW);
        intent.setData(Uri.parse("castle-local-notification:" + Uri.encode(id)));
        intent.putExtra(LocalNotificationReceiver.EXTRA_ID, id);
        intent.putExtra(LocalNotificationReceiver.EXTRA_TITLE, title);
        intent.putExtra(LocalNotificationReceiver.EXTRA_TEXT, text);

        int flags = PendingIntent.FLAG_UPDATE_CURRENT;
        if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
            flags = flags | PendingIntent.FLAG_IMMUTABLE;
        }
        return PendingIntent.getBroadcast(context, 0, intent, flags);
    }

    /* Notification as remembered in PREFERENCES, under the key equal to
       the notification id. Stored as JSON, so that the title and text
       can contain any characters. */
    private static class Remembered
    {
        /* Absolute time to show, System.currentTimeMillis() based. */
        long when;
        String title, text;

        String serialize() throws JSONException
        {
            JSONObject json = new JSONObject();
            json.put("when", when);
            json.put("title", title);
            json.put("text", text);
            return json.toString();
        }

        /* Returns null if the value is not valid. */
        static Remembered parse(Object value)
        {
            if (value == null) {
                return null;
            }
            try {
                JSONObject json = new JSONObject(value.toString());
                Remembered result = new Remembered();
                result.when = json.getLong("when");
                result.title = json.getString("title");
                result.text = json.getString("text");
                return result;
            } catch (JSONException e) {
                return null;
            }
        }
    }

    /* Remember the notification, to schedule it again after a device reboot
       (a reboot forgets all alarms). */
    private void remember(String id, long when, String title, String text)
    {
        Remembered remembered = new Remembered();
        remembered.when = when;
        remembered.title = title;
        remembered.text = text;
        try {
            preferences().edit().putString(id, remembered.serialize()).apply();
        } catch (JSONException e) {
            logWarning(CATEGORY, "Cannot remember notification \"" + id +
                "\", it will not be scheduled again after a reboot: " + e.getMessage());
        }
        updateBootReceiverEnabled(getActivity());
    }

    private void forget(String id)
    {
        preferences().edit().remove(id).apply();
        updateBootReceiverEnabled(getActivity());
    }

    private SharedPreferences preferences()
    {
        return getActivity().getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
    }

    public static final String PREFERENCES = "io.castleengine.local_notifications";

    /* Enable LocalNotificationBootReceiver only when some notification is
       pending, disable it otherwise. Call this after every change to the
       remembered notifications (in PREFERENCES).

       The receiver is disabled in AndroidManifest.xml, so by default Android
       does not start our application after a device reboot. It only needs
       to start then when there is something to schedule again.
       This is the approach of AndroidX WorkManager (see its RescheduleReceiver
       and UnfinishedWorkListener).

       Static and takes a Context, as it is called also from the receivers,
       that have no activity. */
    public static void updateBootReceiverEnabled(Context context)
    {
        boolean enabled = !context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE).
            getAll().isEmpty();
        ComponentName receiver = new ComponentName(context, LocalNotificationBootReceiver.class);
        try {
            PackageManager packageManager = context.getPackageManager();
            /* Anything other than "enabled" means disabled here, including
               the "default" state, as the manifest says android:enabled="false". */
            boolean enabledNow = packageManager.getComponentEnabledSetting(receiver) ==
                PackageManager.COMPONENT_ENABLED_STATE_ENABLED;
            if (enabled == enabledNow) {
                return;
            }
            packageManager.setComponentEnabledSetting(receiver,
                enabled ?
                    PackageManager.COMPONENT_ENABLED_STATE_ENABLED :
                    PackageManager.COMPONENT_ENABLED_STATE_DISABLED,
                PackageManager.DONT_KILL_APP);
        } catch (Exception e) {
            /* Must not crash the game, or a receiver that runs without it. */
            logWarning(CATEGORY, "Cannot " + (enabled ? "enable" : "disable") +
                " rescheduling notifications after a reboot: " + e.getMessage());
        }
    }

    private void schedule(String id, long delaySeconds, String title, String text)
    {
        requestNotificationPermission();

        long when = System.currentTimeMillis() + delaySeconds * 1000;
        scheduleAt(getActivity(), alarmManager(),
            notificationIntent(getActivity(), id, title, text), when);
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
        alarmManager().cancel(notificationIntent(getActivity(), id, "", ""));
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
        /* Do not show anything whose moment passed while the device was off. */
        rescheduleRemembered(context, 0);
    }

    /* Set again the alarms of the remembered notifications, because Android
       possibly forgot them. Setting an alarm that already exists is harmless,
       it just replaces it.

       Notifications that should have been shown more than maxLate milliseconds
       ago are dropped, we will never show them.
       The ones that should have been shown more recently are left untouched:
       Android possibly did not deliver their alarm yet (alarms are not exact),
       or they wait for the user to answer the permission dialog (see onResume). */
    private static void rescheduleRemembered(Context context, long maxLate)
    {
        AlarmManager alarms = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        SharedPreferences preferences =
            context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = preferences.edit();
        long now = System.currentTimeMillis();

        for (Map.Entry<String, ?> entry : preferences.getAll().entrySet()) {
            String id = entry.getKey();
            Remembered remembered = Remembered.parse(entry.getValue());

            if (remembered == null || remembered.when <= now - maxLate) {
                alarms.cancel(notificationIntent(context, id, "", ""));
                editor.remove(id);
            } else
            if (remembered.when > now) {
                scheduleAt(context, alarms,
                    notificationIntent(context, id, remembered.title, remembered.text),
                    remembered.when);
            }
        }

        editor.apply();
        updateBootReceiverEnabled(context);
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
