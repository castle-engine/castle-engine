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
 *
 * How it works: every scheduled notification is remembered in PREFERENCES,
 * and has an alarm that makes Android call us (showIfDue) at its time.
 * The remembered notifications decide what is shown, an alarm only tells
 * us to look at them. So it is not a problem when Android forgets an alarm
 * (we set the alarms again, see rescheduleRemembered) or delivers an alarm
 * that is no longer wanted (showIfDue ignores it).
 */
public class ServiceLocalNotifications extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceLocalNotifications";

    /* Name of the shared preferences with the remembered notifications. */
    private static final String PREFERENCES = "io.castleengine.local_notifications";

    /* Permission necessary to show notifications since Android 13. */
    private static final String PERMISSION = "android.permission.POST_NOTIFICATIONS";

    /* Whether the PERMISSION was already requested in this run of
       the application. We ask once, at the first scheduled notification,
       not at startup: asking makes sense only when we actually have
       something to show.

       We do not ask again in this run, regardless of what the user answers.
       This includes the case when the question disappeared without an answer
       (e.g. the user left the application when the question was displayed,
       and got back to it using the application icon): for us it is the same
       as a refusal.
       This is also set when the game itself asked for this permission.

       We ask again in the next run of the application. But Android shows
       the question to the user only 2 times. Later it immediately answers
       that the permission is not granted, and the user can allow
       the notifications only in the system settings. */
    private static boolean permissionAsked;

    /* We asked for the PERMISSION and the answer did not come yet.
       The notifications whose time comes now stay remembered (see showIfDue)
       and are shown when the user agrees (see onPermissionResult).

       Static, because Android may create the activity (and so this service)
       again while the question is displayed. The answer comes to the new
       instance then.

       In the rare cases when MainActivity.requestPermission gives no answer
       (see there), this stays true until the application process ends.
       The notifications are then kept (not forgotten) when their time comes
       while the notifications are not allowed. */
    private static boolean waitingForPermission;

    public ServiceLocalNotifications(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "local_notifications";
    }

    /* Set again the alarms of the remembered notifications when the application
       starts. Android forgets the alarms not only at a device reboot (handled
       by LocalNotificationBootReceiver) but also e.g. when the user uses
       "Force stop" on the application. */
    @Override
    public void onCreate()
    {
        rescheduleRemembered(getActivity());
    }

    /* Notification as remembered in PREFERENCES, under the key equal to
       the notification id. Stored as JSON, so that the title and text
       can contain any characters. */
    private static class Remembered
    {
        /* Absolute time to show, System.currentTimeMillis() based. */
        final long when;
        final String title, text;

        Remembered(long when, String title, String text)
        {
            this.when = when;
            this.title = title;
            this.text = text;
        }

        String serialize() throws JSONException
        {
            JSONObject json = new JSONObject();
            json.put("when", when);
            json.put("title", title);
            json.put("text", text);
            return json.toString();
        }

        /* Returns null if the value is null or not valid. */
        static Remembered parse(Object value)
        {
            if (value == null) {
                return null;
            }
            try {
                JSONObject json = new JSONObject(value.toString());
                return new Remembered(json.getLong("when"),
                    json.getString("title"), json.getString("text"));
            } catch (JSONException e) {
                return null;
            }
        }
    }

    /* Static and takes a Context, like all the methods that are used also
       when our application is not running (Android starts the process
       just for LocalNotificationReceiver or LocalNotificationBootReceiver),
       so there is no activity. */
    private static SharedPreferences preferences(Context context)
    {
        return context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
    }

    /* Remember the notification, replacing the one remembered before
       with the same id. */
    private static void remember(Context context, String id, Remembered notification)
        throws JSONException
    {
        preferences(context).edit().putString(id, notification.serialize()).apply();
        updateBootReceiverEnabled(context);
    }

    private static void forget(Context context, String id)
    {
        preferences(context).edit().remove(id).apply();
        updateBootReceiverEnabled(context);
    }

    /* Enable LocalNotificationBootReceiver only when some notification is
       remembered, disable it otherwise. Called after every change to the
       remembered notifications (in PREFERENCES).

       The receiver is disabled in AndroidManifest.xml, so by default Android
       does not start our application after a device reboot. It only needs
       to start then when there is something to schedule again.
       This is the approach of AndroidX WorkManager (see its RescheduleReceiver
       and UnfinishedWorkListener). */
    private static void updateBootReceiverEnabled(Context context)
    {
        boolean enabled = !preferences(context).getAll().isEmpty();
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

    /* PendingIntent of the alarm for the notification with this id:
       it makes LocalNotificationReceiver call showIfDue(context, id).

       The id is placed in the intent data (as URI), because Android tells
       PendingIntents apart by the intent action, data, class (not by extras).
       So setting the alarm for the same id again replaces the previous alarm,
       canceling finds it, and different ids never affect each other. */
    private static PendingIntent alarmIntent(Context context, String id)
    {
        Intent intent = new Intent(context, LocalNotificationReceiver.class);
        intent.setAction(LocalNotificationReceiver.ACTION_SHOW);
        intent.setData(Uri.parse("castle-local-notification:" + Uri.encode(id)));
        intent.putExtra(LocalNotificationReceiver.EXTRA_ID, id);

        int flags = PendingIntent.FLAG_UPDATE_CURRENT;
        if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
            flags = flags | PendingIntent.FLAG_IMMUTABLE;
        }
        return PendingIntent.getBroadcast(context, 0, intent, flags);
    }

    private static AlarmManager alarmManager(Context context)
    {
        return (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
    }

    /* Make the remembered notification appear at its time: show it now when
       the time has come, otherwise set the alarm for it (replacing the previous
       alarm for this id, if any).

       We use setAndAllowWhileIdle: it survives the device dozing, and unlike
       the "exact" alarms it requires no special permission (which Android 12+
       grants sparingly). The cost is that Android may delay the notification:
       usually not much, but it does not promise the exact time. It delivers
       the alarm within an hour on Android 12 and newer, older versions may
       delay it more when the delay is long, and all versions when the device
       saves the battery. This is acceptable for a game event reminder. */
    private static void showOrSetAlarm(Context context, String id, long when)
    {
        if (when <= System.currentTimeMillis()) {
            /* Do not use an alarm for this: Android would fire it only after
               a few seconds, even when its time has already passed.
               Cancel the alarm set before for this id (if any),
               it is not needed anymore. */
            alarmManager(context).cancel(alarmIntent(context, id));
            showIfDue(context, id);
            return;
        }

        try {
            PendingIntent intent = alarmIntent(context, id);
            if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
                alarmManager(context).setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, when, intent);
            } else {
                alarmManager(context).set(AlarmManager.RTC_WAKEUP, when, intent);
            }
        } catch (RuntimeException e) {
            /* Android refuses to set too many alarms (500) for one application.
               Must not crash the game, or a receiver that runs without it.
               The notification stays remembered, so we will try again
               in the next rescheduleRemembered. */
            logWarning(CATEGORY, "Cannot set alarm for notification \"" + id + "\": " +
                e.getMessage());
        }
    }

    private void schedule(String id, long delaySeconds, String title, String text)
    {
        Context context = getActivity();
        Remembered notification = new Remembered(
            System.currentTimeMillis() + delaySeconds * 1000, title, text);

        try {
            remember(context, id, notification);
        } catch (JSONException e) {
            logError(CATEGORY, "Cannot schedule notification \"" + id + "\": " + e.getMessage());
            return;
        }

        /* The new notification replaces the previous one with the same id,
           also when the previous one is already displayed. */
        LocalNotificationReceiver.hide(context, id);

        if (!permissionAsked &&
            Build.VERSION.SDK_INT >= 33 && // Build.VERSION_CODES.TIRAMISU
            !LocalNotificationReceiver.notificationsEnabled(context)) {
            /* Note that requestPermission may answer immediately (calling
               onPermissionResult), so waitingForPermission must be set before. */
            permissionAsked = true;
            waitingForPermission = true;
            getActivity().requestPermission(PERMISSION);
        }

        showOrSetAlarm(context, id, notification.when);
        logInfo(CATEGORY, "Scheduled notification \"" + id + "\" in " + delaySeconds + " seconds");
    }

    @Override
    public void onPermissionResult(String permission, boolean granted)
    {
        if (!PERMISSION.equals(permission)) {
            return;
        }

        /* Do not ask again, also when it was the game that asked (not us). */
        permissionAsked = true;

        if (waitingForPermission) {
            waitingForPermission = false;
            /* Show the notifications whose time came while we waited
               (or forget them, when the user refused). */
            rescheduleRemembered(getActivity());
        }
    }

    private void cancel(String id)
    {
        Context context = getActivity();

        /* Canceling an unknown id is not an error: the game may cancel
           a notification it never scheduled, or one that already fired. */
        forget(context, id);
        alarmManager(context).cancel(alarmIntent(context, id));
        LocalNotificationReceiver.hide(context, id);
        logInfo(CATEGORY, "Canceled notification \"" + id + "\"");
    }

    /* Show the notification with this id, if it is remembered and its time
       has come. Called by LocalNotificationReceiver when the alarm fires. */
    public static void showIfDue(Context context, String id)
    {
        Remembered notification = Remembered.parse(preferences(context).getString(id, null));
        if (notification == null) {
            /* The notification was canceled, or already shown,
               when the alarm was already on its way to us. */
            return;
        }
        if (notification.when > System.currentTimeMillis()) {
            /* The notification was scheduled again for a later time, and this
               is the previous alarm: it was already on its way to us, or Android
               refused to replace it (see showOrSetAlarm).
               Make sure the alarm for the right time is set. */
            showOrSetAlarm(context, id, notification.when);
            return;
        }

        if (LocalNotificationReceiver.notificationsEnabled(context)) {
            forget(context, id);
            LocalNotificationReceiver.show(context, id, notification.title, notification.text);
        } else
        if (!waitingForPermission) {
            forget(context, id);
            logInfo(CATEGORY, "Not showing notification \"" + id +
                "\", the user did not allow the notifications");
        }
        /* Otherwise the user is deciding now whether to allow the notifications.
           Leave the notification remembered, onPermissionResult will get back
           to it. */
    }

    /* Set again the alarms of all the remembered notifications, because Android
       forgot them (after a device reboot) or possibly forgot them.
       Setting an alarm that already exists is harmless, it just replaces it.

       The notifications whose time has already passed (e.g. when the device
       was turned off) are shown now. This is consistent with what the user
       sees when the device was working at that time but nobody looked at it:
       the notification waits on the screen.

       Called at the application start, by LocalNotificationBootReceiver,
       and when the user answers our request for the PERMISSION. */
    public static void rescheduleRemembered(Context context)
    {
        int count = 0;
        for (Map.Entry<String, ?> entry : preferences(context).getAll().entrySet()) {
            String id = entry.getKey();
            Remembered notification = Remembered.parse(entry.getValue());
            if (notification == null) {
                forget(context, id);
            } else {
                showOrSetAlarm(context, id, notification.when);
                count++;
            }
        }

        /* Necessary even when we did not change the remembered notifications
           above: Android may restore them from a backup (when the application
           is installed again), without restoring the state of the receiver. */
        updateBootReceiverEnabled(context);

        if (count != 0) {
            logInfo(CATEGORY, "Checked " + count + " remembered notifications");
        }
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
