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

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.Build;

import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

/**
 * Receives the alarms set by ServiceLocalNotifications, and shows
 * the notifications.
 *
 * This runs even when our application is not running: Android starts the
 * process just for this. So nothing here may assume that MainActivity,
 * or any Castle Game Engine state, exists.
 */
public class LocalNotificationReceiver extends BroadcastReceiver
{
    private static final String CATEGORY = "LocalNotificationReceiver";

    public static final String ACTION_SHOW = "io.castleengine.SHOW_LOCAL_NOTIFICATION";
    public static final String EXTRA_ID = "id";

    private static final String CHANNEL_ID = "castle_local_notifications";

    /* Android identifies a notification by a (tag, number) pair.
       We pass our notification id (String) as the tag, so this number can be
       constant. Deriving the number from id.hashCode() instead would mix up
       notifications whose ids happen to have the same hash
       (like "Aa" and "BB"). */
    private static final int NOTIFICATION_ID = 0;

    /* Create the notification channel, required since Android 8.
       Safe to call many times, creating an existing channel does nothing. */
    private static void createChannel(Context context)
    {
        if (Build.VERSION.SDK_INT < 26) { // Build.VERSION_CODES.O
            return;
        }

        /* Name the channel after the application: it is what the user sees
           in the system notification settings, and this service has no
           channels to tell apart. */
        CharSequence name = context.getApplicationInfo().loadLabel(context.getPackageManager());
        NotificationChannel channel = new NotificationChannel(CHANNEL_ID, name,
            NotificationManager.IMPORTANCE_DEFAULT);
        NotificationManager manager =
            (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        if (manager != null) {
            manager.createNotificationChannel(channel);
        }
    }

    /* The small icon shown in the status bar.

       Android draws it as a silhouette (only the alpha channel counts),
       so the colorful launcher icon would be a white blob. We ship a
       monochrome drawable for this, and look it up by name to avoid
       depending on the R class of the project's package. */
    private static int smallIcon(Context context)
    {
        // The project's own icon, if it provided one (small_icon parameter).
        int result = context.getResources().getIdentifier(
            "castle_notification_small_icon", "drawable", context.getPackageName());
        if (result == 0) {
            // The default bell, part of this service.
            result = context.getResources().getIdentifier(
                "ic_castle_notification", "drawable", context.getPackageName());
        }
        if (result == 0) {
            ServiceAbstract.logWarning(CATEGORY, "Cannot find the default icon for local notifications, using the application icon instead. This should not happen (as the default icon is part of this service).");
            result = context.getApplicationInfo().icon;
        }
        return result;
    }

    /* The large icon, shown in color inside the notification:
       the application's own icon. This is what makes the notification
       recognizable as coming from this application, as the small icon
       is only a silhouette. Returns null if the icon cannot be read,
       the notification is then shown without a large icon. */
    private static Bitmap largeIcon(Context context)
    {
        try {
            Drawable icon = context.getPackageManager().getApplicationIcon(context.getPackageName());
            if (icon instanceof BitmapDrawable) {
                return ((BitmapDrawable) icon).getBitmap();
            }
            /* Adaptive icons (Android 8+) are not bitmaps, draw them. */
            int width = Math.max(icon.getIntrinsicWidth(), 1);
            int height = Math.max(icon.getIntrinsicHeight(), 1);
            Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
            Canvas canvas = new Canvas(bitmap);
            icon.setBounds(0, 0, width, height);
            icon.draw(canvas);
            return bitmap;
        } catch (Exception e) {
            ServiceAbstract.logWarning(CATEGORY, "Cannot read the application icon: " + e.getMessage());
            return null;
        }
    }

    /* Intent to open the application when the notification is tapped. */
    private static PendingIntent openApplication(Context context)
    {
        Intent intent = context.getPackageManager().
            getLaunchIntentForPackage(context.getPackageName());
        if (intent == null) {
            return null;
        }
        /* The activity uses launchMode="singleTask", so this brings the
           running game to the front instead of starting a second copy. */
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TOP);

        int flags = PendingIntent.FLAG_UPDATE_CURRENT;
        if (Build.VERSION.SDK_INT >= 23) { // Build.VERSION_CODES.M
            flags = flags | PendingIntent.FLAG_IMMUTABLE;
        }
        return PendingIntent.getActivity(context, 0, intent, flags);
    }

    /* Remove a notification that is already displayed. */
    public static void hide(Context context, String id)
    {
        NotificationManagerCompat.from(context).cancel(id, NOTIFICATION_ID);
    }

    /* Can we show notifications now. False when the user did not grant
       the POST_NOTIFICATIONS permission (Android 13+), or disabled
       the notifications of this application in the system settings. */
    public static boolean notificationsEnabled(Context context)
    {
        return NotificationManagerCompat.from(context).areNotificationsEnabled();
    }

    @Override
    public void onReceive(Context context, Intent intent)
    {
        if (intent == null || !ACTION_SHOW.equals(intent.getAction())) {
            return;
        }

        String id = intent.getStringExtra(EXTRA_ID);
        if (id == null) {
            return;
        }

        ServiceLocalNotifications.showIfDue(context, id);
    }

    /* Show the notification now. Android silently ignores this when
       the notifications are not allowed, so check notificationsEnabled before. */
    public static void show(Context context, String id, String title, String text)
    {
        createChannel(context);

        NotificationCompat.Builder builder = new NotificationCompat.Builder(context, CHANNEL_ID).
            setSmallIcon(smallIcon(context)).
            setContentTitle(title != null ? title : "").
            setContentText(text != null ? text : "").
            setPriority(NotificationCompat.PRIORITY_DEFAULT).
            setAutoCancel(true);

        Bitmap large = largeIcon(context);
        if (large != null) {
            builder.setLargeIcon(large);
        }

        PendingIntent open = openApplication(context);
        if (open != null) {
            builder.setContentIntent(open);
        }

        try {
            NotificationManagerCompat.from(context).notify(id, NOTIFICATION_ID, builder.build());
        } catch (SecurityException e) {
            /* Should not happen (Android ignores the notification silently
               when the user did not allow the notifications), but this must
               not crash a game that is possibly not even running now. */
            ServiceAbstract.logWarning(CATEGORY,
                "Cannot show notification, permission not granted: " + e.getMessage());
        }
    }
}
