/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.view.View;
import android.os.Build;
import android.os.Vibrator;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

/**
 * Vibrations, callled from Castle Game Engine.
 * While the code is fairly trivial, it's good to put it in a separate
 * component, since this requires special permission on Android.
 */
public class ComponentVibrate extends ComponentAbstract
{
    public ComponentVibrate(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "vibrate";
    }

    /* See
       http://stackoverflow.com/questions/13950338/how-to-make-an-android-device-vibrate
       http://developer.android.com/reference/android/os/Vibrator.html
    */

    private void vibrate(long milliseconds)
    {
        Vibrator vibs = (Vibrator) getActivity().getSystemService(Context.VIBRATOR_SERVICE);
        vibs.vibrate(milliseconds);
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("vibrate")) {
            long milliseconds = Long.parseLong(parts[1]);
            vibrate(milliseconds);
            return true;
        } else {
            return false;
        }
    }
}
