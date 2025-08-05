/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package io.castleengine;

import android.Manifest;
import android.view.View;
import android.os.Build;
import android.os.Vibrator;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

/**
 * Vibrations, callled from Castle Game Engine.
 * While the code is fairly trivial, it's good to put it in a separate
 * service, since this requires special permission on Android.
 */
public class ServiceVibrate extends ServiceAbstract
{
    public ServiceVibrate(MainActivity activity)
    {
        super(activity);
        getActivity().requestPermission(Manifest.permission.VIBRATE);
    }

    public String getName()
    {
        return "vibrate";
    }

    /* See
       http://stackoverflow.com/questions/13950338/how-to-make-an-android-device-vibrate
       http://developer.android.com/reference/android/os/Vibrator.html
    */

    @SuppressWarnings("deprecation")
    private void vibrate(long milliseconds)
    {
        Vibrator vibs = (Vibrator) getActivity().getSystemService(Context.VIBRATOR_SERVICE);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            // not deprecated:
            android.os.VibrationEffect effect =
                android.os.VibrationEffect.createOneShot(milliseconds,
                    android.os.VibrationEffect.DEFAULT_AMPLITUDE);
            vibs.vibrate(effect);
        } else {
            // deprecated in new API versions,
            // but necessary to work on old devices that don't have VibrationEffect
            vibs.vibrate(milliseconds);
        }
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
