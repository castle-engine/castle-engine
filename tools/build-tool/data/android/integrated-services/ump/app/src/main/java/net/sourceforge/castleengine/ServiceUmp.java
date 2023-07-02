/* -*- tab-width: 4 -*- */

/*
  Copyright 2023 Michalis Kamburelis, Andrzej Kilijanski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/
package net.sourceforge.castleengine;

import android.util.Log;
import android.view.View;

// needed for @NotNull
import androidx.annotation.NonNull;

import com.google.android.ump.ConsentForm;
import com.google.android.ump.ConsentInformation;
import com.google.android.ump.ConsentRequestParameters;
import com.google.android.ump.FormError;
import com.google.android.ump.UserMessagingPlatform;


/**
 * Integration of Google User Messaging Platform with Castle Game Engine.
 */
public class ServiceUmp extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceUmp";
    private final boolean debug = false; // set to true for debug (more logs)

    private static final int NO_ERROR = -1; // no error constant

    private boolean initialized;

    public ServiceUmp(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "ump";
    }

    private void initialize(String bannerUnitId, String interstitialUnitId, String rewardedUnitId, String[] aTestDeviceIds)
    {
        if (initialized) {
            return;
        }

        initialized = true;
        logInfo(CATEGORY, "Ump initialized");
    }

    private void checkConsent()
    {
        logInfo(CATEGORY, "Ump checkConsent() - START");


        logInfo(CATEGORY, "Ump checkConsent() - STOP");
    }


    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("ump-check-consent")) {
            checkConsent();
            return true;
        } else /*
        if (parts.length == 2 && parts[0].equals("ads-admob-banner-show")) {
            int gravity = Integer.parseInt(parts[1]);
            bannerShow(gravity);
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("ads-admob-banner-hide")) {
            bannerHide();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-show-interstitial") && parts[1].equals("wait-until-loaded")) {
            interstitialDisplay(true);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-show-interstitial") && parts[1].equals("no-wait")) {
            interstitialDisplay(false);
            return true;
        } else 
        if (parts.length == 2 && parts[0].equals("ads-admob-show-reward") && parts[1].equals("wait-until-loaded")) {
            rewardedDisplay(true);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-show-reward") && parts[1].equals("no-wait")) {
            rewardedDisplay(false);
            return true;
        } else */{
            return false;
        }
        
    }
}
