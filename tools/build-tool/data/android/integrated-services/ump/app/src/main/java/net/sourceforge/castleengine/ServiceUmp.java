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
// needed for @Nullable
import androidx.annotation.Nullable;

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

    private ConsentInformation consentInformation;
    private ConsentForm consentForm;

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

    /* https://developers.google.com/interactive-media-ads/ump/android/quick-start?hl=pl */
    private void checkConsent()
    {
        logInfo(CATEGORY, "Ump checkConsent() - START");
        // Set tag for underage of consent. Here false means users are not underage.
        ConsentRequestParameters params = new ConsentRequestParameters
            .Builder()
            .setTagForUnderAgeOfConsent(false)
            .build();

        consentInformation = UserMessagingPlatform.getConsentInformation(getActivity());
        consentInformation.requestConsentInfoUpdate(
            getActivity(),
            params,
            new ConsentInformation.OnConsentInfoUpdateSuccessListener() {
                @Override
                public void onConsentInfoUpdateSuccess() {
                    // The consent information state was updated.
                    // You are now ready to check if a form is available.
                    logInfo(CATEGORY, "Success");
                    if (consentInformation.isConsentFormAvailable()) {
                        loadForm();
                    }
                }
            },
            new ConsentInformation.OnConsentInfoUpdateFailureListener() {
                @Override
                public void onConsentInfoUpdateFailure(FormError formError) {
                    // Handle the error.
                    logInfo(CATEGORY, "Error");
                }
        });

        logInfo(CATEGORY, "Ump checkConsent() - STOP");
    }

    private void loadForm() {
        UserMessagingPlatform.loadConsentForm(
            getActivity(), new UserMessagingPlatform.OnConsentFormLoadSuccessListener() {
              @Override
              public void onConsentFormLoadSuccess(ConsentForm consForm) {
                consentForm = consForm;
                logInfo(CATEGORY, "Consent form load success");
                //if (consentInformation.getConsentStatus() == ConsentInformation.ConsentStatus.REQUIRED) {
                    consentForm.show(
                        getActivity(),
                        new ConsentForm.OnConsentFormDismissedListener() {
                            @Override
                            public void onConsentFormDismissed(@Nullable FormError formError) {
                                // Handle dismissal by reloading form.
                                //loadForm();
                            }
                        }
                    );
                //}
              }
            },
            new UserMessagingPlatform.OnConsentFormLoadFailureListener() {
              @Override
              public void onConsentFormLoadFailure(FormError formError) {
                // Handle the error.
              }
            }
        );
    }
      


    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("ump-check-consent")) {
            checkConsent();
            return true;
        } else {
            return false;
        }
        
    }
}
