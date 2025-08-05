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

import java.util.Map;

import android.app.Activity;
import android.app.Application;

import com.appsflyer.AppsFlyerLib;
import com.appsflyer.AppsFlyerConversionListener;

/**
 * Apps Flyer ( https://www.appsflyer.com/ )
 * integration with Castle Game Engine Android application.
 * See also https://github.com/AppsFlyerSDK/AndroidSampleApp/blob/master/app/src/main/java/com/appsflyer/androidsampleapp/AFApplication.java
 */
public class ServiceAppsFlyer extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceAppsFlyer";

    private static final String AF_DEV_KEY = "${ANDROID.APPS_FLYER.DEV_KEY}";

    public ServiceAppsFlyer(MainActivity activity)
    {
        super(activity);

        AppsFlyerConversionListener conversionDataListener =
            new AppsFlyerConversionListener() {
                @Override
                public void onInstallConversionDataLoaded(Map<String, String> conversionData) {
                    for (String attrName : conversionData.keySet()) {
                        logInfo(CATEGORY, "onInstallConversionDataLoaded: Attribute: " + attrName + " = " + conversionData.get(attrName));
                    }
                }
                @Override
                public void onInstallConversionFailure(String errorMessage) {
                    logInfo(CATEGORY, "onInstallConversionFailure: error getting conversion data: " + errorMessage);
                }
                @Override
                public void onAppOpenAttribution(Map<String, String> conversionData) {
                    for (String attrName : conversionData.keySet()) {
                        logInfo(CATEGORY, "onAppOpenAttribution: Attribute: " + attrName + " = " + conversionData.get(attrName));
                    }
                }
                @Override
                public void onAttributionFailure(String errorMessage) {
                    logInfo(CATEGORY, "onAttributionFailure: " + errorMessage);
                }
            };
        Application app = getActivity().getApplication();
        AppsFlyerLib.getInstance().init(AF_DEV_KEY, conversionDataListener,
            getActivity().getApplicationContext());
        AppsFlyerLib.getInstance().startTracking(app, AF_DEV_KEY);

        // for debugging,
        // observe by "adb logcat | grep AppsFlyer"
        // AppsFlyerLib.getInstance().setDebugLog(true);
    }

    public String getName()
    {
        return "apps_flyer";
    }
}
