/* -*- tab-width: 4 -*- */
package io.castleengine;

import java.util.HashMap;

import android.app.Application;
import android.util.Log;

import com.helpshift.Core;
import com.helpshift.All;
import com.helpshift.exceptions.InstallException;

/* TODO: it's not nice to change the Application class in a service.
   We'll probably invert a service-like system for extending application at some point.
   For now, Helpshift is the only service that needs to extend application.
*/

public class HelpshiftApplication extends Application
{
    private static final String CATEGORY = "HelpshiftApplication";

    private boolean helpshiftInitialized = false;

    public boolean getHelpshiftInitialized()
    {
        return helpshiftInitialized;
    }

    @Override
    public void onCreate()
    {
        super.onCreate();

        HashMap config = new HashMap();
        try {
            Core.init(All.getInstance());
            Core.install(this,
                "${ANDROID.HELPSHIFT.API_KEY}",
                "${ANDROID.HELPSHIFT.DOMAIN}",
                "${ANDROID.HELPSHIFT.APP_ID}",
                config);
            helpshiftInitialized = true;
        } catch (InstallException e) {
            logError(CATEGORY, "InstallException at initializing Helpshift: " + e.getMessage());
        }
    }
}
