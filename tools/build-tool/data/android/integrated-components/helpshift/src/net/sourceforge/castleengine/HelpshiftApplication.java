/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.HashMap;

import android.app.Application;

import com.helpshift.Core;
import com.helpshift.All;

public class HelpshiftApplication extends Application
{
    @Override
    public void onCreate()
    {
        super.onCreate();

        HashMap config = new HashMap();
        Core.init(All.getInstance());
        Core.install(this,
                     "${ANDROID.HELPSHIFT.API_KEY}",
                     "${ANDROID.HELPSHIFT.DOMAIN}",
                     "${ANDROID.HELPSHIFT.APP_ID}",
                     config);
    }
}
