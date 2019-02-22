/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.app.Activity;
import android.os.Bundle;

import com.facebook.FacebookSdk;
import com.facebook.LoggingBehavior;
import com.facebook.appevents.AppEventsLogger;
import com.facebook.appevents.AppEventsConstants;

/**
 * Facebook ( https://developers.facebook.com/docs/android/ )
 * integration with Castle Game Engine Android application.
 */
public class ServiceFacebook extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceFacebook";

    private AppEventsLogger logger;

    public ServiceFacebook(MainActivity activity)
    {
        super(activity);
        logger = AppEventsLogger.newLogger(getActivity());

        // useful for development
        // FacebookSdk.setIsDebugEnabled(true);
        // FacebookSdk.addLoggingBehavior(LoggingBehavior.APP_EVENTS);
    }

    public String getName()
    {
        return "facebook";
    }

    private void logAchievedLevel(String level)
    {
        Bundle params = new Bundle();
        params.putString(AppEventsConstants.EVENT_PARAM_LEVEL, level);
        logger.logEvent(AppEventsConstants.EVENT_NAME_ACHIEVED_LEVEL, params);
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("facebook-log-achieved-level")) {
            logAchievedLevel(parts[1]);
            return true;
        } else
        {
            return false;
        }
    }
}
