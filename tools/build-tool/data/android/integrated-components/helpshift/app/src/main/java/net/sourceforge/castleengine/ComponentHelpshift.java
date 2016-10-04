/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;

import com.helpshift.support.Support;

/**
 * Helpshift (https://www.helpshift.com/)
 * integration with Castle Game Engine Android application.
 */
public class ComponentHelpshift extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentHelpshift";

    public String getName()
    {
        return "helpshift";
    }

    public ComponentHelpshift(MainActivity activity)
    {
        super(activity);
    }

    private boolean getHelpshiftInitialized()
    {
        return ((HelpshiftApplication) getActivity().getApplication()).getHelpshiftInitialized();
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("helpshift-show-conversation")) {
            if (getHelpshiftInitialized()) {
                Support.showConversation(getActivity());
            }
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("helpshift-show-faqs")) {
            if (getHelpshiftInitialized()) {
                Support.showFAQs(getActivity());
            }
            return true;
        } else
        {
            return false;
        }
    }
}
