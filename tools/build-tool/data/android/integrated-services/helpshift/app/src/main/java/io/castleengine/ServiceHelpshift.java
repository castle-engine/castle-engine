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

import android.util.Log;

import com.helpshift.support.Support;

/**
 * Helpshift (https://www.helpshift.com/)
 * integration with Castle Game Engine Android application.
 */
public class ServiceHelpshift extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceHelpshift";

    public String getName()
    {
        return "helpshift";
    }

    public ServiceHelpshift(MainActivity activity)
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
