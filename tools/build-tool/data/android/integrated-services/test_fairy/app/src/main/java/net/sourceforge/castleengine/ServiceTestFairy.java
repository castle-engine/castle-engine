/* -*- tab-width: 4 -*- */

/*
  Copyright 2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package net.sourceforge.castleengine;

import android.app.Activity;
import android.os.Bundle;

import com.testfairy.TestFairy;

/**
 * TestFairy ( https://docs.testfairy.com/Android/Integrating_Android_SDK.html )
 * integration with Castle Game Engine Android application.
 */
public class ServiceTestFairy extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceTestFairy";

    public ServiceTestFairy(MainActivity activity)
    {
        super(activity);

        /* Do not specify domain anymore?
           Free TestFairy users do not have a domain,
           and it seems not necessary to specify it anyway. */
        // TestFairy.setServerEndpoint("https://${ANDROID.TEST_FAIRY.DOMAIN}.testfairy.com");

        TestFairy.begin(activity, "${ANDROID.TEST_FAIRY.SDK_APP_TOKEN}");
    }

    public String getName()
    {
        return "test_fairy";
    }
}
