/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package io.castleengine;

/**
 * Integration of FMOD with Castle Game Engine on Android.
 */
public class ServiceFmod extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceFmod";

    public ServiceFmod(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "fmod";
    }

    @Override
    public void onCreate()
    {
        /* FMOD docs recommend calling this, see
           https://www.fmod.com/resources/documentation-api?version=2.1&page=platforms-android.html#asset-manager
           In fact it is required to load Android asset files using FMOD. */
        org.fmod.FMOD.init(getActivity());
    }

    @Override
    public void onDestroy()
    {
        org.fmod.FMOD.close();
    }
}
