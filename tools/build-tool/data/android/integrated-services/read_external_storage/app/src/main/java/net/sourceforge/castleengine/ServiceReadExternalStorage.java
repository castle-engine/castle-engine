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

package io.castleengine;

import android.Manifest;
import android.app.Activity;

public class ServiceReadExternalStorage extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceReadExternalStorage";

    public ServiceReadExternalStorage(MainActivity activity)
    {
        super(activity);
        getActivity().requestPermission(Manifest.permission.READ_EXTERNAL_STORAGE);
    }

    public String getName()
    {
        return "read_external_storage";
    }
}
