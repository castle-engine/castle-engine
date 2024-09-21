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

public class ServiceTestlib extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceTestlib";

    public ServiceTestlib(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "internal_test_cpp_lib";
    }
}
