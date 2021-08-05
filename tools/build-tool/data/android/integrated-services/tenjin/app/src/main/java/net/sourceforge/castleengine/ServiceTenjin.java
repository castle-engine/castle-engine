/* -*- tab-width: 4 -*- */

/*
  Copyright 2021-2021 Michalis Kamburelis.

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

import com.tenjin.android.TenjinSDK;

/**
 * Tenjin ( https://www.tenjin.com/ )
 * integration with Castle Game Engine Android application.
 */
public class ServiceTenjin extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceTenjin";

    private boolean connected;
    private TenjinSDK tenjinInstance;

    public ServiceTenjin(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "tenjin";
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("tenjin-send-event")) {
            if (connected) {
                tenjinInstance.eventWithName(parts[1]);
            }
            return true;
        } else
        {
            return false;
        }
    }

    @Override
    public void onResume()
    {
        tenjinInstance = TenjinSDK.getInstance(getActivity(), "${ANDROID.TENJIN.API_KEY}");
        tenjinInstance.connect();
        connected = true;
    }

    @Override
    public void onPurchase(AvailableProduct product, String purchaseData, String signature)
    {
        if (!connected) {
            return;
        }

        double price = ((double) product.priceAmountMicros / 10000.0);
        tenjinInstance.transaction(product.id, product.priceCurrencyCode, 1, price, purchaseData, signature);
    }
}
