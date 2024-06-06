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

package io.castleengine;

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

    private boolean initialized;
    private boolean resumed;
    private String apiKey = ""; // never null
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
        if (parts.length == 2 && parts[0].equals("tenjin-initialize")) {
            apiKey = parts[1];
            if (resumed) {
                initialize();
            }
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("tenjin-send-event")) {
            if (initialized) {
                tenjinInstance.eventWithName(parts[1]);
            }
            return true;
        } else
        {
            return false;
        }
    }

    // Initilize Tenjin. Call only when resumed, and apiKey is non-empty.
    private void initialize()
    {
        if ((!resumed) || apiKey.isEmpty()) {
            logWarning(CATEGORY, "initialize called when not resumed, or not set apiKey");
            return;
        }

        tenjinInstance = TenjinSDK.getInstance(getActivity(), apiKey);
        tenjinInstance.connect();
        initialized = true;
    }

    @Override
    public void onResume()
    {
        resumed = true;
        if (!apiKey.isEmpty()) {
            initialize();
        }
    }

    @Override
    public void onPause()
    {
        resumed = false;
    }

    @Override
    public void onPurchase(AvailableProduct product, String originalJson, String signature)
    {
        if (!initialized) {
            return;
        }

        double price = ((double) product.priceAmountMicros / 1000000.0);
        tenjinInstance.transaction(product.id, product.priceCurrencyCode, 1, price, originalJson, signature);
    }
}
