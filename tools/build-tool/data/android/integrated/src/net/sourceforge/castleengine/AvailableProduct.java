/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/* Information about the product available for purchase.
   Used by in-app purchases and analytics components.
*/
public class AvailableProduct
{
    public AvailableProduct(String aId)
    {
        id = aId;
    }

    /* Unique product identifier. */
    String id;

    /* Product category. Used only by analytics for now. */
    String category;

    /* When reporting purchase to analytics, we may report the price of the product.
       *This is not necessarily the price user paid for the item!!*
       You have to provide the information about item prices using the
       in-app-purchases-set-available-products message ComponentGoogleInAppPurchases.
       It's *your responsibility to provide prices equal to what you set in
       the Google Play console*.

       Currency is in ISO 4217 format.
       Integer price is in cents.
       Just like for GameAnalytics (although it's used for all analytics implementations):
       https://github.com/GameAnalytics/GA-SDK-ANDROID/wiki/Business-Event .

       Leave currency = null, price = 0 if not known.
    */
    String analyticsCurrency;
    int analyticsPrice;
}
