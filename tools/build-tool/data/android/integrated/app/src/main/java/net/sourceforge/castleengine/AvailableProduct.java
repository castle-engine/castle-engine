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

    /* Price, as a string in local user currency.
     * As defined in the store.
     * null if not known yet
     */
    String price;

    /* Title and description of the product, as defined in the store.
     * May be translated to current user language.
     *
     * null if not known yet (not received from the store yet).
     */
    String title, description;

    /* Price in micro-units, where 1,000,000 micro-units equal one unit of the currency.
     * 0 if not known yet.
     * See https://developer.android.com/google/play/billing/billing_reference.html
     */
    long priceAmountMicros;

    /* ISO 4217 currency code for price.
     * null if not known yet.
     * See https://developer.android.com/google/play/billing/billing_reference.html
     */
    String priceCurrencyCode;
}
