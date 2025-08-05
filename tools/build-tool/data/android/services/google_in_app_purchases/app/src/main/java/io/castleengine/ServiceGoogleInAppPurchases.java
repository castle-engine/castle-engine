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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.LinkedList;
import java.util.Map.Entry;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.billingclient.api.AcknowledgePurchaseParams;
import com.android.billingclient.api.AcknowledgePurchaseResponseListener;
import com.android.billingclient.api.BillingClient;
import com.android.billingclient.api.BillingClient.SkuType;
import com.android.billingclient.api.BillingClient.BillingResponseCode;
import com.android.billingclient.api.BillingClientStateListener;
import com.android.billingclient.api.BillingFlowParams;
import com.android.billingclient.api.BillingResult;
import com.android.billingclient.api.ConsumeParams;
import com.android.billingclient.api.ConsumeResponseListener;
import com.android.billingclient.api.Purchase;
import com.android.billingclient.api.Purchase.PurchaseState;
import com.android.billingclient.api.PurchasesResponseListener;
import com.android.billingclient.api.PurchasesUpdatedListener;
import com.android.billingclient.api.SkuDetails;
import com.android.billingclient.api.SkuDetailsParams;
import com.android.billingclient.api.SkuDetailsResponseListener;

/**
 * Integration of Google Billing API with Castle Game Engine.
 */
public class ServiceGoogleInAppPurchases extends ServiceAbstract
    implements PurchasesUpdatedListener, AcknowledgePurchaseResponseListener
{
    private static final String CATEGORY = "ServiceGoogleInAppPurchases";
    // Log some internal information.
    private final boolean debug = false;

    BillingClient billingClient;
    boolean billingClientConnected = false;

    // Purchase tokens for known owned products, saved on this list to allow consuming items.
    Map<String, String> purchaseTokens;

    /* Products available. For now, their list (count and ids)
     * must be initialized by the native code, although this assumption should
     * not affect much logic. The remaining information is filled when the store
     * answers.
     *
     * null if not initialized yet. */
    Map<String, AvailableProduct> availableProducts;

    /* operationQueue stuff -------------------------------------------------- */

    /*
     * The operations requested are queued.
     *
     * - They will be performed in the order as requested.
     *
     * - No two operations will be performed at once. This avoids problems
     *   with trying to consume items when refreshAvailableForPurchaseAndPurchased
     *   is on-going (e.g. because onResume occurred), which could result in getting
     *   owns() notifications twice, causing consume() calls twice.
     *
     *   Note that OperationPurchase however cannot be done nicely like this.
     *   We have to listen on it all the time,
     *
     * - Old:
     *   Some billing operations are executed in background threads,
     *   otherwise they would block the main thread. This is completely hidden here,
     *   using AsyncTask.
     *
     *   New: This is actually no longer a problem. The new Google Play Billing API
     *   has proper asynchronous API for everything, and there's no need to use AsyncTask.
     */
    LinkedList<Operation> operationQueue;

    private void currentOperationFinished() {

        if (operationQueue.poll() == null) {
            logError(CATEGORY, "currentOperationFinished(), but operationQueue is empty");
            return;
        }
        Operation o = operationQueue.peek();
        if (o != null) {
            o.run();
        }
    }

    private void addOperation(Operation o)
    {
        boolean wasEmpty = operationQueue.peek() == null;
        operationQueue.add(o);
        if (wasEmpty) {
            o.run();
        }
    }

    /* Operation ------------------------------------------------------------- */

    private abstract static class Operation {
        /* Override and perform the actual work here,
         * and call currentOperationFinished() at the end.
         *
         * This is called from the main thread, and should call
         * currentOperationFinished() from the main thread, thus completely
         * hiding any possible threads inside.
         */
        public abstract void run();
    }

    /* OperationRefreshPrices ---------------------------------- */

    /* Refresh available for purchase stuff.
     * Get the list of prices for availableProducts.
     * Calls messages
     * - in-app-purchases-can-purchase (for each available product)
     * - in-app-purchases-refreshed-prices (at the end)
     *
     * Secured (silently ignored) if
     * - availableProducts are null,
     * - not billingClientConnected.
     * So e.g. onResume can safely call it (even though availableProducts
     * may be unset yet) and setAvailableProducts may safely call it (even though
     * billingClientConnected may be false).
     */
    private class OperationRefreshPrices extends Operation
        implements SkuDetailsResponseListener
    {
        @Override
        public void onSkuDetailsResponse(@NonNull BillingResult billingResult,
                                         @Nullable List<SkuDetails> skuDetailsList) {
            try {
                if (billingResult.getResponseCode() == BillingResponseCode.OK && skuDetailsList != null) {
                    logInfo(CATEGORY, skuDetailsList.size() + " items available for purchase.");
                    for (SkuDetails skuDetails : skuDetailsList) {
                        String productId = skuDetails.getSku();
                        AvailableProduct product;
                        if (availableProducts.containsKey(productId)) {
                            product = availableProducts.get(productId);
                        } else {
                            logWarning(CATEGORY, "Product " + productId + " reported by querySkuDetailsAsync, but not in availableProducts");
                            product = new AvailableProduct(productId);
                            availableProducts.put(product.id, product);
                        }

                        product.price = skuDetails.getPrice();
                        product.title = skuDetails.getTitle();
                        product.description = skuDetails.getDescription();
                        product.priceAmountMicros = skuDetails.getPriceAmountMicros();
                        product.priceCurrencyCode = skuDetails.getPriceCurrencyCode();
                        product.skuDetails = skuDetails;
                        messageSend(new String[]{"in-app-purchases-can-purchase",
                                product.id,
                                product.price,
                                product.title,
                                product.description,
                                Long.toString(product.priceAmountMicros),
                                product.priceCurrencyCode
                        });
                        if (debug) {
                            logInfo(CATEGORY, "Product " + productId + " available for purchase, price: " + product.price + " " + product.priceCurrencyCode);
                        }
                    }
                    messageSend(new String[]{"in-app-purchases-refreshed-prices"});
                } else {
                    logError(CATEGORY, "Error when refreshing price: " + billingResult.getDebugMessage());
                }
            } finally {
                currentOperationFinished();
            }
        }

        public final void run()
        {
            if (availableProducts != null && billingClientConnected) {
                // calculate productList

                List<String> productList = new ArrayList<>();
                for (Entry<String, AvailableProduct> entry : availableProducts.entrySet()) {
                    productList.add(entry.getKey());
                }

                SkuDetailsParams.Builder params = SkuDetailsParams.newBuilder();
                params.setSkusList(productList).setType(SkuType.INAPP);
                billingClient.querySkuDetailsAsync(params.build(), this);
            } else {
                currentOperationFinished();
            }
        }
    }

    /* OperationRefreshPurchased --------------------------------------------- */

    /* Refresh currently purchased (owned) stuff.
     * Calls messages
     * - in-app-purchases-owns (for each owned product)
     * - in-app-purchases-refreshed-purchases (at the end)
     *
     * Like OperationRefreshPrices, secured (silently ignored)
     * if availableProducts is null or billingClientConnected = false.
     */
    private class OperationRefreshPurchased extends Operation
        implements PurchasesResponseListener
    {
        @Override
        public void onQueryPurchasesResponse(@NonNull BillingResult billingResult, @Nullable List<Purchase> purchases)
        {
            try {
                // See https://developer.android.com/google/play/billing/integrate#show-products
                if (billingResult.getResponseCode() == BillingResponseCode.OK && purchases != null) {
                    /* To allow purchases outside of app (e.g. promo codes redeemed in Google Play),
                       we react to purchases detected by OperationRefreshPurchased (done from onResume)
                       just like to regular purchases.
                       They will be acknowledged, send to analytics, call owns(). */
                    handlePurchases(purchases);
                    messageSend(new String[]{"in-app-purchases-refreshed-purchases"});
                } else if (billingResult.getResponseCode() == BillingResponseCode.USER_CANCELED) {
                    // No error or warning, this is normal
                    logInfo(CATEGORY, "User cancelled the purchase");
                } else {
                    logWarning(CATEGORY, "Error when getting purchased items: " + billingResult.getDebugMessage());
                }
            } finally {
                currentOperationFinished();
            }
        }

        public final void run()
        {
            if (availableProducts != null && billingClientConnected) {
                billingClient.queryPurchasesAsync(SkuType.INAPP, this);
            } else {
                currentOperationFinished();
            }
        }
    }

    /* OperationPurchase ----------------------------------------------------- */

    private class OperationPurchase extends Operation
    {
        public String productName;

        public final void run()
        {
            try {
                if (!billingClientConnected) {
                    logError(CATEGORY, "Cannot purchase " + productName + " because billing client not connected");
                    return;
                }

                if (!availableProducts.containsKey(productName)) {
                    logError(CATEGORY, "Cannot purchase " + productName + " because it is not known in availableProducts");
                    return;
                }

                AvailableProduct product = availableProducts.get(productName);
                if (product.skuDetails == null) {
                    logError(CATEGORY, "Cannot purchase " + productName + " because skuDetails not known yet (Google Play Billing did not initialize yet the information about products)");
                    return;
                }

                BillingFlowParams purchaseParams = BillingFlowParams.newBuilder()
                    .setSkuDetails((SkuDetails)product.skuDetails)
                    .build();

                BillingResult billingResult = billingClient.launchBillingFlow(getActivity(), purchaseParams);
                if (billingResult.getResponseCode() != BillingResponseCode.OK) {
                    logError(CATEGORY, "Error when launchBillingFlow: " + billingResult.getDebugMessage());
                }
            } finally{
                currentOperationFinished();
            }
        }
    }

    /* OperationConsume ------------------------------------------------------ */

    private class OperationConsume extends Operation
        implements ConsumeResponseListener
    {
        public String productName;

        @Override
        public void onConsumeResponse(@NonNull BillingResult billingResult, @NonNull String purchaseToken)
        {
            try {
                if (billingResult.getResponseCode() == BillingResponseCode.OK) {
                    logInfo(CATEGORY, "Consumed item " + productName);
                    messageSend(new String[]{"in-app-purchases-consumed", productName});
                } else {
                    logError(CATEGORY, "Failed to consume item " + productName + ", response: " + billingResult.getDebugMessage());
                }
            } finally {
                currentOperationFinished();
            }
        }

        public final void run()
        {
            if (!purchaseTokens.containsKey(productName)) {
                logError(CATEGORY, "Cannot consume item " + productName + ", purchaseToken unknown (it seems item is not purchased yet)");
                currentOperationFinished();
                return;
            }
            String purchaseToken = purchaseTokens.get(productName);

            ConsumeParams consumeParams = ConsumeParams.newBuilder()
                .setPurchaseToken(purchaseToken)
                .build();
            billingClient.consumeAsync(consumeParams, this);
        }
    }

    /* main class implementation --------------------------------------------- */

    private void acknowledgeIfNeeded(Purchase purchase)
    {
        /* We need to do acknowledgePurchase so that one-time purchases (non-consumable)
           are acknowledged, and not refunded.

           For consumables, this is not necessary.
           Pascal must call Consume which calls Java consumeAsync,
           and it acknowledges already.
           But it seems simpler to acknowledge just always.
        */
        if (!purchase.isAcknowledged()) {
            String purchaseToken = purchase.getPurchaseToken();

            AcknowledgePurchaseParams acknowledgePurchaseParams =
                AcknowledgePurchaseParams.newBuilder()
                    .setPurchaseToken(purchaseToken)
                    .build();
            billingClient.acknowledgePurchase(acknowledgePurchaseParams, this);
        }
    }

    private void handlePurchases(@NonNull List<Purchase> purchases)
    {
        for (Purchase purchase : purchases) {
            String signature = purchase.getSignature();

            // Watch out, signature may be null, tested on Pawel Android 4.3.
            // Should not be possible with Android SDK versions though, result is @NonNull
            /*
            if (signature == null) {
                logWarning(CATEGORY, "Missing purchase signature");
            }
            */

            /* Only handle purchases with state PURCHASED.
               Ignore PENDING purchases, see https://developer.android.com/google/play/billing/integrate#pending
            */
            if (purchase.getPurchaseState() == PurchaseState.PURCHASED) {

                /* Use purchaseTokens to avoid applying the same purchase multiple times.
                 *
                 * This way:
                 *
                 * - We avoid sending multiple "in-app-purchases-owns" (from owns() call)
                 *   for the same item, when no new purchase occurred.
                 *   While this is not really prohibited (our CastleInAppPurchases documentation
                 *   talks about this, and insists that you depend on SuccessfullyConsumed
                 *   for consumables), we try to avoid it in the common case.
                 *
                 *   And it would occur almost always after buying (if not for this safeguard).
                 *   We refresh purchases after onResume, which happens also when returning
                 *   from "buy" dialog. In this case, OperationPurchase and then OperationRefreshPurchased
                 *   notify about owning the same item (unless the OperationConsume
                 *   will happen between, but it may not have a chance).
                 *   So you would always get two "in-app-purchases-owns" messages,
                 *   which in some cases means getting two consume() calls,
                 *   which means you get 1 error
                 *
                 *     Cannot consume item " + productName + ", purchaseToken unknown (it seems item is not purchased yet)"
                 *
                 *   for every purchase.
                 *
                 * - We avoid sending acknowledgeIfNeeded 2x times for the same item.
                 *   Asking for acknowledge 2nd time, when the 1st acknowledge request is in progress,
                 *   would mean that we send to Google request to acknowledge something that is already
                 *   (on server) acknowledged. Resulting in logs like this:
                 *
                 *     09-30 01:37:36.618 28171 30469 I dragon_squash: ServiceGoogleInAppPurchases: Acknowledged purchase OK
                 *     09-30 01:37:36.722 28171 31165 E dragon_squash: ServiceGoogleInAppPurchases: Error acknowledging purchase: Server error, please try again.
                 *
                 *   (so purchase caused 2x acknowledgeIfNeeded, and 2nd one of course failed on server).
                 *
                 * - Finally, this effectively checks that each purchase token in used only once.
                 *
                 *   So it's some anti-fraud system (but limited to frontend) recommended by
                 *   https://developer.android.com/google/play/billing/security ,
                 *   https://developer.android.com/google/play/billing/developer-payload .
                 */
                String purchaseToken = purchase.getPurchaseToken();
                if (!purchaseTokens.containsValue(purchaseToken)) {
                    acknowledgeIfNeeded(purchase);

                    for (String productName : purchase.getSkus()) {
                        purchaseTokens.put(productName, purchaseToken);

                        if (debug) {
                            logInfo(CATEGORY, "Product " + productName + " purchased");
                        }

                        AvailableProduct product;
                        if (availableProducts.containsKey(productName)) {
                            product = availableProducts.get(productName);
                        } else {
                            product = new AvailableProduct(productName);
                        }

                        String originalJson = purchase.getOriginalJson();

                        getActivity().onPurchase(product, originalJson, signature);
                        owns(productName, purchase);
                    }
                }
            }
        }
    }

    /* Handle purchases, initiated by our own code (from OperationPurchase) and outside of our code
       (e.g. by user using promo code in Google Play app).
     */
    @Override
    public void onPurchasesUpdated(BillingResult responseCode, @Nullable List<Purchase> purchases)
    {
        if (responseCode.getResponseCode() == BillingResponseCode.OK && purchases != null) {
            handlePurchases(purchases);
        } else {
            logWarning(CATEGORY, "Purchase failed: " + responseCode.getDebugMessage());
        }
    }

    @Override
    public void onAcknowledgePurchaseResponse(@NonNull BillingResult billingResult)
    {
        if (billingResult.getResponseCode() == BillingResponseCode.OK) {
            logInfo(CATEGORY, "Acknowledged purchase OK");
        } else {
            logError(CATEGORY, "Error acknowledging purchase: " + billingResult.getDebugMessage());
        }
    }

    public ServiceGoogleInAppPurchases(MainActivity activity)
    {
        super(activity);
        purchaseTokens = new HashMap<>();
        operationQueue = new LinkedList<>();
    }

    public String getName()
    {
        return "google-in-app-purchases";
    }

    @Override
    public void onCreate()
    {
        billingClient = BillingClient.newBuilder(getActivity())
            .enablePendingPurchases()
            .setListener(this)
            .build();
        billingClient.startConnection(new BillingClientStateListener() {
            @Override
            public void onBillingSetupFinished(@NonNull BillingResult billingResult) {
                if (billingResult.getResponseCode() == BillingResponseCode.OK) {
                    billingClientConnected = true;
                    addOperation(new OperationRefreshPrices());
                    addOperation(new OperationRefreshPurchased());
                } else {
                    logError(CATEGORY, "Error connecting BillingClient: " + billingResult.getDebugMessage());
                }
            }

            @Override
            public void onBillingServiceDisconnected() {
                billingClientConnected = false;
                // TODO: Try to restart the connection on the next request to
                // Google Play by calling the startConnection() method.
            }
        });
    }

    @Override
    public void onResume()
    {
        /* Get purchases on resume, if user bought something while we were paused.
           Follows "Recognizing out-of-app purchases" from
           https://developer.android.com/google/play/billing/migrate */
        addOperation(new OperationRefreshPurchased());
    }

    private void purchase(String productName)
    {
        OperationPurchase o = new OperationPurchase();
        o.productName = productName;
        addOperation(o);
    }

    private void consume(String productName)
    {
        OperationConsume o = new OperationConsume();
        o.productName = productName;
        addOperation(o);
    }

    /* Internal notification that productName is owned, along with the Purchase instance
     * (never null) that indicates the ownership.
     *
     * This can be called only when purchase.getPurchaseState() == PurchaseState.PURCHASED!
     */
    private void owns(String productName, Purchase purchase)
    {
        // Should not happen, according to our usage of owns()
        int purchaseState = purchase.getPurchaseState();
        if (purchaseState != PurchaseState.PURCHASED) {
            logError(CATEGORY, "Ignoring owns(" + productName + "), as item not in PURCHASED state: " + purchaseState);
            return;
        }

        messageSend(new String[]{"in-app-purchases-owns", productName});
    }

    private void setAvailableProducts(String productsListStr)
    {
        availableProducts = new HashMap<>();
        String[] productsList = splitString(productsListStr, 2);
        for (String productStr : productsList) {
            String[] productInfo = splitString(productStr, 3);
            if (productInfo.length != 2) {
                logError(CATEGORY, "Product info invalid: " + productStr);
                continue;
            }
            AvailableProduct product = new AvailableProduct(productInfo[0]);
            product.category = productInfo[1];
            availableProducts.put(product.id, product);
        }

        addOperation(new OperationRefreshPrices());
        addOperation(new OperationRefreshPurchased());
    }

    private void refreshPurchases()
    {
        addOperation(new OperationRefreshPurchased());
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("in-app-purchases-set-available-products")) {
            setAvailableProducts(parts[1]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("in-app-purchases-purchase")) {
            purchase(parts[1]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("in-app-purchases-consume")) {
            consume(parts[1]);
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("in-app-purchases-refresh-purchases")) {
            refreshPurchases();
            return true;
        } else
        {
            return false;
        }
    }
}
