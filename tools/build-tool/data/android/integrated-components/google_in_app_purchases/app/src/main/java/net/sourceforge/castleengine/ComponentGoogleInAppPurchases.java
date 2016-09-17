/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import org.json.JSONObject;
import org.json.JSONException;

import android.content.ServiceConnection;
import android.content.Intent;
import android.content.ComponentName;
import android.content.Context;
import android.content.IntentSender.SendIntentException;
import android.content.BroadcastReceiver;
import android.content.IntentFilter;
import android.os.IBinder;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.RemoteException;
import android.util.Log;
import android.app.PendingIntent;
import android.app.Activity;

import com.android.vending.billing.IInAppBillingService;

/**
 * Integration of Google Billing API with Castle Game Engine.
 */
public class ComponentGoogleInAppPurchases extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentGoogleInAppPurchases";
    private static int REQUEST_PURCHASE = 9200;
    // Log some internal information.
    private final boolean debug = false;

    IInAppBillingService mBillingService;
    String mPayLoad;
    // Purchase tokens for known owns products, saved on this list to allow consuming items.
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
     *   is going (e.g. because onResume occured), which could result in getting
     *   owns() notifications twice, causing consume() calls twice.
     *
     * - Some billing operations are executed in background threads,
     *   otherwise they would block the main thread. This is completely hidden here.
     */
    LinkedList<Operation> operationQueue;

    private final void currentOperationFinished()
    {
        if (operationQueue.poll() == null) {
            Log.e(TAG, "currentOperationFinished(), but operationQueue is empty");
            return;
        }
        Operation o = operationQueue.peek();
        if (o != null) {
            o.run();
        }
    }

    private final void addOperation(Operation o)
    {
        boolean wasEmpty = operationQueue.peek() == null;
        operationQueue.add(o);
        if (wasEmpty) {
            o.run();
        }
    }

    /* Operation ------------------------------------------------------------- */

    private abstract class Operation {
        /* Override and perform the actual work here,
         * and call currentOperationFinished() at end.
         *
         * This is called from the main thread, and should call
         * currentOperationFinished() from the main thread, thus completely
         * hiding any possible threads inside.
         */
        public abstract void run();

        /* You may override and handle the result here. */
        public void purchaseActivityResult(int resultCode, Intent data)
        {
            Log.i(TAG, "Received REQUEST_PURCHASE activity result, but the current operation is not a purchase.");
        }
    }

    /* OperationRefreshAvailableForPurchase ---------------------------------- */

    /* Refresh available for purchase stuff.
     * Get the list of prices for availableProducts.
     *
     * Secured (silently ignored) if availableProducts or mBillingService are null.
     * So e.g. onServiceConnected can safely call if (even though availableProducts
     * may be unset yet) and setAvailableProducts may safely call it (even though
     * mBillingService be unset yet).
     */
    private class OperationRefreshAvailableForPurchase extends Operation {

        /* Class to pass productList to RefreshAvailableForPurchaseTask.
         *
         * Reason: Passing ArrayList<String> makes Java warnings about unsafe code:
         * warning: [unchecked] unchecked generic array creation for varargs parameter of type ArrayList<String>[]
         * new RefreshAvailableForPurchaseTask().execute(availableProducts);
         * (compile with
         * ant "-Djava.compilerargs=-Xlint:unchecked -Xlint:deprecation" debug
         * to see it, see http://stackoverflow.com/questions/7682150/use-xlintdeprecation-with-android ).
         */
        private class RefreshAvailableForPurchaseInput {
            ArrayList<String> productList;
        }

        private class RefreshAvailableForPurchaseTask extends AsyncTask<RefreshAvailableForPurchaseInput, Void, Bundle> {
            protected Bundle doInBackground(RefreshAvailableForPurchaseInput... inputs) {
                ArrayList<String> productList = inputs[0].productList;
                Bundle queryProducts = new Bundle();
                queryProducts.putStringArrayList("ITEM_ID_LIST", productList);
                try {
                    return mBillingService.getSkuDetails(3, getActivity().getPackageName(), "inapp", queryProducts);
                } catch (RemoteException e) {
                    Log.e(TAG, "RemoteException at getSkuDetails: " + e.getMessage());
                    return null;
                }
            }

            protected void onPostExecute(Bundle productDetails) {
                try {
                    if (productDetails == null) {
                        return; // exit in case there was a RemoteException
                    }
                    try {
                        int response = productDetails.getInt("RESPONSE_CODE");
                        if (response == InAppPurchasesHelper.BILLING_RESPONSE_RESULT_OK) {
                            ArrayList<String> responseList
                              = productDetails.getStringArrayList("DETAILS_LIST");

                            Log.i(TAG, responseList.size() + " items available for purchase.");
                            for (String thisResponse : responseList) {
                                JSONObject object = new JSONObject(thisResponse);
                                String productId = object.getString("productId");

                                AvailableProduct product;
                                if (availableProducts.containsKey(productId)) {
                                    product = availableProducts.get(productId);
                                } else {
                                    Log.w(TAG, "Product " + productId + " reported by getSkuDetails, but not in availableProducts");
                                    product = new AvailableProduct(productId);
                                    availableProducts.put(product.id, product);
                                }

                                product.price = object.getString("price");
                                product.title = object.getString("title");
                                product.description = object.getString("description");
                                product.priceAmountMicros = Long.parseLong(object.getString("price_amount_micros"));
                                product.priceCurrencyCode = object.getString("price_currency_code");
                                messageSend(new String[]{"in-app-purchases-can-purchase",
                                    product.id,
                                    product.price,
                                    product.title,
                                    product.description,
                                    Long.toString(product.priceAmountMicros),
                                    product.priceCurrencyCode
                                });
                                if (debug) {
                                    Log.i(TAG, "Product " + productId + " available for purchase, details: " + thisResponse);
                                }
                            }
                        } else {
                            Log.w(TAG, "Error response when getting list of stuff available for purchase: " + InAppPurchasesHelper.billingResponseToStr(response));
                        }
                    } catch (JSONException e) {
                        Log.e(TAG, "Failed to parse getSkuDetails data:  " + e.getMessage());
                    }
                } finally {
                    currentOperationFinished();
                }
            }
        }

        public final void run()
        {
            if (availableProducts != null && mBillingService != null) {
                // calculate input
                RefreshAvailableForPurchaseInput input = new RefreshAvailableForPurchaseInput();
                input.productList = new ArrayList<String>();
                for (Entry<String, AvailableProduct> entry : availableProducts.entrySet()) {
                    input.productList.add(entry.getKey());
                }

                /* We use AsyncTask to avoid using synchronous billing methods on the main
                   UI thread. See
                   http://developer.android.com/reference/android/os/AsyncTask.html
                   http://stackoverflow.com/questions/24768070/where-do-i-put-code-to-pass-a-request-to-the-in-app-billing-service-in-android-i
                */
                new RefreshAvailableForPurchaseTask().execute(input);
            } else {
                currentOperationFinished();
            }
        }
    }

    /* OperationRefreshPurchased --------------------------------------------- */

    /* Refresh currently purchased (owned) stuff.
     *
     * Like OperationRefreshAvailableForPurchase, secured (silently ignored)
     * if availableProducts or mBillingService are null.
     */
    private class OperationRefreshPurchased extends Operation {

        private class RefreshPurchasedTask extends AsyncTask<Void, Void, Bundle> {
            protected Bundle doInBackground(Void... input) {
                try {
                    return mBillingService.getPurchases(3, getActivity().getPackageName(), "inapp", null);
                } catch (RemoteException e) {
                    Log.e(TAG, "RemoteException when getting purchased stuff: " + e.getMessage());
                    return null;
                }
            }

            protected void onPostExecute(Bundle ownedItems) {
                try {
                    if (ownedItems == null) {
                        return; // exit in case there was a RemoteException
                    }
                    int response = ownedItems.getInt("RESPONSE_CODE");
                    if (response == InAppPurchasesHelper.BILLING_RESPONSE_RESULT_OK) {
                        ArrayList<String> ownedProducts =
                            ownedItems.getStringArrayList("INAPP_PURCHASE_ITEM_LIST");
                        ArrayList<String>  purchaseDataList =
                            ownedItems.getStringArrayList("INAPP_PURCHASE_DATA_LIST");
                        // Not available, it seems.
                        // ArrayList<String>  signatureList =
                        //     ownedItems.getStringArrayList("INAPP_DATA_SIGNATURE");
                        String continuationToken =
                            ownedItems.getString("INAPP_CONTINUATION_TOKEN");
                        // if (signatureList == null) {
                        //     Log.w(TAG, "Missing INAPP_DATA_SIGNATURE");
                        // }

                        for (int i = 0; i < purchaseDataList.size(); ++i) {
                            String purchaseData = purchaseDataList.get(i);
                            // String signature;
                            // if (signatureList != null) {
                            //     signature = signatureList.get(i);
                            // } else {
                            //     signature = null;
                            // }
                            String productId = ownedProducts.get(i);
                            owns(productId, purchaseData);
                        }

                        messageSend(new String[]{"in-app-purchases-known-completely"});

                        if (continuationToken != null) {
                            Log.e(TAG, "getPurchases returned continuationToken != null, not supported now");
                        }
                    } else {
                        Log.w(TAG, "Error when getting owned items: " + InAppPurchasesHelper.billingResponseToStr(response));
                    }
                } finally {
                    currentOperationFinished();
                }
            }
        }

        public final void run()
        {
            if (availableProducts != null && mBillingService != null) {
                new RefreshPurchasedTask().execute();
            } else {
                currentOperationFinished();
            }
        }
    }

    /* OperationPurchase ----------------------------------------------------- */

    private class OperationPurchase extends Operation {

        public final void purchaseActivityResult(int resultCode, Intent data)
        {
            try {
                if (resultCode == Activity.RESULT_OK) {
                    //int responseCode = data.getIntExtra("RESPONSE_CODE", 0);
                    String purchaseData = data.getStringExtra("INAPP_PURCHASE_DATA");
                    // Watch out, signature may be null, tested on Pawel Android 4.3.
                    String signature = data.getStringExtra("INAPP_DATA_SIGNATURE");
                    if (signature == null) {
                        Log.w(TAG, "Missing INAPP_DATA_SIGNATURE");
                    }

                    try {
                        JSONObject jo = new JSONObject(purchaseData);
                        String productId = jo.getString("productId");
                        String payLoadReceived = jo.getString("developerPayload");
                        if (debug) {
                            Log.i(TAG, "Product " + productId + " purchased, details: " + purchaseData);
                        }
                        if (payLoadReceived.equals(mPayLoad)) {
                            AvailableProduct product;
                            if (availableProducts.containsKey(productId)) {
                                product = availableProducts.get(productId);
                            } else {
                                product = new AvailableProduct(productId);
                            }
                            getActivity().onPurchase(product, purchaseData, signature);
                            owns(productId, purchaseData);
                        } else {
                            Log.e(TAG, "Rejecting buying the " + productId + " because received payload (" + payLoadReceived + ")  does not match send payload");
                        }
                    }
                    catch (JSONException e) {
                        Log.e(TAG, "Failed to parse purchase data: " + e.getMessage());
                    }
                } else {
                    Log.w(TAG, "Purchase result not OK: " +resultCode);
                }
            } finally {
                currentOperationFinished();
            }
        }

        public String productName;

        public final void run()
        {
            if (mBillingService == null) {
                currentOperationFinished();
                return;
            }

            try {
                Bundle buyIntentBundle = mBillingService.getBuyIntent(
                    3, getActivity().getPackageName(), productName, "inapp", mPayLoad);
                int responseCode = buyIntentBundle.getInt("RESPONSE_CODE");
                if (responseCode != InAppPurchasesHelper.BILLING_RESPONSE_RESULT_OK) {
                    Log.e(TAG, "Error when starting buy intent: " + InAppPurchasesHelper.billingResponseToStr(responseCode));
                    currentOperationFinished();
                    return;
                }
                PendingIntent pendingIntent = buyIntentBundle.getParcelable("BUY_INTENT");
                if (pendingIntent == null) {
                    Log.e(TAG, "pendingIntent == null, this should not happen");
                    currentOperationFinished();
                    return;
                }
                getActivity().startIntentSenderForResult(pendingIntent.getIntentSender(),
                    REQUEST_PURCHASE, new Intent(), Integer.valueOf(0), Integer.valueOf(0),
                    Integer.valueOf(0));
            } catch (SendIntentException e) {
                Log.e(TAG, "SendIntentException when sending buy intent: " + e.getMessage());
            } catch (RemoteException e) {
                Log.e(TAG, "RemoteException when sending buy intent: " + e.getMessage());
            }
        }
    }

    /* OperationConsume ------------------------------------------------------ */

    private class OperationConsume extends Operation {
        private class ConsumeInput {
            String productName;
            String purchaseToken;
        }

        private class ConsumePurchaseTask extends AsyncTask<ConsumeInput, Void, Integer> {
            // Exit code when consumePurchase throws RemoteException,
            // Different than existing responses of billing API,
            // see http://developer.android.com/google/play/billing/billing_reference.html
            private final int REMOTE_EXCEPTION_ERROR = 100;

            private String mProductName;

            @Override
            protected Integer doInBackground(ConsumeInput... consumeInputs) {
                ConsumeInput consumeInput = consumeInputs[0]; // just take 1st param
                mProductName = consumeInput.productName;
                try {
                    return mBillingService.consumePurchase(3, getActivity().getPackageName(),
                        consumeInput.purchaseToken);
                } catch (RemoteException e) {
                    Log.e(TAG, "RemoteException when getting purchased stuff: " + e.getMessage());
                    return REMOTE_EXCEPTION_ERROR;
                }
            }

            protected void onPostExecute(Integer response) {
                try {
                    if (response == InAppPurchasesHelper.BILLING_RESPONSE_RESULT_OK) {
                        Log.i(TAG, "Consumed item " + mProductName);
                        messageSend(new String[]{"in-app-purchases-consumed", mProductName});
                    } else {
                        Log.e(TAG, "Failed to consume item " + mProductName + ", response: " + response);
                    }
                } finally {
                    currentOperationFinished();
                }
            }
        }

        public String productName;

        public final void run()
        {
            if (!purchaseTokens.containsKey(productName)) {
                Log.e(TAG, "Cannot consume item " + productName + ", purchaseToken unknown (it seems item is not purchased yet)");
                currentOperationFinished();
                return;
            }
            ConsumeInput consumeInput = new ConsumeInput();
            consumeInput.productName = productName;
            consumeInput.purchaseToken = purchaseTokens.get(productName);
            purchaseTokens.remove(productName);
            new ConsumePurchaseTask().execute(consumeInput);
        }
    }

    /* main class implementation --------------------------------------------- */

    public ComponentGoogleInAppPurchases(MainActivity activity)
    {
        super(activity);
        purchaseTokens = new HashMap<String, String>();
        operationQueue = new LinkedList<Operation>();
    }

    public String getName()
    {
        return "google-in-app-purchases";
    }

    ServiceConnection mBillingConnection = new ServiceConnection() {
        @Override
        public void onServiceDisconnected(ComponentName name) {
            mBillingService = null;
        }

        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            mBillingService = IInAppBillingService.Stub.asInterface(service);
            addOperation(new OperationRefreshAvailableForPurchase());
            addOperation(new OperationRefreshPurchased());
        }
    };

    @Override
    public void onCreate()
    {
        RandomString randomString = new RandomString(36);
        // For now, randomize mPayLoad just once at the beginning of the app.
        // This avoids thinking what happens when user initiated 2 purchases
        // in a short time.
        mPayLoad = randomString.nextString();
        //Log.i(TAG, "Billing payload: " + mPayLoad);

        myPromoReceiver = new MyPromoReceiver();

        Intent serviceIntent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
        serviceIntent.setPackage("com.android.vending");
        getActivity().bindService(serviceIntent, mBillingConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    public void onDestroy() {
        if (mBillingService != null) {
            getActivity().unbindService(mBillingConnection);
        }
    }

    private class MyPromoReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.i(TAG, "PURCHASES_UPDATED received, possibly user redeemed promo code now, refreshing.");
            addOperation(new OperationRefreshPurchased());
        }
    }
    private MyPromoReceiver myPromoReceiver;

    @Override
    public void onResume()
    {
        /* Get purchases on resume, if user bought something while we were paused.
           Follows
           https://developer.android.com/google/play/billing/billing_promotions.html */
        addOperation(new OperationRefreshPurchased());

        /* Listen for promo code redeeming while app is running, following
           https://developer.android.com/google/play/billing/billing_promotions.html */
        IntentFilter promoFilter = new IntentFilter("com.android.vending.billing.PURCHASES_UPDATED");
        getActivity().registerReceiver(myPromoReceiver, promoFilter);
    }

    @Override
    public void onPause()
    {
        /* Stop listening for promo code redeeming while app is running, following
           https://developer.android.com/google/play/billing/billing_promotions.html */
        getActivity().unregisterReceiver(myPromoReceiver);
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

    private void owns(String productName, String purchaseData)
    {
        String purchaseToken;

        try {
            JSONObject jo = new JSONObject(purchaseData);
            int purchaseState = jo.getInt("purchaseState");
            if (purchaseState == 1) {
                Log.w(TAG, "Ignoring ownage of a cancelled item " + productName);
                return;
            }
            purchaseToken = jo.getString("purchaseToken");

            /* Use purchaseTokens to avoid sending multiple "in-app-purchases-owns"
             * for the same item, when no new purchase occured.
             * While this is not really prohibited (our CastleInAppPurchases documentation
             * talks about this, and insists that you depend on SuccessfullyConsumed
             * for consumables), we try to avoid it in the common case.
             *
             * And it would occur almost always after buying (if not for this safeguard).
             * We refresh purchases after onResume, which happens also when returning
             * from "buy" dialog. In this case, OperationPurchase and then OperationRefreshPurchased
             * notify about owning the same item (unless the OperationConsume
             * will happen between, but it may not have a chance).
             * So you would always get two "in-app-purchases-owns" messages,
             * which in some cases means getting two consume() calls,
             * which means you get 1 error
             *
             *   Cannot consume item " + productName + ", purchaseToken unknown (it seems item is not purchased yet)"
             *
             * for every purchase.
             */
            String knownPurchaseToken = purchaseTokens.get(productName);
            if (purchaseToken == null || !purchaseToken.equals(knownPurchaseToken)) {
                purchaseTokens.put(productName, purchaseToken);
                messageSend(new String[]{"in-app-purchases-owns", productName});
            }
        } catch (JSONException e) {
            Log.e(TAG, "Failed to parse purchaseData in owns: " + e.getMessage());
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent intent) {
        if (requestCode == REQUEST_PURCHASE) {
            Operation o = operationQueue.peek();
            if (o == null) {
                Log.e(TAG, "Received REQUEST_PURCHASE activity result, but no purchase operation is in progress now.");
            } else {
                o.purchaseActivityResult(resultCode, intent);
            }
        }
    }

    private void setAvailableProducts(String productsListStr)
    {
        availableProducts = new HashMap<String, AvailableProduct>();
        String[] productsList = splitString(productsListStr, 2);
        for (String productStr : productsList) {
            String[] productInfo = splitString(productStr, 3);
            if (productInfo.length != 2) {
                Log.e(TAG, "Product info invalid: " + productStr);
                continue;
            }
            AvailableProduct product = new AvailableProduct(productInfo[0]);
            product.category = productInfo[1];
            availableProducts.put(product.id, product);
        }

        addOperation(new OperationRefreshAvailableForPurchase());
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
        } else {
            return false;
        }
    }
}
