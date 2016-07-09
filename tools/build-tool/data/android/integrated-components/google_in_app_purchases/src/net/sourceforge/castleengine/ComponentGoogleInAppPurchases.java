/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;
import java.util.LinkedList;

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
 * Integration of Google Biling API with Castle Game Engine.
 */
public class ComponentGoogleInAppPurchases extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentGoogleInAppPurchases";
    private static int REQUEST_PURCHASE = 9200;

    IInAppBillingService mBilingService;
    String mPayLoad;
    // Purchase tokens for known owns skus, saved on this list to allow consuming items.
    Map<String, String> purchaseTokens;
    // Product names available, according to native code. Used to query product info.
    // null if not initialized yet.
    ArrayList<String> availableProducts;

    // Billing response codes
    public static final int BILLING_RESPONSE_RESULT_OK = 0;

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
     * - Some biling operations are executed in background threads,
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
     * Secured (silently ignored) if availableProducts or mBilingService are null.
     * So e.g. onServiceConnected can safely call if (even though availableProducts
     * may be unset yet) and setAvailableProducts may safely call it (even though
     * mBilingService be unset yet).
     */
    private class OperationRefreshAvailableForPurchase extends Operation {

        /* Class to pass skuList to RefreshAvailableForPurchaseTask.
         *
         * Reason: Passing ArrayList<String> makes Java warnings about unsafe code:
         * warning: [unchecked] unchecked generic array creation for varargs parameter of type ArrayList<String>[]
         * new RefreshAvailableForPurchaseTask().execute(availableProducts);
         * (compile with
         * ant "-Djava.compilerargs=-Xlint:unchecked -Xlint:deprecation" debug
         * to see it, see http://stackoverflow.com/questions/7682150/use-xlintdeprecation-with-android ).
         */
        private class RefreshAvailableForPurchaseInput {
            ArrayList<String> skuList;
        }

        private class RefreshAvailableForPurchaseTask extends AsyncTask<RefreshAvailableForPurchaseInput, Void, Bundle> {
            protected Bundle doInBackground(RefreshAvailableForPurchaseInput... inputs) {
                ArrayList<String> skuList = inputs[0].skuList;
                Bundle querySkus = new Bundle();
                querySkus.putStringArrayList("ITEM_ID_LIST", skuList);
                try {
                    return mBilingService.getSkuDetails(3, getActivity().getPackageName(), "inapp", querySkus);
                } catch (RemoteException e) {
                    Log.e(TAG, "RemoteException at getSkuDetails.");
                    e.printStackTrace();
                    return null;
                }
            }

            protected void onPostExecute(Bundle skuDetails) {
                try {
                    if (skuDetails == null) {
                        return; // exit in case there was a RemoteException
                    }
                    try {
                        int response = skuDetails.getInt("RESPONSE_CODE");
                        if (response == 0) {
                           ArrayList<String> responseList
                              = skuDetails.getStringArrayList("DETAILS_LIST");

                           Log.i(TAG, responseList.size() + " items available for purchase.");
                           for (String thisResponse : responseList) {
                              JSONObject object = new JSONObject(thisResponse);
                              String sku = object.getString("productId");
                              String price = object.getString("price");
                              messageSend(new String[]{"in-app-purchases-can-purchase", sku, price});
                              // Log.i(TAG, "You can buy " + sku + " for " + price);
                           }
                        } else {
                            Log.w(TAG, "Response when getting list of stuff available for purchase: " + response);
                        }
                    } catch (JSONException e) {
                        Log.e(TAG, "Failed to parse getSkuDetails data.");
                        e.printStackTrace();
                    }
                } finally {
                    currentOperationFinished();
                }
            }
        }

        public final void run()
        {
            if (availableProducts != null && mBilingService != null) {
                RefreshAvailableForPurchaseInput input = new RefreshAvailableForPurchaseInput();
                input.skuList = availableProducts;

                /* We use AsyncTask to avoid using synchronous biling methods on the main
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
     * if availableProducts or mBilingService are null.
     */
    private class OperationRefreshPurchased extends Operation {

        private class RefreshPurchasedTask extends AsyncTask<Void, Void, Bundle> {
            protected Bundle doInBackground(Void... input) {
                try {
                    return mBilingService.getPurchases(3, getActivity().getPackageName(), "inapp", null);
                } catch (RemoteException e) {
                    Log.e(TAG, "RemoteException when getting purchased stuff.");
                    e.printStackTrace();
                    return null;
                }
            }

            protected void onPostExecute(Bundle ownedItems) {
                try {
                    if (ownedItems == null) {
                        return; // exit in case there was a RemoteException
                    }
                    int response = ownedItems.getInt("RESPONSE_CODE");
                    if (response == BILLING_RESPONSE_RESULT_OK) {
                        ArrayList<String> ownedSkus =
                            ownedItems.getStringArrayList("INAPP_PURCHASE_ITEM_LIST");
                        ArrayList<String>  purchaseDataList =
                            ownedItems.getStringArrayList("INAPP_PURCHASE_DATA_LIST");
                        /*ArrayList<String>  signatureList =
                            ownedItems.getStringArrayList("INAPP_DATA_SIGNATURE"); */
                        String continuationToken =
                            ownedItems.getString("INAPP_CONTINUATION_TOKEN");

                        for (int i = 0; i < purchaseDataList.size(); ++i) {
                            String purchaseData = purchaseDataList.get(i);
                            // String signature = signatureList.get(i);
                            String sku = ownedSkus.get(i);
                            owns(sku, purchaseData /*, signature*/);
                        }

                        messageSend(new String[]{"in-app-purchases-known-completely"});

                        if (continuationToken != null) {
                            Log.e(TAG, "getPurchases returned continuationToken != null, not supported now");
                        }
                    } else {
                        Log.w(TAG, "Getting owned items result not OK " + response);
                    }
                } finally {
                    currentOperationFinished();
                }
            }
        }

        public final void run()
        {
            if (availableProducts != null && mBilingService != null) {
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
                    // Do not look at signature --- is null for our stuff,
                    // on Pawel Android 4.3? Nevermind, it is unused anyway.
                    //String signature = data.getStringExtra("INAPP_DATA_SIGNATURE");

                    try {
                        JSONObject jo = new JSONObject(purchaseData);
                        String sku = jo.getString("productId");
                        String payLoadReceived = jo.getString("developerPayload");
                        if (payLoadReceived.equals(mPayLoad)) {
                            owns(sku, purchaseData/*, signature*/);
                        } else {
                            Log.e(TAG, "Rejecting buying the " + sku + " because received payload (" + payLoadReceived + ")  does not match send payload");
                        }
                    }
                    catch (JSONException e) {
                        Log.e(TAG, "Failed to parse purchase data.");
                        e.printStackTrace();
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
            if (mBilingService == null) {
                currentOperationFinished();
                return;
            }

            try {
                Bundle buyIntentBundle = mBilingService.getBuyIntent(3, getActivity().getPackageName(),
                    productName, "inapp", mPayLoad);
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
                Log.e(TAG, "SendIntentException when sending buy intent.");
                e.printStackTrace();
            } catch (RemoteException e) {
                Log.e(TAG, "RemoteException when sending buy intent.");
                e.printStackTrace();
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
            // Different than existing responses of biling API,
            // see http://developer.android.com/google/play/billing/billing_reference.html
            private final int REMOTE_EXCEPTION_ERROR = 100;

            private String mProductName;

            protected Integer doInBackground(ConsumeInput... consumeInputs) {
                ConsumeInput consumeInput = consumeInputs[0]; // just take 1st param
                mProductName = consumeInput.productName;
                try {
                    return mBilingService.consumePurchase(3, getActivity().getPackageName(),
                        consumeInput.purchaseToken);
                } catch (RemoteException e) {
                    Log.e(TAG, "RemoteException when getting purchased stuff.");
                    e.printStackTrace();
                    return REMOTE_EXCEPTION_ERROR;
                }
            }

            protected void onPostExecute(Integer response) {
                try {
                    if (response == BILLING_RESPONSE_RESULT_OK) {
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

    ServiceConnection mBilingConnection = new ServiceConnection() {
        @Override
        public void onServiceDisconnected(ComponentName name) {
            mBilingService = null;
        }

        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            mBilingService = IInAppBillingService.Stub.asInterface(service);
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
        //Log.i(TAG, "Biling payload: " + mPayLoad);

        myPromoReceiver = new MyPromoReceiver();

        Intent serviceIntent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
        serviceIntent.setPackage("com.android.vending");
        getActivity().bindService(serviceIntent, mBilingConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    public void onDestroy() {
        if (mBilingService != null) {
            getActivity().unbindService(mBilingConnection);
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

    private void owns(String productName, String purchaseData/*, String signature*/)
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
            Log.e(TAG, "Failed to parse purchaseData in owns.");
            e.printStackTrace();
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

    private void setAvailableProducts(String[] products)
    {
        availableProducts = new ArrayList<String>(Arrays.asList(products));
        addOperation(new OperationRefreshAvailableForPurchase());
        addOperation(new OperationRefreshPurchased());
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("in-app-purchases-set-available-products")) {
            setAvailableProducts(parts[1].split(","));
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
