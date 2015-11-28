/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;

import org.json.JSONObject;
import org.json.JSONException;

import android.content.ServiceConnection;
import android.content.Intent;
import android.content.ComponentName;
import android.content.Context;
import android.content.IntentSender.SendIntentException;
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

    public ComponentGoogleInAppPurchases(MainActivity activity)
    {
        super(activity);
        purchaseTokens = new HashMap<String, String>();
    }

    ServiceConnection mBilingConnection = new ServiceConnection() {
        @Override
        public void onServiceDisconnected(ComponentName name) {
            mBilingService = null;
        }

        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            mBilingService = IInAppBillingService.Stub.asInterface(service);

            /* refresh available and purchased stuff.
             * If availableProducts == null, this is deferred for later. */
            if (availableProducts != null) {
                refreshAvailableForPurchase();
                refreshPurchased();
            }
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
        }
    }

    /**
     * Get the list of prices for availableProducts.
     * Call only when availableProducts != null.
     */
    private void refreshAvailableForPurchase()
    {
        if (mBilingService == null) {
            return;
        }

        RefreshAvailableForPurchaseInput input = new RefreshAvailableForPurchaseInput();
        input.skuList = availableProducts;

        /* We use AsyncTask to avoid using synchronous biling methods on the main
           UI thread. See
           http://developer.android.com/reference/android/os/AsyncTask.html
           http://stackoverflow.com/questions/24768070/where-do-i-put-code-to-pass-a-request-to-the-in-app-billing-service-in-android-i
        */
        new RefreshAvailableForPurchaseTask().execute(input);
    }

    private void purchase(String skuName)
    {
        if (mBilingService == null) {
            return;
        }

        try {
            Bundle buyIntentBundle = mBilingService.getBuyIntent(3, getActivity().getPackageName(),
                skuName, "inapp", mPayLoad);
            PendingIntent pendingIntent = buyIntentBundle.getParcelable("BUY_INTENT");
            if (pendingIntent == null) {
                Log.e(TAG, "pendingIntent == null, this should not happen");
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

    private void purchaseActivityResult(int resultCode, Intent data)
    {
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
    }

    private class ConsumeInput {
        String sku;
        String purchaseToken;
    }

    private class ConsumePurchaseTask extends AsyncTask<ConsumeInput, Void, Integer> {
        // Exit code when consumePurchase throws RemoteException,
        // Different than existing responses of biling API,
        // see http://developer.android.com/google/play/billing/billing_reference.html
        private final int REMOTE_EXCEPTION_ERROR = 100;

        private String mSku;

        protected Integer doInBackground(ConsumeInput... consumeInputs) {
            ConsumeInput consumeInput = consumeInputs[0]; // just take 1st param
            mSku = consumeInput.sku;
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
            if (response == BILLING_RESPONSE_RESULT_OK) {
                Log.i(TAG, "Consumed item " + mSku);
                messageSend(new String[]{"in-app-purchases-consumed", mSku});
            } else {
                Log.e(TAG, "Failed to consume item " + mSku + ", response: " + response);
            }
        }
    }

    private void consume(String sku)
    {
        if (!purchaseTokens.containsKey(sku)) {
            Log.e(TAG, "Cannot consume item " + sku + ", purchaseToken unknown");
            return;
        }
        ConsumeInput consumeInput = new ConsumeInput();
        consumeInput.sku = sku;
        consumeInput.purchaseToken = purchaseTokens.get(sku);
        purchaseTokens.remove(sku);
        new ConsumePurchaseTask().execute(consumeInput);
    }

    private void owns(String sku, String purchaseData/*, String signature*/)
    {
        String purchaseToken;

        try {
            JSONObject jo = new JSONObject(purchaseData);
            int purchaseState = jo.getInt("purchaseState");
            if (purchaseState == 1) {
                Log.w(TAG, "Ignoring ownage of a cancelled item " + sku);
                return;
            }
            purchaseToken = jo.getString("purchaseToken");
            purchaseTokens.put(sku, purchaseToken);
        } catch (JSONException e) {
            Log.e(TAG, "Failed to parse purchaseData in owns.");
            e.printStackTrace();
            return;
        }

        messageSend(new String[]{"in-app-purchases-owns", sku});
    }

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
        }
    }

    private void refreshPurchased()
    {
        if (mBilingService == null) {
            return;
        }
        new RefreshPurchasedTask().execute();
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent intent) {
        if (requestCode == REQUEST_PURCHASE) {
            Log.i(TAG, "onActivityResult - REQUEST_PURCHASE");
            purchaseActivityResult(resultCode, intent);
        }
    }

    private void setAvailableProducts(String[] products)
    {
        availableProducts = new ArrayList<String>(Arrays.asList(products));
        if (mBilingService != null) {
            refreshAvailableForPurchase();
            refreshPurchased();
        }
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
