/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/* Helper with in-app purchases codes, and converting them to strings.
   See https://developer.android.com/google/play/billing/billing_reference.html */
public class InAppPurchasesHelper
{
    public static final int BILLING_RESPONSE_RESULT_OK = 0;
    public static final int BILLING_RESPONSE_RESULT_USER_CANCELED = 1;
    public static final int BILLING_RESPONSE_RESULT_SERVICE_UNAVAILABLE = 2;
    public static final int BILLING_RESPONSE_RESULT_BILLING_UNAVAILABLE = 3;
    public static final int BILLING_RESPONSE_RESULT_ITEM_UNAVAILABLE = 4;
    public static final int BILLING_RESPONSE_RESULT_DEVELOPER_ERROR = 5;
    public static final int BILLING_RESPONSE_RESULT_ERROR = 6;
    public static final int BILLING_RESPONSE_RESULT_ITEM_ALREADY_OWNED = 7;
    public static final int BILLING_RESPONSE_RESULT_ITEM_NOT_OWNED = 8;

    public static final String billingResponseToStr(int response)
    {
        String enumStr, descriptionStr;
        switch (response) {
            case BILLING_RESPONSE_RESULT_OK:
                enumStr = "BILLING_RESPONSE_RESULT_OK";
                descriptionStr = "Success";
                break;
            case BILLING_RESPONSE_RESULT_USER_CANCELED:
                enumStr = "BILLING_RESPONSE_RESULT_USER_CANCELED";
                descriptionStr = "User pressed back or canceled a dialog";
                break;
            case BILLING_RESPONSE_RESULT_SERVICE_UNAVAILABLE:
                enumStr = "BILLING_RESPONSE_RESULT_SERVICE_UNAVAILABLE";
                descriptionStr = "Network connection is down";
                break;
            case BILLING_RESPONSE_RESULT_BILLING_UNAVAILABLE:
                enumStr = "BILLING_RESPONSE_RESULT_BILLING_UNAVAILABLE";
                descriptionStr = "Billing API version is not supported for the type requested";
                break;
            case BILLING_RESPONSE_RESULT_ITEM_UNAVAILABLE:
                enumStr = "BILLING_RESPONSE_RESULT_ITEM_UNAVAILABLE";
                descriptionStr = "Requested product is not available for purchase";
                break;
            case BILLING_RESPONSE_RESULT_DEVELOPER_ERROR:
                enumStr = "BILLING_RESPONSE_RESULT_DEVELOPER_ERROR";
                descriptionStr = "Invalid arguments provided to the API. This error can also indicate that the application was not correctly signed or properly set up for In-app Billing in Google Play, or does not have the necessary permissions in its manifest";
                break;
            case BILLING_RESPONSE_RESULT_ERROR:
                enumStr = "BILLING_RESPONSE_RESULT_ERROR";
                descriptionStr = "Fatal error during the API action";
                break;
            case BILLING_RESPONSE_RESULT_ITEM_ALREADY_OWNED:
                enumStr = "BILLING_RESPONSE_RESULT_ITEM_ALREADY_OWNED";
                descriptionStr = "Failure to purchase since item is already owned";
                break;
            case BILLING_RESPONSE_RESULT_ITEM_NOT_OWNED:
                enumStr = "BILLING_RESPONSE_RESULT_ITEM_NOT_OWNED";
                descriptionStr = "Failure to consume since item is not owned";
                break;
            default:
                enumStr = "Unknown";
                descriptionStr = "Unknown";
                break;
        }
        return enumStr + " (" + response + "): " + descriptionStr;
    }
}
