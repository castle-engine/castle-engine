/*
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/* iOS in-app purchases integration with Castle Game Engine.

   Apple documentation:
   - https://developer.apple.com/in-app-purchase/
   - guide: https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/StoreKitGuide/Chapters/ShowUI.html#//apple_ref/doc/uid/TP40008267-CH3-SW5
   - reference: https://developer.apple.com/documentation/storekit/in_app_purchase

   TODO: Implement "Promoting Your In-App Purchases" for iOS 11
   https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/StoreKitGuide/PromotingIn-AppPurchases/PromotingIn-AppPurchases.html#//apple_ref/doc/uid/TP40008267-CH11-SW8
*/

#import "InAppPurchasesService.h"
#import "../AppDelegate.h"
#import "../CgeUtils.h"

@implementation InAppPurchasesService

/* Set self field availableProducts,
   and get store information (e.g. price) for each available product,
   sending in-app-purchases-can-purchase messages,
   and then in-app-purchases-refreshed-prices.
*/
- (void)setAvailableProducts:(NSString*) availableProductsStr
{
    // calculate availableProducts from availableProductsStr
    availableProducts = [[NSMutableArray alloc] init];
    NSArray* availableProductsListOfStrings = [availableProductsStr componentsSeparatedByString:@"\2"];
    for (NSString* productStr in availableProductsListOfStrings) {
        NSArray* productInfo = [productStr componentsSeparatedByString:@"\3"];
        if ([productInfo count] != 2) {
            NSLog(@"WARNING: Product info invalid: %@", productStr);
            continue;
        }
        AvailableProduct* product = [[AvailableProduct alloc] init];
        product.id = [productInfo objectAtIndex: 0];
        product.category = [productInfo objectAtIndex: 1];
        [availableProducts addObject: product];
    }

    // calculate availableProductsIds from availableProducts
    NSMutableArray* availableProductsIds = [[NSMutableArray alloc] init];
    for (AvailableProduct* product in availableProducts) {
        [availableProductsIds addObject: product.id];
    }
    // [availableProductsIds addObject: @"test-invalid-id"]; // use for testing

    // start SKProductsRequest
    SKProductsRequest *productsRequest = [[SKProductsRequest alloc]
        initWithProductIdentifiers:[NSSet setWithArray:availableProductsIds]];
    // Keep a strong reference to the request.
    self->request = productsRequest;
    productsRequest.delegate = self;
    [productsRequest start];
}

/* Implements the delegate (part of SKProductsRequestDelegate)
   that receives the answer to query initiated in setAvailableProducts.
*/
- (void)productsRequest:(SKProductsRequest*) request
    didReceiveResponse:(SKProductsResponse*) response;
{
    // warn about response.invalidProductIdentifiers
    for (NSString *invalidIdentifier in response.invalidProductIdentifiers) {
        NSLog(@"WARNING: Invalid product id: %@", invalidIdentifier);
    }

    // store response.products info inside our availableProducts
    for (SKProduct* skProduct in response.products) {
        AvailableProduct* product = [self getAvailableProduct: skProduct.productIdentifier];
        if (product == nil) {
            NSLog(@"WARNING: AppStore product id is not on local availableProducts list: %@", skProduct.productIdentifier);
            continue;
        }
        product.skProduct = skProduct;
        product.title = skProduct.localizedTitle;
        product.productDescription = skProduct.localizedDescription;

        // title and description may be nil if IAP are in rejected state
        if (product.title == nil) {
            NSLog(@"WARNING: AppStore product localizedTitle is nil, for id %@. This can happen, ignoring...", skProduct.productIdentifier);
        }
        if (product.productDescription == nil) {
            NSLog(@"WARNING: AppStore product localizedDescription is nil, for id %@. This can happen, ignoring...", skProduct.productIdentifier);
        }

        // format price, using code recommended by Apple docs
        {
            NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
            [numberFormatter setFormatterBehavior: NSNumberFormatterBehavior10_4];
            [numberFormatter setNumberStyle: NSNumberFormatterCurrencyStyle];
            [numberFormatter setLocale: skProduct.priceLocale];
            product.price = [numberFormatter stringFromNumber: skProduct.price];
        }

        // format price for analytics,
        // using code recommened by GameAnalytics
        // https://github.com/GameAnalytics/GA-SDK-IOS/wiki/Business-Event
        {
            NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
            [numberFormatter setLocale: skProduct.priceLocale];
            product.priceCurrencyCode = [numberFormatter currencyCode];
            product.priceAmountCents = [[skProduct.price decimalNumberByMultiplyingBy:[NSDecimalNumber decimalNumberWithString:@"100"]] intValue];
            NSDecimalNumber* priceAmountMicros = [skProduct.price decimalNumberByMultiplyingBy:[NSDecimalNumber decimalNumberWithString:@"1000000"]];
            product.priceAmountMicros = [NSString stringWithFormat:@"%@", priceAmountMicros];
        }

        // send in-app-purchases-can-purchase about this item
        [self messageSend: @[@"in-app-purchases-can-purchase",
            product.id,
            product.price,
            stringNonNil(product.title),
            stringNonNil(product.productDescription),
            product.priceAmountMicros,
            product.priceCurrencyCode]];
    }

    [self messageSend: @[@"in-app-purchases-refreshed-prices"]];
}

/* Get the AvailableProduct with given id, or nil if not found. */
- (AvailableProduct*)getAvailableProduct:(NSString*) id
{
    for (AvailableProduct* product in availableProducts) {
        if ([product.id isEqualToString:id]) {
            return product;
        }
    }
    return nil;
}

/* Call when we *know* that given product is owned.
   This notifies Pascal code that we own given product,
   by sending in-app-purchases-owns.
*/
- (void)owns:(NSString*) productId
{
    [self messageSend: @[@"in-app-purchases-owns", productId]];
}

/* Purchase the item, by communicating with AppStore server
   and eventually sending in-app-purchases-owns.
*/
- (void)purchase:(NSString*) productId
{
    AvailableProduct* product = [self getAvailableProduct: productId];
    if (product == nil) {
        NSLog(@"WARNING: Product unknown: %@", productId);
        return;
    }
    if (product.skProduct == nil) {
        NSLog(@"WARNING: Product known locally, but not known to the AppStore (yet): %@", productId);
        return;
    }
    SKMutablePayment *payment = [SKMutablePayment paymentWithProduct: product.skProduct];
    payment.quantity = 1;
    [[SKPaymentQueue defaultQueue] addPayment:payment];
}

/* Override applicationDidFinishLaunchingWithOptions to set self
   as "transaction observer" for purchases.
*/
- (void)application:(UIApplication *) application didFinishLaunchingWithOptions:(NSDictionary *) launchOptions
{
    [super application: application didFinishLaunchingWithOptions: launchOptions];
    [[SKPaymentQueue defaultQueue] addTransactionObserver: self];
}

/* React to payment transactions.
   This is automatically called because we have indicated self using "addTransactionObserver".
*/
- (void)paymentQueue:(SKPaymentQueue *)queue updatedTransactions:(NSArray *)transactions
{
    for (SKPaymentTransaction *transaction in transactions) {
        switch (transaction.transactionState) {
            case SKPaymentTransactionStatePurchasing:
                // TODO: pass this state to UI?

                NSLog(@"In-app purchases transaction in progress.");

                // Do not call finishTransaction on "purchasing" state:
                // https://developer.apple.com/documentation/storekit/skpaymentqueue/1506003-finishtransaction
                // [[SKPaymentQueue defaultQueue] finishTransaction: transaction];

                break;

            case SKPaymentTransactionStateDeferred:
                // TODO: pass this state to UI, to notify user a transaction is "deferred":
                // https://developer.apple.com/library/content/technotes/tn2259/_index.html

                NSLog(@"In-app purchases transaction deferred.");

                // Do not call finishTransaction on "deferred" state:
                // https://stackoverflow.com/questions/26187148/deferred-transactions
                // [[SKPaymentQueue defaultQueue] finishTransaction: transaction];

                break;

            case SKPaymentTransactionStateFailed:
                NSLog(@"In-app purchases transaction failed (this is normal if user cancelled the purchase).");

                // We need to call finishTransaction,
                // otherwise it will be reported again next time we open the application.
                [[SKPaymentQueue defaultQueue] finishTransaction: transaction];

                break;

            case SKPaymentTransactionStatePurchased:
                if (transaction.payment == nil ||
                    transaction.payment.productIdentifier == nil) {
                    NSLog(@"WARNING: Transaction payment or payment.productIdentifier empty");
                } else {
                    NSString* productId = transaction.payment.productIdentifier;
                    NSLog(@"In-app purchases transaction success: %@", productId);
                    [self owns: productId];

                    AvailableProduct* product = [self getAvailableProduct: productId];
                    if (product != nil) {
                        [getAppDelegate() onPurchase: product withTransaction: transaction];
                    } else {
                        NSLog(@"WARNING: In-app purchases: purchased unknown product: %@", productId);
                    }
                }

                // We need to call finishTransaction,
                // otherwise it will be reported again next time we open the application.
                [[SKPaymentQueue defaultQueue] finishTransaction: transaction];

                break;

            case SKPaymentTransactionStateRestored:
                NSLog(@"In-app purchases transaction restored (will be processed later too): %@", transaction.payment.productIdentifier);

                // We need to call finishTransaction,
                // otherwise it will be reported again next time we open the application.
                [[SKPaymentQueue defaultQueue] finishTransaction: transaction];

                break;

            default:
                NSLog(@"WARNING: Unexpected transaction state %@", @(transaction.transactionState));
                // Do not call finishTransaction in this case, leave it in transaction queue
                break;
        }
    }
}

/* Consume the item, by communicating with AppStore server
   and eventually sending in-app-purchases-consumed.
*/
- (void)consume:(NSString*) productId
{
    // On iOS, there is no communication necessary with the AppStore when consuming the items,
    // see https://stackoverflow.com/questions/22599763/how-to-consume-a-purchased-item-with-in-app-purchase-on-ios
    [self messageSend: @[@"in-app-purchases-consumed", productId]];
}

/* Called when restorePurchases finishes without errors.
   This is automatically called because we have indicated self using "addTransactionObserver".
*/
- (void)paymentQueueRestoreCompletedTransactionsFinished:(SKPaymentQueue *)queue
{
    for (SKPaymentTransaction* transaction in queue.transactions) {
        NSLog(@"In-app purchases transaction restored: %@", transaction.payment.productIdentifier);
        [self owns: transaction.payment.productIdentifier];
    }
    [self messageSend: @[@"in-app-purchases-refreshed-purchases"]];
}

/* Called when restorePurchases finishes with an error.
   This is automatically called because we have indicated self using "addTransactionObserver".
*/
- (void)paymentQueue:(SKPaymentQueue *)queue restoreCompletedTransactionsFailedWithError:(NSError *)error
{
    NSLog(@"WARNING: Restoring completed transactions failed with error.");
    [self messageSend: @[@"in-app-purchases-refreshed-purchases"]];
}

/* Ask AppStore to replay purchases for already-purchased items.
   This may ask user to log-in to AppStore, so should not be called automatically
   at application launch.

   In response, this will call
   - in-app-purchases-owns (for each owned product)
   - in-app-purchases-refreshed-purchases (once, when done).
*/
- (void)refreshPurchases
{
    [[SKPaymentQueue defaultQueue] restoreCompletedTransactions];
}

/* Handle messages received from Pascal code
   (send from src/services/castleinapppurchases.pas).
*/
- (bool)messageReceived:(NSArray*) message
{
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"in-app-purchases-set-available-products"])
    {
        NSString* availableProductsStr = [message objectAtIndex: 1];
        [self setAvailableProducts: availableProductsStr];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"in-app-purchases-purchase"])
    {
        NSString* productId = [message objectAtIndex: 1];
        [self purchase: productId];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"in-app-purchases-consume"])
    {
        NSString* productId = [message objectAtIndex: 1];
        [self consume: productId];
        return TRUE;
    } else
    if (message.count == 1 &&
        [[message objectAtIndex: 0] isEqualToString:@"in-app-purchases-refresh-purchases"])
    {
        [self refreshPurchases];
        return TRUE;
    }

    return FALSE;
}

@end
