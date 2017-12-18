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

#import "InAppPurchasesService.h"

@implementation InAppPurchasesService

/* Get information (e.g. price) for each available product,
   sending in-app-purchases-can-purchase messages.

   Also get information what which items are currently owned,
   sending in-app-purchases-owns and in-app-purchases-known-completely
   messages.
*/
- (void)setAvailableProducts:(NSString*) availableProductsStr
{
    // TODO: Split availableProducts string into information:

    availableProducts = [[NSMutableArray alloc] init];

    // TODO: use this below:
    // AvailableProduct* aProduct = [[AvailableProduct alloc] init];
    // aProduct.id = ...;
    // aProduct.category = ...;
    // [availableProducts addObject: aProduct];

    // availableProducts = new HashMap<String, AvailableProduct>();
    // String[] productsList = splitString(productsListStr, 2);
    // for (String productStr : productsList) {
    //     String[] productInfo = splitString(productStr, 3);
    //     if (productInfo.length != 2) {
    //         Log.e(TAG, "Product info invalid: " + productStr);
    //         continue;
    //     }
    //     AvailableProduct product = new AvailableProduct(productInfo[0]);
    //     product.category = productInfo[1];
    //     availableProducts.put(product.id, product);
    // }

    // TODO
    // RefreshAvailableForPurchase
    // - asks about all availableProducts
    // - for every item, calls
    // messageSend(new String[]{"in-app-purchases-can-purchase",
    //     product.id,
    //     product.price,
    //     product.title,
    //     product.description,
    //     Long.toString(product.priceAmountMicros),
    //     product.priceCurrencyCode
    // });

    // TODO
    // RefreshPurchased calls
    // owns() for all owned items
    // and then
    // messageSend(new String[]{"in-app-purchases-known-completely"});

    [self messageSend: @[@"in-app-purchases-known-completely"]];
}

/* Call when we *know* that given product is owned.
   This notifies Pascal code that we own given product,
   by sending in-app-purchases-owns.
*/
- (void)owns:(NSString*) product
{
    [self messageSend: @[@"in-app-purchases-owns", product]];
}

/* Purchase the item, by communicating with AppStore server
   and eventually sending in-app-purchases-owns.
*/
- (void)purchase:(NSString*) product
{
    // TODO: talk with server before sending
    [self owns: product];
}

/* Consume the item, by communicating with AppStore server
   and eventually sending in-app-purchases-consumed.
*/
- (void)consume:(NSString*) product
{
    // TODO: talk with server before sending
    [self messageSend: @[@"in-app-purchases-consumed", product]];
}

/* Handle messages received from Pascal code (src/services/castleinapppurchases.pas). */
- (bool)messageReceived:(NSArray* )message
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
        NSString* product = [message objectAtIndex: 1];
        [self purchase: product];
        return TRUE;
    } else
    if (message.count == 2 &&
        [[message objectAtIndex: 0] isEqualToString:@"in-app-purchases-consume"])
    {
        NSString* product = [message objectAtIndex: 1];
        [self consume: product];
        return TRUE;
    }

    return FALSE;
}

@end
