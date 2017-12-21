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

/* Apple Game Center integration with Castle Game Engine. */

#import "../ServiceAbstract.h"
#import "AvailableProduct.h"

@interface InAppPurchasesService : ServiceAbstract <SKProductsRequestDelegate, SKPaymentTransactionObserver>
{
    /* Available products.
       This list contains the latest known information,
       combining the information received from the Pascal code (like product ids),
       and from Apple AppStore (like prices).
       Array of AvailableProduct instances. */
    NSMutableArray* availableProducts;

    /* Reference to SKProductsRequest, to not let it be released too early. */
    SKProductsRequest* request;
}

@end
