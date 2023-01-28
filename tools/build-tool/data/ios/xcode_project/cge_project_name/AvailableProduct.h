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

#import <StoreKit/StoreKit.h>

/* Information about the product available for purchase.
   Used by in-app purchases and (TODO) analytics services.
*/
@interface AvailableProduct : NSObject
{
}

/* Unique product identifier. */
@property (nonatomic, retain) NSString* id;

/* Product category. Used (TODO) only by analytics for now. */
@property (nonatomic, retain) NSString* category;

/* Title (from the AppStore, localized). nil if not known yet. */
@property (nonatomic, retain) NSString* title;

/* Description (from the AppStore, localized). nil if not known yet.
   Called productDescription, not description, since NSObject
   already includes "description". */
@property (nonatomic, retain) NSString* productDescription;

/* Price (from the AppStore, localized). nil if not known yet. */
@property (nonatomic, retain) NSString* price;

/* Complete SKProduct (from the AppStore) describing this. nil if not known yet. */
@property (nonatomic, retain) SKProduct* skProduct;

/* Price currency, to be passed to analytics.
   This is taken from NSNumberFormatter.currencyCode:
   https://developer.apple.com/documentation/foundation/nsnumberformatter/1410463-currencycode?language=objc
   It is usually a 3-letter code.
   nil if not known yet.
*/
@property (nonatomic, retain) NSString* priceCurrencyCode;

/* Price amount in micros, to be passed to analytics.

   This is expressed as a string, as it seems the Objective-C doesn't have
   a "long" type to hold this information precisely?
   At least, for some reason, they use NSDecimalNumber and NSDecimalString
   in their in-app-purchases API.
   It seems they have 8-byte integer (like Pascal Int64) but only on 64-bit runtime?
   https://useyourloaf.com/blog/format-string-issue-using-nsinteger/
*/
@property (nonatomic, retain) NSString* priceAmountMicros;

/* Price amount in cents (100 cents is one unit), to be passed to analytics.
   0 if not known yet.
*/
@property (nonatomic) NSInteger priceAmountCents;

@end
