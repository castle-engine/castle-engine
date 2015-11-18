{
  Copyright 2015-2015 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ In-app purchases (TInAppPurchases). }
unit CastleInAppPurchases;

interface

uses FGL,
  CastleStringUtils;

type
  { A product that can be bought by in-app purchases (TInAppPurchases). }
  TInAppProduct = class
  private
    FName: string;
    FPriceRaw: string;
    FOwns: boolean;
    FSuccessfullyConsumed: boolean;
  public
    property Name: string read FName;
    { Price, as received from Google.
      May be empty before receiving, may contain various weird chars unsupported
      by many fonts. Usually use @link(Price) to display the price as a string. }
    property PriceRaw: string read FPriceRaw;
    function Price(const ValueWhenUnknown: string = 'loading...'): string;
    property Owns: boolean read FOwns;
    { Item was consumable, and was just consumed. We should "provision"
      it now, which means that we should set @code(SuccessfullyConsumed:=false),
      and perform whatever is necessary upon consuming --- e.g. continue
      the game or increase player's gold. }
    property SuccessfullyConsumed: boolean read FSuccessfullyConsumed write FSuccessfullyConsumed;
  end;

  { Manage in-app purchases in your game.

    Usage: simply construct an instance of this class (or a subclass --
    it is useful to override some methods of this class for a particular game).
    Call @link(SetAvailableProducts) to get the prices and ownership information
    about items. Use various methods to query information about the products
    and to buy products (@link(Purchase)).

    To include the necessary integration code in your Android project,
    you must declare your Android project type as "integrated".
    See https://sourceforge.net/p/castle-engine/wiki/Android%20development/ .
    Also make sure that your game uses @link(TMessaginig) class,
    in particular that it calls @code(Messaging.Update) continuously and exports
    an extra JNI function from CastleMessaging unit in the Android library.
    See @link(TMessaginig) docs for details.

    @bold(You have to create the products to purchase, and their prices in various
    currencies, in the Google Developer Console.) The names of products
    you provide to @link(SetAvailableProducts) or @link(Product) methods
    should correspond to product names you set in the Google Developer Console. }
  TInAppPurchases = class
  private
  type
    TProductList = specialize TFPGObjectList<TInAppProduct>;
  var
    FDebugMockupBuying: boolean;
    List: TProductList;
    function MessageReceived(const Received: TCastleStringList): boolean;
  protected
    { Called when the knowledge about what do we own is complete. }
    procedure KnownCompletely; virtual;
    { Called when the product is successfully consumed,
      in response to @link(Consume) call.

      In this class, this simply
      sets @code(Product.SuccessfullyConsumed) flag to @true,
      and waits for some other code (maybe overridden SuccessfullyConsumed
      implementation, maybe something else) will handle it and reset
      the @code(Product.SuccessfullyConsumed) flag to @false. }
    procedure SuccessfullyConsumed(const AProduct: TInAppProduct); virtual;
    { Called when the product becomes owned, in particular when it's
      successfully bought. }
    procedure Owns(const AProduct: TInAppProduct); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { Initialize a list of product names for which to query prices
      from server. }
    procedure SetAvailableProducts(const Strings: array of string);

    { Initiate a purchase of given item. }
    procedure Purchase(const AProduct: TInAppProduct);

    { Initiate a consumption of a consumable item.
      You should listen on a "successfull consumption" (override
      @link(SuccessfullyConsumed)
      method and/or watch @link(TInAppProduct.SuccessfullyConsumed))
      until you actually act on the consumption (increase player gold or such). }
    procedure Consume(const AProduct: TInAppProduct);

    { Find product with given name.

      Creates and adds new product, if not found (useful in case
      you asked for a product before information about it arrived from the net,
      or before you even called SetAvailableProducts with it). }
    function Product(const Name: string): TInAppProduct;

    { Purely for debug purposes, mockup buying (pretend that all purchases succeed). }
    property DebugMockupBuying: boolean
      read FDebugMockupBuying write FDebugMockupBuying default false;
   end;

implementation

uses SysUtils,
  CastleMessaging, CastleUtils, CastleLog, CastleUnicode;

{ TInAppProduct -------------------------------------------------------------- }

function TInAppProduct.Price(const ValueWhenUnknown: string): string;
begin
  if PriceRaw = '' then
    Result := ValueWhenUnknown else
    { note: do not use SReplaceChars, as these are UTF-8 chars, not 8-bit chars. }
    Result := SReplacePatterns(PriceRaw, [' ', 'ł'], [' ', 'l'], [soMatchCase]);
end;

{ TInAppPurchases ------------------------------------------------------------ }

constructor TInAppPurchases.Create;
begin
  inherited;
  List := TProductList.Create(true);
  Messaging.OnReceive.Add(@MessageReceived);
end;

destructor TInAppPurchases.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  FreeAndNil(List);
  inherited;
end;

function TInAppPurchases.MessageReceived(const Received: TCastleStringList): boolean;
begin
  Result := false;
  if (Received.Count = 3) and
     (Received[0] = 'in-app-purchases-can-purchase') then
  begin
    Product(Received[1]).FPriceRaw := Received[2];
    Result := true;
  end else
  if (Received.Count = 2) and
     (Received[0] = 'in-app-purchases-owns') then
  begin
    Owns(Product(Received[1]));
    Result := true;
  end else
  if (Received.Count = 2) and
     (Received[0] = 'in-app-purchases-consumed') then
  begin
    SuccessfullyConsumed(Product(Received[1]));
    Result := true;
  end else
  if (Received.Count = 1) and
     (Received[0] = 'in-app-purchases-known-completely') then
  begin
    KnownCompletely;
    Result := true;
  end;
end;

function TInAppPurchases.Product(const Name: string): TInAppProduct;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    if List[I].Name = Name then
      Exit(List[I]);

  { not found, so create new }
  Result := TInAppProduct.Create;
  Result.FName := Name;
  if DebugMockupBuying then
    Result.FPriceRaw := '10,00 USD'; //'6,15 zł';
  List.Add(Result);
end;

procedure TInAppPurchases.Owns(const AProduct: TInAppProduct);
begin
  AProduct.FOwns := true;
end;

procedure TInAppPurchases.Purchase(const AProduct: TInAppProduct);
begin
  if DebugMockupBuying then
    Owns(AProduct) else
    Messaging.Send(['in-app-purchases-purchase', AProduct.Name]);
end;

procedure TInAppPurchases.Consume(const AProduct: TInAppProduct);
begin
  if DebugMockupBuying then
    SuccessfullyConsumed(AProduct) else
    Messaging.Send(['in-app-purchases-consume', AProduct.Name]);
end;

procedure TInAppPurchases.SuccessfullyConsumed(const AProduct: TInAppProduct);
begin
  AProduct.SuccessfullyConsumed := true;
end;

procedure TInAppPurchases.KnownCompletely;
var
  I: Integer;
  LogStr: string;
begin
  if Log then
  begin
    LogStr := 'Product details known completely:' + NL;
    for I := 0 to List.Count - 1 do
      LogStr += 'Product ' + List[I].Name +
        ', price ' + List[I].Price +
        ', owned ' + CastleStringUtils.BoolToStr[List[I].Owns] + NL;
    WritelnLogMultiline('InAppPurchases', LogStr);
  end;
end;

procedure TInAppPurchases.SetAvailableProducts(const Strings: array of string);
begin
  Messaging.Send(['in-app-purchases-set-available-products', GlueStrings(Strings, ',')]);
end;

end.
