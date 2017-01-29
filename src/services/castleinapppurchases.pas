{
  Copyright 2015-2017 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses Classes, FGL,
  CastleStringUtils;

type
  { A product that can be bought by in-app purchases (TInAppPurchases). }
  TInAppProduct = class
  private
    FName: string;
    FPriceRaw, FTitle, FDescription, FPriceCurrencyCode: string;
    FPriceAmountMicros: Int64;
    FOwns: boolean;
    FSuccessfullyConsumed: boolean;
  public
    { Short product identifier, uniquely identifying it in the store. }
    property Name: string read FName;

    { Price, as a string in local user currency.
      Empty before receiving information about the product from the store.
      This may contain various UTF-8 local characters used to describe currency,
      which do not have to be supported in all fonts.

      Use @link(Price) to display the price as a "safe" string, with most unusual
      characters replaced with ASCII (so it should work reliably with any font),
      and with special value if not received yet. }
    property PriceRaw: string read FPriceRaw;

    { The price, as a string in local user currency,
      with most unusual characters replaced with ASCII
      (so it should work reliably with any font),
      and with special value if not received yet.
      @seealso PriceRaw }
    function Price(const ValueWhenUnknown: string = 'loading...'): string;

    { Title of the product, as defined in the store.
      Empty if not known yet.
      May be translated to current user language. }
    property Title: string read FTitle;

    { Description of the product, as defined in the store.
      Empty if not known yet.
      May be translated to current user language. }
    property Description: string read FDescription;

    { Price in micro-units, where 1,000,000 micro-units equal one unit of the currency.
      0 if not known yet. }
    property PriceAmountMicros: Int64 read FPriceAmountMicros;

    { ISO 4217 currency code for price.
      Empty if not known yet. }
    property PriceCurrencyCode: string read FPriceCurrencyCode;

    { Is the product owned now. Use this for non-consumable items
      (things that user buys, and then "owns" for the rest of his life).

      Do not depend on this for consumables (use SuccessfullyConsumed
      for them, and be sure to call @link(TInAppPurchases.SuccessfullyConsumed)
      from @link(TInAppPurchases.Owns) for them). }
    property Owns: boolean read FOwns;

    { Item was consumable, and was just consumed. We should "provision"
      it now, which means that we should set @code(SuccessfullyConsumed:=false),
      and perform whatever is necessary upon consuming --- e.g. continue
      the game or increase player's gold. }
    property SuccessfullyConsumed: boolean read FSuccessfullyConsumed write FSuccessfullyConsumed;
  end;

  TAvailableProduct = record
    Name: string;

    { Category. For now used only for analytics. }
    Category: string;
  end;

  { Manage in-app purchases in your game.

    Usage: simply construct an instance of this class (or a subclass --
    it is useful to override some methods of this class for a particular game).
    Call @link(SetAvailableProducts) to get the prices and ownership information
    about items. Use various methods to query information about the products
    and to buy products (@link(Purchase)).

    To include the necessary integration code in your Android project,
    declare your Android project type as "integrated" with
    the "google_in_app_purchases" component inside CastleEngineManifest.xml.
    See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .

    @bold(You have to create the products to purchase, and their prices in various
    currencies, in the Google Developer Console.) The names of products
    you provide to @link(SetAvailableProducts) or @link(Product) methods
    should correspond to product names you set in the Google Developer Console. }
  TInAppPurchases = class(TComponent)
  private
  type
    TProductList = specialize TFPGObjectList<TInAppProduct>;
  var
    FDebugMockupBuying: boolean;
    List: TProductList;
    FLastAvailableProducts: string;
    function MessageReceived(const Received: TCastleStringList): boolean;
    procedure ReinitializeJavaActivity(Sender: TObject);
  protected
    { Called when the knowledge about what do we own is complete. }
    procedure KnownCompletely; virtual;

    { Called when the product is successfully consumed,
      in response to the @link(Consume) call.

      In this class, this simply
      sets @code(Product.SuccessfullyConsumed) flag to @true,
      and waits for some other code (maybe overridden SuccessfullyConsumed
      implementation, maybe something else) will handle it and reset
      the @code(Product.SuccessfullyConsumed) flag to @false. }
    procedure SuccessfullyConsumed(const AProduct: TInAppProduct); virtual;

    { Called when we know the product is owned, in particular when it's
      successfully bought.

      If the product is a @bold(consumable), which means it has a one-time use
      (and should disappear afterwards, until user will buy it again), then:

      @orderedList(
        @item(@bold(Call the @link(Consume) method) once you know the item is
          owned. You can call @link(Consume) directly from the overridden
          implementation of @name, this is often the simplest approach.)

        @item(@bold(Actually perform the consumption) (bump the player gold,
          grant extra life and so on) @bold(only when the item
          is successfully consumed). You are notified about this by the
          @link(SuccessfullyConsumed) call (you can override it),
          or you can watch if the @link(TInAppProduct.SuccessfullyConsumed)
          flag is set.

          @bold(Do not give any one-time gain as a response to the @name
          call.) Always wait for @link(SuccessfullyConsumed) call.

          This protects you from the scenario when you're notified that
          you own the item multiple times (which
          may happen, since purchases may be resumed asynchronously while
          other code is executing), and you call @link(Consume) twice.
          The @link(SuccessfullyConsumed) will only fire once, if user
          bought item once.)
      ) }
    procedure Owns(const AProduct: TInAppProduct); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Initialize a list of products for which to query prices from server.
      The overloaded version with TAvailableProduct allows to provide additional
      information to the in-app payment system, see @link(TAvailableProduct) docs.
      @groupBegin }
    procedure SetAvailableProducts(const Names: array of string);
    procedure SetAvailableProducts(const Products: array of TAvailableProduct);
    { @groupEnd }

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
    function Product(const ProductName: string): TInAppProduct;

    { Purely for debug purposes, mockup buying (pretend that all purchases succeed). }
    property DebugMockupBuying: boolean
      read FDebugMockupBuying write FDebugMockupBuying default false;
  end;

implementation

uses SysUtils,
  CastleMessaging, CastleUtils, CastleLog, CastleUnicode,
  CastleApplicationProperties;

{ Convert many UTF-8 special characters to their ASCII counterparts.
  This is useful for converting arbitrary UTF-8 strings for display,
  when your font may not contain various local UTF-8 special characters. }
function ConvertSpecialsToAscii(const S: string): string;
begin
  Result := SReplacePatterns(S,
    { useful list of local chars from
      http://stackoverflow.com/questions/2096667/convert-unicode-to-ascii-without-changing-the-string-length-in-java/2097224#comment46476953_2097224
      with something from
      http://www.xe.com/symbols.php
    }
    [' ', 'r̀', 'r̂', 'r̃', 'r̈', 'ʼ' , 'ŕ', 'ř', 't̀', 't̂', 'ẗ', 'ţ', 'ỳ', 'ỹ', 'ẙ', 'ʼ' , 'y̎', 'ý', 'ÿ', 'ŷ', 'p̂', 'p̈', 's̀', 's̃', 's̈', 's̊', 'ʼ' , 's̸', 'ś', 'ŝ', 'Ş', 'ş', 'š', 'd̂', 'd̃', 'd̈', 'ď', 'ʼ' , 'ḑ', 'f̈', 'f̸', 'g̀', 'g̃', 'g̈', 'ʼ̧', '‌', '​', '‌', '​', '́', 'ĝ', 'ǧ', 'ḧ', 'ĥ', 'j̈', 'j', 'ʼ' , 'ḱ', 'k̂', 'k̈', 'k̸', 'ǩ', 'l̂', 'l̃', 'l̈', 'Ł', 'ł', 'ẅ', 'ẍ', 'c̃', 'c̈', 'c̊', 'c', 'ʼ' , 'c̸', 'Ç', 'ç', 'ç', 'ć', 'ĉ', 'č', 'v̂', 'v̈', 'v', 'ʼ' , 'v̸', 'b́', 'b̧', 'ǹ', 'n̂', 'n̈', 'n̊', 'n', 'ʼ' , 'ń', 'ņ', 'ň', 'ñ', 'm̀', 'm̂', 'm̃', '‌', '​', 'm̈', '‌', '​', 'm̊', 'm̌', 'ǵ', 'ß', '€', '£'],
    [' ', 'r', 'r', 'r', 'r', '''', 'r', 'r', 't', 't', 't', 't', 'y', 'y', 'y', '''', 'y', 'y', 'y', 'y', 'p', 'p', 's', 's', 's', 's', '''', 's', 's', 's', 'S', 's', 's', 'd', 'd', 'd', 'd', '''', 'd', 'f', 'f', 'g', 'g', 'g', '' , '', '', '', '', '', 'g', 'g', 'h', 'h', 'j', 'j', '''', 'k', 'k', 'k', 'K', 'k', 'l', 'l', 'l', 'L', 'l', 'w', 'x', 'c', 'c', 'c', 'c', '''', 'c', 'C', 'c', 'c', 'c', 'c', 'c', 'v', 'v', 'v', '''', 'v', 'b', 'b', 'n', 'n', 'n', 'n', 'n', '''', 'n', 'n', 'n', 'n', 'm', 'm', 'm', '', '', 'm', '', '', 'm', 'm', 'g', 'B', 'EUR', 'L'],
    false);
end;

{ TInAppProduct -------------------------------------------------------------- }

function TInAppProduct.Price(const ValueWhenUnknown: string): string;
begin
  if PriceRaw = '' then
    Result := ValueWhenUnknown else
    { note: do not use SReplaceChars, as these are UTF-8 chars, not 8-bit chars. }
    Result := ConvertSpecialsToAscii(PriceRaw);
end;

{ TInAppPurchases ------------------------------------------------------------ }

constructor TInAppPurchases.Create(AOwner: TComponent);
begin
  inherited;
  List := TProductList.Create(true);
  Messaging.OnReceive.Add(@MessageReceived);
  ApplicationProperties.OnInitializeJavaActivity.Add(@ReinitializeJavaActivity);
end;

destructor TInAppPurchases.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnInitializeJavaActivity.Remove(@ReinitializeJavaActivity);
  FreeAndNil(List);
  inherited;
end;

procedure TInAppPurchases.ReinitializeJavaActivity(Sender: TObject);
begin
  { in case Java activity got killed and is created again, reinitialize components }
  if FLastAvailableProducts <> '' then
    Messaging.Send(['in-app-purchases-set-available-products', FLastAvailableProducts]);
end;

function TInAppPurchases.MessageReceived(const Received: TCastleStringList): boolean;
var
  P: TInAppProduct;
begin
  Result := false;
  if (Received.Count = 7) and
     (Received[0] = 'in-app-purchases-can-purchase') then
  begin
    P := Product(Received[1]);
    P.FPriceRaw := Received[2];
    P.FTitle := Received[3];
    P.FDescription := Received[4];
    P.FPriceAmountMicros := StrToInt64(Received[5]);
    P.FPriceCurrencyCode := Received[6];
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

function TInAppPurchases.Product(const ProductName: string): TInAppProduct;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    if List[I].Name = ProductName then
      Exit(List[I]);

  { not found, so create new }
  Result := TInAppProduct.Create;
  Result.FName := ProductName;
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
        ', price: ' + List[I].Price +
        ', owned: ' + BoolToStr(List[I].Owns, true) +
        ', title: ' + List[I].Title +
        ', description: ' + List[I].Description +
        ', price amount micros: ' + IntToStr(List[I].PriceAmountMicros) +
        ', price currency code: ' + List[I].PriceCurrencyCode +
        NL;
    WritelnLogMultiline('InAppPurchases', LogStr);
  end;
end;

procedure TInAppPurchases.SetAvailableProducts(const Names: array of string);
var
  Products: array of TAvailableProduct;
  I: Integer;
begin
  SetLength(Products, Length(Names));
  for I := 0 to High(Names) do
  begin
    Products[I].Name := Names[I];
    Products[I].Category := '';
  end;
  SetAvailableProducts(Products);
end;

procedure TInAppPurchases.SetAvailableProducts(const Products: array of TAvailableProduct);
var
  I: Integer;
begin
  FLastAvailableProducts := '';
  for I := 0 to High(Products) do
  begin
    FLastAvailableProducts += Products[I].Name + Chr(3) + Products[I].Category;
    if I < High(Products) then
      FLastAvailableProducts += Chr(2);
  end;
  Messaging.Send(['in-app-purchases-set-available-products', FLastAvailableProducts]);
end;

end.
