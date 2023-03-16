{
  Copyright 2015-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ In-app purchases (TInAppPurchases).
  See https://castle-engine.io/in_app_purchases
  for detailed instructions how to use this. }
unit CastleInAppPurchases;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
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

      Caution: @italic(this may not be up-to-date knowledge).
      Call @link(TInAppPurchases.RefreshPurchases) to refresh it
      (on Android, this happens automatically on app launch;
      but on iOS, it cannot be done automatically, as it requires logging to AppStore
      -- this is also explicitly said in Apple docs).
      Watch for @link(TInAppPurchases.Owns) and @link(TInAppPurchases.OnRefreshedPurchases)
      then.

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

  { Information about product possible to be bought given to
    @link(TInAppPurchases.SetAvailableProducts). }
  TAvailableProduct = record
    { Unique product identifier. }
    Name: string;

    { Category. For now used only for analytics. }
    Category: string;
  end;

  { Manage in-app purchases in your game.
    See https://castle-engine.io/in_app_purchases
    for detailed instructions how to use this. }
  TInAppPurchases = class(TComponent)
  private
    type
      TProductList = {$ifdef FPC}specialize{$endif} TObjectList<TInAppProduct>;
    var
      FDebugMockupBuying: boolean;
      List: TProductList;
      FLastAvailableProducts: string;
      FOnRefreshedPrices: TNotifyEvent;
      FOnRefreshedPurchases: TNotifyEvent;
    function MessageReceived(const Received: TCastleStringList;
      const ReceivedStream: TMemoryStream): boolean;
    procedure ReinitializeJavaActivity(Sender: TObject);
    procedure LogProducts(const Message: string);
  protected
    { Called when the knowledge about what do we own is complete. }
    procedure KnownCompletely; virtual; deprecated 'use RefreshedPrices or RefreshedPurchases';

    { See @link(OnRefreshedPrices) for information when is this called.
      The default implementation of this method in this class just calls
      @link(OnRefreshedPrices). }
    procedure RefreshedPrices; virtual;

    { See @link(OnRefreshedPurchases) for information when is this called.
      The default implementation of this method in this class just calls
      @link(OnRefreshedPurchases). }
    procedure RefreshedPurchases; virtual;

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

      If the product is @bold(not consumable) (which means that it can be owned
      only once, and it's owned forever once bought): Note that this method
      may be called multiple times, because there are various situations in which
      we may "gain knowledge" that user owns this item (e.g.
      each @link(RefreshPurchases) call).
      Write your code to react gracefullly to this, such that calling this method
      on an already-owned item is handled correctly.

      E.g. if you increase some stat (e.g. "gold owned")
      when user buys a "chest of gold", and "chest of gold"
      is non-consumable (you can only own it once, and then you just own it forever),
      then store the fact that you "already increased gold because of the chest ownership"
      in the user persistent data (see https://castle-engine.io/manual_user_prefs.php).
      Do not just increase the "gold owned" at every call of this method.

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
    procedure SetAvailableProducts(const Names: array of string); overload;
    procedure SetAvailableProducts(const Products: array of TAvailableProduct); overload;
    { @groupEnd }

    { Initiate a purchase of given item. }
    procedure Purchase(const AProduct: TInAppProduct);

    { Initiate a consumption of a consumable item.
      You should listen on a "successful consumption" (override
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

    { Call to refresh the state of owned (purchased) items from server.
      This will call @link(Owns) on the owned items, and then @link(RefreshedPurchases)
      or @link(OnRefreshedPurchases).

      This is necessary to be called explicitly on iOS (for AppStore), as it will ask
      user to login to AppStore.
      On Android, this is done automatically at app start, and doesn't ask user anything. }
    procedure RefreshPurchases;

    { For debug purposes, immediately set all @link(TInAppProduct.Owns) to @false.
      This does @italic(not) reset any persistent knowledge about the ownership of the items
      of course, it merely resets the temporary (in CGE) knowledge about what is owned.
      If the purchases communicate with any store (Google Play on Android, AppStore on iOS)
      then the purchases will be restored next time the @link(RefreshPurchases) are done. }
    procedure DebugClearPurchases;
  published
    { Called when the prices (and other shop-related information)
      are known about the products. The information is stored inside
      @link(TInAppProduct) instances, e.g. query @link(TInAppProduct.Price),
      @link(TInAppProduct.Title) and so on. Get the @link(TInAppProduct) instance
      by @link(Product) method.

      This signals that we queried the shop for the product information
      like @link(TInAppProduct.Price) and @link(TInAppProduct.Title) and
      @link(TInAppProduct.Description). This is automatically done always
      when launching the application, and sometimes later too.

      It does not mean that we have queried the ownership status of the products,
      for this see @link(OnRefreshedPurchases).

      See also @link(RefreshedPrices) method. Instead of assigning this event,
      you can also override @link(RefreshedPrices) method in descendants. }
    property OnRefreshedPrices: TNotifyEvent read FOnRefreshedPrices write FOnRefreshedPrices;

    { Called when the ownership status of all products is known.
      The information is stored inside @link(TInAppProduct.Owns),
      you can get the @link(TInAppProduct) instance using the @link(Product) method.

      The ownership status is automatically queried when the application starts on Android.
      On iOS, it must be explicitly invoked using @link(RefreshPurchases) (this is a limitation
      of iOS user-interface around this, so it cannot be hidden / workarounded by us). }
    property OnRefreshedPurchases: TNotifyEvent read FOnRefreshedPurchases write FOnRefreshedPurchases;
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
    Result := ValueWhenUnknown
  else
    { note: do not use SReplaceChars, as these are UTF-8 chars, not 8-bit chars. }
    Result := ConvertSpecialsToAscii(PriceRaw);
end;

{ TInAppPurchases ------------------------------------------------------------ }

constructor TInAppPurchases.Create(AOwner: TComponent);
begin
  inherited;
  List := TProductList.Create(true);
  Messaging.OnReceive.Add({$ifdef FPC}@{$endif} MessageReceived);
  ApplicationProperties.OnInitializeJavaActivity.Add({$ifdef FPC}@{$endif} ReinitializeJavaActivity);
end;

destructor TInAppPurchases.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove({$ifdef FPC}@{$endif} MessageReceived);
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnInitializeJavaActivity.Remove({$ifdef FPC}@{$endif} ReinitializeJavaActivity);
  FreeAndNil(List);
  inherited;
end;

procedure TInAppPurchases.ReinitializeJavaActivity(Sender: TObject);
begin
  { in case Java activity got killed and is created again, reinitialize services }
  if FLastAvailableProducts <> '' then
    Messaging.Send(['in-app-purchases-set-available-products', FLastAvailableProducts]);
end;

function TInAppPurchases.MessageReceived(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;
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
     (Received[0] = 'in-app-purchases-refreshed-purchases') then
  begin
    RefreshedPurchases;
    Result := true;
  end else
  if (Received.Count = 1) and
     (Received[0] = 'in-app-purchases-refreshed-prices') then
  begin
    RefreshedPrices;
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
    Owns(AProduct)
  else
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
    FLastAvailableProducts := FLastAvailableProducts + Products[I].Name + Chr(3) + Products[I].Category;
    if I < High(Products) then
      FLastAvailableProducts := FLastAvailableProducts + Chr(2);
  end;
  Messaging.Send(['in-app-purchases-set-available-products', FLastAvailableProducts]);
end;

procedure TInAppPurchases.LogProducts(const Message: string);
var
  I: Integer;
  LogStr: string;
begin
  LogStr := Message + NL;
  for I := 0 to List.Count - 1 do
    LogStr := LogStr + 'Product ' + List[I].Name +
      ', price: ' + List[I].PriceRaw +
      ', owned: ' + BoolToStr(List[I].Owns, true) +
      ', title: ' + List[I].Title +
      ', description: ' + List[I].Description +
      ', price amount micros: ' + IntToStr(List[I].PriceAmountMicros) +
      ', price currency code: ' + List[I].PriceCurrencyCode +
      NL;
  WritelnLogMultiline('InAppPurchases', LogStr);
end;

procedure TInAppPurchases.KnownCompletely;
begin
end;

procedure TInAppPurchases.RefreshPurchases;
begin
  Messaging.Send(['in-app-purchases-refresh-purchases']);
end;

procedure TInAppPurchases.RefreshedPrices;
begin
  LogProducts('Refreshed prices (and other store information) about all products:');
  if Assigned(OnRefreshedPrices) then
    OnRefreshedPrices(Self);
end;

procedure TInAppPurchases.RefreshedPurchases;
begin
  LogProducts('Refreshed purchases (ownership status) of all products:');
  if Assigned(OnRefreshedPurchases) then
    OnRefreshedPurchases(Self);

  {$warnings off} // deliberately calling deprecated, to keep it working
  KnownCompletely;
  {$warnings on}
end;

procedure TInAppPurchases.DebugClearPurchases;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].FOwns := false;
end;

end.
