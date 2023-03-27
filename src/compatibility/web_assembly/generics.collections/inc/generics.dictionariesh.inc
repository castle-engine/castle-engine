{%MainUnit generics.collections.pas}

{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains the generics collections library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types and major refactoring of entire library

    Thanks to mORMot (http://synopse.info) project for the best implementations
    of hashing functions like crc32c and xxHash32 :)

 **********************************************************************}

{$WARNINGS OFF}
type
  TEmptyRecord = record // special record for Dictionary TValue (Dictionary as Set)
  end;

  { TPair }

  TPair<TKey, TValue> = record
  public
    Key: TKey;
    Value: TValue;
    class function Create(AKey: TKey; AValue: TValue): TPair<TKey, TValue>; static;
  end;

  { TCustomDictionary }

  // bug #24283 and #24097 (forward declaration) - should be:
  // TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> = class(TEnumerable<TPair<TKey, TValue> >);
  TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> = class abstract
  public type
    //  workaround... no generics types in generics types
    TDictionaryPair = TPair<TKey, TValue>;
    PDictionaryPair = ^TDictionaryPair;
    PKey = ^TKey;
    PValue = ^TValue;
    THashFactoryClass = THashFactory;
  protected
    FEqualityComparer: IEqualityComparer<TKey>;
    FKeys: TEnumerable<TKey>;
    FValues: TEnumerable<TValue>;
    FMaxLoadFactor: single;
  protected
    procedure SetCapacity(ACapacity: SizeInt); virtual; abstract;
    // bug #24283. workaround for this class because can't inherit from TEnumerable
    function DoGetEnumerator: TEnumerator<TDictionaryPair>; virtual; abstract; {override;}

    procedure SetMaxLoadFactor(AValue: single); virtual; abstract;
    function GetLoadFactor: single; virtual; abstract;
    function GetCapacity: SizeInt; virtual; abstract;
  public
    property MaxLoadFactor: single read FMaxLoadFactor write SetMaxLoadFactor;
    property LoadFactor: single read GetLoadFactor;
    property Capacity: SizeInt read GetCapacity write SetCapacity;

    procedure Clear; virtual; abstract;
    procedure Add(constref APair: TPair<TKey, TValue>); virtual; abstract;
  strict private // bug #24283. workaround for this class because can't inherit from TEnumerable
    function ToArray(ACount: SizeInt): TArray<TDictionaryPair>; overload;
  public
    function ToArray: TArray<TDictionaryPair>; virtual; final; {override; final; // bug #24283} overload;

    constructor Create; virtual; overload;
    constructor Create(ACapacity: SizeInt); virtual; overload;
    constructor Create(ACapacity: SizeInt; const AComparer: IEqualityComparer<TKey>); virtual; overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>); virtual; overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); virtual; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>); virtual; overload;
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); virtual; overload;
    {$ENDIF}

    destructor Destroy; override;
  private
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
  protected
    procedure UpdateItemsThreshold(ASize: SizeInt); virtual; abstract;

    procedure KeyNotify(constref AKey: TKey; ACollectionNotification: TCollectionNotification); virtual;
    procedure ValueNotify(constref AValue: TValue; ACollectionNotification: TCollectionNotification); virtual;
    procedure PairNotify(constref APair: TDictionaryPair; ACollectionNotification: TCollectionNotification); inline;
    procedure SetValue(var AValue: TValue; constref ANewValue: TValue);
  public
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  protected // FItemsLength must be declared at the end of TCustomDictionary
    FItemsLength: SizeInt;
  public
    property Count: SizeInt read FItemsLength;
  end;

  { TCustomDictionaryEnumerator }

  TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS> = class abstract(TEnumerator< T >)
  private
    FDictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>;
    FIndex: SizeInt;
  protected
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual; abstract;
  public
    constructor Create(ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
  end;

  { TDictionaryEnumerable }

  TDictionaryEnumerable<TDictionaryEnumerator: TObject; TDictionaryPointersEnumerator, // ... inherits from TCustomDictionaryEnumerator. workaround...
    T, CUSTOM_DICTIONARY_CONSTRAINTS> = class abstract(TEnumerableWithPointers<T>)
  private
    FDictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>;
    function GetCount: SizeInt;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
    function DoGetEnumerator: TDictionaryEnumerator; override;
  public
    constructor Create(ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
    function ToArray: TArray<T>; override; final;
    property Count: SizeInt read GetCount;
  end;

  // more info : http://en.wikipedia.org/wiki/Open_addressing

  { TOpenAddressingEnumerator }

  TOpenAddressingEnumerator<T, OPEN_ADDRESSING_CONSTRAINTS> = class abstract(TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>)
  protected
    function DoMoveNext: Boolean; override;
  end;

  TOpenAddressingPointersEnumerator<TItem, PDictionaryPair> = class abstract(TEnumerator<PDictionaryPair>)
  private var
    FItems: ^TArray<TItem>;
    FIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: PDictionaryPair; override;
    function GetCurrent: PDictionaryPair; virtual;
  public
    constructor Create(var AItems);
  end;

  TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair> = record
  private type
    PArray = ^TArray<TItem>;
    function Items: PArray; inline;
    function GetCount: SizeInt; inline;
  public
    function GetEnumerator: TPointersEnumerator;
    function ToArray: TArray<PDictionaryPair>;
    property Count: SizeInt read GetCount;
  end;

  TOnGetMemoryLayoutKeyPosition = procedure(Sender: TObject; AKeyPos: UInt32) of object;

  TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS> = class abstract(TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>)
  private type
    PItem = ^TItem;
    TItem = record
      Hash: UInt32;
      Pair: TPair<TKey, TValue>;
    end;

    TItemsArray = array of TItem;
    TPointersEnumerator = class(TOpenAddressingPointersEnumerator<TItem, PDictionaryPair>);
    TPointersCollection = TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair>;
  public type
    PPointersCollection = ^TPointersCollection;
  private var // FItems must be declared as first field 
    FItems: TItemsArray;
    FItemsThreshold: SizeInt;

    procedure Resize(ANewSize: SizeInt);
    procedure PrepareAddingItem;
  protected
    function RealItemsLength: SizeInt; virtual;
    function Rehash(ASizePow2: SizeInt; AForce: Boolean = False): boolean; virtual;
    function FindBucketIndex(constref AKey: TKey): SizeInt; overload; inline;
    function FindBucketIndex(constref AItems: TArray<TItem>; constref AKey: TKey; out AHash: UInt32): SizeInt; virtual; abstract; overload;
  public
    type
      // Enumerators
      TPairEnumerator = class(TOpenAddressingEnumerator<TDictionaryPair, OPEN_ADDRESSING_CONSTRAINTS>)
      protected
        function GetCurrent: TPair<TKey,TValue>; override;
      end;

      TValueEnumerator = class(TOpenAddressingEnumerator<TValue, OPEN_ADDRESSING_CONSTRAINTS>)
      protected
        function GetCurrent: TValue; override;
      end;

      TPValueEnumerator = class(TOpenAddressingEnumerator<PValue, OPEN_ADDRESSING_CONSTRAINTS>)
      protected
        function GetCurrent: PValue; override;
      end;

      TKeyEnumerator = class(TOpenAddressingEnumerator<TKey, OPEN_ADDRESSING_CONSTRAINTS>)
      protected
        function GetCurrent: TKey; override;
      end;

      TPKeyEnumerator = class(TOpenAddressingEnumerator<PKey, OPEN_ADDRESSING_CONSTRAINTS>)
      protected
        function GetCurrent: PKey; override;
      end;

      // Collections
      TValueCollection = class(TDictionaryEnumerable<TValueEnumerator, TPValueEnumerator, TValue, CUSTOM_DICTIONARY_CONSTRAINTS>);

      TKeyCollection = class(TDictionaryEnumerable<TKeyEnumerator, TPKeyEnumerator, TKey, CUSTOM_DICTIONARY_CONSTRAINTS>);

    // bug #24283 - workaround related to lack of DoGetEnumerator
    function GetEnumerator: TPairEnumerator; reintroduce;
  private
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function GetPointers: PPointersCollection; inline;
  private
    function GetItem(const AKey: TKey): TValue; inline;
    procedure SetItem(const AKey: TKey; const AValue: TValue); inline;
    procedure AddItem(var AItem: TItem; constref AKey: TKey; constref AValue: TValue; const AHash: UInt32); inline;
  protected
     // useful for using dictionary as array
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): TValue; virtual;
    function DoAdd(constref AKey: TKey; constref AValue: TValue): SizeInt; virtual;

    procedure UpdateItemsThreshold(ASize: SizeInt); override;

    procedure SetCapacity(ACapacity: SizeInt); override;
    // bug #24283 - can't descadent from TEnumerable
    function DoGetEnumerator: TEnumerator<TDictionaryPair>; override;
    procedure SetMaxLoadFactor(AValue: single); override;
    function GetLoadFactor: single; override;
    function GetCapacity: SizeInt; override;
  public
    // many constructors because bug #25607
    constructor Create(ACapacity: SizeInt; const AComparer: IEqualityComparer<TKey>); override; overload;

    procedure Add(constref APair: TPair<TKey, TValue>); override; overload;
    procedure Add(constref AKey: TKey; constref AValue: TValue); overload; inline;
    procedure Remove(constref AKey: TKey);
    function ExtractPair(constref AKey: TKey): TPair<TKey, TValue>;
    procedure Clear; override;
    procedure TrimExcess;
    function TryGetValue(constref AKey: TKey; out AValue: TValue): Boolean;
    function TryAdd(constref AKey: TKey; constref AValue: TValue): Boolean;
    procedure AddOrSetValue(constref AKey: TKey; constref AValue: TValue);
    function ContainsKey(constref AKey: TKey): Boolean; inline;
    function ContainsValue(constref AValue: TValue): Boolean; overload;
    function ContainsValue(constref AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue>): Boolean; virtual; overload;

    property Items[Index: TKey]: TValue read GetItem write SetItem; default;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property Ptr: PPointersCollection read GetPointers;

    procedure GetMemoryLayout(const AOnGetMemoryLayoutKeyPosition: TOnGetMemoryLayoutKeyPosition);
  end;

  TOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS> = class(TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>)
  private type // for workaround Lazarus bug #25613
    _TItem = record
      Hash: UInt32;
      Pair: TPair<TKey, TValue>;
    end;
  protected
    procedure NotifyIndexChange(AFrom, ATo: SizeInt); virtual;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): TValue; override;
    function FindBucketIndex(constref AItems: TArray<TItem>; constref AKey: TKey; out AHash: UInt32): SizeInt; override; overload;
  end;

  // More info and TODO
  // https://github.com/OpenHFT/UntitledCollectionsProject/wiki/Tombstones-purge-from-hashtable:-theory-and-practice

  TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS> = class abstract(TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>)
  private
    FTombstonesCount: SizeInt;
  protected
    function Rehash(ASizePow2: SizeInt; AForce: Boolean = False): boolean; override;
    function RealItemsLength: SizeInt; override;

    function FindBucketIndexOrTombstone(constref AItems: TArray<TItem>; constref AKey: TKey;
      out AHash: UInt32): SizeInt; virtual; abstract;

    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): TValue; override;
    function DoAdd(constref AKey: TKey; constref AValue: TValue): SizeInt; override;
  public
    property TombstonesCount: SizeInt read FTombstonesCount;
    procedure ClearTombstones; virtual;
    procedure Clear; override;
  end;

  TOpenAddressingSH<OPEN_ADDRESSING_CONSTRAINTS> = class(TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>)
  private type // for workaround Lazarus bug #25613
    _TItem = record
      Hash: UInt32;
      Pair: TPair<TKey, TValue>;
    end;
  protected
    function FindBucketIndex(constref AItems: TArray<TItem>; constref AKey: TKey;
      out AHash: UInt32): SizeInt; override; overload;
    function FindBucketIndexOrTombstone(constref AItems: TArray<TItem>; constref AKey: TKey;
      out AHash: UInt32): SizeInt; override;
  end;

  TOpenAddressingQP<OPEN_ADDRESSING_CONSTRAINTS> = class(TOpenAddressingSH<OPEN_ADDRESSING_CONSTRAINTS>)
  private
    FPrimaryNumberAsSizeApproximation: SizeInt;
  protected
    procedure UpdateItemsThreshold(ASize: SizeInt); override;
    function FindBucketIndex(constref AItems: TArray<TItem>;
      constref AKey: TKey; out AHash: UInt32): SizeInt; override; overload;
    function FindBucketIndexOrTombstone(constref AItems: TArray<TItem>;
      constref AKey: TKey; out AHash: UInt32): SizeInt; override;
  end;

  TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS> = class(TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>)
  private type // for workaround Lazarus bug #25613
    _TItem = record
      Hash: UInt32;
      Pair: TPair<TKey, TValue>;
    end;
  private
    R: UInt32;
  protected
    procedure UpdateItemsThreshold(ASize: SizeInt); override;
    function FindBucketIndex(constref AItems: TArray<TItem>; constref AKey: TKey;
      out AHash: UInt32): SizeInt; override; overload;
    function FindBucketIndexOrTombstone(constref AItems: TArray<TItem>; constref AKey: TKey;
      out AHash: UInt32): SizeInt; override;
  strict protected
    constructor Create(ACapacity: SizeInt; const AComparer: IEqualityComparer<TKey>); override; overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); override; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); override; overload;
    {$ENDIF}
  public // bug #26181 (redundancy of constructors)
    constructor Create(ACapacity: SizeInt); override; overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>); override; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>); override; overload;
    {$ENDIF}
    constructor Create(ACapacity: SizeInt; const AComparer: IExtendedEqualityComparer<TKey>); virtual; overload;
    constructor Create(const AComparer: IExtendedEqualityComparer<TKey>); overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IExtendedEqualityComparer<TKey>); virtual; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>; const AComparer: IExtendedEqualityComparer<TKey>); virtual; overload;
    {$ENDIF}
  end;

  TDeamortizedDArrayCuckooMapEnumerator<T, CUCKOO_CONSTRAINTS> = class abstract(TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>)
  private type // for workaround Lazarus bug #25613
    TItem = record
      Hash: UInt32;
      Pair: TPair<TKey, TValue>;
    end;
    TItemsArray = array of TItem;
  private
    FMainIndex: SizeInt;
  protected
    function DoMoveNext: Boolean; override;
  public
    constructor Create(ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
  end;

  TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair> = class abstract(TEnumerator<PDictionaryPair>)
  private var // FItems must be declared as first field and FQueue as second
    FItems: ^TItemsDArray;
    FQueue: TQueueDictionary;
    FIndex: SizeInt;
    FMainIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: PDictionaryPair; override;
    function GetCurrent: PDictionaryPair; virtual;
  public
    constructor Create(var AItems; AQueue: TQueueDictionary; ACount: SizeInt);
  end;

  TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair> = record
  private type
    PArray = ^TItemsDArray;
    function Items: PArray; inline;
    function GetCount: SizeInt; inline;
    function GetQueue: TQueueDictionary; inline;
  public
    function GetEnumerator: TPointersEnumerator;
    function ToArray: TArray<PDictionaryPair>;
    property Count: SizeInt read GetCount;
  end;

  // more info :
  // http://arxiv.org/abs/0903.0391

  TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS> = class(TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>)
  private const // Lookup Result
    LR_NIL = -1;
    LR_QUEUE = -2;
  private type
    PItem = ^TItem;
    TItem = record
      Hash: UInt32;
      Pair: TPair<TKey, TValue>;
    end;
    TValueForQueue = TItem;

    TQueueDictionary = class(TOpenAddressingLP<TKey, TValueForQueue, TDefaultHashFactory, TLinearProbing>)
    private type // for workaround Lazarus bug #25613
      _TItem = record
        Hash: UInt32;
        Pair: TPair<TKey, TValueForQueue>;
      end;
    private
      FIdx: TList<UInt32>; // list to keep order
    protected
      procedure NotifyIndexChange(AFrom, ATo: SizeInt); override;
      function Rehash(ASizePow2: SizeInt; AForce: Boolean = False): Boolean; override;
    public
      procedure InsertIntoBack(AItem: Pointer);
      procedure InsertIntoHead(AItem: Pointer);
      function IsEmpty: Boolean;
      function Pop: Pointer;
      constructor Create(ACapacity: SizeInt; const AComparer: IEqualityComparer<TKey>); override; overload;
      destructor Destroy; override;
    end;

    // cycle-detection mechanism class
    TCDM = class(TOpenAddressingSH<TKey, TEmptyRecord, TDefaultHashFactory, TLinearProbing>);
    TItemsArray = array of TItem;
    TItemsDArray = array[0..Pred(TCuckooCfg.D)] of TItemsArray;
    TPointersEnumerator = class(TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair>);
    TPointersCollection = TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair>;
  public type
    PPointersCollection = ^TPointersCollection;
  private var
    FItems: TItemsDArray;
    FQueue: TQueueDictionary;  // probably can be optimized - hash TItem give information from TItem.Hash for cuckoo ...
      // currently is kept in "TQueueDictionary = class(TOpenAddressingSH<TKey, TItem, ...>"

    FCDM: TCDM; // cycle-detection mechanism
    FItemsThreshold: SizeInt;
  // sadly there is bug #24848 for class var ...
  {class} var
    CUCKOO_SIGN, CUCKOO_INDEX_SIZE, CUCKOO_HASH_SIGN: UInt32;
    // CUCKOO_MAX_ITEMS_LENGTH: <- to do : calc max length for items based on CUCKOO sign
    // maybe some CDM bloom filter?

    procedure Resize(ANewSize: SizeInt);
    procedure Rehash(ASizePow2: SizeInt);
    procedure PrepareAddingItem;
  protected
    procedure UpdateItemsThreshold(ASize: SizeInt); override;
    function Lookup(constref AKey: TKey; var AHashListOrIndex: PUInt32): SizeInt; inline; overload;
    function Lookup(constref AItems: TItemsDArray; constref AKey: TKey; var AHashListOrIndex: PUInt32): SizeInt; virtual; overload;
  public
    type
      // Enumerators
      TPairEnumerator = class(TDeamortizedDArrayCuckooMapEnumerator<TDictionaryPair, CUCKOO_CONSTRAINTS>)
      protected
        function GetCurrent: TPair<TKey,TValue>; override;
      end;

      TValueEnumerator = class(TDeamortizedDArrayCuckooMapEnumerator<TValue, CUCKOO_CONSTRAINTS>)
      protected
        function GetCurrent: TValue; override;
      end;

      TPValueEnumerator = class(TDeamortizedDArrayCuckooMapEnumerator<PValue, CUCKOO_CONSTRAINTS>)
      protected
        function GetCurrent: PValue; override;
      end;

      TKeyEnumerator = class(TDeamortizedDArrayCuckooMapEnumerator<TKey, CUCKOO_CONSTRAINTS>)
      protected
        function GetCurrent: TKey; override;
      end;

      TPKeyEnumerator = class(TDeamortizedDArrayCuckooMapEnumerator<PKey, CUCKOO_CONSTRAINTS>)
      protected
        function GetCurrent: PKey; override;
      end;

      // Collections
      TValueCollection = class(TDictionaryEnumerable<TValueEnumerator, TPValueEnumerator, TValue, CUSTOM_DICTIONARY_CONSTRAINTS>);

      TKeyCollection = class(TDictionaryEnumerable<TKeyEnumerator, TPKeyEnumerator, TKey, CUSTOM_DICTIONARY_CONSTRAINTS>);

    // bug #24283 - workaround related to lack of DoGetEnumerator
    function GetEnumerator: TPairEnumerator; reintroduce;
  private
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function GetPointers: PPointersCollection; inline;
  private
    function GetItem(const AKey: TKey): TValue; inline;
    procedure SetItem(const AKey: TKey; const AValue: TValue); overload; inline;
    procedure SetItem(constref AValue: TValue; const AHashListOrIndex: PUInt32; ALookupResult: SizeInt); overload;

    procedure AddItem(constref AItems: TItemsDArray; constref AKey: TKey; constref AValue: TValue; const AHashList: PUInt32); overload;
    procedure DoAdd(const AKey: TKey; const AValue: TValue; const AHashList: PUInt32); overload; inline;
    function DoRemove(const AHashListOrIndex: PUInt32; ALookupResult: SizeInt;
      ACollectionNotification: TCollectionNotification): TValue;

    function GetQueueCount: SizeInt;
  protected
    procedure SetCapacity(ACapacity: SizeInt); override;
    // bug #24283 - can't descadent from TEnumerable
    function DoGetEnumerator: TEnumerator<TDictionaryPair>; override;
    procedure SetMaxLoadFactor(AValue: single); override;
    function GetLoadFactor: single; override;
    function GetCapacity: SizeInt; override;
  strict protected // bug #26181
    constructor Create(ACapacity: SizeInt; const AComparer: IEqualityComparer<TKey>); override; overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); override; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); override; overload;
    {$ENDIF}
  public
    // TODO: function TryFlushQueue(ACount: SizeInt): SizeInt;

    constructor Create; override; overload;
    constructor Create(ACapacity: SizeInt); override; overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>); override; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>); override; overload;
    {$ENDIF}
    constructor Create(ACapacity: SizeInt; const AComparer: IExtendedEqualityComparer<TKey>); virtual; overload;
    constructor Create(const AComparer: IExtendedEqualityComparer<TKey>); overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IExtendedEqualityComparer<TKey>); virtual; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<TDictionaryPair>; const AComparer: IExtendedEqualityComparer<TKey>); virtual; overload;
    {$ENDIF}
    destructor Destroy; override;

    procedure Add(constref APair: TPair<TKey, TValue>); override; overload;
    procedure Add(constref AKey: TKey; constref AValue: TValue); overload;
    procedure Remove(constref AKey: TKey);
    function ExtractPair(constref AKey: TKey): TPair<TKey, TValue>;
    procedure Clear; override;
    procedure TrimExcess;
    function TryGetValue(constref AKey: TKey; out AValue: TValue): Boolean;
    procedure AddOrSetValue(constref AKey: TKey; constref AValue: TValue);
    function ContainsKey(constref AKey: TKey): Boolean; inline;
    function ContainsValue(constref AValue: TValue): Boolean; overload;
    function ContainsValue(constref AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue>): Boolean; virtual; overload;

    property Items[Index: TKey]: TValue read GetItem write SetItem; default;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property Ptr: PPointersCollection read GetPointers;

    property QueueCount: SizeInt read GetQueueCount;
    procedure GetMemoryLayout(const AOnGetMemoryLayoutKeyPosition: TOnGetMemoryLayoutKeyPosition);
  end;

  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);

  TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS> = class(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>)
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(constref AKey: TKey; ACollectionNotification: TCollectionNotification); override;
    procedure ValueNotify(constref AValue: TValue; ACollectionNotification: TCollectionNotification); override;
  public
    // can't be as "Create(AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt = 0)"
    // because bug #25607
    constructor Create(AOwnerships: TDictionaryOwnerships); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships;
      const AComparer: IExtendedEqualityComparer<TKey>); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt;
      const AComparer: IExtendedEqualityComparer<TKey>); overload;
  end;

  TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS> = class(TOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>)
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(constref AKey: TKey; ACollectionNotification: TCollectionNotification); override;
    procedure ValueNotify(constref AValue: TValue; ACollectionNotification: TCollectionNotification); override;
  public
    // can't be as "Create(AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt = 0)"
    // because bug #25607
    constructor Create(AOwnerships: TDictionaryOwnerships); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships;
      const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt;
      const AComparer: IEqualityComparer<TKey>); overload;
  end;

  // useful generics overloads
  TOpenAddressingLP<TKey, TValue, THashFactory> = class(TOpenAddressingLP<TKey, TValue, THashFactory, TLinearProbing>);
  TOpenAddressingLP<TKey, TValue>  = class(TOpenAddressingLP<TKey, TValue, TDefaultHashFactory, TLinearProbing>);

  TObjectOpenAddressingLP<TKey, TValue, THashFactory> = class(TObjectOpenAddressingLP<TKey, TValue, THashFactory, TLinearProbing>);
  TObjectOpenAddressingLP<TKey, TValue> = class(TObjectOpenAddressingLP<TKey, TValue, TDefaultHashFactory, TLinearProbing>);

  // Linear Probing with Tombstones (LPT)
  TOpenAddressingLPT<TKey, TValue, THashFactory> = class(TOpenAddressingSH<TKey, TValue, THashFactory, TLinearProbing>);
  TOpenAddressingLPT<TKey, TValue> = class(TOpenAddressingSH<TKey, TValue, TDefaultHashFactory, TLinearProbing>);

  TOpenAddressingQP<TKey, TValue, THashFactory> = class(TOpenAddressingQP<TKey, TValue, THashFactory, TQuadraticProbing>);
  TOpenAddressingQP<TKey, TValue> = class(TOpenAddressingQP<TKey, TValue, TDefaultHashFactory, TQuadraticProbing>);

  TOpenAddressingDH<TKey, TValue, THashFactory> = class(TOpenAddressingDH<TKey, TValue, THashFactory, TDoubleHashing>);
  TOpenAddressingDH<TKey, TValue> = class(TOpenAddressingDH<TKey, TValue, TDelphiDoubleHashFactory, TDoubleHashing>);

  TCuckooD2<TKey, TValue, THashFactory> = class(TDeamortizedDArrayCuckooMap<TKey, TValue, THashFactory, TDeamortizedCuckooHashingCfg_D2>);
  TCuckooD2<TKey, TValue> = class(TDeamortizedDArrayCuckooMap<TKey, TValue, TDelphiDoubleHashFactory, TDeamortizedCuckooHashingCfg_D2>);

  TCuckooD4<TKey, TValue, THashFactory> = class(TDeamortizedDArrayCuckooMap<TKey, TValue, THashFactory, TDeamortizedCuckooHashingCfg_D4>);
  TCuckooD4<TKey, TValue> = class(TDeamortizedDArrayCuckooMap<TKey, TValue, TDelphiQuadrupleHashFactory, TDeamortizedCuckooHashingCfg_D4>);

  TCuckooD6<TKey, TValue, THashFactory> = class(TDeamortizedDArrayCuckooMap<TKey, TValue, THashFactory, TDeamortizedCuckooHashingCfg_D6>);
  TCuckooD6<TKey, TValue> = class(TDeamortizedDArrayCuckooMap<TKey, TValue, TDelphiSixfoldHashFactory, TDeamortizedCuckooHashingCfg_D6>);

  TObjectCuckooD2<TKey, TValue, THashFactory> = class(TObjectDeamortizedDArrayCuckooMap<TKey, TValue, THashFactory, TDeamortizedCuckooHashingCfg_D2>);
  TObjectCuckooD2<TKey, TValue> = class(TObjectDeamortizedDArrayCuckooMap<TKey, TValue, TDelphiDoubleHashFactory, TDeamortizedCuckooHashingCfg_D2>);

  TObjectCuckooD4<TKey, TValue, THashFactory> = class(TObjectDeamortizedDArrayCuckooMap<TKey, TValue, THashFactory, TDeamortizedCuckooHashingCfg_D4>);
  TObjectCuckooD4<TKey, TValue> = class(TObjectDeamortizedDArrayCuckooMap<TKey, TValue, TDelphiQuadrupleHashFactory, TDeamortizedCuckooHashingCfg_D4>);

  TObjectCuckooD6<TKey, TValue, THashFactory> = class(TObjectDeamortizedDArrayCuckooMap<TKey, TValue, THashFactory, TDeamortizedCuckooHashingCfg_D6>);
  TObjectCuckooD6<TKey, TValue> = class(TObjectDeamortizedDArrayCuckooMap<TKey, TValue, TDelphiSixfoldHashFactory, TDeamortizedCuckooHashingCfg_D6>);

  // for normal programmers to normal use =)
  TDictionary<TKey, TValue> = class(TOpenAddressingLP<TKey, TValue>);
  TObjectDictionary<TKey, TValue> = class(TObjectOpenAddressingLP<TKey, TValue>);

  TFastHashMap<TKey, TValue> = class(TCuckooD2<TKey, TValue>);
  TFastObjectHashMap<TKey, TValue> = class(TObjectCuckooD2<TKey, TValue>);

  THashMap<TKey, TValue> = class(TCuckooD4<TKey, TValue>);
  TObjectHashMap<TKey, TValue> = class(TObjectCuckooD4<TKey, TValue>);
