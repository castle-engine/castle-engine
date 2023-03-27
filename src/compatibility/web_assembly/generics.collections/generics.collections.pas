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

unit Generics.Collections;

{$MODE DELPHI}{$H+}
{$MACRO ON}
{$COPERATORS ON}
{$DEFINE CUSTOM_DICTIONARY_CONSTRAINTS := TKey, TValue, THashFactory}
{$DEFINE OPEN_ADDRESSING_CONSTRAINTS := TKey, TValue, THashFactory, TProbeSequence}
{$DEFINE CUCKOO_CONSTRAINTS := TKey, TValue, THashFactory, TCuckooCfg}
{$DEFINE TREE_CONSTRAINTS := TKey, TValue, TInfo}
{$WARNINGS OFF}
{$HINTS OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
    RtlConsts, Classes, SysUtils, Generics.MemoryExpanders, Generics.Defaults,
    Generics.Helpers, Generics.Strings;

{.$define EXTRA_WARNINGS}
{.$define ENABLE_METHODS_WITH_TEnumerableWithPointers}

type
  EAVLTree = class(Exception);
  EIndexedAVLTree = class(EAVLTree);

  TDuplicates = Classes.TDuplicates;

  {$ifdef VER3_0_0}
  TArray<T> = array of T;
  {$endif}

  // bug #24254 workaround
  // should be TArray = record class procedure Sort<T>(...) etc.
  TBinarySearchResult = record
    FoundIndex, CandidateIndex: SizeInt;
    CompareResult: SizeInt;
  end;

  TCustomArrayHelper<T> = class abstract
  private
    type
      // bug #24282
      TComparerBugHack = TComparer<T>;
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt; const AComparer: IComparer<T>);
      virtual; abstract;
  public
    class procedure Sort(var AValues: array of T); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>);   overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>; AIndex, ACount: SizeInt); overload;

    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt): Boolean; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>): Boolean; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out ASearchResult: TBinarySearchResult): Boolean; overload;
  end {$ifdef EXTRA_WARNINGS}experimental{$endif}; // will be renamed to TCustomArray (bug #24254)

  TArrayHelper<T> = class(TCustomArrayHelper<T>)
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt; const AComparer: IComparer<T>); override;
  public
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
  end {$ifdef EXTRA_WARNINGS}experimental{$endif}; // will be renamed to TArray (bug #24254)

  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(ASender: TObject; constref AItem: T; AAction: TCollectionNotification)
    of object;

  { TEnumerator }

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: boolean;
  end;

  { TEnumerable }

  TEnumerable<T> = class abstract
  public type
    PT = ^T;
  protected // no forward generics declarations (needed by TPointersCollection<T, PT>), this should be moved into TEnumerableWithPointers
    function GetPtrEnumerator: TEnumerator<PT>; virtual; abstract;
  protected
    function ToArrayImpl(ACount: SizeInt): TArray<T>; overload; // used by descendants
  protected
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: TEnumerator<T>; inline;
    function ToArray: TArray<T>; virtual; overload;
  end;

  // error: no memory left for TCustomPointersEnumerator<PT> version
  TCustomPointersEnumerator<T, PT> = class abstract(TEnumerator<PT>);

  TCustomPointersCollection<T, PT> = object
  strict private type
    TLocalEnumerable = TEnumerable<T>; // compiler has bug for directly usage of TEnumerable<T>
  protected
    function Enumerable: TLocalEnumerable; inline;
  public
    function GetEnumerator: TEnumerator<PT>;
  end;

  TEnumerableWithPointers<T> = class(TEnumerable<T>)
  strict private type
    TPointersCollection = TCustomPointersCollection<T, PT>;
    PPointersCollection = ^TPointersCollection;
  private
    function GetPtr: PPointersCollection; inline;
  public
    property Ptr: PPointersCollection read GetPtr;
  end;

  // More info: http://stackoverflow.com/questions/5232198/about-vectors-growth
  // TODO: custom memory managers (as constraints)
  {$DEFINE CUSTOM_LIST_CAPACITY_INC := Result + Result div 2} // ~approximation to golden ratio: n = n * 1.5 }
  // {$DEFINE CUSTOM_LIST_CAPACITY_INC := Result * 2} // standard inc
  TCustomList<T> = class abstract(TEnumerableWithPointers<T>)
  public type
    PT = ^T;
  protected
    type // bug #24282
      TArrayHelperBugHack = TArrayHelper<T>;
  private
    FOnNotify: TCollectionNotifyEvent<T>;
    function GetCapacity: SizeInt; inline;
  protected
    FLength: SizeInt;
    FItems: array of T;

    function PrepareAddingItem: SizeInt; virtual;
    function PrepareAddingRange(ACount: SizeInt): SizeInt; virtual;
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); virtual;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; virtual;
    procedure SetCapacity(AValue: SizeInt); virtual; abstract;
    function GetCount: SizeInt; virtual;
  public
    function ToArray: TArray<T>; override; final;

    property Count: SizeInt read GetCount;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

    procedure TrimExcess; virtual; abstract;
  end;

  TCustomListEnumerator<T> = class abstract(TEnumerator<T>)
  private
    FList: TCustomList<T>;
    FIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual;
  public
    constructor Create(AList: TCustomList<T>);
  end;

  TCustomListWithPointers<T> = class(TCustomList<T>)
  public type
    TPointersEnumerator = class(TCustomPointersEnumerator<T, PT>)
    protected
      FList: TCustomListWithPointers<T>;
      FIndex: SizeInt;
      function DoMoveNext: boolean; override;
      function DoGetCurrent: PT; override;
    public
      constructor Create(AList: TCustomListWithPointers<T>);
    end;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
  end;

  TList<T> = class(TCustomListWithPointers<T>)
  private var
    FComparer: IComparer<T>;
  protected
    // bug #24287 - workaround for generics type name conflict (Identifier not found)
    // next bug workaround - for another error related to previous workaround
    // change order (method must be declared before TEnumerator declaration)
    function DoGetEnumerator: {Generics.Collections.}TEnumerator<T>; override;
  public
    // with this type declaration i found #24285, #24285
    type
      // bug workaround
      TEnumerator = class(TCustomListEnumerator<T>);

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    procedure SetCapacity(AValue: SizeInt); override;
    procedure SetCount(AValue: SizeInt);
    procedure InitializeList; virtual;
    procedure InternalInsert(AIndex: SizeInt; constref AValue: T);
  private
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; const AValue: T);
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(ACollection: TEnumerable<T>); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>); overload;
    {$ENDIF}
    destructor Destroy; override;

    function Add(constref AValue: T): SizeInt; virtual;
    procedure AddRange(constref AValues: array of T); virtual; overload;
    procedure AddRange(const AEnumerable: IEnumerable<T>); overload;
    procedure AddRange(AEnumerable: TEnumerable<T>); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    procedure AddRange(AEnumerable: TEnumerableWithPointers<T>); overload;
    {$ENDIF}

    procedure Insert(AIndex: SizeInt; constref AValue: T); virtual;
    procedure InsertRange(AIndex: SizeInt; constref AValues: array of T); virtual; overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>); overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerableWithPointers<T>); overload;
    {$ENDIF}

    function Remove(constref AValue: T): SizeInt;
    procedure Delete(AIndex: SizeInt); inline;
    procedure DeleteRange(AIndex, ACount: SizeInt);
    function ExtractIndex(const AIndex: SizeInt): T; overload;
    function Extract(constref AValue: T): T; overload;

    procedure Exchange(AIndex1, AIndex2: SizeInt); virtual;
    procedure Move(AIndex, ANewIndex: SizeInt); virtual;

    function First: T; inline;
    function Last: T; inline;

    procedure Clear;

    function Contains(constref AValue: T): Boolean; inline;
    function IndexOf(constref AValue: T): SizeInt; virtual;
    function LastIndexOf(constref AValue: T): SizeInt; virtual;

    procedure Reverse;

    procedure TrimExcess; override;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(constref AItem: T; out AIndex: SizeInt): Boolean; overload;
    function BinarySearch(constref AItem: T; out AIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;

    property Count: SizeInt read FLength write SetCount;
    property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  end;

  TCollectionSortStyle = (cssNone,cssUser,cssAuto);
  TCollectionSortStyles = Set of TCollectionSortStyle;

  TSortedList<T> = class(TList<T>)
  private
    FDuplicates: TDuplicates;
    FSortStyle: TCollectionSortStyle;
    function GetSorted: boolean;
    procedure SetSorted(AValue: boolean);
    procedure SetSortStyle(AValue: TCollectionSortStyle);
  protected
    procedure InitializeList; override;
  public
    function Add(constref AValue: T): SizeInt; override; overload;
    procedure AddRange(constref AValues: array of T); override; overload;
    procedure Insert(AIndex: SizeInt; constref AValue: T); override;
    procedure Exchange(AIndex1, AIndex2: SizeInt); override;
    procedure Move(AIndex, ANewIndex: SizeInt); override;
    procedure InsertRange(AIndex: SizeInt; constref AValues: array of T); override; overload;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property SortStyle: TCollectionSortStyle read FSortStyle write SetSortStyle;

    function ConsistencyCheck(ARaiseException: boolean = true): boolean; virtual;
  end;

  TThreadList<T> = class
  private
    FList: TList<T>;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(constref AValue: T);
    procedure Remove(constref AValue: T);
    procedure Clear;

    function LockList: TList<T>;
    procedure UnlockList; inline;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  TQueue<T> = class(TCustomList<T>)
  public type
    TPointersEnumerator = class(TCustomPointersEnumerator<T, PT>)
    protected
      FQueue: TQueue<T>;
      FIndex: SizeInt;
      function DoMoveNext: boolean; override;
      function DoGetCurrent: PT; override;
    public
      constructor Create(AQueue: TQueue<T>);
    end;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
  protected
    // bug #24287 - workaround for generics type name conflict (Identifier not found)
    // next bug workaround - for another error related to previous workaround
    // change order (function must be declared before TEnumerator declaration}
    function DoGetEnumerator: {Generics.Collections.}TEnumerator<T>; override;
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>)
      public
        constructor Create(AQueue: TQueue<T>);
      end;

    function GetEnumerator: TEnumerator; reintroduce;
  private
    FLow: SizeInt;
  protected
    procedure SetCapacity(AValue: SizeInt); override;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; override;
    function GetCount: SizeInt; override;
  public
    constructor Create(ACollection: TEnumerable<T>); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>); overload;
    {$ENDIF}
    destructor Destroy; override;
    procedure Enqueue(constref AValue: T);
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    procedure Clear;
    procedure TrimExcess; override;
  end;

  TStack<T> = class(TCustomListWithPointers<T>)
  protected
  // bug #24287 - workaround for generics type name conflict (Identifier not found)
  // next bug workaround - for another error related to previous workaround
  // change order (function must be declared before TEnumerator declaration}
    function DoGetEnumerator: {Generics.Collections.}TEnumerator<T>; override;
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>);

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; override;
    procedure SetCapacity(AValue: SizeInt); override;
  public
    constructor Create(ACollection: TEnumerable<T>); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>); overload;
    {$ENDIF}
    destructor Destroy; override;
    procedure Clear;
    procedure Push(constref AValue: T);
    function Pop: T; inline;
    function Peek: T;
    function Extract: T; inline;
    procedure TrimExcess; override;
  end;

  TObjectList<T: class> = class(TList<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>; AOwnsObjects: Boolean = True); overload;
    {$ENDIF}
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  TObjectQueue<T: class> = class(TQueue<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>; AOwnsObjects: Boolean = True); overload;
    {$ENDIF}
    procedure Dequeue;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  TObjectStack<T: class> = class(TStack<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>; AOwnsObjects: Boolean = True); overload;
    {$ENDIF}
    function Pop: T;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  PObject = ^TObject;

{$I inc\generics.dictionariesh.inc}

  { TCustomHashSet<T> }

  TCustomSet<T> = class(TEnumerableWithPointers<T>)
  protected
    FOnNotify: TCollectionNotifyEvent<T>;
  public type
    PT = ^T;
  protected type
    TCustomSetEnumerator = class(TEnumerator<T>)
    protected var
      FEnumerator: TEnumerator<T>;
      function DoMoveNext: boolean; override;
      function DoGetCurrent: T; override;
      function GetCurrent: T; virtual; abstract;
    public
      constructor Create(ASet: TCustomSet<T>); virtual; abstract;
      destructor Destroy; override;
    end;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    function GetCount: SizeInt; virtual; abstract;
    function GetCapacity: SizeInt; virtual; abstract;
    procedure SetCapacity(AValue: SizeInt); virtual; abstract;
    function GetOnNotify: TCollectionNotifyEvent<T>; virtual; abstract;
    procedure SetOnNotify(AValue: TCollectionNotifyEvent<T>); virtual; abstract;
  public
    constructor Create; virtual; abstract; overload;
    constructor Create(ACollection: TEnumerable<T>); overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    constructor Create(ACollection: TEnumerableWithPointers<T>); overload;
    {$ENDIF}
    function GetEnumerator: TCustomSetEnumerator; reintroduce; virtual; abstract;

    function Add(constref AValue: T): Boolean; virtual; abstract;
    function Remove(constref AValue: T): Boolean; virtual; abstract;
    function Extract(constref AValue: T): T; virtual; abstract;

    procedure Clear; virtual; abstract;
    function Contains(constref AValue: T): Boolean; virtual; abstract;
    function AddRange(constref AValues: array of T): Boolean; overload;
    function AddRange(const AEnumerable: IEnumerable<T>): Boolean; overload;
    function AddRange(AEnumerable: TEnumerable<T>): Boolean; overload;
    {$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
    function AddRange(AEnumerable: TEnumerableWithPointers<T>): Boolean; overload;
    {$ENDIF}
    procedure UnionWith(AHashSet: TCustomSet<T>);
    procedure IntersectWith(AHashSet: TCustomSet<T>);
    procedure ExceptWith(AHashSet: TCustomSet<T>);
    procedure SymmetricExceptWith(AHashSet: TCustomSet<T>);

    property Count: SizeInt read GetCount;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    procedure TrimExcess; virtual; abstract;

    property OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
  end;

  { THashSet<T> }

  THashSet<T> = class(TCustomSet<T>)
  private
    procedure InternalDictionaryNotify(ASender: TObject; constref AItem: T; AAction: TCollectionNotification);
  protected
    FInternalDictionary: TOpenAddressingLP<T, TEmptyRecord>;
  public type
    THashSetEnumerator = class(TCustomSetEnumerator)
    protected type
      TDictionaryEnumerator = TDictionary<T, TEmptyRecord>.TKeyEnumerator;
      function GetCurrent: T; override;
    public
      constructor Create(ASet: TCustomSet<T>); override;
    end;

    TPointersEnumerator = class(TCustomPointersEnumerator<T, PT>)
    protected
      FEnumerator: TEnumerator<PT>;
      function DoMoveNext: boolean; override;
      function DoGetCurrent: PT; override;
    public
      constructor Create(AHashSet: THashSet<T>);
    end;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
    function GetCount: SizeInt; override;
    function GetCapacity: SizeInt; override;
    procedure SetCapacity(AValue: SizeInt); override;
    function GetOnNotify: TCollectionNotifyEvent<T>; override;
    procedure SetOnNotify(AValue: TCollectionNotifyEvent<T>); override;
  public
    constructor Create; override; overload;
    constructor Create(const AComparer: IEqualityComparer<T>); virtual; overload;
    destructor Destroy; override;
    function GetEnumerator: TCustomSetEnumerator; override;

    function Add(constref AValue: T): Boolean; override;
    function Remove(constref AValue: T): Boolean; override;
    function Extract(constref AValue: T): T; override;

    procedure Clear; override;
    function Contains(constref AValue: T): Boolean; override;

    procedure TrimExcess; override;
  end;

  TPair<TKey, TValue, TInfo> = record
  public
    Key: TKey;
    Value: TValue;
    Info: TInfo;
  end;

  TAVLTreeNode<TREE_CONSTRAINTS, TTree> = record
  private type
    TNodePair = TPair<TREE_CONSTRAINTS>;
  public type
    PNode = ^TAVLTreeNode<TREE_CONSTRAINTS, TTree>;
  public
    Parent, Left, Right: PNode;
    Balance: Integer;
    Data: TNodePair;
    function Successor: PNode;
    function Precessor: PNode;
    function TreeDepth: integer;
    procedure ConsistencyCheck(ATree: TObject); // workaround for internal error 2012101001 (no generic forward declarations)
    function GetCount: SizeInt;
    property Key: TKey read Data.Key write Data.Key;
    property Value: TValue read Data.Value write Data.Value;
    property Info: TInfo read Data.Info write Data.Info;
  end;

  TCustomTreeEnumerator<T, PNode, TTree> = class abstract(TEnumerator<T>)
  protected
    FCurrent: PNode;
    FTree: TTree;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual; abstract;
  public
    constructor Create(ATree: TObject);
    property Current: T read GetCurrent;
  end;

  TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator,
    T, PT, PNode, TTree> = class abstract(TEnumerableWithPointers<T>)
  private
    FTree: TTree;
    function GetCount: SizeInt; inline;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
    function DoGetEnumerator: TTreeEnumerator; override;
  public
    constructor Create(ATree: TTree);
    function ToArray: TArray<T>; override; final;
    property Count: SizeInt read GetCount;
  end;

  TAVLTreeEnumerator<T, PNode, TTree> = class(TCustomTreeEnumerator<T, PNode, TTree>)
  protected
    FLowToHigh: boolean;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(ATree: TObject; ALowToHigh: boolean = true);
    property LowToHigh: boolean read FLowToHigh;
  end;

  TNodeNotifyEvent<PNode> = procedure(ASender: TObject; ANode: PNode; AAction: TCollectionNotification; ADispose: boolean) of object;

  TCustomAVLTreeMap<TREE_CONSTRAINTS> = class
  private type
    TTree = class(TCustomAVLTreeMap<TREE_CONSTRAINTS>);
  public type
    TNode = TAVLTreeNode<TREE_CONSTRAINTS, TTree>;
    PNode = ^TNode;
    PPNode = ^PNode;
    TTreePair = TPair<TKey, TValue>;
    PKey = ^TKey;
    PValue = ^TValue;
  private type
    // type exist only for generic constraint in TNodeCollection (non functional - PPNode has no sense)
    TPNodeEnumerator = class(TAVLTreeEnumerator<PPNode, PNode, TTree>);
  private var
    FDuplicates: TDuplicates;
    FComparer: IComparer<TKey>;
  protected
    FCount: SizeInt;
    FRoot: PNode;
    FKeys: TEnumerable<TKey>;
    FValues: TEnumerable<TValue>;
    FOnNodeNotify: TNodeNotifyEvent<PNode>;
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;

    procedure NodeAdded(ANode: PNode); virtual;
    procedure DeletingNode(ANode: PNode; AOrigin: boolean); virtual;

    function DoRemove(ANode: PNode; ACollectionNotification: TCollectionNotification; ADispose: boolean): TValue;
    procedure DisposeAllNodes(ANode: PNode); overload;

    function Compare(constref ALeft, ARight: TKey): Integer; inline;
    function FindPredecessor(ANode: PNode): PNode;
    function FindInsertNode(ANode: PNode; out AInsertNode: PNode): Integer;

    procedure RotateRightRight(ANode: PNode); virtual;
    procedure RotateLeftLeft(ANode: PNode); virtual;
    procedure RotateRightLeft(ANode: PNode); virtual;
    procedure RotateLeftRight(ANode: PNode); virtual;

    procedure KeyNotify(constref AKey: TKey; ACollectionNotification: TCollectionNotification); inline;
    procedure ValueNotify(constref AValue: TValue; ACollectionNotification: TCollectionNotification); inline;
    procedure NodeNotify(ANode: PNode; ACollectionNotification: TCollectionNotification; ADispose: boolean); inline;
    procedure SetValue(var AValue: TValue; constref ANewValue: TValue);
    function GetItem(const AKey: TKey): TValue;
    procedure SetItem(const AKey: TKey; const AValue: TValue);

    property Items[Index: TKey]: TValue read GetItem write SetItem;
    // for reporting
    procedure WriteStr(AStream: TStream; const AText: string);
  public type
    TPairEnumerator = class(TAVLTreeEnumerator<TTreePair, PNode, TTree>)
    protected
      function GetCurrent: TTreePair; override;
    end;

    TNodeEnumerator = class(TAVLTreeEnumerator<PNode, PNode, TTree>)
    protected
      function GetCurrent: PNode; override;
    end;

    TKeyEnumerator = class(TAVLTreeEnumerator<TKey, PNode, TTree>)
    protected
      function GetCurrent: TKey; override;
    end;

    TPKeyEnumerator = class(TAVLTreeEnumerator<PKey, PNode, TTree>)
    protected
      function GetCurrent: PKey; override;
    end;

    TValueEnumerator = class(TAVLTreeEnumerator<TValue, PNode, TTree>)
    protected
      function GetCurrent: TValue; override;
    end;

    TPValueEnumerator = class(TAVLTreeEnumerator<PValue, PNode, TTree>)
    protected
      function GetCurrent: PValue; override;
    end;

    TNodeCollection = class(TTreeEnumerable<TNodeEnumerator, TPNodeEnumerator, PNode, PPNode, PNode, TTree>)
    private
      property Ptr; // PPNode has no sense, so hide enumerator for PPNode
    end;

    TKeyCollection = class(TTreeEnumerable<TKeyEnumerator, TPKeyEnumerator, TKey, PKey, PNode, TTree>);

    TValueCollection = class(TTreeEnumerable<TValueEnumerator, TPValueEnumerator, TValue, PValue, PNode, TTree>);
  private
    FNodes: TNodeCollection;
    function GetNodeCollection: TNodeCollection;
    procedure InternalAdd(ANode, AParent: PNode); overload;
    function InternalAdd(ANode: PNode; ADispisable: boolean): PNode; overload;
    procedure InternalDelete(ANode: PNode);
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
  public
    constructor Create; virtual; overload;
    constructor Create(const AComparer: IComparer<TKey>); virtual; overload;

    function NewNode: PNode;
    function NewNodeArray(ACount: SizeInt): PNode; overload;
    procedure NewNodeArray(out AArray: TArray<PNode>; ACount: SizeInt); overload;
    procedure DisposeNode(ANode: PNode);
    procedure DisposeNodeArray(ANode: PNode; ACount: SizeInt); overload;
    procedure DisposeNodeArray(var AArray: TArray<PNode>); overload;

    destructor Destroy; override;
    function AddNode(ANode: PNode): boolean; overload; inline;
    function Add(constref APair: TTreePair): PNode; overload; inline;
    function Add(constref AKey: TKey; constref AValue: TValue): PNode; overload; inline;
    function Remove(constref AKey: TKey; ADisposeNode: boolean = true): boolean;
    function ExtractPair(constref AKey: TKey; ADisposeNode: boolean = true): TTreePair; overload;
    function ExtractPair(constref ANode: PNode; ADispose: boolean = true): TTreePair; overload;
    function Extract(constref AKey: TKey; ADisposeNode: boolean): PNode;
    function ExtractNode(ANode: PNode; ADispose: boolean): PNode;
    procedure Delete(ANode: PNode; ADispose: boolean = true); inline;

    function GetEnumerator: TPairEnumerator;
    property Nodes: TNodeCollection read GetNodeCollection;

    procedure Clear(ADisposeNodes: Boolean = true); virtual;

    function FindLowest: PNode;
    function FindHighest: PNode;

    property Count: SizeInt read FCount;

    property Root: PNode read FRoot;
    function Find(constref AKey: TKey): PNode;
    function ContainsKey(constref AKey: TKey; out ANode: PNode): boolean; overload; inline;
    function ContainsKey(constref AKey: TKey): boolean; overload; inline;

    procedure ConsistencyCheck; virtual;
    procedure WriteTreeNode(AStream: TStream; ANode: PNode);
    procedure WriteReportToStream(AStream: TStream);
    function NodeToReportStr(ANode: PNode): string; virtual;
    function ReportAsString: string;

    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;

    property OnNodeNotify: TNodeNotifyEvent<PNode> read FOnNodeNotify write FOnNodeNotify;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;

  TAVLTreeMap<TKey, TValue> = class(TCustomAVLTreeMap<TKey, TValue, TEmptyRecord>)
  public
    property Items; default;
  end;

  TIndexedAVLTreeMap<TKey, TValue> = class(TCustomAVLTreeMap<TKey, TValue, SizeInt>)
  protected
    FLastNode: PNode;
    FLastIndex: SizeInt;

    procedure RotateRightRight(ANode: PNode); override;
    procedure RotateLeftLeft(ANode: PNode); override;
    procedure RotateRightLeft(ANode: PNode); override;
    procedure RotateLeftRight(ANode: PNode); override;

    procedure NodeAdded(ANode: PNode); override;
    procedure DeletingNode(ANode: PNode; AOrigin: boolean); override;
  public
    function GetNodeAtIndex(AIndex: SizeInt): PNode;
    function NodeToIndex(ANode: PNode): SizeInt;

    procedure ConsistencyCheck; override;
    function NodeToReportStr(ANode: PNode): string; override;
  end;

  TAVLTree<T> = class(TAVLTreeMap<T, TEmptyRecord>)
  protected
    property OnKeyNotify;
    property OnValueNotify;
    property Items;
  public type
    TItemEnumerator = TKeyEnumerator;
  public
    function Add(constref AValue: T): PNode; reintroduce; inline;
    function AddNode(ANode: PNode): boolean; reintroduce; inline;

    property OnNotify: TCollectionNotifyEvent<T> read FOnKeyNotify write FOnKeyNotify;
  end;

  TIndexedAVLTree<T> = class(TIndexedAVLTreeMap<T, TEmptyRecord>)
  protected
    property OnKeyNotify;
    property OnValueNotify;
  public type
    TItemEnumerator = TKeyEnumerator;
  public
    function Add(constref AValue: T): PNode; reintroduce; inline;
    function AddNode(ANode: PNode): boolean; reintroduce; inline;

    property OnNotify: TCollectionNotifyEvent<T> read FOnKeyNotify write FOnKeyNotify;
  end;

  TSortedSet<T> = class(TCustomSet<T>)
  private
    procedure InternalAVLTreeNotify(ASender: TObject; constref AItem: T; AAction: TCollectionNotification);
  protected
    FInternalTree: TAVLTree<T>;
  public type
    TSortedSetEnumerator = class(TCustomSetEnumerator)
    protected type
      TTreeEnumerator = TAVLTree<T>.TItemEnumerator;
      function GetCurrent: T; override;
    public
      constructor Create(ASet: TCustomSet<T>); override;
    end;

    TPointersEnumerator = class(TCustomPointersEnumerator<T, PT>)
    protected
      FEnumerator: TEnumerator<PT>;
      function DoMoveNext: boolean; override;
      function DoGetCurrent: PT; override;
    public
      constructor Create(ASortedSet: TSortedSet<T>);
    end;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
    function GetCount: SizeInt; override;
    function GetCapacity: SizeInt; override;
    procedure SetCapacity(AValue: SizeInt); override;
    function GetOnNotify: TCollectionNotifyEvent<T>; override;
    procedure SetOnNotify(AValue: TCollectionNotifyEvent<T>); override;
  public
    constructor Create; override; overload;
    constructor Create(const AComparer: IComparer<T>); virtual; overload;
    destructor Destroy; override;
    function GetEnumerator: TCustomSetEnumerator; override;

    function Add(constref AValue: T): Boolean; override;
    function Remove(constref AValue: T): Boolean; override;
    function Extract(constref AValue: T): T; override;
    procedure Clear; override;
    function Contains(constref AValue: T): Boolean; override;

    procedure TrimExcess; override;
  end;

  TSortedHashSet<T> = class(TCustomSet<T>)
  private
    procedure InternalDictionaryNotify(ASender: TObject; constref AItem: PT; AAction: TCollectionNotification);
  protected
    FInternalDictionary: TOpenAddressingLP<PT, TEmptyRecord>;
    FInternalTree: TAVLTree<T>;
    function DoGetEnumerator: TEnumerator<T>; override;
    function GetCount: SizeInt; override;
    function GetCapacity: SizeInt; override;
    procedure SetCapacity(AValue: SizeInt); override;
    function GetOnNotify: TCollectionNotifyEvent<T>; override;
    procedure SetOnNotify(AValue: TCollectionNotifyEvent<T>); override;
  protected type
    TSortedHashSetEqualityComparer = class(TInterfacedObject, IEqualityComparer<PT>)
    private
      FComparer: IComparer<T>;
      FEqualityComparer: IEqualityComparer<T>;
      function Equals(constref ALeft, ARight: PT): Boolean;
      function GetHashCode(constref AValue: PT): UInt32;
    public
      constructor Create(const AComparer: IComparer<T>); overload;
      constructor Create(const AEqualityComparer: IEqualityComparer<T>); overload;
      constructor Create(const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>); overload;
    end;
  public type
    TSortedHashSetEnumerator = class(TCustomSetEnumerator)
    protected type
      TTreeEnumerator = TAVLTree<T>.TItemEnumerator;
      function GetCurrent: T; override;
    public
      constructor Create(ASet: TCustomSet<T>); override;
    end;

    TPointersEnumerator = class(TCustomPointersEnumerator<T, PT>)
    protected
      FEnumerator: TEnumerator<PT>;
      function DoMoveNext: boolean; override;
      function DoGetCurrent: PT; override;
    public
      constructor Create(ASortedHashSet: TSortedHashSet<T>);
    end;
  protected
    function GetPtrEnumerator: TEnumerator<PT>; override;
  public
    constructor Create; override; overload;
    constructor Create(const AComparer: IEqualityComparer<T>); overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;
    function GetEnumerator: TCustomSetEnumerator; override;

    function Add(constref AValue: T): Boolean; override;
    function Remove(constref AValue: T): Boolean; override;
    function Extract(constref AValue: T): T; override;
    procedure Clear; override;
    function Contains(constref AValue: T): Boolean; override;

    procedure TrimExcess; override;
  end;

function InCircularRange(ABottom, AItem, ATop: SizeInt): Boolean;

var
  EmptyRecord: TEmptyRecord;

implementation

function InCircularRange(ABottom, AItem, ATop: SizeInt): Boolean;
begin
  Result :=
       (ABottom < AItem) and (AItem <= ATop )
    or (ATop < ABottom) and (AItem > ABottom)
    or (ATop < ABottom ) and (AItem <= ATop );
end;

{ TCustomArrayHelper<T> }

class function TCustomArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out AFoundIndex: SizeInt; const AComparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, AComparer, Low(AValues), Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out AFoundIndex: SizeInt): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, TComparerBugHack.Default, Low(AValues), Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch(AValues, AItem, ASearchResult, AComparer, Low(AValues), Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out ASearchResult: TBinarySearchResult): Boolean;
begin
  Result := BinarySearch(AValues, AItem, ASearchResult, TComparerBugHack.Default, Low(AValues), Length(AValues));
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T);
begin
  QuickSort(AValues, Low(AValues), High(AValues), TComparerBugHack.Default);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>);
begin
  QuickSort(AValues, Low(AValues), High(AValues), AComparer);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>; AIndex, ACount: SizeInt);
begin
  if ACount <= 1 then
    Exit;
  QuickSort(AValues, AIndex, Pred(AIndex + ACount), AComparer);
end;

{ TArrayHelper<T> }

class procedure TArrayHelper<T>.QuickSort(var AValues: array of T; ALeft, ARight: SizeInt;
  const AComparer: IComparer<T>);
var
  I, J: SizeInt;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
        while AComparer.Compare(AValues[I], P) < 0 do
          Inc(I);
        while AComparer.Compare(AValues[J], P) > 0 do
          Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
   until ALeft >= ARight;
end;

class function TArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
  AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid: Int32;
begin
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) shr 1);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

        ASearchResult.CompareResult := AComparer.Compare(AValues[imid], AItem);
        // reduce the search
        if (ASearchResult.CompareResult < 0) then
          imin := imid + 1
        else
        begin
          imax := imid;
          if ASearchResult.CompareResult = 0 then
          begin
            ASearchResult.FoundIndex := imid;
            ASearchResult.CandidateIndex := imid;
            Exit(True);
          end;
        end;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

  if (imax = imin) then
  begin
    ASearchResult.CompareResult := AComparer.Compare(AValues[imin], AItem);
    ASearchResult.CandidateIndex := imin;
    if (ASearchResult.CompareResult = 0) then
    begin
      ASearchResult.FoundIndex := imin;
      Exit(True);
    end else
    begin
      ASearchResult.FoundIndex := -1;
      Exit(False);
    end;
  end
  else
  begin
    ASearchResult.CompareResult := 0;
    ASearchResult.FoundIndex := -1;
    ASearchResult.CandidateIndex := -1;
    Exit(False);
  end;
end;

class function TArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
  AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid: Int32;
  LCompare: SizeInt;
begin
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) shr 1);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

        LCompare := AComparer.Compare(AValues[imid], AItem);
        // reduce the search
        if (LCompare < 0) then
          imin := imid + 1
        else
        begin
          imax := imid;
          if LCompare = 0 then
          begin
            AFoundIndex := imid;
            Exit(True);
          end;
        end;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

  LCompare := AComparer.Compare(AValues[imin], AItem);
  if (imax = imin) and (LCompare = 0) then
  begin
    AFoundIndex := imin;
    Exit(True);
  end
  else
  begin
    AFoundIndex := -1;
    Exit(False);
  end;
end;

{ TEnumerator<T> }

function TEnumerator<T>.MoveNext: boolean;
begin
  Exit(DoMoveNext);
end;

{ TEnumerable<T> }

function TEnumerable<T>.ToArrayImpl(ACount: SizeInt): TArray<T>;
var
  i: SizeInt;
  LEnumerator: TEnumerator<T>;
begin
  Result := nil;
  SetLength(Result, ACount);

  try
    LEnumerator := GetEnumerator;

    i := 0;
    while LEnumerator.MoveNext do
    begin
      Result[i] := LEnumerator.Current;
      Inc(i);
    end;
  finally
    LEnumerator.Free;
  end;
end;

function TEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Exit(DoGetEnumerator);
end;

function TEnumerable<T>.ToArray: TArray<T>;
var
  LEnumerator: TEnumerator<T>;
  LBuffer: TList<T>;
begin
  LBuffer := TList<T>.Create;
  try
    LEnumerator := GetEnumerator;

    while LEnumerator.MoveNext do
      LBuffer.Add(LEnumerator.Current);

    Result := LBuffer.ToArray;
  finally
    LBuffer.Free;
    LEnumerator.Free;
  end;
end;

{ TCustomPointersCollection<T, PT> }

function TCustomPointersCollection<T, PT>.Enumerable: TLocalEnumerable;
begin
  Result := TLocalEnumerable(@Self);
end;

function TCustomPointersCollection<T, PT>.GetEnumerator: TEnumerator<PT>;
begin
  Result := Enumerable.GetPtrEnumerator;
end;

{ TEnumerableWithPointers<T> }

function TEnumerableWithPointers<T>.GetPtr: PPointersCollection;
begin
  Result := PPointersCollection(Self);
end;

{ TCustomList<T> }

function TCustomList<T>.PrepareAddingItem: SizeInt;
begin
  Result := Length(FItems);

  if (FLength < 4) and (Result < 4) then
    SetLength(FItems, 4)
  else if FLength = High(FLength) then
    OutOfMemoryError
  else if FLength = Result then
    SetLength(FItems, CUSTOM_LIST_CAPACITY_INC);

  Result := FLength;
  Inc(FLength);
end;

function TCustomList<T>.PrepareAddingRange(ACount: SizeInt): SizeInt;
begin
  if ACount < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount = 0 then
    Exit(FLength - 1);

  if (FLength = 0) and (Length(FItems) = 0) then
    SetLength(FItems, 4)
  else if FLength = High(FLength) then
    OutOfMemoryError;

  Result := Length(FItems);
  while Pred(FLength + ACount) >= Result do
  begin
    SetLength(FItems, CUSTOM_LIST_CAPACITY_INC);
    Result := Length(FItems);
  end;

  Result := FLength;
  Inc(FLength, ACount);
end;

function TCustomList<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

function TCustomList<T>.GetCount: SizeInt;
begin
  Result := FLength;
end;

procedure TCustomList<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, AValue, ACollectionNotification);
end;

function TCustomList<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;
begin
  if (AIndex < 0) or (AIndex >= FLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  Dec(FLength);

  FItems[AIndex] := Default(T);
  if AIndex <> FLength then
  begin
    System.Move(FItems[AIndex + 1], FItems[AIndex], (FLength - AIndex) * SizeOf(T));
    FillChar(FItems[FLength], SizeOf(T), 0);
  end;

  Notify(Result, ACollectionNotification);
end;

function TCustomList<T>.GetCapacity: SizeInt;
begin
  Result := Length(FItems);
end;

{ TCustomListEnumerator<T> }

function TCustomListEnumerator<T>.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FList.FLength <> 0) and (FIndex < FList.FLength)
end;

function TCustomListEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TCustomListEnumerator<T>.GetCurrent: T;
begin
  Result := FList.FItems[FIndex];
end;

constructor TCustomListEnumerator<T>.Create(AList: TCustomList<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

{ TCustomListWithPointers<T>.TPointersEnumerator }

function TCustomListWithPointers<T>.TPointersEnumerator.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FList.FLength <> 0) and (FIndex < FList.FLength)
end;

function TCustomListWithPointers<T>.TPointersEnumerator.DoGetCurrent: PT;
begin
  Result := @FList.FItems[FIndex];;
end;

constructor TCustomListWithPointers<T>.TPointersEnumerator.Create(AList: TCustomListWithPointers<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

{ TCustomListWithPointers<T> }

function TCustomListWithPointers<T>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TPointersEnumerator.Create(Self);
end;

{ TList<T> }

procedure TList<T>.InitializeList;
begin
end;

constructor TList<T>.Create;
begin
  InitializeList;
  FComparer := TComparer<T>.Default;
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  InitializeList;
  FComparer := AComparer;
end;

constructor TList<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  Create;
  for LItem in ACollection do
    Add(LItem);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TList<T>.Create(ACollection: TEnumerableWithPointers<T>);
var
  LItem: PT;
begin
  Create;
  for LItem in ACollection.Ptr^ do
    Add(LItem^);
end;
{$ENDIF}

destructor TList<T>.Destroy;
begin
  SetCapacity(0);
end;

procedure TList<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    Count := AValue;

  SetLength(FItems, AValue);
end;

procedure TList<T>.SetCount(AValue: SizeInt);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AValue > Capacity then
    Capacity := AValue;
  if AValue < Count then
    DeleteRange(AValue, Count - AValue);

  FLength := AValue;
end;

function TList<T>.GetItem(AIndex: SizeInt): T;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
end;

procedure TList<T>.SetItem(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);   
  Notify(FItems[AIndex], cnRemoved);
  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.DoGetEnumerator: {Generics.Collections.}TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.Add(constref AValue: T): SizeInt;
begin
  Result := PrepareAddingItem;
  FItems[Result] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.AddRange(constref AValues: array of T);
begin
  InsertRange(Count, AValues);
end;

procedure TList<T>.AddRange(const AEnumerable: IEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.AddRange(AEnumerable: TEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
procedure TList<T>.AddRange(AEnumerable: TEnumerableWithPointers<T>);
var
  LValue: PT;
begin
  for LValue in AEnumerable.Ptr^ do
    Add(LValue^);
end;
{$ENDIF}

procedure TList<T>.InternalInsert(AIndex: SizeInt; constref AValue: T);
begin
  if AIndex <> PrepareAddingItem then
  begin
    System.Move(FItems[AIndex], FItems[AIndex + 1], ((Count - AIndex) - 1) * SizeOf(T));
    FillChar(FItems[AIndex], SizeOf(T), 0);
  end;

  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.Insert(AIndex: SizeInt; constref AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  InternalInsert(AIndex, AValue);
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; constref AValues: array of T);
var
  i: SizeInt;
  LLength: SizeInt;
  LValue: ^T;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LLength := Length(AValues);
  if LLength = 0 then
    Exit;

  if AIndex <> PrepareAddingRange(LLength) then
  begin
    System.Move(FItems[AIndex], FItems[AIndex + LLength], ((Count - AIndex) - LLength) * SizeOf(T));
    FillChar(FItems[AIndex], SizeOf(T) * LLength, 0);
  end;

  LValue := @AValues[0];
  for i := AIndex to Pred(AIndex + LLength) do
  begin
    FItems[i] := LValue^;
    Notify(LValue^, cnAdded);
    Inc(LValue);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>);
var
  LValue: T;
  i: SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    InternalInsert(Aindex + i, LValue);
    Inc(i);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>);
var
  LValue: T;
  i:  SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    InternalInsert(Aindex + i, LValue);
    Inc(i);
  end;
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerableWithPointers<T>);
var
  LValue: PT;
  i:  SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable.Ptr^ do
  begin
    InternalInsert(Aindex + i, LValue^);
    Inc(i);
  end;
end;
{$ENDIF}

function TList<T>.Remove(constref AValue: T): SizeInt;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    DoRemove(Result, cnRemoved);
end;

procedure TList<T>.Delete(AIndex: SizeInt);
begin
  DoRemove(AIndex, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: SizeInt);
var
  LDeleted: array of T;
  i: SizeInt;
  LMoveDelta: SizeInt;
begin
  if ACount = 0 then
    Exit;

  if (ACount < 0) or (AIndex < 0) or (AIndex + ACount > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LDeleted := nil;
  SetLength(LDeleted, ACount);
  System.Move(FItems[AIndex], LDeleted[0], ACount * SizeOf(T));

  LMoveDelta := Count - (AIndex + ACount);

  if LMoveDelta = 0 then
    FillChar(FItems[AIndex], ACount * SizeOf(T), #0)
  else
  begin
    System.Move(FItems[AIndex + ACount], FItems[AIndex], LMoveDelta * SizeOf(T));
    FillChar(FItems[Count - ACount], ACount * SizeOf(T), #0);
  end;

  Dec(FLength, ACount);

  for i := 0 to High(LDeleted) do
    Notify(LDeleted[i], cnRemoved);
end;

function TList<T>.ExtractIndex(const AIndex: SizeInt): T;
begin
  Result := DoRemove(AIndex, cnExtracted);
end;

function TList<T>.Extract(constref AValue: T): T;
var
  LIndex: SizeInt;
begin
  LIndex := IndexOf(AValue);
  if LIndex < 0 then
    Exit(Default(T));

  Result := DoRemove(LIndex, cnExtracted);
end;

procedure TList<T>.Exchange(AIndex1, AIndex2: SizeInt);
var
  LTemp: T;
begin
  LTemp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := LTemp;
end;

procedure TList<T>.Move(AIndex, ANewIndex: SizeInt);
var
  LTemp: T;
begin
  if ANewIndex = AIndex then
    Exit;

  if (ANewIndex < 0) or (ANewIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LTemp := FItems[AIndex];
  FItems[AIndex] := Default(T);

  if AIndex < ANewIndex then
    System.Move(FItems[Succ(AIndex)], FItems[AIndex], (ANewIndex - AIndex) * SizeOf(T))
  else
    System.Move(FItems[ANewIndex], FItems[Succ(ANewIndex)], (AIndex - ANewIndex) * SizeOf(T));

  FillChar(FItems[ANewIndex], SizeOf(T), #0);
  FItems[ANewIndex] := LTemp;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Pred(Count)];
end;

procedure TList<T>.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TList<T>.Contains(constref AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TList<T>.IndexOf(constref AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := 0 to Count - 1 do
    if FComparer.Compare(AValue, FItems[i]) = 0 then
      Exit(i);
  Result := -1;
end;

function TList<T>.LastIndexOf(constref AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := Count - 1 downto 0 do
    if FComparer.Compare(AValue, FItems[i]) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TList<T>.Reverse;
var
  a, b: SizeInt;
  LTemp: T;
begin
  a := 0;
  b := Count - 1;
  while a < b do
  begin
    LTemp := FItems[a];
    FItems[a] := FItems[b];
    FItems[b] := LTemp;
    Inc(a);
    Dec(b);
  end;
end;

procedure TList<T>.Sort;
begin
  TArrayHelperBugHack.Sort(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TArrayHelperBugHack.Sort(FItems, AComparer, 0, Count);
end;

function TList<T>.BinarySearch(constref AItem: T; out AIndex: SizeInt): Boolean;
begin
  Result := TArrayHelperBugHack.BinarySearch(FItems, AItem, AIndex, FComparer, 0, Count);
end;

function TList<T>.BinarySearch(constref AItem: T; out AIndex: SizeInt; const AComparer: IComparer<T>): Boolean;
begin
  Result := TArrayHelperBugHack.BinarySearch(FItems, AItem, AIndex, AComparer, 0, Count);
end;

{ TSortedList<T> }

procedure TSortedList<T>.InitializeList;
begin
  FSortStyle := cssAuto;
end;

function TSortedList<T>.Add(constref AValue: T): SizeInt;
var
  LSearchResult: TBinarySearchResult;
begin
  if SortStyle <> cssAuto then
    Exit(inherited Add(AValue));
  if TArrayHelperBugHack.BinarySearch(FItems, AValue, LSearchResult, FComparer, 0, Count) then
  case FDuplicates of
    dupAccept: Result := LSearchResult.FoundIndex;
    dupIgnore: Exit(LSearchResult.FoundIndex);
    dupError: raise EListError.Create(SCollectionDuplicate);
  end
  else
  begin
    if LSearchResult.CandidateIndex = -1 then
      Result := 0
    else
      if LSearchResult.CompareResult > 0 then
        Result := LSearchResult.CandidateIndex
      else
        Result := LSearchResult.CandidateIndex + 1;
  end;

  InternalInsert(Result, AValue);
end;

procedure TSortedList<T>.Insert(AIndex: SizeInt; constref AValue: T);
begin
  if FSortStyle = cssAuto then
    raise EListError.Create(SSortedListError)
  else
    inherited;
end;

procedure TSortedList<T>.Exchange(AIndex1, AIndex2: SizeInt);
begin
  if FSortStyle = cssAuto then
    raise EListError.Create(SSortedListError)
  else
    inherited;
end;

procedure TSortedList<T>.Move(AIndex, ANewIndex: SizeInt);
begin
  if FSortStyle = cssAuto then
    raise EListError.Create(SSortedListError)
  else
    inherited;
end;

procedure TSortedList<T>.AddRange(constref AValues: array of T);
var
  i: T;
begin
  for i in AValues do
    Add(i);
end;

procedure TSortedList<T>.InsertRange(AIndex: SizeInt; constref AValues: array of T);
var
  LValue: T;
  i:  SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AValues do
  begin
    InternalInsert(AIndex + i, LValue);
    Inc(i);
  end;
end;

function TSortedList<T>.GetSorted: boolean;
begin
  Result := FSortStyle in [cssAuto, cssUser];
end;

procedure TSortedList<T>.SetSorted(AValue: boolean);
begin
  if AValue then
    SortStyle := cssAuto
  else
    SortStyle := cssNone;
end;

procedure TSortedList<T>.SetSortStyle(AValue: TCollectionSortStyle);
begin
  if FSortStyle = AValue then
    Exit;
  if AValue = cssAuto then
    Sort;
  FSortStyle := AValue;
end;

function TSortedList<T>.ConsistencyCheck(ARaiseException: boolean = true): boolean;
var
  i: Integer;
  LCompare: SizeInt;
begin
  if Sorted then
  for i := 0 to Count-2 do
  begin
    LCompare := FComparer.Compare(FItems[i], FItems[i+1]);
    if LCompare = 0 then
    begin
      if Duplicates <> dupAccept then
        if ARaiseException then
          raise EListError.Create(SCollectionDuplicate)
        else
          Exit(False)
    end
    else
      if LCompare > 0 then
        if ARaiseException then
          raise EListError.Create(SCollectionInconsistency)
        else
          Exit(False)
  end;
  Result := True;
end;

{ TThreadList<T> }

constructor TThreadList<T>.Create;
begin
  inherited Create;
  FDuplicates:=dupIgnore;
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitCriticalSection(FLock);
{$endif}
  FList := TList<T>.Create;
end;

destructor TThreadList<T>.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
{$ifdef FPC_HAS_FEATURE_THREADING}
    DoneCriticalSection(FLock);
{$endif}
  end;
end;

procedure TThreadList<T>.Add(constref AValue: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(AValue) = -1) then
      FList.Add(AValue)
    else if Duplicates = dupError then
      raise EArgumentException.CreateRes(@SDuplicatesNotAllowed);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Remove(constref AValue: T);
begin
  LockList;
  try
    FList.Remove(AValue);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadList<T>.LockList: TList<T>;
begin
  Result:=FList;
{$ifdef FPC_HAS_FEATURE_THREADING}
  System.EnterCriticalSection(FLock);
{$endif}
end;

procedure TThreadList<T>.UnlockList;
begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  System.LeaveCriticalSection(FLock);
{$endif}
end;

{ TQueue<T>.TPointersEnumerator }

function TQueue<T>.TPointersEnumerator.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FQueue.FLength <> 0) and (FIndex < FQueue.FLength)
end;

function TQueue<T>.TPointersEnumerator.DoGetCurrent: PT;
begin
  Result := @FQueue.FItems[FIndex];
end;

constructor TQueue<T>.TPointersEnumerator.Create(AQueue: TQueue<T>);
begin
  inherited Create;
  FIndex := Pred(AQueue.FLow);
  FQueue := AQueue;
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(AQueue: TQueue<T>);
begin
  inherited Create(AQueue);

  FIndex := Pred(AQueue.FLow);
end;

{ TQueue<T> }

function TQueue<T>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TPointersenumerator.Create(Self);
end;

function TQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TQueue<T>.DoGetEnumerator: {Generics.Collections.}TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TQueue<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;
begin
  if Count = 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  Inc(FLow);
  if FLow = FLength then
  begin
    FLow := 0;
    FLength := 0;
  end;
  Notify(Result, ACollectionNotification);
end;

procedure TQueue<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AValue = FLength then
    Exit;

  if (Count > 0) and (FLow > 0) then
  begin
    Move(FItems[FLow], FItems[0], Count * SizeOf(T));
    FillChar(FItems[Count], (FLength - Count) * SizeOf(T), #0);
  end;

  SetLength(FItems, AValue);
  FLength := Count;
  FLow := 0;
end;

function TQueue<T>.GetCount: SizeInt;
begin
  Result := FLength - FLow;
end;

constructor TQueue<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  for LItem in ACollection do
    Enqueue(LItem);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TQueue<T>.Create(ACollection: TEnumerableWithPointers<T>);
var
  LItem: PT;
begin
  for LItem in ACollection.Ptr^ do
    Enqueue(LItem^);
end;
{$ENDIF}

destructor TQueue<T>.Destroy;
begin
  Clear;
end;

procedure TQueue<T>.Enqueue(constref AValue: T);
var
  LIndex: SizeInt;
begin
  LIndex := PrepareAddingItem;
  FItems[LIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DoRemove(FLow, cnRemoved);
end;

function TQueue<T>.Extract: T;
begin
  Result := DoRemove(FLow, cnExtracted);
end;

function TQueue<T>.Peek: T;
begin
  if (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[FLow];
end;

procedure TQueue<T>.Clear;
begin
  while Count <> 0 do
    Dequeue;
  FLow := 0;
  FLength := 0;
end;

procedure TQueue<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TStack<T> }

function TStack<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TStack<T>.DoGetEnumerator: {Generics.Collections.}TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

constructor TStack<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  for LItem in ACollection do
    Push(LItem);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TStack<T>.Create(ACollection: TEnumerableWithPointers<T>);
var
  LItem: PT;
begin
  for LItem in ACollection.Ptr^ do
    Push(LItem^);
end;
{$ENDIF}

function TStack<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;
begin
  if (AIndex < 0) or (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  Dec(FLength);
  Notify(Result, ACollectionNotification);
end;

destructor TStack<T>.Destroy;
begin
  Clear;
end;

procedure TStack<T>.Clear;
begin
  while Count <> 0 do
    Pop;
end;

procedure TStack<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    AValue := Count;

  SetLength(FItems, AValue);
end;

procedure TStack<T>.Push(constref AValue: T);
var
  LIndex: SizeInt;
begin
  LIndex := PrepareAddingItem;
  FItems[LIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TStack<T>.Pop: T;
begin
  Result := DoRemove(FLength - 1, cnRemoved);
end;

function TStack<T>.Peek: T;
begin
  if (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[FLength - 1];
end;

function TStack<T>.Extract: T;
begin
  Result := DoRemove(FLength - 1, cnExtracted);
end;

procedure TStack<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TObjectList<T> }

procedure TObjectList<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);

  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    TObject(AValue).Free;
end;

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TObjectList<T>.Create(ACollection: TEnumerableWithPointers<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;
{$ENDIF}

{ TObjectQueue<T> }

procedure TObjectQueue<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    TObject(AValue).Free;
end;

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectQueue<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TObjectQueue<T>.Create(ACollection: TEnumerableWithPointers<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;
{$ENDIF}

procedure TObjectQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

{ TObjectStack<T> }

procedure TObjectStack<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    TObject(AValue).Free;
end;

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectStack<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TObjectStack<T>.Create(ACollection: TEnumerableWithPointers<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;
{$ENDIF}

function TObjectStack<T>.Pop: T;
begin
  Result := inherited Pop;
end;

{$I inc\generics.dictionaries.inc}

{ TCustomSet<T>.TCustomSetEnumerator }

function TCustomSet<T>.TCustomSetEnumerator.DoMoveNext: boolean;
begin
  Result := FEnumerator.DoMoveNext;
end;

function TCustomSet<T>.TCustomSetEnumerator.DoGetCurrent: T;
begin
  Result := FEnumerator.DoGetCurrent;
end;

destructor TCustomSet<T>.TCustomSetEnumerator.Destroy;
begin
  FEnumerator.Free;
end;

{ TCustomSet<T> }

function TCustomSet<T>.DoGetEnumerator: Generics.Collections.TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

constructor TCustomSet<T>.Create(ACollection: TEnumerable<T>);
var
  i: T;
begin
  Create;
  for i in ACollection do
    Add(i);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TCustomSet<T>.Create(ACollection: TEnumerableWithPointers<T>);
var
  i: PT;
begin
  Create;
  for i in ACollection.Ptr^ do
    Add(i^);
end;
{$ENDIF}

function TCustomSet<T>.AddRange(constref AValues: array of T): Boolean;
var
  i: T;
begin
  Result := True;
  for i in AValues do
    Result := Add(i) and Result;
end;

function TCustomSet<T>.AddRange(const AEnumerable: IEnumerable<T>): Boolean;
var
  i: T;
begin
  Result := True;
  for i in AEnumerable do
    Result := Add(i) and Result;
end;

function TCustomSet<T>.AddRange(AEnumerable: TEnumerable<T>): Boolean;
var
  i: T;
begin
  Result := True;
  for i in AEnumerable do
    Result := Add(i) and Result;
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
function TCustomSet<T>.AddRange(AEnumerable: TEnumerableWithPointers<T>): Boolean;
var
  i: PT;
begin
  Result := True;
  for i in AEnumerable.Ptr^ do
    Result := Add(i^) and Result;
end;
{$ENDIF}

procedure TCustomSet<T>.UnionWith(AHashSet: TCustomSet<T>);
var
  i: PT;
begin
  for i in AHashSet.Ptr^ do
    Add(i^);
end;

procedure TCustomSet<T>.IntersectWith(AHashSet: TCustomSet<T>);
var
  LList: TList<PT>;
  i: PT;
begin
  LList := TList<PT>.Create;

  for i in Ptr^ do
    if not AHashSet.Contains(i^) then
      LList.Add(i);

  for i in LList do
    Remove(i^);

  LList.Free;
end;

procedure TCustomSet<T>.ExceptWith(AHashSet: TCustomSet<T>);
var
  i: PT;
begin
  for i in AHashSet.Ptr^ do
    Remove(i^);
end;

procedure TCustomSet<T>.SymmetricExceptWith(AHashSet: TCustomSet<T>);
var
  LList: TList<PT>;
  i: PT;
begin
  LList := TList<PT>.Create;

  for i in AHashSet.Ptr^ do
    if Contains(i^) then
      LList.Add(i)
    else
      Add(i^);

  for i in LList do
    Remove(i^);

  LList.Free;
end;

{ THashSet<T>.THashSetEnumerator }

function THashSet<T>.THashSetEnumerator.GetCurrent: T;
begin
  Result := TDictionaryEnumerator(FEnumerator).GetCurrent;
end;

constructor THashSet<T>.THashSetEnumerator.Create(ASet: TCustomSet<T>);
begin
  TDictionaryEnumerator(FEnumerator) := THashSet<T>(ASet).FInternalDictionary.Keys.DoGetEnumerator;
end;

{ THashSet<T>.TPointersEnumerator }

function THashSet<T>.TPointersEnumerator.DoMoveNext: boolean;
begin
  Result := FEnumerator.MoveNext;
end;

function THashSet<T>.TPointersEnumerator.DoGetCurrent: PT;
begin
  Result := FEnumerator.Current;
end;

constructor THashSet<T>.TPointersEnumerator.Create(AHashSet: THashSet<T>);
begin
  FEnumerator := AHashSet.FInternalDictionary.Keys.Ptr^.GetEnumerator;
end;

{ THashSet<T> }

procedure THashSet<T>.InternalDictionaryNotify(ASender: TObject; constref AItem: T; AAction: TCollectionNotification);
begin
  FOnNotify(Self, AItem, AAction);
end;

function THashSet<T>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TPointersEnumerator.Create(Self);
end;

function THashSet<T>.GetCount: SizeInt;
begin
  Result := FInternalDictionary.Count;
end;

function THashSet<T>.GetCapacity: SizeInt;
begin
  Result := FInternalDictionary.Capacity;
end;

procedure THashSet<T>.SetCapacity(AValue: SizeInt);
begin
  FInternalDictionary.Capacity := AValue;
end;

function THashSet<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  Result := FInternalDictionary.OnKeyNotify;
end;

procedure THashSet<T>.SetOnNotify(AValue: TCollectionNotifyEvent<T>);
begin
  FOnNotify := AValue;
  if Assigned(AValue) then
    FInternalDictionary.OnKeyNotify := InternalDictionaryNotify
  else
    FInternalDictionary.OnKeyNotify := nil;
end;

function THashSet<T>.GetEnumerator: TCustomSetEnumerator;
begin
  Result := THashSetEnumerator.Create(Self);
end;

constructor THashSet<T>.Create;
begin
  FInternalDictionary := TOpenAddressingLP<T, TEmptyRecord>.Create;
end;

constructor THashSet<T>.Create(const AComparer: IEqualityComparer<T>);
begin
  FInternalDictionary := TOpenAddressingLP<T, TEmptyRecord>.Create(AComparer);
end;

destructor THashSet<T>.Destroy;
begin
  FInternalDictionary.Free;
end;

function THashSet<T>.Add(constref AValue: T): Boolean;
begin
  Result := not FInternalDictionary.ContainsKey(AValue);
  if Result then
    FInternalDictionary.Add(AValue, EmptyRecord);
end;

function THashSet<T>.Remove(constref AValue: T): Boolean;
var
  LIndex: SizeInt;
begin
  LIndex := FInternalDictionary.FindBucketIndex(AValue);
  Result := LIndex >= 0;
  if Result then
    FInternalDictionary.DoRemove(LIndex, cnRemoved);
end;

function THashSet<T>.Extract(constref AValue: T): T;
var
  LIndex: SizeInt;
begin
  LIndex := FInternalDictionary.FindBucketIndex(AValue);
  if LIndex < 0 then
    Exit(Default(T));

  Result := AValue;
  FInternalDictionary.DoRemove(LIndex, cnExtracted);
end;

procedure THashSet<T>.Clear;
begin
  FInternalDictionary.Clear;
end;

function THashSet<T>.Contains(constref AValue: T): Boolean;
begin
  Result := FInternalDictionary.ContainsKey(AValue);
end;

procedure THashSet<T>.TrimExcess;
begin
  FInternalDictionary.TrimExcess;
end;

{ TAVLTreeNode<TREE_CONSTRAINTS, TTree> }

function TAVLTreeNode<TREE_CONSTRAINTS, TTree>.Successor: PNode;
begin
  Result:=Right;
  if Result<>nil then begin
    while (Result.Left<>nil) do Result:=Result.Left;
  end else begin
    Result:=@Self;
    while (Result.Parent<>nil) and (Result.Parent.Right=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

function TAVLTreeNode<TREE_CONSTRAINTS, TTree>.Precessor: PNode;
begin
  Result:=Left;
  if Result<>nil then begin
    while (Result.Right<>nil) do Result:=Result.Right;
  end else begin
    Result:=@Self;
    while (Result.Parent<>nil) and (Result.Parent.Left=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

function TAVLTreeNode<TREE_CONSTRAINTS, TTree>.TreeDepth: integer;
// longest WAY down. e.g. only one node => 0 !
var LeftDepth, RightDepth: integer;
begin
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if LeftDepth>RightDepth then
    Result:=LeftDepth
  else
    Result:=RightDepth;
end;

procedure TAVLTreeNode<TREE_CONSTRAINTS, TTree>.ConsistencyCheck(ATree: TObject);
var
  LTree: TTree absolute ATree;
  LeftDepth: SizeInt;
  RightDepth: SizeInt;
begin
  // test left child
  if Left<>nil then begin
    if Left.Parent<>@Self then
      raise EAVLTree.Create('Left.Parent<>Self');
    if LTree.Compare(Left.Data.Key,Data.Key)>0 then
      raise EAVLTree.Create('Compare(Left.Data,Data)>0');
    Left.ConsistencyCheck(LTree);
  end;
  // test right child
  if Right<>nil then begin
    if Right.Parent<>@Self then
      raise EAVLTree.Create('Right.Parent<>Self');
    if LTree.Compare(Data.Key,Right.Data.Key)>0 then
      raise EAVLTree.Create('Compare(Data,Right.Data)>0');
    Right.ConsistencyCheck(LTree);
  end;
  // test balance
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if Balance<>(LeftDepth-RightDepth) then
    raise EAVLTree.CreateFmt('Balance[%d]<>(RightDepth[%d]-LeftDepth[%d])', [Balance, RightDepth, LeftDepth]);
end;

function TAVLTreeNode<TREE_CONSTRAINTS, TTree>.GetCount: SizeInt;
begin
  Result:=1;
  if Assigned(Left) then Inc(Result,Left.GetCount);
  if Assigned(Right) then Inc(Result,Right.GetCount);
end;

{ TCustomTreeEnumerator<T, PNode, TTree> }

function TCustomTreeEnumerator<T, PNode, TTree>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

constructor TCustomTreeEnumerator<T, PNode, TTree>.Create(ATree: TObject);
begin
  TObject(FTree) := ATree;
end;

{ TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator, T, PT, TREE_CONSTRAINTS> }

function TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator, T, PT, PNode, TTree>.GetCount: SizeInt;
begin
  Result := FTree.Count;
end;

function TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator, T, PT, PNode, TTree>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TTreePointersEnumerator.Create(FTree);
end;

constructor TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator, T, PT, PNode, TTree>.Create(
  ATree: TTree);
begin
  FTree := ATree;
end;

function TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator, T, PT, PNode, TTree>.
  DoGetEnumerator: TTreeEnumerator;
begin
  Result := TTreeEnumerator.Create(FTree);
end;

function TTreeEnumerable<TTreeEnumerator, TTreePointersEnumerator, T, PT, PNode, TTree>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(FTree.Count);
end;

{ TAVLTreeEnumerator<T, PNode, TTree> }

function TAVLTreeEnumerator<T, PNode, TTree>.DoMoveNext: Boolean;
begin
  if FLowToHigh then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Successor
    else
      FCurrent:=FTree.FindLowest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Precessor
    else
      FCurrent:=FTree.FindHighest;
  end;
  Result:=FCurrent<>nil;
end;

constructor TAVLTreeEnumerator<T, PNode, TTree>.Create(ATree: TObject; ALowToHigh: boolean);
begin
  inherited Create(ATree);
  FLowToHigh:=aLowToHigh;
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS>.TPairEnumerator }

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.TPairEnumerator.GetCurrent: TTreePair;
begin
  Result := TTreePair((@FCurrent.Data)^);
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS>.TNodeEnumerator }

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.TNodeEnumerator.GetCurrent: PNode;
begin
  Result := FCurrent;
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS>.TKeyEnumerator }

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := FCurrent.Key;
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS>.TPKeyEnumerator }

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.TPKeyEnumerator.GetCurrent: PKey;
begin
  Result := @FCurrent.Data.Key;
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS>.TValueEnumerator }

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := FCurrent.Value;
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS>.TValueEnumerator }

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.TPValueEnumerator.GetCurrent: PValue;
begin
  Result := @FCurrent.Data.Value;
end;

{ TCustomAVLTreeMap<TREE_CONSTRAINTS> }

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.NodeAdded(ANode: PNode);
begin
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.DeletingNode(ANode: PNode; AOrigin: boolean);
begin
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.DoRemove(ANode: PNode;
  ACollectionNotification: TCollectionNotification; ADispose: boolean): TValue;
begin
  if ANode=nil then
    raise EArgumentNilException.CreateRes(@SArgumentNilNode);

  if (ANode.Left = nil) or (ANode.Right = nil) then
    DeletingNode(ANode, true);

  InternalDelete(ANode);

  Dec(FCount);
  NodeNotify(ANode, ACollectionNotification, ADispose);

  if ADispose then
    Dispose(ANode);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.DisposeAllNodes(ANode: PNode);
begin
  if ANode.Left<>nil then
    DisposeAllNodes(ANode.Left);
  if ANode.Right<>nil then
    DisposeAllNodes(ANode.Right);

  NodeNotify(ANode, cnRemoved, true);
  Dispose(ANode);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.Compare(constref ALeft, ARight: TKey): Integer; inline;
begin
  Result := FComparer.Compare(ALeft, ARight);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.FindPredecessor(ANode: PNode): PNode;
begin
  if ANode <> nil then
  begin
    if ANode.Left <> nil then
    begin
      ANode := ANode.Left;
      while ANode.Right <> nil do ANode := ANode.Right;
    end
    else
    repeat
      Result := ANode;
      ANode := ANode.Parent;
    until (ANode = nil) or (ANode.Right = Result);
  end;
  Result := ANode;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.FindInsertNode(ANode: PNode; out AInsertNode: PNode): Integer;
begin
  AInsertNode := FRoot;
  if AInsertNode = nil then // first item in tree
    Exit(0);

  repeat
    Result := Compare(ANode.Key,AInsertNode.Key);
    if Result < 0 then
    begin
      Result:=-1;
      if AInsertNode.Left = nil then
        Exit;
      AInsertNode := AInsertNode.Left;
    end
    else
    begin
      if Result > 0 then
        Result:=1;
      if AInsertNode.Right = nil then
        Exit;
      AInsertNode := AInsertNode.Right;
      if Result = 0 then
        Break;
    end;
  until false;

  // for equal items (when item already exist) we need to keep 0 result
  while true do
    if Compare(ANode.Key,AInsertNode.Key) < 0 then
    begin
      if AInsertNode.Left = nil then
        Exit;
      AInsertNode := AInsertNode.Left;
    end
    else
    begin
      if AInsertNode.Right = nil then
        Exit;
      AInsertNode := AInsertNode.Right;
    end;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.RotateRightRight(ANode: PNode);
var
  LNode, LParent: PNode;
begin
  LNode := ANode.Right;
  LParent := ANode.Parent;

  ANode.Right := LNode.Left;
  if ANode.Right <> nil then
    ANode.Right.Parent := ANode;

  LNode.Left := ANode;
  LNode.Parent := LParent;
  ANode.Parent := LNode;

  if LParent <> nil then
  begin
    if LParent.Left = ANode then
      LParent.Left := LNode
    else
      LParent.Right := LNode;
  end
  else
    FRoot := LNode;

  if LNode.Balance = -1 then
  begin
    ANode.Balance := 0;
    LNode.Balance := 0;
  end
  else
  begin
    ANode.Balance := -1;
    LNode.Balance := 1;
  end
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.RotateLeftLeft(ANode: PNode);
var
  LNode, LParent: PNode;
begin
  LNode := ANode.Left;
  LParent := ANode.Parent;

  ANode.Left := LNode.Right;
  if ANode.Left <> nil then
    ANode.Left.Parent := ANode;

  LNode.Right := ANode;
  LNode.Parent := LParent;
  ANode.Parent := LNode;

  if LParent <> nil then
  begin
    if LParent.Left = ANode then
      LParent.Left := LNode
    else
      LParent.Right := LNode;
  end
  else
    FRoot := LNode;

  if LNode.Balance = 1 then
  begin
    ANode.Balance := 0;
    LNode.Balance := 0;
  end
  else
  begin
    ANode.Balance := 1;
    LNode.Balance := -1;
  end
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.RotateRightLeft(ANode: PNode);
var
  LRight, LLeft, LParent: PNode;
begin
  LRight := ANode.Right;
  LLeft := LRight.Left;
  LParent := ANode.Parent;

  LRight.Left := LLeft.Right;
  if LRight.Left <> nil then
    LRight.Left.Parent := LRight;

  ANode.Right := LLeft.Left;
  if ANode.Right <> nil then
    ANode.Right.Parent := ANode;

  LLeft.Left := ANode;
  LLeft.Right := LRight;
  ANode.Parent := LLeft;
  LRight.Parent := LLeft;

  LLeft.Parent := LParent;

  if LParent <> nil then
  begin
    if LParent.Left = ANode then
      LParent.Left := LLeft
    else
      LParent.Right := LLeft;
  end
  else
    FRoot := LLeft;

  if LLeft.Balance = -1 then
    ANode.Balance := 1
  else
    ANode.Balance := 0;

  if LLeft.Balance = 1 then
    LRight.Balance := -1
  else
    LRight.Balance := 0;

  LLeft.Balance := 0;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.RotateLeftRight(ANode: PNode);
var
  LLeft, LRight, LParent: PNode;
begin
  LLeft := ANode.Left;
  LRight := LLeft.Right;
  LParent := ANode.Parent;

  LLeft.Right := LRight.Left;
  if LLeft.Right <> nil then
    LLeft.Right.Parent := LLeft;

  ANode.Left := LRight.Right;
  if ANode.Left <> nil then
    ANode.Left.Parent := ANode;

  LRight.Right := ANode;
  LRight.Left := LLeft;
  ANode.Parent := LRight;
  LLeft.Parent := LRight;

  LRight.Parent := LParent;

  if LParent <> nil then
  begin
    if LParent.Left = ANode then
      LParent.Left := LRight
    else
      LParent.Right := LRight;
  end
  else
    FRoot := LRight;

  if LRight.Balance =  1 then
    ANode.Balance := -1
  else
    ANode.Balance := 0;
  if LRight.Balance = -1 then
    LLeft.Balance :=  1
  else
    LLeft.Balance := 0;

  LRight.Balance := 0;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.KeyNotify(constref AKey: TKey; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, AKey, ACollectionNotification);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.ValueNotify(constref AValue: TValue; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, AValue, ACollectionNotification);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.NodeNotify(ANode: PNode; ACollectionNotification: TCollectionNotification; ADispose: boolean);
begin
  if Assigned(FOnValueNotify) then
    FOnNodeNotify(Self, ANode, ACollectionNotification, ADispose);
  KeyNotify(ANode.Key, ACollectionNotification);
  ValueNotify(ANode.Value, ACollectionNotification);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.SetValue(var AValue: TValue; constref ANewValue: TValue);
var
  LOldValue: TValue;
begin
  LOldValue := AValue;
  AValue := ANewValue;

  ValueNotify(LOldValue, cnRemoved);
  ValueNotify(ANewValue, cnAdded);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.WriteStr(AStream: TStream; const AText: string);
begin
  if AText='' then exit;
  AStream.Write(AText[1],Length(AText));
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.GetNodeCollection: TNodeCollection;
begin
  if not Assigned(FNodes) then
    FNodes := TNodeCollection.Create(TTree(Self));
  Result := FNodes;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.InternalAdd(ANode, AParent: PNode);
begin
  Inc(FCount);

  ANode.Parent := AParent;
  NodeAdded(ANode);

  if AParent=nil then
  begin
    FRoot := ANode;
    Exit;
  end;

  // balance after insert

  if AParent.Balance<>0 then
    AParent.Balance := 0
  else
  begin
    if AParent.Left = ANode then
      AParent.Balance := 1
    else
      AParent.Balance := -1;

    ANode := AParent.Parent;

    while ANode <> nil do
    begin
      if ANode.Balance<>0 then
      begin
        if ANode.Balance = 1 then
        begin
          if ANode.Right = AParent then
            ANode.Balance := 0
          else if AParent.Balance = -1 then
            RotateLeftRight(ANode)
          else
            RotateLeftLeft(ANode);
        end
        else
        begin
          if ANode.Left = AParent then
            ANode.Balance := 0
          else if AParent^.Balance = 1 then
            RotateRightLeft(ANode)
          else
            RotateRightRight(ANode);
        end;
        Break;
      end;

      if ANode.Left = AParent then
        ANode.Balance := 1
      else
        ANode.Balance := -1;

      AParent := ANode;
      ANode := ANode.Parent;
    end;
  end;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.InternalAdd(ANode: PNode; ADispisable: boolean): PNode;
var
  LParent: PNode;
begin
  Result := ANode;
  case FindInsertNode(ANode, LParent) of
    -1: LParent.Left := ANode;
    0:
      if Assigned(LParent) then
        case FDuplicates of
          dupAccept: LParent.Right := ANode;
          dupIgnore:
            begin
              LParent.Right := nil;
              if ADispisable then
                Dispose(ANode);
              Exit(LParent);
            end;
          dupError:
            begin
              LParent.Right := nil;
              if ADispisable then
                Dispose(ANode);
              Result := nil;
              raise EListError.Create(SCollectionDuplicate);
            end;
        end;
    1: LParent.Right := ANode;
  end;

  InternalAdd(ANode, LParent);
  NodeNotify(ANode, cnAdded, false);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.InternalDelete(ANode: PNode);
var
  t, y, z: PNode;
  LNest: boolean;
begin
  if (ANode.Left <> nil) and (ANode.Right <> nil) then
  begin
    y := FindPredecessor(ANode);
    y.Info := ANode.Info;
    DeletingNode(y, false);
    InternalDelete(y);
    LNest := false;
  end
  else
  begin
    if ANode.Left <> nil then
    begin
      y := ANode.Left;
      ANode.Left := nil;
    end
    else
    begin
      y := ANode.Right;
      ANode.Right := nil;
    end;
    ANode.Balance := 0;
    LNest  := true;
  end;

  if y <> nil then
  begin
    y.Parent := ANode.Parent;
    y.Left  := ANode.Left;
    if y.Left <> nil then
      y.Left.Parent := y;
    y.Right := ANode.Right;
    if y.Right <> nil then
      y.Right.Parent := y;
    y.Balance := ANode.Balance;
  end;

  if ANode.Parent <> nil then
  begin
    if ANode.Parent.Left = ANode then
      ANode.Parent.Left := y
    else
      ANode.Parent.Right := y;
  end
  else
    FRoot := y;

  if LNest then
  begin
    z := y;
    y := ANode.Parent;
    while y <> nil do
    begin
      if y.Balance = 0 then
      begin
        if y.Left = z then
          y.Balance := -1
        else
          y.Balance := 1;
        break;
      end
      else
      begin
        if ((y.Balance = 1) and (y.Left = z)) or ((y.Balance = -1) and (y.Right = z)) then
        begin
          y.Balance := 0;
          z := y;
          y := y.Parent;
        end
        else
        begin
          if y.Left = z then
            t := y.Right
          else
            t := y.Left;
          if t.Balance = 0 then
          begin
            if y.Balance = 1 then
              RotateLeftLeft(y)
            else
              RotateRightRight(y);
            break;
          end
          else if y.Balance = t.Balance then
          begin
            if y.Balance = 1 then
              RotateLeftLeft(y)
            else
              RotateRightRight(y);
            z := t;
            y := t.Parent;
          end
          else
          begin
            if y.Balance = 1 then
              RotateLeftRight(y)
            else
              RotateRightLeft(y);
            z := y.Parent;
            y := z.Parent;
          end
        end
      end
    end
  end;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.GetKeys: TKeyCollection;
begin
  if not Assigned(FKeys) then
    FKeys := TKeyCollection.Create(TTree(Self));
  Result := TKeyCollection(FKeys);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.GetValues: TValueCollection;
begin
  if not Assigned(FValues) then
    FValues := TValueCollection.Create(TTree(Self));
  Result := TValueCollection(FValues);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.GetItem(const AKey: TKey): TValue;
var
  LNode: PNode;
begin
  LNode := Find(AKey);
  if not Assigned(LNode) then
    raise EAVLTree.CreateRes(@SDictionaryKeyDoesNotExist);
  result := LNode.Value;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.SetItem(const AKey: TKey; const AValue: TValue);
begin
  Find(AKey).Value := AValue;
end;

constructor TCustomAVLTreeMap<TREE_CONSTRAINTS>.Create;
begin
  FComparer := TComparer<TKey>.Default;
end;

constructor TCustomAVLTreeMap<TREE_CONSTRAINTS>.Create(const AComparer: IComparer<TKey>);
begin
  FComparer := AComparer;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.NewNode: PNode;
begin
  Result := AllocMem(SizeOf(TNode));
  Initialize(Result^);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.NewNodeArray(ACount: SizeInt): PNode;
begin
  Result := AllocMem(ACount * SizeOf(TNode));
  Initialize(Result^, ACount);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.NewNodeArray(out AArray: TArray<PNode>; ACount: SizeInt);
var
  i: Integer;
begin
  SetLength(AArray, ACount);
  for i := 0 to ACount-1 do
    AArray[i] := NewNode;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.DisposeNode(ANode: PNode);
begin
  Dispose(ANode);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.DisposeNodeArray(ANode: PNode; ACount: SizeInt);
begin
  Finalize(ANode^, ACount);
  FreeMem(ANode);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.DisposeNodeArray(var AArray: TArray<PNode>);
var
  i: Integer;
begin
  for i := 0 to High(AArray) do
    Dispose(AArray[i]);
  AArray := nil;
end;

destructor TCustomAVLTreeMap<TREE_CONSTRAINTS>.Destroy;
begin
  FKeys.Free;
  FValues.Free;
  FNodes.Free;
  Clear;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.AddNode(ANode: PNode): boolean;
begin
  Result := ANode=InternalAdd(ANode, false);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.Add(constref APair: TTreePair): PNode;
begin
  Result := NewNode;
  Result.Data.Key := APair.Key;
  Result.Data.Value := APair.Value;
  Result := InternalAdd(Result, true);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.Add(constref AKey: TKey; constref AValue: TValue): PNode;
begin
  Result := NewNode;
  Result.Data.Key := AKey;
  Result.Data.Value := AValue;
  Result := InternalAdd(Result, true);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.Remove(constref AKey: TKey; ADisposeNode: boolean): boolean;
var
  LNode: PNode;
begin
  LNode:=Find(AKey);
  if LNode<>nil then begin
    Delete(LNode, ADisposeNode);
    Result:=true;
  end else
    Result:=false;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.ExtractPair(constref AKey: TKey; ADisposeNode: boolean): TTreePair;
var
  LNode: PNode;
begin
  LNode:=Find(AKey);
  if LNode<>nil then
  begin
    Result.Key := AKey;
    Result.Value := DoRemove(LNode, cnExtracted, ADisposeNode);
  end else
    Result := Default(TTreePair);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.ExtractPair(constref ANode: PNode; ADispose: boolean = true): TTreePair;
begin
  Result.Key := ANode.Key;
  Result.Value := DoRemove(ANode, cnExtracted, ADispose);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.Extract(constref AKey: TKey; ADisposeNode: boolean): PNode;
begin
  Result:=Find(AKey);
  if Result<>nil then
  begin
    DoRemove(Result, cnExtracted, false);
    if ADisposeNode then
      Result := nil;
  end;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.ExtractNode(ANode: PNode; ADispose: boolean): PNode;
begin
  DoRemove(ANode, cnExtracted, ADispose);
  if ADispose then
    Result := nil
  else
    Result := ANode;
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.Delete(ANode: PNode; ADispose: boolean);
begin
  DoRemove(ANode, cnRemoved, ADispose);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.Clear(ADisposeNodes: Boolean);
begin
  if (FRoot<>nil) and ADisposeNodes then
    DisposeAllNodes(FRoot);
  fRoot:=nil;
  FCount:=0;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self, true);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.FindLowest: PNode;
begin
  Result:=FRoot;
  if Result<>nil then
    while Result.Left<>nil do Result:=Result.Left;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.FindHighest: PNode;
begin
  Result:=FRoot;
  if Result<>nil then
    while Result.Right<>nil do Result:=Result.Right;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.Find(constref AKey: TKey): PNode;
var
  LComp: SizeInt;
begin
  Result:=FRoot;
  while (Result<>nil) do
  begin
    LComp:=Compare(AKey,Result.Key);
    if LComp=0 then
      Exit;
    if LComp<0 then
      Result:=Result.Left
    else
      Result:=Result.Right
  end;
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.ContainsKey(constref AKey: TKey; out ANode: PNode): boolean;
begin
  ANode := Find(AKey);
  Result := Assigned(ANode);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.ContainsKey(constref AKey: TKey): boolean; overload; inline;
begin
  Result := Assigned(Find(AKey));
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.ConsistencyCheck;
var
  RealCount: SizeInt;
begin
  RealCount:=0;
  if FRoot<>nil then begin
    FRoot.ConsistencyCheck(Self);
    RealCount:=FRoot.GetCount;
  end;
  if Count<>RealCount then
    raise EAVLTree.Create('Count<>RealCount');
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.WriteTreeNode(AStream: TStream; ANode: PNode);
var
  b: String;
  IsLeft: boolean;
  LParent: PNode;
  WasLeft: Boolean;
begin
  if ANode=nil then exit;
  WriteTreeNode(AStream, ANode.Right);
  LParent:=ANode;
  WasLeft:=false;
  b:='';
  while LParent<>nil do begin
    if LParent.Parent=nil then begin
      if LParent=ANode then
        b:='--'+b
      else
        b:='  '+b;
      break;
    end;
    IsLeft:=LParent.Parent.Left=LParent;
    if LParent=ANode then begin
      if IsLeft then
        b:='\-'
      else
        b:='/-';
    end else begin
      if WasLeft=IsLeft then
        b:='  '+b
      else
        b:='| '+b;
    end;
    WasLeft:=IsLeft;
    LParent:=LParent.Parent;
  end;
  b:=b+NodeToReportStr(ANode)+LineEnding;
  WriteStr(AStream, b);
  WriteTreeNode(AStream, ANode.Left);
end;

procedure TCustomAVLTreeMap<TREE_CONSTRAINTS>.WriteReportToStream(AStream: TStream);
begin
  WriteStr(AStream, '-Start-of-AVL-Tree-------------------'+LineEnding);
  WriteTreeNode(AStream, fRoot);
  WriteStr(AStream, '-End-Of-AVL-Tree---------------------'+LineEnding);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.NodeToReportStr(ANode: PNode): string;
begin
  Result:=Format(' Self=%p  Parent=%p  Balance=%d', [ANode, ANode.Parent, ANode.Balance]);
end;

function TCustomAVLTreeMap<TREE_CONSTRAINTS>.ReportAsString: string;
var ms: TMemoryStream;
begin
  Result:='';
  ms:=TMemoryStream.Create;
  try
    WriteReportToStream(ms);
    ms.Position:=0;
    SetLength(Result,ms.Size);
    if Result<>'' then
      ms.Read(Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

{ TIndexedAVLTreeMap<TKey, TValue> }

procedure TIndexedAVLTreeMap<TKey, TValue>.RotateRightRight(ANode: PNode);
var
  LOldRight: PNode;
begin
  LOldRight:=ANode.Right;
  inherited;
  Inc(LOldRight.Data.Info, (1 + ANode.Data.Info));
end;

procedure TIndexedAVLTreeMap<TKey, TValue>.RotateLeftLeft(ANode: PNode);
var
  LOldLeft: PNode;
begin
  LOldLeft:=ANode.Left;
  inherited;
  Dec(ANode.Data.Info, (1 + LOldLeft.Data.Info));
end;

procedure TIndexedAVLTreeMap<TKey, TValue>.RotateRightLeft(ANode: PNode);
var
  LB, LC: PNode;
begin
  LB := ANode.Right;
  LC := LB.Left;
  inherited;
  Dec(LB.Data.Info, 1+LC.Info);
  Inc(LC.Data.Info, 1+ANode.Info);
end;

procedure TIndexedAVLTreeMap<TKey, TValue>.RotateLeftRight(ANode: PNode);
var
  LB, LC: PNode;
begin
  LB := ANode.Left;
  LC := LB.Right;
  inherited;
  Inc(LC.Data.Info, 1+LB.Info);
  Dec(ANode.Data.Info, 1+LC.Info);
end;


procedure TIndexedAVLTreeMap<TKey, TValue>.NodeAdded(ANode: PNode);
var
  LParent, LNode: PNode;
begin
  FLastNode := nil;
  LNode := ANode;
  repeat
    LParent:=LNode.Parent;
    if (LParent=nil) then break;
    if LParent.Left=LNode then
      Inc(LParent.Data.Info);
    LNode:=LParent;
  until false;
end;

procedure TIndexedAVLTreeMap<TKey, TValue>.DeletingNode(ANode: PNode; AOrigin: boolean);
var
  LParent: PNode;
begin
  if not AOrigin then
    Dec(ANode.Data.Info);
  FLastNode := nil;
  repeat
    LParent:=ANode.Parent;
    if (LParent=nil) then exit;
    if LParent.Left=ANode then
      Dec(LParent.Data.Info);
    ANode:=LParent;
  until false;
end;

function TIndexedAVLTreeMap<TKey, TValue>.GetNodeAtIndex(AIndex: SizeInt): PNode;
begin
  if (AIndex<0) or (AIndex>=Count) then
    raise EIndexedAVLTree.CreateFmt('TIndexedAVLTree: AIndex %d out of bounds 0..%d', [AIndex, Count]);

  if FLastNode<>nil then begin
    if AIndex=FLastIndex then
      Exit(FLastNode)
    else if AIndex=FLastIndex+1 then begin
      FLastIndex:=AIndex;
      FLastNode:=FLastNode.Successor;
      Exit(FLastNode);
    end else if AIndex=FLastIndex-1 then begin
      FLastIndex:=AIndex;
      FLastNode:=FLastNode.Precessor;
      Exit(FLastNode);
    end;
  end;

  FLastIndex:=AIndex;
  Result:=FRoot;
  repeat
    if Result.Info>AIndex then
      Result:=Result.Left
    else if Result.Info=AIndex then begin
      FLastNode:=Result;
      Exit;
    end
    else begin
      Dec(AIndex, Result.Info+1);
      Result:=Result.Right;
    end;
  until false;
end;

function TIndexedAVLTreeMap<TKey, TValue>.NodeToIndex(ANode: PNode): SizeInt;
var
  LNode: PNode;
  LParent: PNode;
begin
  if ANode=nil then
    Exit(-1);

  if FLastNode=ANode then
    Exit(FLastIndex);

  LNode:=ANode;
  Result:=LNode.Info;
  repeat
    LParent:=LNode.Parent;
    if LParent=nil then break;
    if LParent.Right=LNode then
      inc(Result,LParent.Info+1);
    LNode:=LParent;
  until false;

  FLastNode:=ANode;
  FLastIndex:=Result;
end;

procedure TIndexedAVLTreeMap<TKey, TValue>.ConsistencyCheck;
var
  LNode: PNode;
  i: SizeInt;
  LeftCount: SizeInt = 0;
begin
  inherited ConsistencyCheck;
  i:=0;
  for LNode in Self.Nodes do
  begin
    if LNode.Left<>nil then
      LeftCount:=LNode.Left.GetCount
    else
      LeftCount:=0;

    if LNode.Info<>LeftCount then
      raise EIndexedAVLTree.CreateFmt('LNode.LeftCount=%d<>%d',[LNode.Info,LeftCount]);

    if GetNodeAtIndex(i)<>LNode then
      raise EIndexedAVLTree.CreateFmt('GetNodeAtIndex(%d)<>%P',[i,LNode]);
    FLastNode:=nil;
    if GetNodeAtIndex(i)<>LNode then
      raise EIndexedAVLTree.CreateFmt('GetNodeAtIndex(%d)<>%P',[i,LNode]);

    if NodeToIndex(LNode)<>i then
      raise EIndexedAVLTree.CreateFmt('NodeToIndex(%P)<>%d',[LNode,i]);
    FLastNode:=nil;
    if NodeToIndex(LNode)<>i then
      raise EIndexedAVLTree.CreateFmt('NodeToIndex(%P)<>%d',[LNode,i]);

    inc(i);
  end;
end;

function TIndexedAVLTreeMap<TKey, TValue>.NodeToReportStr(ANode: PNode): string;
begin
  Result:=Format(' Self=%p  Parent=%p  Balance=%d Idx=%d Info=%d',
             [ANode,ANode.Parent, ANode.Balance, NodeToIndex(ANode), ANode.Info]);
end;

{ TAVLTree<T> }

function TAVLTree<T>.Add(constref AValue: T): PNode;
begin
  Result := inherited Add(AValue, EmptyRecord);
end;

function TAVLTree<T>.AddNode(ANode: PNode): boolean;
begin
  Result := inherited AddNode(ANode);
end;

{ TIndexedAVLTree<T> }

function TIndexedAVLTree<T>.Add(constref AValue: T): PNode;
begin
  Result := inherited Add(AValue, EmptyRecord);
end;

function TIndexedAVLTree<T>.AddNode(ANode: PNode): boolean;
begin
  Result := inherited AddNode(ANode);
end;

{ TSortedSet<T>.TSortedSetEnumerator }

function TSortedSet<T>.TSortedSetEnumerator.GetCurrent: T;
begin
  Result := TTreeEnumerator(FEnumerator).GetCurrent;
end;

constructor TSortedSet<T>.TSortedSetEnumerator.Create(ASet: TCustomSet<T>);
begin
  TTreeEnumerator(FEnumerator) := TSortedSet<T>(ASet).FInternalTree.Keys.DoGetEnumerator;
end;

{ TSortedSet<T>.TPointersEnumerator }

function TSortedSet<T>.TPointersEnumerator.DoMoveNext: boolean;
begin
  Result := FEnumerator.MoveNext;
end;

function TSortedSet<T>.TPointersEnumerator.DoGetCurrent: PT;
begin
  Result := FEnumerator.Current;
end;

constructor TSortedSet<T>.TPointersEnumerator.Create(ASortedSet: TSortedSet<T>);
begin
  FEnumerator := ASortedSet.FInternalTree.Keys.Ptr^.GetEnumerator;
end;

{ TSortedSet<T> }

procedure TSortedSet<T>.InternalAVLTreeNotify(ASender: TObject; constref AItem: T; AAction: TCollectionNotification);
begin
  FOnNotify(Self, AItem, AAction);
end;

function TSortedSet<T>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TPointersEnumerator.Create(Self);
end;

function TSortedSet<T>.GetCount: SizeInt;
begin
  Result := FInternalTree.Count;
end;

function TSortedSet<T>.GetCapacity: SizeInt;
begin
  Result := FInternalTree.Count;
end;

procedure TSortedSet<T>.SetCapacity(AValue: SizeInt);
begin
end;

function TSortedSet<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  Result := FInternalTree.OnKeyNotify;
end;

procedure TSortedSet<T>.SetOnNotify(AValue: TCollectionNotifyEvent<T>);
begin
  FOnNotify := AValue;
  if Assigned(AValue) then
    FInternalTree.OnKeyNotify := InternalAVLTreeNotify
  else
    FInternalTree.OnKeyNotify := nil;
end;

function TSortedSet<T>.GetEnumerator: TCustomSetEnumerator;
begin
  Result := TSortedSetEnumerator.Create(Self);
end;

constructor TSortedSet<T>.Create;
begin
  FInternalTree := TAVLTree<T>.Create;
end;

constructor TSortedSet<T>.Create(const AComparer: IComparer<T>);
begin
  FInternalTree := TAVLTree<T>.Create(AComparer);
end;

destructor TSortedSet<T>.Destroy;
begin
  FInternalTree.Free;
end;

function TSortedSet<T>.Add(constref AValue: T): Boolean;
var
  LNodePtr, LParent: TAVLTree<T>.PNode;
  LNode: TAVLTree<T>.TNode;
  LCompare: Integer;
begin
  LNode.Data.Key := AValue;

  LCompare := FInternalTree.FindInsertNode(@LNode, LParent);

  Result := not((LCompare=0) and Assigned(LParent));
  if not Result then
    Exit;

  LNodePtr := FInternalTree.NewNode;
  LNodePtr^.Data.Key := AValue;

  case LCompare of
    -1: LParent.Left := LNodePtr;
    1: LParent.Right := LNodePtr;
  end;

  FInternalTree.InternalAdd(LNodePtr, LParent);
  FInternalTree.NodeNotify(LNodePtr, cnAdded, false);
end;

function TSortedSet<T>.Remove(constref AValue: T): Boolean;
var
  LNode: TAVLTree<T>.PNode;
begin
  LNode := FInternalTree.Find(AValue);
  Result := Assigned(LNode);
  if Result then
    FInternalTree.Delete(LNode);
end;

function TSortedSet<T>.Extract(constref AValue: T): T;
var
  LNode: TAVLTree<T>.PNode;
begin
  LNode := FInternalTree.Find(AValue);
  if not Assigned(LNode) then
    Exit(Default(T));

  Result := FInternalTree.ExtractPair(LNode).Key;
end;

procedure TSortedSet<T>.Clear;
begin
  FInternalTree.Clear;
end;

function TSortedSet<T>.Contains(constref AValue: T): Boolean;
begin
  Result := FInternalTree.ContainsKey(AValue);
end;

procedure TSortedSet<T>.TrimExcess;
begin
end;

{ TSortedHashSet<T>.TSortedHashSetEqualityComparer }

function TSortedHashSet<T>.TSortedHashSetEqualityComparer.Equals(constref ALeft, ARight: PT): Boolean;
begin
  if Assigned(FComparer) then
    Result := FComparer.Compare(ALeft^, ARight^) = 0
  else
    Result := FEqualityComparer.Equals(ALeft^, ARight^);
end;

function TSortedHashSet<T>.TSortedHashSetEqualityComparer.GetHashCode(constref AValue: PT): UInt32;
begin
  Result := FEqualityComparer.GetHashCode(AValue^);
end;

constructor TSortedHashSet<T>.TSortedHashSetEqualityComparer.Create(const AComparer: IComparer<T>);
begin
  FComparer := AComparer;
  FEqualityComparer := TEqualityComparer<T>.Default;
end;

constructor TSortedHashSet<T>.TSortedHashSetEqualityComparer.Create(const AEqualityComparer: IEqualityComparer<T>);
begin
  FEqualityComparer := AEqualityComparer;
end;

constructor TSortedHashSet<T>.TSortedHashSetEqualityComparer.Create(const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>);
begin
  FComparer := AComparer;
  FEqualityComparer := AEqualityComparer;
end;

{ TSortedHashSet<T>.TSortedHashSetEnumerator }

function TSortedHashSet<T>.TSortedHashSetEnumerator.GetCurrent: T;
begin
  Result := TTreeEnumerator(FEnumerator).Current;
end;

constructor TSortedHashSet<T>.TSortedHashSetEnumerator.Create(ASet: TCustomSet<T>);
begin
  FEnumerator := TSortedHashSet<T>(ASet).FInternalTree.Keys.GetEnumerator;
end;

{ TSortedHashSet<T>.TPointersEnumerator }

function TSortedHashSet<T>.TPointersEnumerator.DoMoveNext: boolean;
begin
  Result := FEnumerator.MoveNext;
end;

function TSortedHashSet<T>.TPointersEnumerator.DoGetCurrent: PT;
begin
  Result := FEnumerator.Current;
end;

constructor TSortedHashSet<T>.TPointersEnumerator.Create(ASortedHashSet: TSortedHashSet<T>);
begin
  FEnumerator := ASortedHashSet.FInternalTree.Keys.Ptr^.GetEnumerator;
end;

{ TSortedHashSet<T> }

procedure TSortedHashSet<T>.InternalDictionaryNotify(ASender: TObject; constref AItem: PT; AAction: TCollectionNotification);
begin
  FOnNotify(Self, AItem^, AAction);
end;

function TSortedHashSet<T>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TPointersEnumerator.Create(Self);
end;

function TSortedHashSet<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TSortedHashSet<T>.GetCount: SizeInt;
begin
  Result := FInternalDictionary.Count;
end;

function TSortedHashSet<T>.GetCapacity: SizeInt;
begin
  Result := FInternalDictionary.Capacity;
end;

procedure TSortedHashSet<T>.SetCapacity(AValue: SizeInt);
begin
  FInternalDictionary.Capacity := AValue;
end;

function TSortedHashSet<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  Result := FInternalTree.OnKeyNotify;
end;

procedure TSortedHashSet<T>.SetOnNotify(AValue: TCollectionNotifyEvent<T>);
begin
  FOnNotify := AValue;
  if Assigned(AValue) then
    FInternalDictionary.OnKeyNotify := InternalDictionaryNotify
  else
    FInternalDictionary.OnKeyNotify := nil;
end;

function TSortedHashSet<T>.GetEnumerator: TCustomSetEnumerator;
begin
  Result := TSortedHashSetEnumerator.Create(Self);
end;

function TSortedHashSet<T>.Add(constref AValue: T): Boolean;
var
  LNode: TAVLTree<T>.PNode;
begin
  Result := not FInternalDictionary.ContainsKey(@AValue);
  if Result then
  begin
    LNode := FInternalTree.Add(AValue);
    FInternalDictionary.Add(@LNode.Data.Key, EmptyRecord);
  end;
end;

function TSortedHashSet<T>.Remove(constref AValue: T): Boolean;
var
  LIndex: SizeInt;
begin
  LIndex := FInternalDictionary.FindBucketIndex(@AValue);
  Result := LIndex >= 0;
  if Result then
  begin
    FInternalDictionary.DoRemove(LIndex, cnRemoved);
    FInternalTree.Remove(AValue);
  end;
end;

function TSortedHashSet<T>.Extract(constref AValue: T): T;
var
  LIndex: SizeInt;
begin
  LIndex := FInternalDictionary.FindBucketIndex(@AValue);
  if LIndex >= 0 then
  begin
    FInternalDictionary.DoRemove(LIndex, cnExtracted);
    FInternalTree.Remove(AValue);
    Result := AValue;
  end else
    Result := Default(T);
end;

procedure TSortedHashSet<T>.Clear;
begin
  FInternalDictionary.Clear;
  FInternalTree.Clear;
end;

function TSortedHashSet<T>.Contains(constref AValue: T): Boolean;
begin
  Result := FInternalDictionary.ContainsKey(@AValue);
end;

constructor TSortedHashSet<T>.Create;
begin
  FInternalTree := TAVLTree<T>.Create;
  FInternalDictionary := TOpenAddressingLP<PT, TEmptyRecord>.Create(TSortedHashSetEqualityComparer.Create(TEqualityComparer<T>.Default));
end;

constructor TSortedHashSet<T>.Create(const AComparer: IEqualityComparer<T>);
begin
  Create(TComparer<T>.Default, AComparer);
end;

constructor TSortedHashSet<T>.Create(const AComparer: IComparer<T>);
begin
  FInternalTree := TAVLTree<T>.Create(AComparer);
  FInternalDictionary := TOpenAddressingLP<PT, TEmptyRecord>.Create(TSortedHashSetEqualityComparer.Create(AComparer));
end;

constructor TSortedHashSet<T>.Create(const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>);
begin
  FInternalTree := TAVLTree<T>.Create(AComparer);
  FInternalDictionary := TOpenAddressingLP<PT, TEmptyRecord>.Create(TSortedHashSetEqualityComparer.Create(AComparer,AEqualityComparer));
end;

destructor TSortedHashSet<T>.Destroy;
begin
  FInternalDictionary.Free;
  FInternalTree.Free;
  inherited;
end;

procedure TSortedHashSet<T>.TrimExcess;
begin
  FInternalDictionary.TrimExcess;
end;

end.
