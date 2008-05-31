{
  Copyright 2002-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(VRML fields --- @link(TVRMLField) class and descendants.) }

unit VRMLFields;

interface

uses VectorMath, Classes, SysUtils, VRMLLexer, KambiUtils, KambiClassUtils,
  Images, KambiStringUtils;

{$define read_interface}

type
  EVRMLFieldAssign = class(Exception);
  EVRMLFieldAssignInvalidClass = class(EVRMLFieldAssign);
  EVRMLFieldAssignFromIsClause = class(EVRMLFieldAssign);

  TVRMLEvent = class;

  TVRMLSaveToStreamProperties = class
  private
    Indent: string;
    DoDiscardNextIndent: boolean;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    Stream: TStream;

    NodeNameBinding: TStringList;

    { If @true, TVRMLNode.SaveToStream will write the expanded
      prototype contents. This means that prototype definitions in the
      file will be actually useless, as every prototype is already
      expanded into normal built-in nodes. Usually you want to pass
      this as @false (default), since user wants to see his prototype
      calls in the file just like they were in source file. }
    WriteExpandedPrototype: boolean;

    { Which VRML version are we writing. }
    VerMajor, VerMinor: Integer;

    procedure IncIndent;
    procedure DecIndent;

    { Just a comfortable routines that simply write given string to a stream,
      using WriteStr(Stream, ...).
      @groupBegin }
    procedure Write(const S: string);
    procedure Writeln(const S: string); overload;
    procedure Writeln; overload;
    procedure WriteIndent(const S: string);
    procedure WritelnIndent(const S: string);
    { @groupEnd }

    { Causes next WriteIndent or WritelnIndent will not write the Indent.
      Useful in some cases to improve readability of generated VRML file. }
    procedure DiscardNextIndent;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Base fields classes) }

  { Common class for VRML field or event. }
  TVRMLFieldOrEvent = class(TPersistent)
  private
    FIsClause: boolean;
    FIsClauseName: string;

    FName: string;
  public
    { Name of the field or event.

      Normal fields/events are inside some VRML node, and then
      they should have a name <> ''. But in some special cases I use
      fields without a name, then this is ''.

      Note that you cannot change this after object creation, since
      Name is used for various purposes (like to generate names for
      TVRMLField.ExposedEvents). }
    property Name: string read FName;

    { Does the field/event reference other field by "IS" clause.
      This is usually caused by specifying "IS" clause instead
      of field value in VRML file.

      Conceptually, we think of such field as "without any value".
      So Equals and EqualsDefaultValue will always return @false for such field.
      Yes, pretty much like in SQL the "null" value.

      @groupBegin }
    property IsClause: boolean read FIsClause write FIsClause;
    property IsClauseName: string read FIsClauseName write FIsClauseName;
    { @groupEnd }

    { Parse only "IS" clause, if it's not present --- don't do nothing.
      For example, for the TVRMLField descendant, this does not try to parse
      field value. }
    procedure ParseIsClause(Lexer: TVRMLLexer);
  end;

  { Base class for all VRML fields.

    Common notes for all descendants: most of them expose field or property
    "Value", this is (surprise, surprise!) the value of the field.
    Many of them also expose DefaultValue and DefaultValueExists
    fields/properties, these should be the default VRML value for this field.
    You can even change DefaultValue after the object is created.

    Most of descendants include constructor that initializes
    both DefaultValue and Value to the same thing, as this is what
    you usually want.

    Some notes about @code(Assign) method (inherited from TPersistent and
    overridied appropriately in TVRMLField descendants):

    @orderedList(
      @item(There are some exceptions, but usually
        assignment is possible only when source and destination field classes
        are equal.)
      @item(Assignment tries to copy everything: name, default value,
        IsClause*, Exposed, and of course current value.

        If you want to copy only the current value, use AssignValue
        (or AssignLerp, where available).))
  }
  TVRMLField = class(TVRMLFieldOrEvent)
  private
    FExposed: boolean;
    procedure SetExposed(Value: boolean);

    FExposedEvents: array [boolean] of TVRMLEvent;
    function GetExposedEvents(InEvent: boolean): TVRMLEvent;
  protected

    { Save field value to a stream. Must be overriden for each specific
      field. SaveToStream writes Indent, Name, ' ', then calls SaveToStreamValue,
      then writes @link(NL).

      Note that SaveToStream in this class
      already takes care of IsClause. If IsClause, it will do
      everything, and not call SaveToStreamValue. So when overriding
      SaveToStreamValue, you can safely assume that IsClause is @false. }
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
      virtual; abstract;

    { Call this inside overriden Assign methods.
      I don't want to place this inside TVRMLField.Assign, since I want
      "inherited" in Assign methods to cause exception. }
    procedure VRMLFieldAssignCommon(Source: TVRMLField);

    procedure AssignValueRaiseInvalidClass(Source: TVRMLField);
  public
    { Normal constrctor.

      @italic(Descendants implementors notes:)
      when implementing constructors in descendants,
      remember that Create in this class actually just calls CreateUndefined,
      and CreateUndefined is virtual. So when calling @code(inherited Create),
      be aware that actually you may be calling your own overriden
      CreateUndefined.

      In fact, in descendants you should focus on moving all the work to
      CreateUndefined constructor.
      The Create constructor should be just a comfortable extension of
      CreateUndefined, that does the same and addiionally gets parameters
      that specify default field value. }
    constructor Create(const AName: string);

    { Virtual constructor, that you can use to construct field instance when
      field class is known only at runtime.

      The idea is that in some cases, you need to create fields using
      variable like FieldClass: TVRMLFieldClass. See e.g. TVRMLInterfaceDeclaration,
      VRML 2.0 feature that simply requires this ability, also
      implementation of TVRMLSimpleMultField.Parse and
      TVRMLSimpleMultField.CreateItemBeforeParse.

      Later you can initialize such instance from string using it's Parse method.

      Note that some exceptional fields simply cannot work when initialized
      by this constructor: these are SFEnum and SFBitMask fields.
      They simply need to know their TSFEnum.EnumNames, or
      TSFBitMask.FlagNames + TSFBitMask.NoneString + TSFBitMask.AllString
      before they can be parsed. I guess that's one of the reasons why these
      field types were entirely removed from VRML 2.0. }
    constructor CreateUndefined(const AName: string); virtual;

    { Parse inits properties from Lexer.

      In this class, Parse only sets IsClause and IsClauseName:
      if we stand on "IS" clause (see VRML 2.0 spec about "IS" clause)
      and IsClauseAllowed then IsClause is set to @true and IsClauseName is set
      appropriately.

      Descendants should override this to read actual field contents.
      Always when overriding, call inherited first and check IsClause.
      If IsClause, then abort any further reading, as the field was
      specified using "IS" clause, so there's no actual value.

      Note that Lexer.NodeNameBinding is ignored by all
      TVRMLField descendants defined in this unit (it's used only
      by TSFNode and TMFNode). }
    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); virtual;

    { Parse field value from X3D XML encoded attribute.
      Attributes in X3D are generally encoded such that normal
      @code(Parse(Lexer, false)) call is appropriate,
      so this is done in this class. }
    procedure ParseX3DXmlAttr(Lexer: TVRMLLexer); virtual;

    { Save the field to the stream.
      If the current field value equals default value and
      SaveWhenDefault is @false (default) then the field will not be saved.
      Otherwise field name, current value / "IS" clause will be saved.

      This writes multiple lines, first line will start with Indent,
      last line will be terminated by newline.

      Note that when Name is '', this also works (writes only field value
      / "IS" clause then).

      SaveProperties.NodeNameBinding has the same meaning as for
      TVRMLNode.SaveToStream,
      see there. It can be ignored, and in fact it is ignored by all
      TVRMLField descendants defined in this unit (it's used only
      by TSFNode and TMFNode). }
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties;
      SaveWhenDefault: boolean = false);

    { zwraca zawsze false w tej klasie. Mozesz to przedefiniowac w podklasach
      aby SaveToStream nie zapisywalo do strumienia pol o wartosci domyslnej.

      Note that when IsClause, this should always return @false
      (as the field doesn't have any value, conceptually). }
    function EqualsDefaultValue: boolean; virtual;

    { @true if the SecondValue object has exactly the same type and properties.
      For this class, this returns just (SecondValue.Name = Name).

      All descendants (that add some property that should be compared)
      should override this like

@longCode(#
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TMyType) and
    (TMyType(SecondValue).MyProperty = MyProperty);
#)

      For varius floating-point fields in this unit:
      we compare each float using EqualityEpsilon,
      i.e. if the difference is < EqualityEpsilon then the floats
      are assumed equal. Pass EqualityEpsilon = 0.0
      to perform *exact* comparison (this case will be optimized
      in implementation, by using routines like CompareMem
      instead of comparing float-by-float).

      Note that this *doesn't* compare the default values of two fields
      instances. This compares only the current values of two fields
      instances, and eventually some other properties that affect
      parsing (like names for TSFEnum and TSFBitMask) or allowed
      future values (like TSFFloat.MustBeNonnegative).
    }
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; virtual;

    { Does this field generate/accept events, that is
      an "exposedField" (in VRML 2.0) or "inputOutput" (in X3D). }
    property Exposed: boolean read FExposed write FExposed default true;

    { These are the set_xxx and xxx_changed events exposed by this field.
      @nil if Exposed is @false. }
    property ExposedEvents [InEvent: boolean]: TVRMLEvent
      read GetExposedEvents;

    { This returns fieldType as for VRML interface declaration statements. }
    class function VRMLTypeName: string; virtual; abstract;

    { Copies the current field value. Contrary to TPersistent.Assign, this
      doesn't copy the rest of properties.

      Source object must not have IsClause (since when IsClause, the Source
      doesn't have any defined value). After setting, our IsClause is always
      changed to @false.

      @raises(EVRMLFieldAssignInvalidClass
        Usually it's required the Source class to be equal to our class,
        if Source classes cannot be assigned we raise EVRMLFieldCannotAssignClass.)

      @raises(EVRMLFieldAssignFromIsClause
        Raised is Source has IsClause = @true. This prevents assignment,
        since it means that Source has no defined value.)

      @raises(EVRMLFieldAssign (Previous two exceptions also inherit from this.)
        Raised in case of any field assignment problem. It's guaranteed that
        in case of such problem, our value will not be modified before
        raising the exception.)

      @italic(Descendants implementors notes):

      In this class, implementation takes care of
      checking Source.IsClause (and eventually raising EVRMLFieldAssignFromIsClause)
      and setting our IsClause to @false. In descendants, you should do like
      @longCode(#
        if Source is <appropriate class> then
        begin
          inherited;
          Value := Source.value;
        end else
          AssignValueRaiseInvalidClass(Source);
      #)

      This will make sure that in case of error (incompatible classes
      or Source.IsClause) nothing will be changed (IsClause and Value will
      remain as they were). }
    procedure AssignValue(Source: TVRMLField); virtual;
  end;

  TVRMLFieldClass = class of TVRMLField;

  TObjectsListItem_2 = TVRMLField;
  {$I objectslist_2.inc}
  TVRMLFieldsListBase = TObjectsList_2;

  TVRMLFieldsList = class(TVRMLFieldsListBase)
  private
    function GetByName(const AName: string): TVRMLField;
  public
    { This is a comfortable property that allows you to access fields by name.
      Exception will be raised if the given Name doesn't exist. }
    property ByName[const AName: string]:TVRMLField read GetByName;

    { Searches for a field with given Name, returns it's index or -1 if not found. }
    function IndexOf(const AName: string): integer;

    { Returns if EventName is an event implicitly exposed by one of our
      exposed fields (i.e. set_xxx or xxx_changed). If yes, then
      returns index of event, and the event reference itself
      (so always @code(Fields[ReturnedIndex].ExposedEvent[ReturnedEvent.InEvent]
      = ReturnedEvent)). Otherwise, returns -1. }
    function IndexOfExposedEvent(const EventName: string;
      out Event: TVRMLEvent): Integer;
  end;

  TVRMLSingleField = class(TVRMLField)
  end;
  TVRMLSingleFieldClass = class of TVRMLSingleField;

  TObjectsListItem_1 = TVRMLSingleField;
  {$I ObjectsList_1.inc}
  TVRMLSingleFieldsList = TObjectsList_1;

  EVRMLMultFieldDifferentCount = class(Exception);

  TVRMLMultField = class(TVRMLField)
  public
    function Count: integer; virtual; abstract;

    { If SecondValue.Count <> Count, raises EVRMLMultFieldDifferentCount }
    procedure CheckCountEqual(SecondValue: TVRMLMultField);
  end;

  { Multiple values VRML field. Remember that such field may always have
    any number of items, including zero.

    Note that we keep MF fields contents in TDyn*Array instances
    (RawItems in this class, also accessible as Items (with more concrete
    class) in descendants). This means that they are in compact form,
    easy for reading, or even for feeding the list into OpenGL.
    That's the main reason why I did not simply implement
    TVRMLSimpleMultField as a descendant of TVRMLSingleFieldsList:
    A long list of vertexes, MFVec3f, would be kept as a list of pointers
    to a lot of TSFVec3f instances. This would be quite memory-consuming,
    and very uncomfortable for access. On the contrary, current implementation
    keeps all these vertexes inside one TDynVector3SingleArray instance,
    that internally keeps all items in one continuos piece of memory.

    @italic(Descendants implementors notes): to make new descendant:

    @unorderedList(
      @item(In CreateUndefined you have to initialize FItemClass and create
        RawItems. In destructor of this class, RawItems are freed, so
        don't worry about this.)

      @item(Override RawItemsAdd.)

      @item(
        If your ItemClass doesn't work 100% correctly when it's initialized
        by CreateUndefined, you may have to override CreateItemBeforeParse.
        Fortunately, VRML specification was careful to choose as multi-valued field
        types' only fields that can behave nicely when initialized by
        CreateUndefined (and in fact VRML 2.0 removed the "bad fields" entirely).)

      @item(Not strictly required, but usually it's comfortable to have
        a constructor that allows you to init default field value
        from some "array of TXxx".)
    ) }
  TVRMLSimpleMultField = class(TVRMLMultField)
  protected
    fItemClass: TVRMLSingleFieldClass;

    { This creates new instance of class ItemClass. It doesn't have to
      have initialized value (in other words, it can be created by
      CreateUndefined), since we'll call his Parse method immediately.
      Default implementation in this class uses simply ItemClass.CreateUndefined. }
    function CreateItemBeforeParse: TVRMLSingleField; virtual;

    { Add Item at the end of RawItems. It's guaranteed that Item
      passes here will be of class ItemClass. You should copy
      Item contents as appropriate (remember that Item instance itself
      may be freed soon, so copy contents, not only some reference). }
    procedure RawItemsAdd(Item: TVRMLSingleField); virtual abstract;
  protected
    { SaveToStreamValue overriden for MF fields. This class handles
      SaveToStreamValue fully, no need to override it again in
      descendants. }
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;

    { RawItemToString(i) must change RawItems[i] into a string that can be used to
      store this is text stream. In descendants, you have to override this. }
    function RawItemToString(ItemNum: integer): string; virtual; abstract;

    { This says when we should do newline when writing this field into a stream.
      It's has purely aesthetical meaning, nothing more.
      In this class SaveToStreamDoNewLineAfterRawItem always returns @true
      (although SaveToStreamValue may sometimes ignore it, if it knows better). }
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; virtual;
  public
    { Items of this field.

      @italic(Descendants implementors notes): You have to initialize this field
      in descendants' constructor, it will be always freed in our
      destructor. }
    RawItems: TDynArrayBase;

    { Number of items in this field. Simply returns RawItems.Count. }
    function Count: integer; override;

    { A corresponding SF field class. All items that will be passed
      to RawItemsAdd will be of this class. }
    property ItemClass: TVRMLSingleFieldClass read fItemClass;

    { Parse MF field. This class handles parsing fully, usually no need to
      override this more in descendants. It uses ItemClass.Parse method. }
    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;

    destructor Destroy; override;

    { Checks equality between this and SecondValue field.
      In addition to inherited(Equals), this also checks that
      Count and ItemClass are equal. All descendants must check
      for equality every item on SecondValue.Items[I] and Items[I]. }
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;

    procedure ParseX3DXmlAttr(Lexer: TVRMLLexer); override;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Single value (SF) VRML fields) }

  { SFBitMask VRML field.

    TSFBitMask is one of the exceptional field types that cannot
    be 100% correctly initialized by CreateUndefined, since
    EnumNames will be left undefined. }
  TSFBitMask = class(TVRMLSingleField)
  private
    fAllString, fNoneString: string;
    fFlagNames: TStringList;

    { Value of this field, as a bit mask.
      VRML 1.0 specification guarantees that SFBitMask has 32 or less flags.
      Actually, defined field values have no more than 3 fields, and
      VRML > 1.0 dropped SFBitMask entirely. So 32 is always enough. }
    fFlags: set of 0..31;
    function GetFlags(i: integer): boolean;
    procedure SetFlags(i: integer; value: boolean);
    function GetFlagNames(i: integer): string;
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    { Value of this field. You can use Index from the range 0 .. FlagsCount - 1. }
    property Flags[i: integer]:boolean read GetFlags write SetFlags;
    function FlagsCount: integer;
    property FlagNames[i: integer]:string read GetFlagNames;

    { Special strings that will be understood by parser as ALL or NONE
      bit values. AllString selects all flags, NoneString selects none.
      AllString may be '' is there's no such string, NoneString
      should never be '' (otherwise, user could not be able to specify
      some SFBitMask values --- NoneString is the only way to specify 0).

      There is usually little sense in using them like "ALL | something"
      (because it means just "ALL") or "NONE | something" (because it means
      just "something"). But it's allowed syntactically.

      @groupBegin }
    property AllString: string read fAllString;
    property NoneString: string read fNoneString;
    { @groupEnd }

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;

    { Are all flag values set to @true currently. }
    function AreAllFlags(value: boolean): boolean;

    { Constructor.

      Remember that arrays AFFlagNames and AFlags
      (AFlags is initial value of Flags) must have equal length.
      Eventually, AFlags may be longer (excessive items will be ignored). }
    constructor Create(const AName: string; const AFlagNames: array of string;
      const ANoneString, AAllString: string; const AFlags: array of boolean);

    destructor Destroy; override;

    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFBool = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: boolean);

    Value: boolean;

    DefaultValue: boolean;
    DefaultValueExists: boolean;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  { SFEnum VRML field.

    TSFEnum is one of the exceptional field types that cannot
    be 100% correctly initialized by CreateUndefined, since
    EnumNames will be left undefined. }
  TSFEnum = class(TVRMLSingleField)
  private
    fEnumNames: TStringList;
    function GetEnumNames(i: integer): string;
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string;
      const AEnumNames: array of string; const AValue: integer);
    destructor Destroy; override;

    { Value between 0 .. EnumCount - 1. By default 0. }
    Value: integer;

    DefaultValue: integer;
    DefaultValueExists: boolean;

    property EnumNames[i: integer]:string read GetEnumNames;
    function EnumNamesCount: integer;
    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFFloat = class(TVRMLSingleField)
  private
    FMustBeNonnegative: boolean;
    FValue: Single;
    procedure SetValue(const AValue: Single);
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: Single); overload;
    constructor Create(const AName: string; const AValue: Single;
      AMustBeNonnegative: boolean); overload;

    property Value: Single read FValue write SetValue;

    DefaultValue: Single;
    DefaultValueExists: boolean;

    { If @true then when trying to set Value to something < 0,
      we'll negate it (in other words, we'll keep Value >= 0 always).
      This is nice e.g. for Sphere.FdRadius field --- some incorrect VRML specify
      negative sphere radius. }
    property MustBeNonnegative: boolean read FMustBeNonnegative default false;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure AssignLerp(const A: Single; Value1, Value2: TSFFloat);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  { SFDouble VRML field. }
  TSFDouble = class(TVRMLSingleField)
  private
    FValue: Double;
    procedure SetValue(const AValue: Double);
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: Double);

    property Value: Double read FValue write SetValue;

    DefaultValue: Double;
    DefaultValueExists: boolean;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure AssignLerp(const A: Double; Value1, Value2: TSFDouble);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFTime = class(TSFDouble)
    class function VRMLTypeName: string; override;
  end;

  TSFImage = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public

    { Value is owned by this object - i.e. in destructor we do Value.Free.

      Value may be IsNull, and then we know that there is no image
      recorded in this field. Value may never be nil.
      Remember --- Value is freed by this object, but if you're altering it in any
      other way, you're responsible for good memory managing. }
    Value: TImage;

    { @param(AValue is the initial value for Value.

        Note - our constructor COPIES passed reference AValue, not it's contents
        (I mean, we do Value := AValue, NOT Value := ImageCopy(AValue),
        so don't Free image given to us (at least, don't do this without clearing
        our Value field)).
        You can pass AValue = nil, then Value will be inited to null image
        TRGBImage.Create.) }
    constructor Create(const AName: string; const AValue: TImage);
    constructor CreateUndefined(const AName: string); override;

    destructor Destroy; override;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;

    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFLong = class(TVRMLSingleField)
  private
    FMustBeNonnegative: boolean;
    FValue: Longint;
    procedure SetValue(const AValue: Longint);
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: Longint); overload;
    constructor Create(const AName: string; const AValue: Longint;
      AMustBeNonnegative: boolean); overload;

    property Value: Longint read FValue write SetValue;

    DefaultValue: Longint;
    DefaultValueExists: boolean;

    { See TSFFloat.MustBeNonnegative for explanation of this. }
    property MustBeNonnegative: boolean read FMustBeNonnegative default false;
    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFInt32 = class(TSFLong)
  public
    class function VRMLTypeName: string; override;
  end;

  TSFMatrix = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AMatrix: TMatrix4Single);

    Matrix: TMatrix4Single;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure AssignLerp(const A: Single; Value1, Value2: TSFMatrix);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFRotation = class(TVRMLSingleField)
  private
    DefaultAxis: TVector3Single;
    DefaultRotationRad: Single;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
    function GetValue: TVector4Single;
    procedure SetValue(const AValue: TVector4Single);
    function GetValueDeg: TVector4Single;
    procedure SetValueDeg(const AValue: TVector4Single);
  public
    constructor Create(const AName: string; const AnAxis: TVector3Single; const ARotationRad: Single);

    Axis: TVector3Single;
    RotationRad: Single;

    { Current rotation value, with last component expressing rotation in radians.

      This internally gets / sets values from @link(Axis), @link(RotationRad),
      it only presents them to you differently. }
    property Value: TVector4Single read GetValue write SetValue;

    { Current rotation value, with last component expressing rotation in degrees.

      So this is just like @link(Value), but last component is in degrees.
      This internally gets / sets values from @link(Axis), @link(RotationRad),
      it only presents them to you differently. }
    property ValueDeg: TVector4Single read GetValueDeg write SetValueDeg;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    { Rotate point Pt around Self. }
    function RotatedPoint(const pt: TVector3Single): TVector3Single;

    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    function EqualsDefaultValue: boolean; override;

    procedure AssignLerp(const A: Single; Value1, Value2: TSFRotation);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFString = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: string);

    Value: string;

    DefaultValue: string;
    DefaultValueExists: boolean;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFVec2f = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: TVector2Single);

    Value: TVector2Single;

    DefaultValue: TVector2Single;
    DefaultValueExists: boolean;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure AssignLerp(const A: Single; Value1, Value2: TSFVec2f);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFVec3f = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: TVector3Single);

    Value: TVector3Single;

    DefaultValue: TVector3Single;
    DefaultValueExists: boolean;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure AssignLerp(const A: Single; Value1, Value2: TSFVec3f);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFColor = class(TSFVec3f)
  public
    class function VRMLTypeName: string; override;
  end;

  TSFVec4f = class(TVRMLSingleField)
  protected
    procedure SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties); override;
  public
    constructor Create(const AName: string; const AValue: TVector4Single);

    Value: TVector4Single;

    DefaultValue: TVector4Single;
    DefaultValueExists: boolean;

    procedure Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure AssignLerp(const A: Single; Value1, Value2: TSFVec4f);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TSFColorRGBA = class(TSFVec4f)
  public
    class function VRMLTypeName: string; override;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Multiple value (MF) VRML fields)

  General implementation comments for MF fields:

    Field DefaultValuesCount may have three valid values (for now):
    -1 (means "no default value for this field")
    0 (means "default value of this field is empty")
    1 (means "default value of this field is one-item array with DefaultValue").

    As you can see, it's not possible to express default values with more
    than one item. That's OK, because nodes from VRML 1.0 and 2.0 specifications
    never have such field (and VRML 2.0 prototypes (that have user-defined
    default field values) actually don't need it). So, for now, more flexible
    DefaultValuesCount is not needed.

    CreateUndefined sets DefaultValuesCount to -1.
}

  { }
  TMFBool = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: Integer;
    DefaultValue: boolean;
  protected
    function RawItemToString(ItemNum: Integer): string; override;
  public
    function Items: TDynBooleanArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string;
      const InitialContent: array of boolean);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFLong = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Longint;
    FSaveToStreamLineUptoNegative: boolean;
  protected
    function RawItemToString(ItemNum: integer): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    { Set this to @true to make SaveToStreamDoNewLineAfterRawItem
      answer @true only after negative indexes. This makes SaveToStream
      have nice output for fields like IndexedFaceSet.coordIndex. }
    property SaveToStreamLineUptoNegative: boolean
      read FSaveToStreamLineUptoNegative write FSaveToStreamLineUptoNegative
      default false;

    function Items: TDynLongintArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of Longint);
    constructor CreateMFLong(const AName: string; const InitialContent: array of Longint;
      const ASaveToStreamLineUptoNegative: boolean);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFInt32 = class(TMFLong)
  public
    class function VRMLTypeName: string; override;
  end;

  TMFVec2f = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector2Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynVector2SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of TVector2Single);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    { @raises(EVRMLMultFieldDifferentCount When Value1.Count <> Value2.Count) }
    procedure AssignLerp(const A: Single; Value1, Value2: TMFVec2f);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFVec3f = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector3Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynVector3SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of TVector3Single);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    { @raises(EVRMLMultFieldDifferentCount When Value1.Count <> Value2.Count) }
    procedure AssignLerp(const A: Single; Value1, Value2: TMFVec3f);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFColor = class(TMFVec3f)
  public
    class function VRMLTypeName: string; override;
  end;

  TMFVec4f = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector4Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynVector4SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of TVector4Single);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    { @raises(EVRMLMultFieldDifferentCount When Value1.Count <> Value2.Count) }
    procedure AssignLerp(const A: Single; Value1, Value2: TMFVec4f);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFColorRGBA = class(TMFVec4f)
    class function VRMLTypeName: string; override;
  end;

  TMFRotation = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: Integer;
    DefaultValue: TVector4Single;
  protected
    function RawItemToString(ItemNum: Integer): string; override;
  public
    function Items: TDynVector4SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string;
      const InitialContent: array of TVector4Single);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    { @raises(EVRMLMultFieldDifferentCount When Value1.Count <> Value2.Count) }
    procedure AssignLerp(const A: Single; Value1, Value2: TMFRotation);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFFloat = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynSingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string;
      const InitialContent: array of Single);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    { @raises(EVRMLMultFieldDifferentCount When Value1.Count <> Value2.Count) }
    procedure AssignLerp(const A: Single; Value1, Value2: TMFFloat);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFDouble = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Double;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynDoubleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string;
      const InitialContent: array of Double);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    { @raises(EVRMLMultFieldDifferentCount When Value1.Count <> Value2.Count) }
    procedure AssignLerp(const A: Double; Value1, Value2: TMFDouble);
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  TMFTime = class(TMFDouble)
  public
    class function VRMLTypeName: string; override;
  end;

  TMFString = class(TVRMLSimpleMultField)
  private
    DefaultValuesCount: Integer;
    DefaultValue: string;
  protected
    function RawItemToString(ItemNum: Integer): string; override;
  public
    function Items: TDynStringArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of string);
    constructor CreateUndefined(const AName: string); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TVRMLField;
      const EqualityEpsilon: Single): boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TVRMLField); override;

    class function VRMLTypeName: string; override;
  end;

  { Stores information about available VRML field classes.
    The only use for now is to make a mapping from VRML field name to
    actual class (needed by VRML interface declarations). }
  TVRMLFieldsManager = class
  private
    Registered: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterClass(AClass: TVRMLFieldClass);
    procedure RegisterClasses(const Classes: array of TVRMLFieldClass);

    { Return field class for given name. Returns @nil if not found. }
    function FieldTypeNameToClass(const TypeName: string): TVRMLFieldClass;
  end;

  {$I vrmlevents.inc}

var
  VRMLFieldsManager: TVRMLFieldsManager;

{$undef read_interface}

implementation

uses Math, VRMLErrors;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{$I vrmlevents.inc}

{ TVRMLSaveToStreamProperties ------------------------------------------------ }

const
  { IndentIncrement is string or char. It's used by SaveToStream }
  IndentIncrement = CharTab;

constructor TVRMLSaveToStreamProperties.Create(AStream: TStream);
begin
  inherited Create;
  Stream := AStream;
  NodeNameBinding := TStringListCaseSens.Create;
end;

destructor TVRMLSaveToStreamProperties.Destroy;
begin
  FreeAndNil(NodeNameBinding);
  inherited;
end;

procedure TVRMLSaveToStreamProperties.IncIndent;
var
  L: Integer;
begin
  L := Length(Indent) + 1;
  SetLength(Indent, L);
  Indent[L] := IndentIncrement;
end;

procedure TVRMLSaveToStreamProperties.DecIndent;
begin
  SetLength(Indent, Length(Indent) - 1);
end;

procedure TVRMLSaveToStreamProperties.Write(const S: string);
begin
  WriteStr(Stream, S);
end;

procedure TVRMLSaveToStreamProperties.Writeln(const S: string);
begin
  WriteStr(Stream, S);
  WriteStr(Stream, NL);
end;

procedure TVRMLSaveToStreamProperties.Writeln;
begin
  WriteStr(Stream, NL);
end;

procedure TVRMLSaveToStreamProperties.WriteIndent(const S: string);
begin
  if DoDiscardNextIndent then
    DoDiscardNextIndent := false else
    WriteStr(Stream, Indent);
  WriteStr(Stream, S);
end;

procedure TVRMLSaveToStreamProperties.WritelnIndent(const S: string);
begin
  WriteIndent(S);
  WriteStr(Stream, NL);
end;

procedure TVRMLSaveToStreamProperties.DiscardNextIndent;
begin
  DoDiscardNextIndent := true;
end;

{ TVRMLFieldOrEvent ---------------------------------------------------------- }

procedure TVRMLFieldOrEvent.ParseIsClause(Lexer: TVRMLLexer);
begin
  FIsClause := Lexer.TokenIsKeyword(vkIS);
  if FIsClause then
  begin
    Lexer.NextToken;
    FIsClauseName := Lexer.TokenName;
    Lexer.NextToken;
  end;
end;

{ TVRMLField ------------------------------------------------------------- }

constructor TVRMLField.Create(const AName: string);
begin
  CreateUndefined(AName);
end;

constructor TVRMLField.CreateUndefined(const AName: string);
begin
  inherited Create;
  FName := AName;

  { Set Exposed to true by the property, to force FExposedEvents initialization }
  FExposed := false;
  Exposed := true;
end;

function TVRMLField.GetExposedEvents(InEvent: boolean): TVRMLEvent;
begin
  Result := FExposedEvents[InEvent];
end;

const
  SetPrefix = 'set_';
  ChangedSuffix = '_changed';

procedure TVRMLField.SetExposed(Value: boolean);
begin
  if Value <> Exposed then
  begin
    FExposed := Value;
    if Exposed then
    begin
      FExposedEvents[false] := TVRMLEvent.Create(Name + ChangedSuffix,
        TVRMLFieldClass(Self.ClassType), false);
      FExposedEvents[true] := TVRMLEvent.Create(SetPrefix + Name,
        TVRMLFieldClass(Self.ClassType), true);
    end else
    begin
      FreeAndNil(FExposedEvents[false]);
      FreeAndNil(FExposedEvents[true]);
    end;
  end;
end;

procedure TVRMLField.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties;
  SaveWhenDefault: boolean);
begin
  if SaveWhenDefault or (not EqualsDefaultValue) then
  begin
    if Name <> '' then SaveProperties.WriteIndent(Name + ' ');
    { We depend here on the fact that EqualsDefaultValue is always @false
      when IsClause, otherwise fields with IsClause could be omitted by
      check "if not EqualsDefaultValue then" above. }
    if IsClause then
      SaveProperties.Write('IS ' + IsClauseName) else
      SaveToStreamValue(SaveProperties);
    SaveProperties.Writeln;
  end;
end;

function TVRMLField.EqualsDefaultValue: boolean;
begin
  Result := false;
end;

function TVRMLField.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
  Result := (not IsClause) and (SecondValue.Name = Name);
end;

procedure TVRMLField.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  if IsClauseAllowed then
    ParseIsClause(Lexer) else
    FIsClause := false;
end;

procedure TVRMLField.ParseX3DXmlAttr(Lexer: TVRMLLexer);
begin
  Parse(Lexer, false);
end;

procedure TVRMLField.VRMLFieldAssignCommon(Source: TVRMLField);
begin
  FName := Source.Name;
  FExposed := Source.Exposed;
  FIsClause := Source.IsClause;
  FIsClauseName := Source.IsClauseName;
end;

procedure TVRMLField.AssignValueRaiseInvalidClass(Source: TVRMLField);
begin
  raise EVRMLFieldAssignInvalidClass.CreateFmt('Cannot assign VRML field ' +
    '%s (%s) from %s (%s)',
    [        Name,        VRMLTypeName,
      Source.Name, Source.VRMLTypeName]);
end;

procedure TVRMLField.AssignValue(Source: TVRMLField);
begin
  if Source.IsClause then
    raise EVRMLFieldAssignFromIsClause.CreateFmt('Cannot assign from VRML field ' +
      '%s (%s) because it has an IS "%s" clause',
      [Source.Name, Source.VRMLTypeName, Source.IsClauseName]);
  FIsClause := false;
end;

{ TVRMLFieldsList ------------------------------------------------------------- }

function TVRMLFieldsList.IndexOf(const AName: string): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].Name = AName then exit;
 result := -1;
end;

function TVRMLFieldsList.GetByName(const AName: string): TVRMLField;
var i: integer;
begin
 i := IndexOf(AName);
 if i >= 0 then
  result := Items[i] else
  raise Exception.Create('Field name '+AName+' not found');
end;

function TVRMLFieldsList.IndexOfExposedEvent(const EventName: string;
  out Event: TVRMLEvent): Integer;
var
  InEvent: boolean;
begin
  { This implementation is quite optimized.
    Instead of browsing all fields and their ExposedEvents,
    looking for EventName event, instead we examine EventName
    to look whether this has any chance of being set_xxx or xxx_changed
    event. So we utilize the fact that exposed events have consistent
    naming. }

  if IsPrefix(SetPrefix, EventName, false) then
  begin
    InEvent := true;
    Result := IndexOf(SEnding(EventName, Length(SetPrefix) + 1));
  end else
  if IsSuffix(ChangedSuffix, EventName, false) then
  begin
    InEvent := false;
    Result := IndexOf(Copy(EventName, 1,
      Length(EventName) - Length(ChangedSuffix)));
  end else
    Result := -1;

  { check is field really exposed now }
  if (Result <> -1) and (not Items[Result].Exposed) then
  begin
    Result := -1;
  end;

  if Result <> -1 then
  begin
    Event := Items[Result].ExposedEvents[InEvent];
  end;
end;

{ TVRMLMultField ------------------------------------------------------------- }

procedure TVRMLMultField.CheckCountEqual(SecondValue: TVRMLMultField);
begin
 if SecondValue.Count <> Count then
  raise EVRMLMultFieldDifferentCount.CreateFmt(
    'Different length of multiple-value fields "%s" and "%s": "%d" and "%d"',
    [ Name,
      SecondValue.Name,
      Count,
      SecondValue.Count ]);
end;

{ TVRMLSimpleMultField ------------------------------------------------------- }

destructor TVRMLSimpleMultField.Destroy;
begin
 RawItems.Free;
 inherited;
end;

function TVRMLSimpleMultField.Count: integer;
begin result := RawItems.Count end;

function TVRMLSimpleMultField.CreateItemBeforeParse: TVRMLSingleField;
begin
 result := ItemClass.CreateUndefined('');
end;

procedure TVRMLSimpleMultField.Parse(
  Lexer: TVRMLLexer; IsClauseAllowed: boolean);
var SingleItem: TVRMLSingleField;
begin
  inherited;

  RawItems.SetLength(0);

  if IsClause then Exit;

  RawItems.AllowedCapacityOverflow := 100;
  SingleItem := nil;
  try
   SingleItem := CreateItemBeforeParse;

   if Lexer.Token = vtOpenSqBracket then
   begin
    Lexer.NextToken;

    while Lexer.Token <> vtCloseSqBracket do
    {zawsze w tym miejscu albo stoimy na "]" albo na kolejnej wartosci pola SF}
    begin
     SingleItem.Parse(Lexer, false);
     RawItemsAdd(SingleItem);

     if Lexer.Token = vtCloseSqBracket then break;

     if Lexer.VRMLVerMajor < 2 then
     begin
       Lexer.CheckTokenIs(vtComma);
       Lexer.NextToken;
     end;
    end;

    { Our handling of commas is specified by VRML 1.0 spec:
      - When the list has no items, "[]" is allowed but "[,]" is not.
      - When there are some items on the list, the last item *may*
        be followed by a comma.
      For VRML 2.0 this all doesn't matter, comma is just a whitespace
      and Lexer will never return such token. }

    Lexer.NextToken;
   end else
   begin
    {one single field - not enclosed in [] brackets}
    SingleItem.Parse(Lexer, false);
    RawItemsAdd(SingleItem);
   end;

  finally
    FreeAndNil(SingleItem);
    RawItems.AllowedCapacityOverflow := 4;
  end;
end;

procedure TVRMLSimpleMultField.ParseX3DXmlAttr(Lexer: TVRMLLexer);
var
  SingleItem: TVRMLSingleField;
begin
  { This is much easier and simpler in XML encoding than it was
    in classic encoding. We don't have to check for [ and ] tokens,
    comma is ignored (it was only for VRML 1.0 anyway), we just read
    single values up to the end of stream. }

  RawItems.SetLength(0);

  RawItems.AllowedCapacityOverflow := 100;
  SingleItem := CreateItemBeforeParse;
  try
    while Lexer.Token <> vtEnd do
    begin
      SingleItem.Parse(Lexer, false);
      RawItemsAdd(SingleItem);
    end;
  finally
    FreeAndNil(SingleItem);
    RawItems.AllowedCapacityOverflow := 4;
  end;
end;

procedure TVRMLSimpleMultField.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
var i: integer;
    WriteIndentNextTime: boolean;
begin
  { The general "for I := ..." code below can handle correctly any RawItems.Count
    value. But for aesthetics, i.e. more clear output for humans,
    I handle the RawItems.Count = 0 and 1 cases separately. }
  case RawItems.Count of
    0: SaveProperties.Write('[]');
    1: SaveProperties.Write(RawItemToString(0));
    else
      begin
        SaveProperties.Writeln('[');
        SaveProperties.IncIndent;

        WriteIndentNextTime := true;
        for i := 0 to RawItems.Count-1 do
        begin
          if WriteIndentNextTime then SaveProperties.WriteIndent('');
          SaveProperties.Write(RawItemToString(i) +',');
          { After the last item we always write newline,
            no matter what's SaveToStreamDoNewLineAfterRawItem }
          if (i = RawItems.Count - 1) or
             SaveToStreamDoNewLineAfterRawItem(i) then
            begin SaveProperties.Writeln; WriteIndentNextTime := true end else
            begin SaveProperties.Write(' '); WriteIndentNextTime := false; end;
        end;

        SaveProperties.DecIndent;
        SaveProperties.WritelnIndent(']');
      end;
  end;
end;

function TVRMLSimpleMultField.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
 result := true;
end;

function TVRMLSimpleMultField.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TVRMLSimpleMultField) and
   (TVRMLSimpleMultField(SecondValue).Count = Count) and
   (TVRMLSimpleMultField(SecondValue).ItemClass = ItemClass);
end;

{ simple helpful parsing functions ---------------------------------------- }

{ This returns Float, not just Single, because it's used by TSFDouble
  that wants double-precision preserved. }
function ParseFloat(Lexer: TVRMLLexer): Float;
begin
  Lexer.CheckTokenIs(TokenNumbers, 'float number');
  result := Lexer.TokenFloat;
  Lexer.NextToken;
end;

procedure ParseVector(var Vector: array of Single; Lexer: TVRMLLexer);
var i: integer;
begin
 for i := 0 to High(Vector) do Vector[i] := ParseFloat(Lexer);
end;

function ParseLongWord(Lexer: TVRMLLexer): LongWord;
begin
 Lexer.CheckTokenIs(vtInteger);
 result := Lexer.TokenInteger;
 Lexer.NextToken;
end;

{ TSFBool -------------------------------------------------------------------- }

constructor TSFBool.Create(const AName: string; const AValue: boolean);
begin
  inherited Create(AName);

  Value := AValue;
  DefaultValue := AValue;
  DefaultValueExists := true;
end;

procedure TSFBool.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);

  procedure VRML2BooleanIntegerNonFatalError;
  begin
    if Lexer.VRMLVerMajor >= 2 then
      VRMLNonFatalError('In VRML >= 2.0 you cannot express boolean values ' +
        'as 0 (instead of FALSE) or 1 (instead of TRUE)');
  end;

const
  SBoolExpected = 'boolean constant (TRUE, FALSE)';
begin
  inherited;
  if IsClause then Exit;

  Lexer.CheckTokenIs([vtKeyword, vtInteger], SBoolExpected);
  if Lexer.Token = vtKeyword then
  begin
   if Lexer.TokenKeyword = vkTrue then Value := true else
    if Lexer.TokenKeyword = vkFalse then Value := false else
     raise EVRMLParserError.Create(Lexer,
       'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
  end else
  begin
   if Lexer.TokenInteger = 1 then
   begin
     Value := true;
     VRML2BooleanIntegerNonFatalError;
   end else
   if Lexer.TokenInteger = 0 then
   begin
     Value := false;
     VRML2BooleanIntegerNonFatalError;
   end else
     raise EVRMLParserError.Create(Lexer,
       'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
  end;
  Lexer.NextToken;
end;

procedure TSFBool.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
 if Value then SaveProperties.Write(VRMLKeywords[vkTrue]) else
               SaveProperties.Write(VRMLKeywords[vkFalse])
end;

function TSFBool.EqualsDefaultValue: boolean;
begin
 result := (not IsClause) and DefaultValueExists and (DefaultValue = Value);
end;

function TSFBool.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFBool) and
   (TSFBool(SecondValue).Value = Value);
end;

procedure TSFBool.Assign(Source: TPersistent);
begin
 if Source is TSFBool then
 begin
  DefaultValue       := TSFBool(Source).DefaultValue;
  DefaultValueExists := TSFBool(Source).DefaultValueExists;
  Value              := TSFBool(Source).Value;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFBool.AssignValue(Source: TVRMLField);
begin
  if Source is TSFBool then
  begin
    inherited;
    Value := TSFBool(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFBool.VRMLTypeName: string;
begin
  Result := 'SFBool';
end;

{ TSFFloat ------------------------------------------------------------------- }

procedure TSFFloat.SetValue(const AValue: Single);
begin
 if MustBeNonnegative then
  FValue := Abs(AValue) else
  FValue := AValue;
end;

constructor TSFFloat.Create(const AName: string; const AValue: Single);
begin
  Create(AName, AValue, false);
end;

constructor TSFFloat.Create(const AName: string; const AValue: Single; AMustBeNonnegative: boolean);
begin
  inherited Create(AName);

  FMustBeNonnegative := AMustBeNonnegative;
  Value := AValue;
  DefaultValue := Value; { DefaultValue := Value, nie AValue, zeby SetValue moglo ew. zmienic Value }
  DefaultValueExists := true;
end;

procedure TSFFloat.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  Value := ParseFloat(Lexer);
end;

procedure TSFFloat.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Write(FloatToRawStr(Value));
end;

function TSFFloat.EqualsDefaultValue: boolean;
begin
 result := (not IsClause) and DefaultValueExists and (DefaultValue = Value)
end;

function TSFFloat.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFFloat) and
   (TSFFloat(SecondValue).MustBeNonnegative = MustBeNonnegative) and
   FloatsEqual(TSFFloat(SecondValue).Value, Value, EqualityEpsilon);
end;

procedure TSFFloat.AssignLerp(const A: Single; Value1, Value2: TSFFloat);
begin
 Value := Lerp(A, Value1.Value, Value2.Value);
end;

procedure TSFFloat.Assign(Source: TPersistent);
begin
 if Source is TSFFloat then
 begin
  DefaultValue       := TSFFloat(Source).DefaultValue;
  DefaultValueExists := TSFFloat(Source).DefaultValueExists;
  FValue             := TSFFloat(Source).Value;
  FMustBeNonnegative := TSFFloat(Source).MustBeNonnegative;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFFloat.AssignValue(Source: TVRMLField);
begin
  if Source is TSFFloat then
  begin
    inherited;
    Value := TSFFloat(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFFloat.VRMLTypeName: string;
begin
  Result := 'SFFloat';
end;

{ TSFDouble -------------------------------------------------------------------- }

constructor TSFDouble.Create(const AName: string; const AValue: Double);
begin
  inherited Create(AName);

  Value := AValue;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSFDouble.SetValue(const AValue: Double);
begin
  FValue := AValue;
end;

procedure TSFDouble.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  Value := ParseFloat(Lexer);
end;

procedure TSFDouble.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Write(FloatToRawStr(Value));
end;

function TSFDouble.EqualsDefaultValue: boolean;
begin
  Result := (not IsClause) and DefaultValueExists and (DefaultValue = Value);
end;

function TSFDouble.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFDouble) and
   FloatsEqual(TSFDouble(SecondValue).Value, Value, EqualityEpsilon);
end;

procedure TSFDouble.AssignLerp(const A: Double; Value1, Value2: TSFDouble);
begin
  Value := Lerp(A, Value1.Value, Value2.Value);
end;

procedure TSFDouble.Assign(Source: TPersistent);
begin
  if Source is TSFDouble then
  begin
    DefaultValue       := TSFDouble(Source).DefaultValue;
    DefaultValueExists := TSFDouble(Source).DefaultValueExists;
    FValue             := TSFDouble(Source).Value;
    VRMLFieldAssignCommon(TVRMLField(Source));
  end else
    inherited;
end;

procedure TSFDouble.AssignValue(Source: TVRMLField);
begin
  if Source is TSFDouble then
  begin
    inherited;
    Value := TSFDouble(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFDouble.VRMLTypeName: string;
begin
  Result := 'SFDouble';
end;

{ TSFTime -------------------------------------------------------------------- }

class function TSFTime.VRMLTypeName: string;
begin
  Result := 'SFTime';
end;

{ TSFImage ------------------------------------------------------------------- }

constructor TSFImage.Create(const AName: string; const AValue: TImage);
begin
  inherited Create(AName);

  if AValue <> nil then
  begin
    FreeAndNil(Value);
    Value := AValue;
  end;
end;

constructor TSFImage.CreateUndefined(const AName: string);
begin
  inherited;

  { Value must be initialized to non-nil. }
  Value := TRGBImage.Create;
end;

destructor TSFImage.Destroy;
begin
 FreeAndNil(Value);
 inherited;
end;

procedure TSFImage.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);

  procedure ReplaceValue(NewValue: TImage);
  begin
   FreeAndNil(Value);
   Value := NewValue;
  end;

var
  w, h, comp, pixel: LongWord;
  i: Cardinal;
  RGBPixels: PArray_Vector3Byte;
  AlphaPixels: PArray_Vector4Byte;
begin
  inherited;

  { Note that we should never let Value to be nil too long,
    because even if this method exits with exception, Value should
    always remain non-nil.
    That's why I'm doing below Value.Null instead of FreeAndNil(Value)
    and I'm using ReplaceValue to set new Value.
    This way if e.g. TRGBImage.Create with out of mem exception,
    Value will still remain non-nil.

    This is all because I just changed Images unit interface to class-like
    and I want to do minimal changes to VRMLFields unit to not break
    anything. TODO -- this will be solved better in the future, by simply
    allowing Value to be nil at any time.
    }

  Value.Null;

  if IsClause then Exit;

  { TODO: we convert here 1 and 2 components to 3 and 4 (that is,
    we convert grayscale to RGB). This is a limitation of our Images unit. }

  w := ParseLongWord(Lexer);
  h := ParseLongWord(Lexer);
  comp := ParseLongWord(Lexer);

  { If w or h =0 then w*h = 0 so we don't have to read anything more.
    We leave Value.IsNull in this case. }
  if (w <> 0) and (h <> 0) then
  begin
   case comp of
    1: begin
        ReplaceValue(TRGBImage.Create(w, h));
        RGBPixels := PArray_Vector3Byte(Value.RawPixels);
        for i := 0 to w*h-1 do
        begin
         pixel := ParseLongWord(Lexer);
         RGBPixels^[i, 0] := pixel and $FF;
         RGBPixels^[i, 1] := pixel and $FF;
         RGBPixels^[i, 2] := pixel and $FF;
        end;
       end;
    2: begin
        ReplaceValue(TAlphaImage.Create(w, h));
        AlphaPixels := PArray_Vector4Byte(Value.RawPixels);
        for i := 0 to w*h-1 do
        begin
         pixel := ParseLongWord(Lexer);
         AlphaPixels^[i, 0] := (pixel shr 8) and $FF;
         AlphaPixels^[i, 1] := (pixel shr 8) and $FF;
         AlphaPixels^[i, 2] := (pixel shr 8) and $FF;
         AlphaPixels^[i, 3] := pixel and $FF;
        end;
       end;
    3: begin
        ReplaceValue(TRGBImage.Create(w, h));
        RGBPixels := PArray_Vector3Byte(Value.RawPixels);
        for i := 0 to w*h-1 do
        begin
         pixel := ParseLongWord(Lexer);
         RGBPixels^[i, 0] := (pixel shr 16) and $FF;
         RGBPixels^[i, 1] := (pixel shr 8) and $FF;
         RGBPixels^[i, 2] := pixel and $FF;
        end;
       end;
    4: begin
        ReplaceValue(TAlphaImage.Create(w, h));
        AlphaPixels := PArray_Vector4Byte(Value.RawPixels);
        for i := 0 to w*h-1 do
        begin
         pixel := ParseLongWord(Lexer);
         AlphaPixels^[i, 0] := (pixel shr 24) and $FF;
         AlphaPixels^[i, 1] := (pixel shr 16) and $FF;
         AlphaPixels^[i, 2] := (pixel shr 8) and $FF;
         AlphaPixels^[i, 3] := pixel and $FF;
        end;
       end;
    else raise EVRMLParserError.Create(Lexer, Format('Invalid components count'+
           ' for SFImage : is %d, should be 1, 2, 3 or 4.',[comp]));
   end;
  end;
end;

procedure TSFImage.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
var rgb: TVector3Byte;
    rgba: TVector4Byte;
    i: Cardinal;
    pixel: LongWord;
begin
 if Value.IsNull then
  SaveProperties.Write('0 0 1') else
 begin
  SaveProperties.Writeln(Format('%d %d %d', [Value.Width, Value.Height,
    Value.ColorComponentsCount]));
  SaveProperties.IncIndent;
  SaveProperties.WriteIndent('');
  {$I NoRQCheckBegin.inc}
  if Value is TRGBImage then
  begin
   for i := 0 to Value.Width*Value.Height-1 do
   begin
    rgb := PArray_Vector3Byte(TRGBImage(Value).RGBPixels)^[i];
    pixel := (rgb[0] shl 16) or (rgb[1] shl 8) or rgb[2];
    SaveProperties.Write(Format('0x%.6x ', [pixel]));
   end;
  end else
  if Value is TAlphaImage then
  begin
   for i := 0 to Value.Width*Value.Height-1 do
   begin
    rgba := PArray_Vector4Byte(TAlphaImage(Value).AlphaPixels)^[i];
    pixel := (rgba[0] shl 24) or (rgba[1] shl 16) or (rgba[2] shl 8) or rgba[3];
    SaveProperties.Write(Format('0x%.8x ', [pixel]));
   end;
  end else
   raise Exception.Create('TSFImage.SaveToStreamValue - not implemented TImage descendant');
  {$I NoRQCheckEnd.inc}
  SaveProperties.DecIndent;
 end;
end;

function TSFImage.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFImage) and
   { TODO: compare values
   (TSFImage(SecondValue).Value = Value) }true;
end;

procedure TSFImage.Assign(Source: TPersistent);
begin
 if Source is TSFImage then
 begin
  FreeAndNil(Value);
  Value := TSFImage(Source).Value.MakeCopy;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFImage.AssignValue(Source: TVRMLField);
begin
  if Source is TSFImage then
  begin
    inherited;
    FreeAndNil(Value);
    Value := TSFImage(Source).Value.MakeCopy;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFImage.VRMLTypeName: string;
begin
  Result := 'SFImage';
end;

{ TSFLong -------------------------------------------------------------------- }

procedure TSFLong.SetValue(const AValue: Longint);
begin
 if MustBeNonnegative then
  FValue := Abs(AValue) else
  FValue := AValue;
end;

constructor TSFLong.Create(const AName: string; const AValue: Longint);
begin
  Create(AName, AValue, false);
end;

constructor TSFLong.Create(const AName: string; const AValue: Longint; AMustBeNonnegative: boolean);
begin
  inherited Create(AName);

  FMustBeNonnegative := AMustBeNonnegative;
  Value := AValue;
  DefaultValue := Value; { DefaultValue := Value, nie AValue, zeby SetValue moglo ew. zmienic Value }
  DefaultValueExists := true;
end;

procedure TSFLong.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  Lexer.CheckTokenIs(vtInteger);
  Value := Lexer.TokenInteger;
  Lexer.NextToken;
end;

procedure TSFLong.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Write(IntToStr(Value));
end;

function TSFLong.EqualsDefaultValue: boolean;
begin
 result := (not IsClause) and DefaultValueExists and (DefaultValue = Value)
end;

function TSFLong.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 { Note that this means that SFInt32 and SFLong will actually be considered
   equal. That's Ok, we want this. }
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFLong) and
   (TSFLong(SecondValue).MustBeNonnegative = MustBeNonnegative) and
   (TSFLong(SecondValue).Value = Value);
end;

procedure TSFLong.Assign(Source: TPersistent);
begin
 if Source is TSFLong then
 begin
  DefaultValue       := TSFLong(Source).DefaultValue;
  DefaultValueExists := TSFLong(Source).DefaultValueExists;
  FValue             := TSFLong(Source).Value;
  FMustBeNonnegative := TSFLong(Source).MustBeNonnegative;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFLong.AssignValue(Source: TVRMLField);
begin
  if Source is TSFLong then
  begin
    inherited;
    Value := TSFLong(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFLong.VRMLTypeName: string;
begin
  Result := 'SFLong';
end;

{ TSFInt32 ------------------------------------------------------------------- }

class function TSFInt32.VRMLTypeName: string;
begin
  Result := 'SFInt32';
end;

{ TSFMatrix ------------------------------------------------------------------ }

constructor TSFMatrix.Create(const AName: string; const AMatrix: TMatrix4Single);
begin
  inherited Create(AName);
  Matrix := AMatrix;
end;

procedure TSFMatrix.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
var
  col: integer;
begin
  inherited;
  if IsClause then Exit;

  for col := 0 to 3 do ParseVector(Matrix[col], Lexer);
end;

procedure TSFMatrix.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Writeln(VectorToRawStr(Matrix[0]));
  SaveProperties.IncIndent;
  SaveProperties.WritelnIndent(VectorToRawStr(Matrix[1]));
  SaveProperties.WritelnIndent(VectorToRawStr(Matrix[2]));
  SaveProperties.WritelnIndent(VectorToRawStr(Matrix[3]));
  SaveProperties.DecIndent;
end;

function TSFMatrix.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFMatrix) and
   MatricesEqual(TSFMatrix(SecondValue).Matrix, Matrix, EqualityEpsilon);
end;

procedure TSFMatrix.AssignLerp(const A: Single; Value1, Value2: TSFMatrix);
begin
 Matrix[0] := VLerp(A, Value1.Matrix[0], Value2.Matrix[0]);
 Matrix[1] := VLerp(A, Value1.Matrix[1], Value2.Matrix[1]);
 Matrix[2] := VLerp(A, Value1.Matrix[2], Value2.Matrix[2]);
 Matrix[3] := VLerp(A, Value1.Matrix[3], Value2.Matrix[3]);
end;

procedure TSFMatrix.Assign(Source: TPersistent);
begin
 if Source is TSFMatrix then
 begin
  Matrix := TSFMatrix(Source).Matrix;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFMatrix.AssignValue(Source: TVRMLField);
begin
  if Source is TSFMatrix then
  begin
    inherited;
    Matrix := TSFMatrix(Source).Matrix;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFMatrix.VRMLTypeName: string;
begin
  Result := 'SFMatrix';
end;

{ TSFRotation ---------------------------------------------------------------- }

constructor TSFRotation.Create(const AName: string;
  const AnAxis: TVector3Single; const ARotationRad: Single);
begin
  inherited Create(AName);

  Axis := AnAxis;
  RotationRad := ARotationRad;

  DefaultAxis := Axis;
  DefaultRotationRad := RotationRad;
  DefaultValueExists := true;
end;

procedure TSFRotation.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  ParseVector(Axis, Lexer);
  RotationRad := ParseFloat(Lexer);
end;

function TSFRotation.GetValue: TVector4Single;
begin
  Move(Axis[0], Result[0], SizeOf(Single) * 3);
  Result[3] := RotationRad;
end;

procedure TSFRotation.SetValue(const AValue: TVector4Single);
begin
 Axis[0] := AValue[0];
 Axis[1] := AValue[1];
 Axis[2] := AValue[2];
 RotationRad := AValue[3];
end;

function TSFRotation.GetValueDeg: TVector4Single;
begin
  Move(Axis[0], Result[0], SizeOf(Single) * 3);
  Result[3] := RadToDeg(RotationRad);
end;

procedure TSFRotation.SetValueDeg(const AValue: TVector4Single);
begin
 Axis[0] := AValue[0];
 Axis[1] := AValue[1];
 Axis[2] := AValue[2];
 RotationRad := DegToRad(AValue[3]);
end;

procedure TSFRotation.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Write(VectorToRawStr(Axis) +' ' +FloatToRawStr(RotationRad));
end;

function TSFRotation.RotatedPoint(const pt: TVector3Single): TVector3Single;
begin
  if not IsZeroVector(Axis) then
    Result := RotatePointAroundAxisRad(RotationRad, pt, Axis) else
  begin
    { Safeguard against rotation around zero vector, which produces unpredictable
      results (actually, Result would be filled with Nan values).
      VRML spec says that SFRotation should always specify a normalized vector. }
    Result := Pt;
    VRMLNonFatalError('SFRotation field specifies rotation around zero vector');
  end;
end;

function TSFRotation.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFRotation) and
   VectorsEqual(TSFRotation(SecondValue).Axis, Axis, EqualityEpsilon) and
   FloatsEqual(TSFRotation(SecondValue).RotationRad, RotationRad, EqualityEpsilon);
end;

function TSFRotation.EqualsDefaultValue: boolean;
begin
  Result := (not IsClause) and DefaultValueExists and
    VectorsPerfectlyEqual(DefaultAxis, Axis) and
    (DefaultRotationRad = RotationRad);
end;

procedure TSFRotation.AssignLerp(const A: Single; Value1, Value2: TSFRotation);
begin
 Axis        := VLerp(A, Value1.Axis, Value2.Axis);
 RotationRad :=  Lerp(A, Value1.RotationRad, Value2.RotationRad);
end;

procedure TSFRotation.Assign(Source: TPersistent);
begin
 if Source is TSFRotation then
 begin
  Axis        := TSFRotation(Source).Axis;
  RotationRad := TSFRotation(Source).RotationRad;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFRotation.AssignValue(Source: TVRMLField);
begin
  if Source is TSFRotation then
  begin
    inherited;
    Axis := TSFRotation(Source).Axis;
    RotationRad := TSFRotation(Source).RotationRad;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFRotation.VRMLTypeName: string;
begin
  Result := 'SFRotation';
end;

{ TSFString ------------------------------------------------------------------ }

constructor TSFString.Create(const AName: string; const AValue: string);
begin
  inherited Create(AName);

  Value := AValue;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSFString.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  Lexer.CheckTokenIs(vtString);
  Value := Lexer.TokenString;
  Lexer.NextToken;
end;

procedure TSFString.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
 SaveProperties.Write(StringToVRMLStringToken(Value));
end;

function TSFString.EqualsDefaultValue: boolean;
begin
 result := (not IsClause) and DefaultValueExists and (DefaultValue = Value);
end;

function TSFString.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFString) and
   (TSFString(SecondValue).Value = Value);
end;

procedure TSFString.Assign(Source: TPersistent);
begin
 if Source is TSFString then
 begin
  DefaultValue       := TSFString(Source).DefaultValue;
  DefaultValueExists := TSFString(Source).DefaultValueExists;
  Value              := TSFString(Source).Value;
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFString.AssignValue(Source: TVRMLField);
begin
  if Source is TSFString then
  begin
    inherited;
    Value := TSFString(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFString.VRMLTypeName: string;
begin
  Result := 'SFString';
end;

{ ----------------------------------------------------------------------------
  Common SF fields based on vectors implementation }

{$define IMPLEMENT_SF_CLASS_USING_VECTORS :=
constructor TSF_CLASS.Create(const AName: string; const AValue: TSF_STATIC_ITEM);
begin
  inherited Create(AName);

  Value := AValue;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

procedure TSF_CLASS.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
begin
  inherited;
  if IsClause then Exit;

  ParseVector(Value, Lexer);
end;

procedure TSF_CLASS.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Write(VectorToRawStr(Value));
end;

function TSF_CLASS.EqualsDefaultValue: boolean;
begin
  result := (not IsClause) and
    DefaultValueExists and VectorsPerfectlyEqual(DefaultValue, Value);
end;

function TSF_CLASS.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
  Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
    (SecondValue is TSF_CLASS) and
    VectorsEqual(TSF_CLASS(SecondValue).Value, Value, EqualityEpsilon);
end;

procedure TSF_CLASS.AssignLerp(const A: Single; Value1, Value2: TSF_CLASS);
begin
  Value := VLerp(A, Value1.Value, Value2.Value);
end;

procedure TSF_CLASS.Assign(Source: TPersistent);
begin
  if Source is TSF_CLASS then
  begin
    DefaultValue       := TSF_CLASS(Source).DefaultValue;
    DefaultValueExists := TSF_CLASS(Source).DefaultValueExists;
    Value              := TSF_CLASS(Source).Value;
    VRMLFieldAssignCommon(TVRMLField(Source));
  end else
    inherited;
end;

procedure TSF_CLASS.AssignValue(Source: TVRMLField);
begin
  if Source is TSF_CLASS then
  begin
    inherited;
    Value := TSF_CLASS(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;
}

{$define TSF_CLASS := TSFVec2f}
{$define TSF_STATIC_ITEM := TVector2Single}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec3f}
{$define TSF_STATIC_ITEM := TVector3Single}
IMPLEMENT_SF_CLASS_USING_VECTORS

{$define TSF_CLASS := TSFVec4f}
{$define TSF_STATIC_ITEM := TVector4Single}
IMPLEMENT_SF_CLASS_USING_VECTORS

{ TSFVec2f ------------------------------------------------------------------- }

class function TSFVec2f.VRMLTypeName: string;
begin
  Result := 'SFVec2f';
end;

{ TSFVec3f ------------------------------------------------------------------- }

class function TSFVec3f.VRMLTypeName: string;
begin
  Result := 'SFVec3f';
end;

{ TSFColor ------------------------------------------------------------------- }

class function TSFColor.VRMLTypeName: string;
begin
  Result := 'SFColor';
end;

{ TSFVec4f ------------------------------------------------------------------- }

class function TSFVec4f.VRMLTypeName: string;
begin
  Result := 'SFVec4f';
end;

{ TSFColorRGBA --------------------------------------------------------------- }

class function TSFColorRGBA.VRMLTypeName: string;
begin
  Result := 'SFColorRGBA';
end;

{ TSFBitMask ------------------------------------------------------------ }

constructor TSFBitMask.Create(const AName: string; const AFlagNames: array of string;
  const ANoneString, AAllString: string; const AFlags: array of boolean);
var i: integer;
begin
  inherited Create(AName);

  fFlagNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AFlagNames, fFlagNames);
  for i := 0 to FlagsCount-1 do Flags[i] := AFlags[i];
  fNoneString := ANoneString;
  fAllString := AAllString;

  Assert(NoneString <> '', 'NoneString must be defined for SFBitMask');
end;

destructor TSFBitMask.Destroy;
begin
 fFlagNames.Free;
 inherited;
end;

function TSFBitMask.GetFlags(i: integer): boolean;
begin result := i in fFlags end;
procedure TSFBitMask.SetFlags(i: integer; value: boolean);
begin if value then Include(fFlags, i) else Exclude(fFlags, i) end;

function TSFBitMask.FlagsCount: integer;
begin result := fFlagNames.Count end;
function TSFBitMask.GetFlagNames(i: integer): string;
begin result := fFlagNames[i] end;

procedure TSFBitMask.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);

  procedure InterpretTokenAsFlagName;
  var i: integer;
  begin
   Lexer.CheckTokenIs(vtName, 'bit mask constant');
   i := fFlagNames.IndexOf(Lexer.TokenName);
   if i >= 0 then
    Flags[i] := true else
   if Lexer.TokenName = fAllString then
    fFlags:=[0..FlagsCount-1] else
   if Lexer.TokenName = fNoneString then
    {don't set anything; uwaga: flaga NONE nie powoduje wyczyszczenia innych flag,
    czyli np. ( FLAG_1 | NONE ) znaczy tyle samo co FLAG_1 } else
    raise EVRMLParserError.Create(Lexer,
      'Expected bit mask constant, got '+Lexer.DescribeToken);
  end;

begin
  inherited;

  fFlags:=[];

  if IsClause then Exit;

  if Lexer.Token = vtOpenBracket then
  begin
   repeat
    Lexer.NextToken;
    InterpretTokenAsFlagName;
    Lexer.NextToken;
   until Lexer.Token <> vtBar;
   Lexer.CheckTokenIs(vtCloseBracket);
   Lexer.NextToken;
  end else
  begin
   InterpretTokenAsFlagName;
   Lexer.NextToken;
  end;
end;

function TSFBitMask.AreAllFlags(value: boolean): boolean;
var i: integer;
begin
 for i := 0 to FlagsCount-1 do
  if Flags[i] <> value then exit(false);
 exit(true);
end;

procedure TSFBitMask.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
var i: integer;
    PrecedeWithBar: boolean;
begin
 if AreAllFlags(false) then
  SaveProperties.Write(NoneString) else
 begin
  {zapisywanie do strumienia AllString to taka estetyka - zawsze przeciez
   mozemy wyrazic All flags po prostu zapisujac je wszystkie. }
  if (AllString <> '') and AreAllFlags(true) then
   SaveProperties.Write(AllString) else
  begin
   PrecedeWithBar := false; { pierwszy element nie bedzie poprzedzony '|' }
   SaveProperties.Write('(');
   for i := 0 to FlagsCount-1 do
    if Flags[i] then
    begin
     if PrecedeWithBar then SaveProperties.Write('|') else PrecedeWithBar := true;
     SaveProperties.Write(FlagNames[i]);
    end;
   SaveProperties.Write(')');
  end;
 end;
end;

function TSFBitMask.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFBitMask) and
   (TSFBitMask(SecondValue).FFlagNames.Equals(FFlagNames)) and
   (TSFBitMask(SecondValue).FFlags = FFlags) and
   (TSFBitMask(SecondValue).AllString = AllString) and
   (TSFBitMask(SecondValue).NoneString = NoneString);
end;

procedure TSFBitMask.Assign(Source: TPersistent);
begin
 if Source is TSFBitMask then
 begin
  FAllString  := TSFBitMask(Source).AllString;
  FNoneString := TSFBitMask(Source).NoneString;
  FFlags      := TSFBitMask(Source).FFlags;
  FFlagNames.Assign(TSFBitMask(Source).FFlagNames);
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFBitMask.AssignValue(Source: TVRMLField);
begin
  if Source is TSFBitMask then
  begin
    inherited;
    FFlags := TSFBitMask(Source).FFlags;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFBitMask.VRMLTypeName: string;
begin
  Result := 'SFBitMask';
end;

{ TSFEnum ----------------------------------------------------------------- }

constructor TSFEnum.Create(const AName: string; const AEnumNames: array of string; const AValue: integer);
begin
  inherited Create(AName);

  fEnumNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AEnumNames, fEnumNames);
  Value := AValue;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

destructor TSFEnum.Destroy;
begin
  fEnumNames.Free;
  inherited;
end;

function TSFEnum.GetEnumNames(i: integer): string;
begin result := fEnumNames[i] end;
function TSFEnum.EnumNamesCount: integer;
begin result := fEnumNames.Count end;

procedure TSFEnum.Parse(Lexer: TVRMLLexer; IsClauseAllowed: boolean);
var
  val: integer;
begin
  inherited;
  if IsClause then Exit;

  Lexer.CheckTokenIs(vtName, 'enumerated type constant');
  val := fEnumNames.IndexOf(Lexer.TokenName);
  if val = -1 then
   raise EVRMLParserError.Create(Lexer,
     'Expected enumerated type constant, got '+Lexer.DescribeToken);
  Value := val;
  Lexer.NextToken;
end;

procedure TSFEnum.SaveToStreamValue(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.Write(EnumNames[Value]);
end;

function TSFEnum.EqualsDefaultValue: boolean;
begin
 result := (not IsClause) and DefaultValueExists and (DefaultValue = Value);
end;

function TSFEnum.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TSFEnum) and
   (TSFEnum(SecondValue).FEnumNames.Equals(FEnumNames)) and
   (TSFEnum(SecondValue).Value = Value);
end;

procedure TSFEnum.Assign(Source: TPersistent);
begin
 if Source is TSFEnum then
 begin
  DefaultValue       := TSFEnum(Source).DefaultValue;
  DefaultValueExists := TSFEnum(Source).DefaultValueExists;
  Value              := TSFEnum(Source).Value;
  FEnumNames.Assign(TSFEnum(Source).FEnumNames);
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TSFEnum.AssignValue(Source: TVRMLField);
begin
  if Source is TSFEnum then
  begin
    inherited;
    Value := TSFEnum(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFEnum.VRMLTypeName: string;
begin
  Result := 'SFEnum';
end;

{ multiple value fields ----------------------------------------------------- }

{ Note that because of FPC 2.0.2 bug, code below will not compile
  with FPC 2.0.2 in objfpc mode. For objfpc mode I would have to
  change below Items.Items[I] to Items.ItemsArray^[I],
  i.e. Items property of my dynamic array classes will not work
  correctly in objfpc mode in FPC 2.0.2.
  Fixed in FPC 2.0.3 and 2.1.1 (revision 2911).
}

{$ifdef FPC_OBJFPC}
  {$ifdef VER2_0_2}
    {$fatal This code will not compile with FPC 2.0.2 in objfpc mode}
  {$endif}
{$endif}

{$define IMPLEMENT_MF_CLASS:=
constructor TMF_CLASS.Create(const AName: string;
  const InitialContent: array of TMF_STATIC_ITEM);
begin
  inherited Create(AName);

  Items.AppendArray(InitialContent);

  (* inicjuj DefaultValuesCount, inicjuj tez DefaultValue
     jesli DefaultValuesCount = 1 *)
  case High(InitialContent) + 1 of
    0: DefaultValuesCount := 0;
    1: begin
         DefaultValuesCount := 1;
         DefaultValue := InitialContent[0];
       end;
    else DefaultValuesCount := -1;
  end;
end;

constructor TMF_CLASS.CreateUndefined(const AName: string);
begin
  inherited;

  FItemClass := TMF_CLASS_ITEM;
  RawItems := TMF_DYN_STATIC_ITEM_ARRAY.Create;
  DefaultValuesCount := -1;
end;

function TMF_CLASS.Items: TMF_DYN_STATIC_ITEM_ARRAY;
begin result := TMF_DYN_STATIC_ITEM_ARRAY(RawItems) end;

procedure TMF_CLASS.RawItemsAdd(Item: TVRMLSingleField);
begin
 Items.AppendItem(TMF_CLASS_ITEM(Item).Value);
end;

procedure TMF_CLASS.Assign(Source: TPersistent);
begin
 if Source is TMF_CLASS then
 begin
  DefaultValuesCount := TMF_CLASS(Source).DefaultValuesCount;
  DefaultValue       := TMF_CLASS(Source).DefaultValue;
  Items.Assign(TMF_CLASS(Source).Items);
  VRMLFieldAssignCommon(TVRMLField(Source));
 end else
  inherited;
end;

procedure TMF_CLASS.AssignValue(Source: TVRMLField);
begin
  if Source is TMF_CLASS then
  begin
    inherited;
    Items.Assign(TMF_CLASS(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;
}

{ dla niektorych klas MF nie bedzie mialo znaczenia ktorej wersji
  IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_* uzyjemy.

  Ale dla niektorych typow TMF_STATIC_ITEM operator "=" moze nie byc
  standardowo dostepny (i calkiem slusznie, bo dla tych typow nie zawsze
  chcielibysmy robic dokladne porownanie; TERAZ jednak wlasnie tego chcemy).
  Np. dla typow TVector2/3Single. Dlatego musimy wtedy uzywac metody
  CompareMem. Ale metoda CompareMem tez nie jest zawsze dobra - dla
  stringow na przyklad ta metoda jest bez sensu. Ale dla stringow metoda
  z operatorem "=" ma sens.

  W tej chwili nie ma klasy MF ktora wymagalaby jakiegos jeszcze innego
  traktowania ale nietrudno sobie taka wyobrazic. Nie wszystkie
  typy mozemy przeciez sensownie porownywac operatorem "=" lub CompareMem,
  np. gdybysmy mieli TMFImage.

  Notka: dla klas dla ktorych obie wersje (CompareMem i "=") sa dobre
  uzywam wersji "=" (bo jest bezpieczniejsza na typach).
}

{$define IMPLEMENT_MF_CLASS_USING_EQUALITY_OP:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result := (not IsClause) and
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
     (DefaultValue = Items.Items[0]));
end;

function TMF_CLASS.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not (TMF_CLASS(SecondValue).Items.Items[I] = Items.Items[I]) then
    Exit(false);
end;
}

{$define IMPLEMENT_MF_CLASS_USING_COMPARE_MEM:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result:= (not IsClause) and
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
      CompareMem(@DefaultValue, Items.Pointers[0], SizeOf(TMF_STATIC_ITEM)) );
end;

function TMF_CLASS.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not CompareMem(@TMF_CLASS(SecondValue).Items.Items[I], @Items.Items[I],
     SizeOf(TMF_STATIC_ITEM)) then
    Exit(false);
end;
}

{$define IMPLEMENT_MF_CLASS_USING_VECTORS:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result := (not IsClause) and
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
      VectorsPerfectlyEqual(DefaultValue, Items.Items[0]) );
end;

function TMF_CLASS.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not VectorsEqual(TMF_CLASS(SecondValue).Items.Items[I], Items.Items[I],
     EqualityEpsilon) then
    Exit(false);
end;

function TMF_CLASS.RawItemToString(ItemNum: Integer): string;
begin
  Result := VectorToRawStr(Items.Items[ItemNum])
end;

procedure TMF_CLASS.AssignLerp(const A: Single; Value1, Value2: TMF_CLASS);
var
  I: Integer;
begin
  Value1.CheckCountEqual(Value2);
  Items.Count := Value1.Items.Count;

  for I := 0 to Items.Count - 1 do
    Items.Items[I] := VLerp(A, Value1.Items.Items[I], Value2.Items.Items[I]);
end;
}

{$define IMPLEMENT_MF_CLASS_USING_FLOATS_EQUAL:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
  result := (not IsClause) and
    ((DefaultValuesCount = 0) and (Count = 0)) or
    ((DefaultValuesCount = 1) and (Count = 1) and
     (DefaultValue = Items.Items[0]) );
end;

function TMF_CLASS.Equals(SecondValue: TVRMLField;
  const EqualityEpsilon: Single): boolean;
var
  I: Integer;
begin
 Result := (inherited Equals(SecondValue, EqualityEpsilon)) and
   (SecondValue is TMF_CLASS);

 if Result then
  for I := 0 to Items.Count - 1 do
   if not FloatsEqual(TMF_CLASS(SecondValue).Items.Items[I], Items.Items[I],
     EqualityEpsilon) then
    Exit(false);
end;
}

{$define TMF_CLASS := TMFBool}
{$define TMF_STATIC_ITEM := boolean}
{$define TMF_CLASS_ITEM := TSFBool}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynBooleanArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_EQUALITY_OP

{$define TMF_CLASS := TMFLong}
{$define TMF_STATIC_ITEM := Longint}
{$define TMF_CLASS_ITEM := TSFLong}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynLongintArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_EQUALITY_OP

{$define TMF_CLASS := TMFVec2f}
{$define TMF_STATIC_ITEM := TVector2Single}
{$define TMF_CLASS_ITEM := TSFVec2f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector2SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec3f}
{$define TMF_STATIC_ITEM := TVector3Single}
{$define TMF_CLASS_ITEM := TSFVec3f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector3SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFVec4f}
{$define TMF_STATIC_ITEM := TVector4Single}
{$define TMF_CLASS_ITEM := TSFVec4f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector4SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFRotation}
{$define TMF_STATIC_ITEM := TVector4Single}
{$define TMF_CLASS_ITEM := TSFRotation}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector4SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_VECTORS

{$define TMF_CLASS := TMFFloat}
{$define TMF_STATIC_ITEM := Single}
{$define TMF_CLASS_ITEM := TSFFloat}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynSingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_FLOATS_EQUAL

{$define TMF_CLASS := TMFDouble}
{$define TMF_STATIC_ITEM := Double}
{$define TMF_CLASS_ITEM := TSFDouble}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynDoubleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_FLOATS_EQUAL

{$define TMF_CLASS := TMFString}
{$define TMF_STATIC_ITEM := string}
{$define TMF_CLASS_ITEM := TSFString}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynStringArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_USING_EQUALITY_OP

{ TMFBool ------------------------------------------------------------------ }

function TMFBool.RawItemToString(ItemNum: integer): string;
begin
  if Items.Items[ItemNum] then
    Result := VRMLKeywords[vkTrue] else
    Result := VRMLKeywords[vkFalse];
end;

class function TMFBool.VRMLTypeName: string;
begin
  Result := 'MFBool';
end;

{ TMFLong -------------------------------------------------------------------- }

constructor TMFLong.CreateMFLong(const AName: string; const InitialContent: array of Longint;
 const ASaveToStreamLineUptoNegative: boolean);
begin
  Create(AName, InitialContent);
  SaveToStreamLineUptoNegative := ASaveToStreamLineUptoNegative;
end;

function TMFLong.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
 if SaveToStreamLineUptoNegative then
  result := Items.Items[ItemNum] < 0 else
  result := inherited;
end;

function TMFLong.RawItemToString(ItemNum: integer): string;
begin result := IntToStr(Items.Items[ItemNum]) end;

class function TMFLong.VRMLTypeName: string;
begin
  Result := 'MFLong';
end;

{ TMFInt32 ------------------------------------------------------------------- }

class function TMFInt32.VRMLTypeName: string;
begin
  Result := 'MFInt32';
end;

{ TMFVec2f ------------------------------------------------------------------- }

class function TMFVec2f.VRMLTypeName: string;
begin
  Result := 'MFVec2f';
end;

{ TMFVec3f ------------------------------------------------------------------- }

class function TMFVec3f.VRMLTypeName: string;
begin
  Result := 'MFVec3f';
end;

{ TMFColor ------------------------------------------------------------------- }

class function TMFColor.VRMLTypeName: string;
begin
  Result := 'MFColor';
end;

{ TMFVec4f ------------------------------------------------------------------- }

class function TMFVec4f.VRMLTypeName: string;
begin
  Result := 'MFVec4f';
end;

{ TMFColorRGBA ------------------------------------------------------------------- }

class function TMFColorRGBA.VRMLTypeName: string;
begin
  Result := 'MFColorRGBA';
end;

{ TMFRotation ---------------------------------------------------------------- }

class function TMFRotation.VRMLTypeName: string;
begin
  Result := 'MFRotation';
end;

{ TMFFloat ------------------------------------------------------------------- }

function TMFFloat.RawItemToString(ItemNum: integer): string;
begin result := FloatToRawStr(Items.Items[ItemNum]) end;

procedure TMFFloat.AssignLerp(const A: Single; Value1, Value2: TMFFloat);
var
  I: Integer;
begin
 Value1.CheckCountEqual(Value2);
 Items.Count := Value1.Items.Count;

 for I := 0 to Items.Count - 1 do
  Items.Items[I] := Lerp(A, Value1.Items.Items[I], Value2.Items.Items[I]);
end;

class function TMFFloat.VRMLTypeName: string;
begin
  Result := 'MFFloat';
end;

{ TMFDouble -------------------------------------------------------------------- }

function TMFDouble.RawItemToString(ItemNum: integer): string;
begin result := FloatToRawStr(Items.Items[ItemNum]) end;

procedure TMFDouble.AssignLerp(const A: Double; Value1, Value2: TMFDouble);
var
  I: Integer;
begin
 Value1.CheckCountEqual(Value2);
 Items.Count := Value1.Items.Count;

 for I := 0 to Items.Count - 1 do
  Items.Items[I] := Lerp(A, Value1.Items.Items[I], Value2.Items.Items[I]);
end;

class function TMFDouble.VRMLTypeName: string;
begin
  Result := 'MFDouble';
end;

{ TMFTime -------------------------------------------------------------------- }

class function TMFTime.VRMLTypeName: string;
begin
  Result := 'MFTime';
end;

{ TMFString ------------------------------------------------------------------ }

function TMFString.RawItemToString(ItemNum: integer): string;
begin result := StringToVRMLStringToken(Items.Items[ItemNum]) end;

class function TMFString.VRMLTypeName: string;
begin
  Result := 'MFString';
end;

{ TVRMLFieldsManager --------------------------------------------------------- }

constructor TVRMLFieldsManager.Create;
begin
  inherited;
  Registered := TStringList.Create;
  { All VRML names are case-sensitive. }
  Registered.CaseSensitive := true;
end;

destructor TVRMLFieldsManager.Destroy;
begin
  FreeAndNil(Registered);
  inherited;
end;

procedure TVRMLFieldsManager.RegisterClass(AClass: TVRMLFieldClass);
begin
  Registered.AddObject(AClass.VRMLTypeName, TObject(AClass));
end;

procedure TVRMLFieldsManager.RegisterClasses(
  const Classes: array of TVRMLFieldClass);
var
  I: Integer;
begin
  for I := 0 to High(Classes) do
    RegisterClass(Classes[I]);
end;

function TVRMLFieldsManager.FieldTypeNameToClass(
  const TypeName: string): TVRMLFieldClass;
var
  I: Integer;
begin
  I := Registered.IndexOf(TypeName);
  if I <> -1 then
    Result := TVRMLFieldClass(Registered.Objects[I]) else
    Result := nil;
end;

initialization
  VRMLFieldsManager := TVRMLFieldsManager.Create;

  VRMLFieldsManager.RegisterClasses([
    TSFBitMask,
    TSFEnum,
    TSFBool,     TMFBool,
    TSFFloat,    TMFFloat,
    TSFImage,
    TSFLong,     TMFLong,
    TSFInt32,    TMFInt32,
    TSFMatrix,
    TSFRotation, TMFRotation,
    TSFString,   TMFString,
    TSFDouble,   TMFDouble,
    TSFTime,     TMFTime,
    TSFVec2f,    TMFVec2f,
    TSFVec3f,    TMFVec3f,
    TSFColor,    TMFColor,
    TSFVec4f,    TMFVec4f,
    TSFColorRGBA,TMFColorRGBA
    ]);
finalization
  FreeAndNil(VRMLFieldsManager);
end.
