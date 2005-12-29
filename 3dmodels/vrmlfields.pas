{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(VRML fields -- @link(TVRMLField) class and descendants.) }

unit VRMLFields;

interface

uses VectorMath, Classes, SysUtils, VRMLLexer, KambiUtils, KambiClassUtils,
  Images;

{$define read_interface}

const
  { IndentIncrement is string or char. It's used by SaveToStream }
  IndentIncrement = CharTab;

type

{ fields base classes ------------------------------------------------------ }

  TVRMLField = class
  private
    fName: string;
  protected
    { kazda klasa musi to pokryc; SaveToStream zapisuje
      Indent, Name, ' ', potem wywoluje SaveToStreamValue, potem zapisuje nl. }
    procedure SaveToStreamValue(Stream: TStream;
      const Indent: string); virtual; abstract;
  public
    { spoza tego modulu nigdy nie tworz obiektow tej klasy z Name = '',
      tzn. zawsze Name musi byc zdefiniowane.
      (w tym module mozemy gdzieniegdzie uzywac wewnetrznie takich obiektow,
      np. pozwolilo to nam bardzo wygodnie zapisac TMultiField.Parse.) }
    property Name: string read fName;
    constructor Create(const AName: string);
    { Parse : init Self properties from Lexer. Must be redefined in each
      field class. }
    procedure Parse(Lexer: TVRMLLexer); virtual; abstract;
    { O ile not EqualsDefaultValue to kazde pole bedzie zapisane jako jedna lub
      wiecej linii.
      (notka wewnetrzna dla implementacji tego modulu - nie probuj nigdy
      zapisac pol ktorych Name = '') }
    procedure SaveToStream(Stream: TStream; const Indent: string);
    { zwraca zawsze false w tej klasie. Mozesz to przedefiniowac w podklasach
      aby SaveToStream nie zapisywalo do strumienia pol o wartosci domyslnej. }
    function EqualsDefaultValue: boolean; virtual;
  end;

  TObjectsListItem_2 = TVRMLField;
  {$I objectslist_2.inc}
  TVRMLFieldsListBase = TObjectsList_2;

  TVRMLFieldsList = class(TVRMLFieldsListBase)
  private
    function GetByName(const AName: string): TVRMLField;
  public
    {ByName to wygodne property pozwalajace operowac na polach
     podajac ich nazwe. Uwaga - jezeli nazwa nie istnieje - wyjatek
     Exception. }
    property ByName[const AName: string]:TVRMLField read GetByName;
    {NameIndex. Zwraca -1 jezeli nie znalazl.}
    function NameIndex(const AName: string): integer;
  end;

  TVRMLSingleField = class(TVRMLField)
  end;
  TVRMLSingleFieldClass = class of TVRMLSingleField;

  { TVRMLSimpleSingleField to takie pole SFField ktore mozemy utworzyc
    prostym CreateUndefined('nazwa') i potem mozemy zainicjowac je robiac Parse.
    W rezultacie moga miec wirtualny konstruktor CreateUndefined(string), co jest
    nieraz znacznym ulatwieniem w implementacji.
    Wiekszosc pol SF jest wlasnie taka - wyjatkami sa SFEnum i SFBitMask
    ktore aby moc sie sparsowac musza znac swoje EnumNames /
    FlagNames+NoneString+AllString. }
  TVRMLSimpleSingleField = class(TVRMLSingleField)
    constructor CreateUndefined(const AName: string); virtual;
  end;
  TVRMLSimpleSingleFieldClass = class of TVRMLSimpleSingleField;

  TObjectsListItem_1 = TVRMLSingleField;
  {$I ObjectsList_1.inc}
  TVRMLSingleFieldsList = TObjectsList_1;

  {pamietaj - lista MF fields moze miec zero elementow !
   MultFields w destruktorze zwalniaja wszystkie swoje RawItems.
   W kazdym typie potomnym TVRMLMultField MUSISZ zdefiniowac fItemClass
   w konstruktorze (inaczej bedzie = nil i bedzie error).

   Notka o wydajnosci implementacji : wydaje sie pociagajacym uproszczeniem
   zeby zapisac TVRMLMultField jako opakowanie na liste TVRMLSingleFieldsList.
   Ale takie rozwiazanie spowodowaloby ze dla dlugich pol MFField (a takie
   bedziemy czesto dostawac, tysiace vertexow w Coordinate3 to nic niezwyklego)
   bedziemy strasznie rozrzucali po pamieci duzo malenkich rekordow (b. duza
   fragmentacja pamieci, kiepska wydajnosc jej zarzadzaniem) no i, co bardzo
   wazne, nie bedziemy mogli uzywac vertex arrays OpenGL'a na tablicach
   vertexow i normali i texcoords. Szczegolnie ta druga wada moze powodowac
   bardzo duza strate szybkosci renderowania wiec nie mozemy sobie na to pozwolic.
   Tym bardziej ze gdy ladnie sobie wszystko zapiszemy uzywanie TDynArray
   moze nie byc wcale takie straszne.

   Co trzeba zrobic w podklasach aby zaimplementowac konkretne MFField ?
   - w Create zainicjowac fItemClass, utworzyc RawItems
   - pokryc RawItemsAdd
   - jezeli not (ItemClass is TVRMLSimpleSingleField) to musisz pokryc
     CreateItemBeforeParse
   - nie jest to zadnym wymaganiem ale zazwyczaj bedzie wygodnie jesli
     konstruktor bedzie pobieral jako argument array of Typ aby zainicjowac
     od razu swoja tablice. }
  TVRMLMultField = class(TVRMLField)
  protected
    fItemClass: TVRMLSingleFieldClass;
    { CreateItemBeforeParse ma za zadanie utworzyc nowy obiekt klasy
      ItemClass ktorego wartosci moga byc niezdefiniwane bo za chwile
      zainicjujemy go wywolujac jego metode Parse. W tym wlasnie
      miejscu przydaje sie nam wirtualny konstruktor klasy
      TVRMLSimpleSingleField - domyslna implementacja CreateItemBeforeParse
      wywoluje po prostu TVRMLSimpleSingleField(ItemClass).CreateUndefined
      co zadziala dobrze o ole tylko ItemClass jest podklasa
      TVRMLSimpleSingleField. Jezeli nie - to musisz przedefiniowac metode
      CreateItemBeforeParse. }
    function CreateItemBeforeParse: TVRMLSingleField; virtual;
    { musisz pokryc ta metode w podklasie, powinna ona dodawac do
      RawItems na koncu Item (ktory na pewno jest klasy ItemClass).
      Pamietaj ze musisz sobie odpowiednio skopiowac zawartosc Item
      (a nie tylko jakis wskaznik do niej) bo obiekt Item moze zostac
      niedlugo zniszczony. }
    procedure RawItemsAdd(Item: TVRMLSingleField); virtual abstract;
  protected
    { nie ma potrzeby definiowania SaveToStreamValue w podklasach,
        zdefiniuj tylko RawItemToString(i) ktore zamienia RawItems[i]
        na string ktory moze byc zapisany jako wartosc tego pola w VRMLu.
        W niniejszej klasie zajmujemy sie wszystkim.
      Jezeli chcesz, mozesz w podklasie pokryc implementacje
        SaveToStreamDoNewLineAfterRawItem - w tej klasie zawsze odpowiada
        true. Ale zwroc uwage ze wyniki zwracane przez
        SaveToStreamDoNewLineAfterRawItem moga byc niekiedy ignorowane
        (czasami po prostu w tej klasie wiemy ze NA PEWNO tak jak robimy
        bedzie ladniej wygladalo; bo tak czy siak, tu chodzi tylko o estetyke) }
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    function RawItemToString(ItemNum: integer): string; virtual; abstract;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; virtual;
  public
    { kazda podklasa musi w konstruktorze utworzyc sobie ta tablice
      (w destruktorze my samy juz zajmiemy sie zwalnianiem tej tablicy) }
    RawItems: TDynArrayBase;
    { po prostu RawItems.Count }
    function Count: integer;
    { wszystkie elementy jakie beda trafiac do RawItemsAdd beda tej klasy.
      Nie jest tu zdefiniowana zaleznosc miedzy elementami tej klasy a
      elementami tablicy RawItems - musisz w kazdej podklasie okreslic
      ta zaleznosc definiujac RawItemsAdd. }
    property ItemClass: TVRMLSingleFieldClass read fItemClass;
    { nie ma potrzeby definiowania Parse w zadnej podklasie pola MF.
      Tutejsze Parse dziala dla kazdego pola typu MF, uzywajac Parse
      klasy ItemClass. }
    procedure Parse(Lexer: TVRMLLexer); override;

    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

{ single value fields ----------------------------------------------------- }

  TSFBitMask = class(TVRMLSingleField)
  private
    fAllString, fNoneString: string;
    fFlagNames: TStringList;
    {specyfikacja VRML'a 1.0 gwarantuje ze SFBitMask ma 32 lub mniej flag,
     zreszta zdefiniowane pola nie wychodza ponad 3 flagi.
     W VRML'u 97 w ogole nie ma typu pola SFBitMask.}
    fFlags: set of 0..31;
    function GetFlags(i: integer): boolean;
    procedure SetFlags(i: integer; value: boolean);
    function GetFlagNames(i: integer): string;
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
  public
    {Flags okresla wartosci wszystkich flag - pytaj go o liczby z przedzialu
     0..FlagsCount-1}
    property Flags[i: integer]:boolean read GetFlags write SetFlags;
    function FlagsCount: integer;
    property FlagNames[i: integer]:string read GetFlagNames;

    {AllString i NoneString : specjalne flagi ktorych uzycie powoduje
     odpowiednio zaznaczenie wszystkich flag i nie zaznaczanie zadnej.
     AllString istnieje tylko jezeli jest podana wartosc <> '',
     NoneString musi byc zawsze <> '' (zawsze musi byc podany;
     to nam pozwala myslec bardziej prosto o tych flagach -
     kazda ich postac jest dozwolona, kazda mozna odczytac i zapisac
     do pliku).
     Nie ma sensu mieszania ich z innymi flagami (ALL | ze wszystkim daje
     ciagle ALL, a NONE z czymkolwiek daje to cokolwiek) ale jest to
     dopuszczalne skladniowo (tzn. parser i lekser to przyjma),
     ALL jest zazwyczaj tylko wygodnym skrotem ale istnienie flagi NONE
     ma zasadnicze znaczenie : poniewaz SFBitMask zawsze musi bc zapisane
     jako przynajmniej jedna flaga, to jedynym sposobem aby zapisanie
     wszystkich flag = false jest uzycie NONE.  }
    property AllString: string read fAllString;
    property NoneString: string read fNoneString;

    procedure Parse(Lexer: TVRMLLexer); override;

    { zwraca true jesli wszystkie flagi sa = value }
    function AreAllFlags(value: boolean): boolean;

    { pamietaj - tablica AFLagNames i AFlags (poczatkowa wartosc Flags)
      musza miec tyle samo elementow, ew. AFlags moze byc dluzsza (dodatkowe
      elementy beda ignorowane) }
    constructor Create(const AName: string; const AFlagNames: array of string;
      const ANoneString, AAllString: string; const AFlags: array of boolean);
    destructor Destroy; override;
  end;

  TSFBool = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    DefaultValue: boolean;
    DefaultValueExists: boolean;
  public
    Value: boolean;
    constructor Create(const AName: string; const AValue: boolean);
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFColor = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    DefaultValue: TVector3Single;
    DefaultValueExists: boolean;
  public
    Value: TVector3Single;
    constructor Create(const AName: string; const AValue: TVector3Single);
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFEnum = class(TVRMLSingleField)
  private
    fEnumNames: TStringList;
    function GetEnumNames(i: integer): string;
    DefaultValue: integer;
    DefaultValueExists: boolean;
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
  public
    Value: integer; { wartosc z 0..EnumCount-1; domyslnie 0 }
    property EnumNames[i: integer]:string read GetEnumNames;
    function EnumNamesCount: integer;
    procedure Parse(Lexer: TVRMLLexer); override;
    constructor Create(const AName: string;
      const AEnumNames: array of string; const AValue: integer);
    destructor Destroy; override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFFloat = class(TVRMLSimpleSingleField)
  private
    FMustBeNonnegative: boolean;
    FValue: Single;
    DefaultValue: Single;
    DefaultValueExists: boolean;
    procedure SetValue(const AValue: Single);
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
  public
    property Value: Single read FValue write SetValue;
    { jezeli true to przy probie ustawienia Value na X gdzie X < 0
      ustawi Value := -X (a wiec NIE robi clamp do 0 w rodzaju Value := Max(0, X)
      tylko Value := Abs(X); to jest cos dobrego dla np. Sphere.FdRadius). }
    property MustBeNonnegative: boolean read FMustBeNonnegative; { = false }
    constructor Create(const AName: string; const AValue: Single); overload;
    constructor Create(const AName: string; const AValue: Single; AMustBeNonnegative: boolean); overload;
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFImage = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
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

    destructor Destroy; override;

    procedure Parse(Lexer: TVRMLLexer); override;
  end;

  TSFLong = class(TVRMLSimpleSingleField)
  private
    FMustBeNonnegative: boolean;
    FValue: Longint;
    DefaultValue: Longint;
    DefaultValueExists: boolean;
    procedure SetValue(const AValue: Longint);
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
  public
    property Value: Longint read FValue write SetValue;
    { komentarz - jak dla TSFFloat.MustBeNonnegative }
    property MustBeNonnegative: boolean read FMustBeNonnegative; { = false }
    constructor Create(const AName: string; const AValue: Longint); overload;
    constructor Create(const AName: string; const AValue: Longint; AMustBeNonnegative: boolean); overload;
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFMatrix = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
  public
    Matrix: TMatrix4Single;
    constructor Create(const AName: string; const AMatrix: TMatrix4Single);
    procedure Parse(Lexer: TVRMLLexer); override;
  end;

  TSFRotation = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    procedure SetValue(const AValue: TVector4Single);
  public
    Axis: TVector3Single;
    RotationRad: Single;
    property Value: TVector4Single write SetValue;
    constructor Create(const AName: string; const AnAxis: TVector3Single; const ARotationRad: Single);
    procedure Parse(Lexer: TVRMLLexer); override;
    { rotate point pt around self }
    function RotatedPoint(const pt: TVector3Single): TVector3Single;
  end;

  TSFString = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    DefaultValue: string;
    DefaultValueExists: boolean;
  public
    Value: string;
    constructor Create(const AName: string; const AValue: string);
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFVec2f = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    DefaultValue: TVector2Single;
    DefaultValueExists: boolean;
  public
    Value: TVector2Single;
    constructor Create(const AName: string; const AValue: TVector2Single);
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

  TSFVec3f = class(TVRMLSimpleSingleField)
  protected
    procedure SaveToStreamValue(Stream: TStream; const Indent: string); override;
    DefaultValue: TVector3Single;
    DefaultValueExists: boolean;
  public
    Value: TVector3Single;
    constructor Create(const AName: string; const AValue: TVector3Single);
    procedure Parse(Lexer: TVRMLLexer); override;
    function EqualsDefaultValue: boolean; override;
  end;

{ multiple value fields ---------------------------------------------------
  (wewnetrzny komentarz : pola DefaultValue* : pole DefaultValuesCount
     moze miec w tej chwili trzy wartosci : -1 (nie ma (nie jest znana) domyslnej
     wartosci dla pola), 0 (domyslna wartosc pola to 0 elementow), 1 (domylna
     wartosc pola to 1 element o wartosci DefaultValue).
   Pola multi nie maja CreateUndefined - gdyby mialy to w CreateUndefined
     ustawialibysmy DefaultValuesCount na -1. A tak cala inicjacje pol
     DefaultValue* robimy w normalnym konstruktorze Create.
  )
}

  TMFColor = class(TVRMLMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector3Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynVector3SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of TVector3Single);
    function EqualsDefaultValue: boolean; override;
  end;

  TMFLong = class(TVRMLMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Longint;
  protected
    function RawItemToString(ItemNum: integer): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    { jesli SaveToStreamLineUptoMinusOne to w tej klasie przedefiniujemy
      SaveToStreamDoNewLineAfterRawItem zeby odpowiadal true tylko gdy
      indeksujemy liczby ujemne. W ten sposob mozesz sprawic ze np.
      IndexedFaceSet.coordIndex sa ladnie wypisywane. }
    SaveToStreamLineUptoNegative: boolean; { = false }

    function Items: TDynLongintArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of Longint);
    constructor CreateMFLong(const AName: string; const InitialContent: array of Longint;
      const ASaveToStreamLineUptoNegative: boolean);
    function EqualsDefaultValue: boolean; override;
  end;

  TMFVec2f = class(TVRMLMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector2Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynVector2SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of TVector2Single);
    function EqualsDefaultValue: boolean; override;
  end;

  TMFVec3f = class(TVRMLMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: TVector3Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynVector3SingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of TVector3Single);
    function EqualsDefaultValue: boolean; override;
  end;

  TMFFloat = class(TVRMLMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: Single;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynSingleArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of Single);
    function EqualsDefaultValue: boolean; override;
  end;

  TMFString = class(TVRMLMultField)
  private
    DefaultValuesCount: integer;
    DefaultValue: string;
  protected
    function RawItemToString(ItemNum: integer): string; override;
  public
    function Items: TDynStringArray;
    procedure RawItemsAdd(Item: TVRMLSingleField); override;
    constructor Create(const AName: string; const InitialContent: array of string);
    function EqualsDefaultValue: boolean; override;
  end;

const
  { nie mozesz zmieniac ponizszych stalych, ich definicja jako wlasnie takie
    jest przeciez czescia jezyka VRMLa. Ale mozesz uzywac w wielu miejscach
    ponizszych stalych aby zapisywac cos elegancko (i uniknac potencjalnych
    pomylek przy wpisywaniu wartosci ponizszych stalych) }
  StdVRMLCamPos: TVector3Single = (0, 0, 1);
  StdVRMLCamDir: TVector3Single = (0, 0, -1);
  StdVRMLCamUp: TVector3Single = (0, 1, 0);

{ Zamien CamDir i CamUp na orientation VRMLa 1.0.
  Orientation VRMLa wyraza CamDir i CamUp podajac wektor 4 elementowy
  (SFRotation) ktorego pierwsze 3 pola to Axis a czwarte pole to Angle.
  Obroc standardowe Dir i Up VRMLa w/g Axis o kat Angle a otrzymasz
  CamDir i CamUp. Zadaniem jest wyliczyc wlasnie takie Orientation dla
  zadanych juz CamDir i CamUp. Podane CamDir / Up musza byc prostopadle
  i niezerowe, ich dlugosci sa bez znaczenia. }
function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single; overload;
procedure CamDirUp2Orient(CamDir, CamUp: TVector3Single;
  var OrientAxis: TVector3Single; var OrientRadAngle: Single); overload;

{$undef read_interface}

implementation

uses Math;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TVRMLField ------------------------------------------------------------- }

constructor TVRMLField.Create(const AName: string);
begin
 inherited Create;
 fName := AName;
end;

procedure TVRMLField.SaveToStream(Stream: TStream; const Indent: string);
begin
 Assert(Name <> '', 'VRML field name must be defined to allow saving field to stream');
 if not EqualsDefaultValue then
 begin
  WriteStr(Stream, Indent +Name +' ');
  SaveToStreamValue(Stream, Indent);
  WriteStr(Stream, nl);
 end;
end;

function TVRMLField.EqualsDefaultValue: boolean;
begin result := false end;

{ TVRMLFieldsList ------------------------------------------------------------- }

function TVRMLFieldsList.NameIndex(const AName: string): integer;
begin
 for result := 0 to Count-1 do
  if Items[result].Name = AName then exit;
 result := -1;
end;

function TVRMLFieldsList.GetByName(const AName: string): TVRMLField;
var i: integer;
begin
 i := NameIndex(AName);
 if i >= 0 then
  result := Items[i] else
  raise Exception.Create('Field name '+AName+' not found');
end;

{ TVRMLSimpleSingleField ----------------------------------------------------- }

constructor TVRMLSimpleSingleField.CreateUndefined(const AName: string);
begin
 Create(AName);
end;

{ TVRMLMultField ---------------------------------------------------------- }

constructor TVRMLMultField.Create(const AName: string);
begin
 inherited Create(AName);
end;

destructor TVRMLMultField.Destroy;
begin
 RawItems.Free;
 inherited;
end;

function TVRMLMultField.Count: integer;
begin result := RawItems.Count end;

function TVRMLMultField.CreateItemBeforeParse: TVRMLSingleField;
begin
 result := TVRMLSimpleSingleFieldClass(ItemClass).CreateUndefined('');
end;

procedure TVRMLMultField.Parse(Lexer: TVRMLLexer);
var SingleItem: TVRMLSingleField;
begin
 RawItems.SetLength(0);
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
    SingleItem.Parse(Lexer);
    RawItemsAdd(SingleItem);

    if Lexer.Token = vtCloseSqBracket then break;

    Lexer.ChecktokenIs(vtComma);
    Lexer.NextToken;
   end;
   {powyzsza petla obejmuje przypadek 0-elementowej listy [] i nie dopuszcza
    0-elementowej listy postaci [,] zgodnie ze specyfikacja. Natomiast gdy
    na liscie jest 1 lub wiecej elementow za ostatnim elementem przecinek
    jest dozwolony choc niewymagany. Wszystko zgodnie ze specyfikacja VRML'a.}

   Lexer.NextToken;
  end else
  begin
   {one single field - not enclosed in [] brackets}
   SingleItem.Parse(Lexer);
   RawItemsAdd(SingleItem);
  end;

 finally
  FreeAndNil(SingleItem);
  RawItems.AllowedCapacityOverflow := 4;
 end;
end;

procedure TVRMLMultField.SaveToStreamValue(Stream: TStream; const Indent: string);
var i: integer;
    WriteIndentNextTime: boolean;
begin
 { kod ogolny generowalby poprawne wartosci takze dla RawItems.Count = 0 i 1.
   Ale zalatwiam je specjalnym kodem dla estetyki. }
 if RawItems.Count = 0 then
  WriteStr(Stream, '[]') else
 if RawItems.Count = 1 then
  WriteStr(Stream, RawItemToString(0)) else
 begin
  WriteStr(Stream, '[' +nl);
  WriteIndentNextTime := true;
  for i := 0 to RawItems.Count-1 do
  begin
   if WriteIndentNextTime then WriteStr(Stream, Indent +IndentIncrement);
   WriteStr(Stream, RawItemToString(i) +', ');
   {za ostatnim elementem listy zawsze piszemy newline,
    bez wzgledu na wynik SaveToStreamDoNewLineAfterRawItem}
   if (i = RawItems.Count-1) or SaveToStreamDoNewLineAfterRawItem(i) then
    begin WriteStr(Stream, nl); WriteIndentNextTime := true end else
    WriteIndentNextTime := false;
  end;
  WriteStr(Stream, Indent +']');
 end;
end;

function TVRMLMultField.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
 result := true;
end;

{ --------------------------------------------------------------------------
  all fields names (lines below are convenient to expand with regular expr.) :
SFBitMask
SFBool
SFColor
SFEnum
SFFloat
SFImage
SFLong
SFMatrix
SFRotation
SFString
SFVec2f
SFVec3f
MFColor
MFLong
MFVec2f
MFVec3f
MFFloat
MFString
}

{ simple helpful parsing functions ---------------------------------------- }

function ParseFloat(Lexer: TVRMLLexer): Single;
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

{ single value fields (simple ones) ---------------------------------------- }

constructor TSFBool.Create(const AName: string; const AValue: boolean);
begin
 CreateUndefined(AName);
 Value := AValue;
 DefaultValue := AValue;
 DefaultValueExists := true;
end;

procedure TSFBool.Parse(Lexer: TVRMLLexer);
const SBoolExpected = 'boolean constant (TRUE, FALSE, 0 or 1)';
begin
 Lexer.CheckTokenIs([vtKeyword, vtInteger], SBoolExpected);
 if Lexer.Token = vtKeyword then
 begin
  if Lexer.TokenKeyword = vkTrue then Value := true else
   if Lexer.TokenKeyword = vkFalse then Value := false else
    raise EVRMLParserError.Create(Lexer, 'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
 end else
 begin
  if Lexer.TokenInteger = 1 then Value := true else
   if Lexer.TokenInteger = 0 then Value := false else
    raise EVRMLParserError.Create(Lexer, 'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
 end;
 Lexer.NextToken;
end;

procedure TSFBool.SaveToStreamValue(Stream: TStream; const Indent: string);
begin
 if Value then WriteStr(Stream, VRMLKeywords[vkTrue]) else
               WriteStr(Stream, VRMLKeywords[vkFalse])
end;

function TSFBool.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue = Value)
end;

constructor TSFColor.Create(const AName: string; const AValue: TVector3Single);
begin
 CreateUndefined(AName);
 Value := AValue;
 DefaultValue := AValue;
 DefaultValueExists := true;
end;

procedure TSFColor.Parse(Lexer: TVRMLLexer);
begin ParseVector(Value, Lexer); end;

procedure TSFColor.SaveToStreamValue(Stream: TStream; const Indent: string);
begin WriteStr(Stream, VectorToRawStr(Value)); end;

function TSFColor.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue[0] = Value[0])
                              and (DefaultValue[1] = Value[1])
                              and (DefaultValue[2] = Value[2]);
end;

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
 CreateUndefined(AName);
 FMustBeNonnegative := AMustBeNonnegative;
 Value := AValue;
 DefaultValue := Value; { DefaultValue := Value, nie AValue, zeby SetValue moglo ew. zmienic Value }
 DefaultValueExists := true;
end;

procedure TSFFloat.Parse(Lexer: TVRMLLexer);
begin Value := ParseFloat(Lexer); end;

procedure TSFFloat.SaveToStreamValue(Stream: TStream; const Indent: string);
begin WriteStr(Stream, FloatToRawStr(Value)); end;

function TSFFloat.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue = Value)
end;

constructor TSFImage.Create(const AName: string; const AValue: TImage);
begin
 CreateUndefined(AName);
 if AValue = nil then
  Value := TRGBImage.Create else
  Value := AValue;
end;

destructor TSFImage.Destroy;
begin
 FreeAndNil(Value);
 inherited;
end;

procedure TSFImage.Parse(Lexer: TVRMLLexer);

  procedure ReplaceValue(NewValue: TImage);
  begin
   FreeAndNil(Value);
   Value := NewValue;
  end;

var w, h, comp, pixel: LongWord;
    i: Cardinal;
    RGBPixels: PArray_Vector3Byte;
    AlphaPixels: PArray_Vector4Byte;
begin
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

 { sorry - we convert here 1 and 2 components to 3 and 4 (that is,
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
        RGBPixels[i, 0] := pixel and $FF;
        RGBPixels[i, 1] := pixel and $FF;
        RGBPixels[i, 2] := pixel and $FF;
       end;
      end;
   2: begin
       ReplaceValue(TAlphaImage.Create(w, h));
       AlphaPixels := PArray_Vector4Byte(Value.RawPixels);
       for i := 0 to w*h-1 do
       begin
        pixel := ParseLongWord(Lexer);
        AlphaPixels[i, 0] := (pixel shr 8) and $FF;
        AlphaPixels[i, 1] := (pixel shr 8) and $FF;
        AlphaPixels[i, 2] := (pixel shr 8) and $FF;
        AlphaPixels[i, 3] := pixel and $FF;
       end;
      end;
   3: begin
       ReplaceValue(TRGBImage.Create(w, h));
       RGBPixels := PArray_Vector3Byte(Value.RawPixels);
       for i := 0 to w*h-1 do
       begin
        pixel := ParseLongWord(Lexer);
        RGBPixels[i, 0] := (pixel shr 16) and $FF;
        RGBPixels[i, 1] := (pixel shr 8) and $FF;
        RGBPixels[i, 2] := pixel and $FF;
       end;
      end;
   4: begin
       ReplaceValue(TAlphaImage.Create(w, h));
       AlphaPixels := PArray_Vector4Byte(Value.RawPixels);
       for i := 0 to w*h-1 do
       begin
        pixel := ParseLongWord(Lexer);
        AlphaPixels[i, 0] := (pixel shr 24) and $FF;
        AlphaPixels[i, 1] := (pixel shr 16) and $FF;
        AlphaPixels[i, 2] := (pixel shr 8) and $FF;
        AlphaPixels[i, 3] := pixel and $FF;
       end;
      end;
   else raise EVRMLParserError.Create(Lexer, Format('Invalid components count'+
          ' for SFImage : is %d, should be 1, 2, 3 or 4.',[comp]));
  end;
 end;
end;

procedure TSFImage.SaveToStreamValue(Stream: TStream; const Indent: string);
var rgb: TVector3Byte;
    rgba: TVector4Byte;
    i: Cardinal;
    pixel: LongWord;
begin
 if Value.IsNull then
  WriteStr(Stream, '0 0 1') else
 begin
  WriteStr(Stream, Format('%d %d %d', [Value.Width, Value.Height,
    Value.ColorComponentsCount]) +nl +Indent +IndentIncrement);
  {$I NoRQCheckBegin.inc}
  if Value is TRGBImage then
  begin
   for i := 0 to Value.Width*Value.Height-1 do
   begin
    rgb := PArray_Vector3Byte(TRGBImage(Value).RGBPixels)[i];
    pixel := (rgb[0] shl 16) or (rgb[1] shl 8) or rgb[2];
    WriteStr(Stream, Format('0x%.6x ', [pixel]));
   end;
  end else
  if Value is TAlphaImage then
  begin
   for i := 0 to Value.Width*Value.Height-1 do
   begin
    rgba := PArray_Vector4Byte(TAlphaImage(Value).AlphaPixels)[i];
    pixel := (rgba[0] shl 24) or (rgba[1] shl 16) or (rgba[2] shl 8) or rgba[3];
    WriteStr(Stream, Format('0x%.8x ', [pixel]));
   end;
  end else
   raise Exception.Create('TSFImage.SaveToStreamValue - not implemented TImage descendant');
  {$I NoRQCheckEnd.inc}
 end;
end;

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
 CreateUndefined(AName);
 FMustBeNonnegative := AMustBeNonnegative;
 Value := AValue;
 DefaultValue := Value; { DefaultValue := Value, nie AValue, zeby SetValue moglo ew. zmienic Value }
 DefaultValueExists := true;
end;

procedure TSFLong.Parse(Lexer: TVRMLLexer);
begin
 Lexer.CheckTokenIs(vtInteger);
 Value := Lexer.TokenInteger;
 Lexer.NextToken;
end;

procedure TSFLong.SaveToStreamValue(Stream: TStream; const Indent: string);
begin WriteStr(Stream, IntToStr(Value)); end;

function TSFLong.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue = Value)
end;

constructor TSFMatrix.Create(const AName: string; const AMatrix: TMatrix4Single);
begin
 CreateUndefined(AName);
 Matrix := AMatrix;
end;

procedure TSFMatrix.Parse(Lexer: TVRMLLexer);
var col: integer;
begin
 for col := 0 to 3 do ParseVector(Matrix[col], Lexer);
end;

procedure TSFMatrix.SaveToStreamValue(Stream: TStream; const Indent: string);
begin
 WriteStr(Stream, VectorToRawStr(Matrix[0]) +nl +
                  Indent +IndentIncrement +VectorToRawStr(Matrix[1]) +nl +
                  Indent +IndentIncrement +VectorToRawStr(Matrix[2]) +nl +
                  Indent +IndentIncrement +VectorToRawStr(Matrix[3]) );
end;

constructor TSFRotation.Create(const AName: string; const AnAxis: TVector3Single; const ARotationRad: Single);
begin
 CreateUndefined(AName);
 Axis := AnAxis;
 RotationRad := ARotationRad;
end;

procedure TSFRotation.Parse(Lexer: TVRMLLexer);
begin
 ParseVector(Axis, Lexer);
 RotationRad := ParseFloat(Lexer);
end;

procedure TSFRotation.SetValue(const AValue: TVector4Single);
begin
 Axis[0] := AValue[0];
 Axis[1] := AValue[1];
 Axis[2] := AValue[2];
 RotationRad := AValue[3];
end;

procedure TSFRotation.SaveToStreamValue(Stream: TStream; const Indent: string);
begin
 WriteStr(Stream, VectorToRawStr(Axis) +' ' +FloatToRawStr(RotationRad));
end;

function TSFRotation.RotatedPoint(const pt: TVector3Single): TVector3Single;
begin
 result := RotatePointAroundAxisRad(RotationRad, pt, Axis);
end;

constructor TSFString.Create(const AName: string; const AValue: string);
begin
 CreateUndefined(AName);
 Value := AValue;
 DefaultValue := Value;
 DefaultValueExists := true;
end;

procedure TSFString.Parse(Lexer: TVRMLLexer);
begin
 Lexer.CheckTokenIs(vtString);
 Value := Lexer.TokenString;
 Lexer.NextToken;
end;

procedure TSFString.SaveToStreamValue(Stream: TStream; const Indent: string);
begin
 WriteStr(Stream, StringToVRMLStringToken(Value));
end;

function TSFString.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue = Value)
end;

constructor TSFVec2f.Create(const AName: string; const AValue: TVector2Single);
begin
 CreateUndefined(AName);
 Value := AValue;
 DefaultValue := Value;
 DefaultValueExists := true;
end;

procedure TSFVec2f.Parse(Lexer: TVRMLLexer);
begin ParseVector(Value, Lexer) end;

procedure TSFVec2f.SaveToStreamValue(Stream: TStream; const Indent: string);
begin WriteStr(Stream, VectorToRawStr(Value)); end;

function TSFVec2f.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue[0] = Value[0])
                              and (DefaultValue[1] = Value[1]);
end;

constructor TSFVec3f.Create(const AName: string; const AValue: TVector3Single);
begin
 CreateUndefined(AName);
 Value := AValue;
 DefaultValue := Value;
 DefaultValueExists := true;
end;

procedure TSFVec3f.Parse(Lexer: TVRMLLexer);
begin ParseVector(Value, Lexer) end;

procedure TSFVec3f.SaveToStreamValue(Stream: TStream; const Indent: string);
begin WriteStr(Stream, VectorToRawStr(Value)); end;

function TSFVec3f.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue[0] = Value[0])
                              and (DefaultValue[1] = Value[1])
                              and (DefaultValue[2] = Value[2]);
end;

{ SFBitMask ------------------------------------------------------------ }

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

procedure TSFBitMask.Parse(Lexer: TVRMLLexer);

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
 fFlags:=[];

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

procedure TSFBitMask.SaveToStreamValue(Stream: TStream; const Indent: string);
var i: integer;
    PrecedeWithBar: boolean;
begin
 if AreAllFlags(false) then
  WriteStr(Stream, NoneString) else
 begin
  {zapisywanie do strumienia AllString to taka estetyka - zawsze przeciez
   mozemy wyrazic All flags po prostu zapisujac je wszystkie. }
  if (AllString <> '') and AreAllFlags(true) then
   WriteStr(Stream, AllString) else
  begin
   PrecedeWithBar := false; { pierwszy element nie bedzie poprzedzony '|' }
   WriteStr(Stream, '(');
   for i := 0 to FlagsCount-1 do
    if Flags[i] then
    begin
     if PrecedeWithBar then WriteStr(Stream, '|') else PrecedeWithBar := true;
     WriteStr(Stream, FlagNames[i]);
    end;
   WriteStr(Stream, ')');
  end;
 end;
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

procedure TSFEnum.Parse(Lexer: TVRMLLexer);
var val: integer;
begin
 Lexer.CheckTokenIs(vtName, 'enumerated type constant');
 val := fEnumNames.IndexOf(Lexer.TokenName);
 if val = -1 then
  raise EVRMLParserError.Create(Lexer,
    'Expected enumerated type constant, got '+Lexer.DescribeToken);
 Value := val;
 Lexer.NextToken;
end;

procedure TSFEnum.SaveToStreamValue(Stream: TStream; const Indent: string);
begin
 WriteStr(Stream, EnumNames[Value]);
end;

function TSFEnum.EqualsDefaultValue: boolean;
begin
 result := DefaultValueExists and (DefaultValue = Value)
end;

{ multiple value fields ----------------------------------------------------- }

{$define IMPLEMENT_MF_CLASS:=
constructor TMF_CLASS.Create(const AName: string; const InitialContent: array of TMF_STATIC_ITEM);
begin
 inherited Create(AName);

 fItemClass := TMF_CLASS_ITEM;
 RawItems := TMF_DYN_STATIC_ITEM_ARRAY.Create;
 Items.AppendArray(InitialContent);

 (* inicjuj DefaultValuesCount, inicjuj tez DefaultValue jesli DefaultValuesCount = 1 *)
 case High(InitialContent)+1 of
  0: DefaultValuesCount := 0;
  1: begin
      DefaultValuesCount := 1;
      DefaultValue := InitialContent[0];
     end;
  else DefaultValuesCount := -1;
 end;
end;

function TMF_CLASS.Items: TMF_DYN_STATIC_ITEM_ARRAY;
begin result := TMF_DYN_STATIC_ITEM_ARRAY(RawItems) end;

procedure TMF_CLASS.RawItemsAdd(Item: TVRMLSingleField);
begin
 Items.AppendItem(TMF_CLASS_ITEM(Item).Value);
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
  traktowania ale niektrudno sobie taka wyobrazic. Nie wszystkie
  typy mozemy przeciez sensownie porownywac operatorem "=" lub CompareMem,
  np. gdybysmy mieli TMFImage.

  Notka: dla klas dla ktorych obie wersje (CompareMem i "=") sa dobre
  uzywam wersji "=" (bo jest bezpieczniejsza na typach).
}

{$define IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_EQUALITY_OP:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
 result:=((DefaultValuesCount = 0) and (Count = 0)) or
         ((DefaultValuesCount = 1) and (Count = 1) and (DefaultValue = Items.Items[0]));
end;
}

{$define IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_COMPARE_MEM:=
function TMF_CLASS.EqualsDefaultValue: boolean;
begin
 result:=((DefaultValuesCount = 0) and (Count = 0)) or
         ((DefaultValuesCount = 1) and (Count = 1) and
           CompareMem(@DefaultValue, Items.Pointers[0], SizeOf(TMF_STATIC_ITEM)) );
end;
}

{$define TMF_CLASS := TMFColor}
{$define TMF_STATIC_ITEM := TVector3Single}
{$define TMF_CLASS_ITEM := TSFColor}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector3SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_COMPARE_MEM

{$define TMF_CLASS := TMFLong}
{$define TMF_STATIC_ITEM := Longint}
{$define TMF_CLASS_ITEM := TSFLong}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynLongintArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_EQUALITY_OP

{$define TMF_CLASS := TMFVec2f}
{$define TMF_STATIC_ITEM := TVector2Single}
{$define TMF_CLASS_ITEM := TSFVec2f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector2SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_COMPARE_MEM

{$define TMF_CLASS := TMFVec3f}
{$define TMF_STATIC_ITEM := TVector3Single}
{$define TMF_CLASS_ITEM := TSFVec3f}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynVector3SingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_COMPARE_MEM

{$define TMF_CLASS := TMFFloat}
{$define TMF_STATIC_ITEM := Single}
{$define TMF_CLASS_ITEM := TSFFloat}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynSingleArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_EQUALITY_OP

{$define TMF_CLASS := TMFString}
{$define TMF_STATIC_ITEM := string}
{$define TMF_CLASS_ITEM := TSFString}
{$define TMF_DYN_STATIC_ITEM_ARRAY := TDynStringArray}
IMPLEMENT_MF_CLASS
IMPLEMENT_MF_CLASS_EQUALS_DEFAULT_VALUE_USING_EQUALITY_OP

function TMFColor.RawItemToString(ItemNum: integer): string;
begin result := VectorToRawStr(Items.Items[ItemNum]) end;

function TMFLong.RawItemToString(ItemNum: integer): string;
begin result := IntToStr(Items.Items[ItemNum]) end;

function TMFVec2f.RawItemToString(ItemNum: integer): string;
begin result := VectorToRawStr(Items.Items[ItemNum]) end;

function TMFVec3f.RawItemToString(ItemNum: integer): string;
begin result := VectorToRawStr(Items.Items[ItemNum]) end;

function TMFFloat.RawItemToString(ItemNum: integer): string;
begin result := FloatToRawStr(Items.Items[ItemNum]) end;

function TMFString.RawItemToString(ItemNum: integer): string;
begin result := StringToVRMLStringToken(Items.Items[ItemNum]) end;

{ pare rzeczy speszyl dla TMFLong ------------------------------------------- }

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

{ global functions ----------------------------------------------------- }

procedure CamDirUp2Orient(CamDir, CamUp: TVector3Single;
  var OrientAxis: TVector3Single; var OrientRadAngle: Single);
{ Poczatkowo byl tutaj kod based on Stephen Chenney's ANSI C code orient.c.
  Byl w nim bledzik (patrz testUnits.Test_VRMLFields - TestOrints[4])
  i nawet teraz nie wiem jaki bo ostatecznie zrozumialem sama idee tamtego kodu
  i zapisalem tutaj rzeczy po swojemu, i ku mojej radosci nie mam tego bledu.

  Tutejsze funkcje lokalne operujace na kwaternionach zamierzam
  odseparowac kiedys, jak tylko bede chcial gdzies jeszcze uzyc kwaternionow.

  Niniejszym ustalam sobie ze jesli gdzies potraktuje kwaternion jako wektor
  4 x skalar to bede mial na mysli ze pierwsze trzy skladowe okreslaja wektor
  a ostatnia skladowa - kat, albo (ogolniej) ze pierwsze trzy skladowe
  to wspolczynniki przy i, j, k a ostatnia skladowa to czesc rzeczywista.
  Widzialem rozne konwencje tego, ale bede sie trzymal powyzszego bo
  - tak jest podawane SFRotation VRMLa (ktore nie jest kwaternionem ale
    jest podobne)
  - tak bylo podawane na PGK gdzie pierwszy raz zobaczylem kwaternion

  Pomysl na ta funkcje: mamy CamDir i CamUp. Zeby je zamienic na
  orientation VRMLa, czyli axis i angle obrotu standardowego dir
  (0, 0, -1) i standardowego up (0, 1, 0), wyobrazamy sobie jaka transformacje
  musielibysmy zrobic standardowym dir/up zeby zamienic je na nasze CamDir/Up.
  1) najpierw bierzemy wektor prostop. do standardowego dir i CamDir.
     Obracamy sie wokol niego zeby standardowe dir nalozylo sie na CamDir.
  2) Teraz obracamy sie wokol CamDir tak zeby standardowe up (ktore juz
     zostalo obrocone przez transformacje pierwsza) nalozylo sie na CamUp.
     Mozemy to zrobic bo CamDir i CamUp sa prostopadle i maja dlugosc 1,
     podobnie jak standardowe dir i up.
  Zlozenie tych dwoch transformacji to jest nasza szukana transformacja.

  Jedyny problem jaki pozostaje to czym jest transformacja ? Jezeli mowimy
  o macierzy to jest prosto, macierze dwoch obrotow umiemy skonstruowac
  i wymnozyc ale na koncu dostajemy macierz. A chcemy miec axis+angle.
  A wiec quaternion.
  Moznaby to zrobic inaczej (np. wyciagnac z matrix quaternion lub
  wyciagajac z matrix katy eulera i konwertujac je na quaternion)
  ale najwygodniej jest skorzystac tutaj z mozliwosci mnozenia kwaternionow:
  przemnoz quaterniony obrotu q*p a orztymasz quaternion ktory za pomoca
  jednego obrotu wyraza zlozenie dwoch obrotow, p i q (najpierw p, potem q).
  To jest wlasnie idea z kodu Stephen Chenney's "orient.c".
}

  type
    TQuaternion = record vect_part: TVector3Single; real_part: Single end;

  function AxisAngleCos_To_Quaternion(const Axis: TVector3Single;
    const angle_cos: Single): TQuaternion;
  { zamien Axis i angle_cos na kwaternion odpowiedniego obrotu.
    Jezeli Axis jest znormalizowane to kwaternion tez wyjdzie znormalizowany. }
  var sin_half_angle, cos_half_angle, AngleRad: Float;
  begin
   {* The quaternion requires half angles. *}
   AngleRad := ArcCos(Clamped(angle_cos, -1.0, 1.0));
   SinCos(AngleRad/2, sin_half_angle, cos_half_angle);

   result.vect_part := VectorScale(axis, sin_half_angle);
   result.real_part := cos_half_angle;
  end;

  function QQMul(const q1, q2: TQuaternion): TQuaternion;
  { mnozenie dwoch kwaternionow (dowolnych) }
  begin
   result.real_part := q1.real_part * q2.real_part - VectorDotProduct(q1.vect_part, q2.vect_part);
   result.vect_part := VectorProduct(q1.vect_part, q2.vect_part);
   VectorAddTo1st(result.vect_part, VectorScale(q1.vect_part, q2.real_part));
   VectorAddTo1st(result.vect_part, VectorScale(q2.vect_part, q1.real_part));
  end;

  procedure Quaternion_To_AxisAngle(const q: TQuaternion; var axis: TVector3Single;
    var angle: Single);
  { q musi byc znormalizowanym kwaternionem obrotu,
    tzn. <sin(alfa/2)*normalized(wektor), cos(alfa/2)> }
  var half_angle, sin_half_angle: Single;
  begin
   half_angle := ArcCos(q.real_part);
   sin_half_angle := Sin(half_angle);
   angle := half_angle * 2;
   if IsZero(sin_half_angle) then
   begin
    { Jezeli IsZero(sin_half_angle) to znaczy ze q.vect_part = ZeroVector.
      Wiec skoro q jest znormalizowany to Sqr(q.real_part) = 1 a wiec
      q.real_part = -1 lub 1. A wiec Cos(Angle/2) = -1 lub 1 a wiec
      Angle/2 = Pi * k dla k calkowitych. A wiec Angle = 2k * Pi a wiec
      Angle jest rownowazne zero. (Angle = 2Pi czy -2Pi to przeciez to samo
      co Angle = 0).

      Jesli Angle = 2k * Pi to wszystko ok bo to znaczy ze Angle jest
      rownowazny 0 a wiec Axis ktore chcemy wyliczyc jest bez znaczenia.
      Ustawiamy wtedy Axis na cokolwiek (ale NIE na wektor zerowy
      (jak to bylo w oryginalnym kodzie orient.c), ustawianie wektora zerowego
      jest bez sensu, nigdy nie mozna jako Axis dawac wektora zerowego.).

      Wpp. (jezeli IsZero(sin_half_angle) ale nie IsZero(Angle)) to wykrylismy
      blad, to znaczy ze quaternion wcale nie byl ladnym znorm. quaternionem
      obrotu. }
    if IsZero(Angle) then
     Axis := Vector3Single(0, 0, 1) else
     raise EVectorMathInvalidOp.Create('Invalid quaternion in Quaternion_To_AxisAngle');
   end else
    Axis := VectorScale(q.vect_part, 1/sin_half_angle);
  end;

  function Quaternion_Rotate(q: TQuaternion; const Point: TVector3Single): TVector3Single;
  { q to znormalizowany kwaternion obrotu. Wynik: punkt Point odwrocony o
    kwaternion, zgodnie ze wzorkiem q * P * q^(-1) gdzie P = <Point, 0> }
  var PointQuat, ResultQuat: TQuaternion;
  begin
   PointQuat.real_part := 0.0;
   PointQuat.vect_part := Point;

   ResultQuat := QQMul(q, PointQuat);
   { q := q^(-1). Poniewaz wiemy ze q jest znormalizowany to mozemy obliczyc
     q^(-1) prosto jako wartosc sprzezona do q. }
   VectorNegateTo1st(q.vect_part);
   ResultQuat := QQMul(ResultQuat, q);

   result := ResultQuat.vect_part;
  end;

var Rot1Axis, Rot2Axis, StdCamUpAfterRot1: TVector3Single;
    Rot1Quat, Rot2Quat, OrientQuat: TQuaternion;
    Rot1CosAngle, Rot2CosAngle: Single;
begin
 NormalizeTo1st(CamDir);
 NormalizeTo1st(CamUp);

 { evaluate Rot1Quat }
 Rot1Axis := Normalized( VectorProduct(StdVRMLCamDir, CamDir) );
 Rot1CosAngle := VectorDotProduct(StdVRMLCamDir, CamDir);
 Rot1Quat := AxisAngleCos_To_Quaternion(Rot1Axis, Rot1CosAngle);

 { evaluate Rot2Quat }
 StdCamUpAfterRot1 := Quaternion_Rotate(Rot1Quat, StdVRMLCamUp);
 { wiemy ze Rot2Axis to CamDir lub -CamDir. Wyznaczamy je jednak w tak
   prosty sposob bo nie przychodzi mi teraz do glowy inny sposob jak rozpoznac
   czy powinnismy tu wziac CamDir czy -CamDir (chodzi o to zeby pozniej obrot
   o Rot2CosAngle byl w dobra strone) }
 Rot2Axis := Normalized( VectorProduct(StdCamUpAfterRot1, CamUp) );
 Rot2CosAngle := VectorDotProduct(StdCamUpAfterRot1, CamUp);
 Rot2Quat := AxisAngleCos_To_Quaternion(Rot2Axis, Rot2CosAngle);

 { evaluate OrientQuat = zlozenie Rot1 i Rot2 (tak, kolejnosc mnozenia QQMul musi
   byc odwrotna) }
 OrientQuat := QQMul(Rot2Quat, Rot1Quat);

 { Extract the axis and angle from the quaternion. }
 Quaternion_To_AxisAngle(OrientQuat, OrientAxis, OrientRadAngle);
end;

function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single;
var OrientAxis: TVector3Single;
    OrientAngle: Single;
begin
 CamDirUp2Orient(CamDir, CamUp, OrientAxis, OrientAngle);
 result := Vector4Single(OrientAxis, OrientAngle);
end;

end.
