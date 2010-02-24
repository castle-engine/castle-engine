{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Generating so-called "space filling curves",
  i.e. curves that "walk" through all points of some defined space.) }

unit SpaceFillingCurves;

interface

uses SysUtils, VectorMath;

type
  { 0 = 0 stopni, 1 = 90 stopni, 2 = 180, 3 = 270 ale tak naprawde to tylko
    kwestia umowy, moznaby przesunac te rzeczy. Chodzi tylko o to ze to musza
    byc 4 kolejne katy (kolejne w sensie CW lub CCW, to bez znaczenia przeciez).}
  TSFCAngle = 0..3;

  TSFCStepFunction = procedure (Angle: TSFCAngle; StepFuncData: Pointer);

{ PeanoCurve i HilbertCurve generuja kolejne punkty ciagu (czy tez krzywej,
  a moze raczej lamanej) Peano lub Hilberta, jak w "Graphic Gems II", gem I.8.
  Step jest funkcja ktora przechodzi na sasiedni punkt 2d w zadanym kierunku
  i "zaznacza go". Zakladajac ze na poczatku punkt poczatkowy jest juz
  "zaznaczony" lamane Peano i Hilberta daja nam pewnosc ze cala plansza 2d
  zostanie "zaznaczona". Dla Peano zapelniana jest plansza o wymiarach 3^Level,
  dla Hilberta 2^Level.

  Orient to skrot od Orientation, Angle to jakis "poczatkowy" Angle (w cudzyslowach
  bo tak naprawde to zalezy od krzywej w jaki sposob te Orient i Angle zostana
  wykorzystane; ale na pewno zdeterminuja ona to po ktorej stronie punktu
  poczatkowego pojawi sie krzywa).

  Przyjmujac ze katy TSFCAngle sa interpretowane jako 0 = 0 stopni = w prawo,
  1 = 90 stopni = w gore, 2 = 180 stopni = w lewo i 3 = 270 stopni = w dol
  to musisz podac nastepujace InitialOrient i Angle aby plansza byla
  zapelniana na lewo i w gore od punktu poczatkowego :
    Peano: InitialOrient = false, Angle = 0.
    Hilbert: InitialOrient = true, Angle = 0. }
procedure PeanoCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
procedure HilbertCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);

type
  { To jest abstrakcyjna klasa reprezentujaca interfejs obiektu ktory
    zwraca kolejne punkty w przestrzeni 2d (0..SizeX-1, 0..SizeY-1)
    w taki sposob ze w momencie gdy EndOfPixels = true to kazdy pixel
    z tej przestrzeni zostal zwrocony dokladnie raz przez NextPixel.
    Innymi slowy, uzywajac konstrukcji w rodzaju
      while not SFCurve.EndOfPixels do DoSomethingOnPixel(SFCurve.NextPixel)
    kazdy punkt o wspolrzednych 0..SizeX-1, 0..SizeY-1 zostanie przekazany
    dokladnie raz do procedury DoSomethingOnPixel().

    Te klasy beda zapisane w taki sposob zeby wywolania NextPixel i
    EndOfPixels dzialaly mozliwie blyskawicznie, za to byc moze skonstruowanie
    obiektu takiej klasy bedzie zajmowalo chwile czasu (innymi slowy,
    bedziemy raczej starali sie robic jakis preprocessing w czasie konstrukcji
    obiektu niz komplikowac obliczenia NextPixel i EndOfPixels).

    Notka : dla SizeX lub SizeY = 0 te klasy tez beda dzialac poprawnie
    (zawsze bedzie EndOfPixels = true). }
  TSpaceFillingCurve = class
  private
    FSizeX, FSizeY, FPixelsCount: Cardinal;
  public
    property SizeX: Cardinal read FSizeX;
    property SizeY: Cardinal read FSizeY;
    property PixelsCount: Cardinal read FPixelsCount; { = SizeX * SizeY }
    constructor Create(ASizeX, ASizeY: Cardinal); virtual;
    function EndOfPixels: boolean; virtual; abstract;
    { Jest bledem (ktory niekoniecznie zostanie wychwycony do wyjatku, zwlaszcza
      w wersji RELEASE) uzycie NextPixel gdy EndOfPixels = true. }
    function NextPixel: TVector2Cardinal; virtual; abstract;
    { Zasymuluj wywolanie NextPixels SkipCount razy, ignorujac wynik.
      Zazwyczaj bedzie jednak mozna to zaimplementowac duzo szybciej niz
        for i := 1 to SkipCount do NextPixels,
      co ma znaczenie bo przeciez SkipCount jakie tu podamy moze byc
      dosc duze (np. dla SizeX = SizeY = 10^3 to mamy PixelsCount = 10^6.
      Chcesz zrobic SkipPixels z polowy obrazka ? To Skipcount = 5*10^5.
      Sporo, zwlaszcza ze np. w podklasie TPrecalcCurve mozna takie
      SkipPixels zaimplementowac jako proste dodawanie.

      Jest bledem (ktory niekoniecznie zostanie wychwycony do wyjatku, zwlaszcza
      w wersji RELEASE) uzycie SkipPixels gdy EndOfPixels = true lub uzycie
      SkipCount > PixelsCount-PixelsDone. }
    procedure SkipPixels(SkipCount: Cardinal); virtual; abstract;
    { sprawia ze generowanie pixeli zaczyna sie od poczatku, tak jakby obiekt
      byl od nowa skonstruowany (ale SizeX, SizeY pozostaje bez zmian). }
    procedure Reset; virtual; abstract;
    { zwraca ile razy NextPixel zostalo juz wywolane, od czasu ostatniego
      Reset'a lub konstrukcji obiektu }
    function PixelsDone: Cardinal; virtual; abstract;
    { zwraca nazwe klasy curve'a. W naszych 3 podklasach zwraca w tym
      momencie 'swapscan', 'hilbert' i 'peano', odpowiednio. }
    class function SFCName: string; virtual; abstract;
  end;

  TSpaceFillingCurveClass = class of TSpaceFillingCurve;

  { To nie jest gotowa klasa, tylko przejsciowa klasa abstrakcyjna
    z ktorej korzystaja THilbertCurve i TPeanoCurve. W podklasach wystarczy
    zdefiniowac metode GeneratePoints ktora wygeneruje do tablicy
    Pixels: PArray_Vector2Single kolejne PixelsCount punktow.
    (jest gwarantowane ze GeneratePixels bedzie wywolywane tylko gdy
    PixelsCount > 0)
    W tej klasie ciag punktow jest zawczasu generowany w konstruktorze i potem,
    przy NextPixel, jest tylko odczytywany (zeby NextPixel zajmowalo mozliwie
    malo czasu, no i naturalnie zazwyczaj napisanie GeneratePixels jest duzo
    prostsze niz zapisanie skomplikowanych NextPixel i EndOfPixel ktore musza
    sobie gdzie zachowywac swoj stan zeby wiedziec jaki pixel nastepnie podac). }
  TPrecalcCurve = class(TSpaceFillingCurve)
  private
    Pixels: PArray_Vector2Cardinal;
    NextPixelNum: Cardinal;
  protected
    procedure GeneratePixels(APixels: PArray_Vector2Cardinal); virtual; abstract;
  public
    constructor Create(ASizeX, ASizeY: Cardinal); override;
    destructor Destroy; override;
    function EndOfPixels: boolean; override;
    function NextPixel: TVector2Cardinal; override;
    procedure SkipPixels(SkipCount: Cardinal); override;
    procedure Reset; override;
    function PixelsDone: Cardinal; override;
  end;

  { Najprostszy na swiecie ciag pixeli zapelniajacych ekran. Idziemy kolejnymi
    wierszami, od dolu do gory, w kazdym parzystym wierszu idziemy w prawo,
    w nieparzystym idziemy w lewo. Ten prosty ciag daje dosc kiepski sposob
    przechodzenia przestrzeni gdy zalezy nam na kreceniu sie po kazdym obszarze
    obrazka mozliwie dlugo i bez przerwy. Ale przynajmniej kazdy kolejny NextPixel
    sasiaduje z poprzednim.

    Byc moze jesli kiedys bedzie taka potrzeba zaimplementuje tej klasie
    specjalny konstruktor, CreateScanCurve, w ktorym bedzie mozna podac
    specjalne wlasciwosci w rodzaju czy isc od dolu czy od gory, czy moze
    isc liniami pionowymi. Albo moze zrobie podklasy tej klasy robiace rozne
    funkcjonalnosci ? Na razie nie jest mi to potrzebne, ale jesli kiedys
    bedzie to latwo bede mogl to tutaj dopisac.

    Kiedys TSwapScanCurve nie bylo podklasa TPrecalcCurve, ale w koncu uznalem
    ze tak jest prosciej i nieco efektywniej czasowo (mimo ze uzywanie
    TPrecalcCurve implikuje zuzycie sporej ilosci pamieci). }
  TSwapScanCurve = class(TPrecalcCurve)
  protected
    procedure GeneratePixels(APixels: PArray_Vector2Cardinal); override;
  public
    class function SFCName: string; override;
  end;

  { Wypelniaj przestrzen ciagiem Hilberta obcietym do wymiarow SizeX, SizeY }
  THilbertCurve = class(TPrecalcCurve)
  protected
    procedure GeneratePixels(APixels: PArray_Vector2Cardinal); override;
  public
    class function SFCName: string; override;
  end;

  { Wypelniaj przestrzen ciagiem Peano obcietym do wymiarow SizeX, SizeY }
  TPeanoCurve = class(TPrecalcCurve)
  protected
    procedure GeneratePixels(APixels: PArray_Vector2Cardinal); override;
  public
    class function SFCName: string; override;
  end;

{ ------------------------------------------------------------
  operacje uzywajace funkcji SFCName (stworzone aby dac userowi jakies
  metody specyfikowania TSpaceFillingCurveClass) }

const
  { wszystkie dostepne koncowe klasy wywodzace sie z TSpaceFillingCurve.
    Mozesz polegac na fakcie ze ta tablica zawsze bedzie indeksowana od zera.
    Uzywane TYLKO przez StrToSFCurveClass, AllSFCurveClassesNames. }
  AvailableSFCurveClasses: array[0..2]of TSpaceFillingCurveClass=
  (TSwapScanCurve, THilbertCurve, TPeanoCurve);

type
  EInvalidSFCurveClassName = class(Exception);

{ dla nazw klas sposrod AvailableSFCurveClasses zwraca odpowiednie
  klasy. Nazwy klas sa przyrownywane not case-sensitive.
  Dla czegokolwiek innego wyjatek EInvalidSFCurveClassName. }
function StrToSFCurveClass(const s: string): TSpaceFillingCurveClass;

{ zwraca wszystkie SFCName dla wszystkich klas AvailableSFCurveClasses,
  rozdzielone przecinkami i ujete w apostrofy. }
function AllSFCurveClassesNames: string;

implementation

uses KambiUtils;

const
  { AngleTurn[Angle, Orient] = (definicja)
      if Orient then
       result := ChangeIntCycle(Angle, 1, 3) else
       result := ChangeIntCycle(Angle, -1, 3);
    Uzywanie tablicy juz przeliczonych wartosci da nam tutaj maly zysk czasowy. }
  AngleTurn: array[TSFCAngle, boolean]of TSFCAngle =
  (
    {Angle = 0} (3, 1),
    {Angle = 1} (0, 2),
    {Angle = 2} (1, 3),
    {Angle = 3} (2, 0)
  );

procedure PeanoCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
{ na podstawie "Graphic Gems II", gem I.8.
  Zmienna Angle jest globalna z punktu widzenia kolejnych rekurencyjnych
  wywolan Peano(), one wszystkie modyfikuja po prostu zadeklarowany powyzej
  parametr Angle. }

  procedure Peano(Orient: boolean; Level: Cardinal);
  begin
   if Level = 0 then Exit;
   Dec(Level);

   Peano(Orient, Level);
   Step(Angle, StepData);
   Peano(not Orient, Level);
   Step(Angle, StepData);
   Peano(Orient, Level);
   Angle := AngleTurn[Angle, not Orient];
   Step(Angle, StepData);
   Angle := AngleTurn[Angle, not Orient];
   Peano(not Orient, Level);
   Step(Angle, StepData);
   Peano(Orient, Level);
   Step(Angle, StepData);
   Peano(not Orient, Level);
   Angle := AngleTurn[Angle, Orient];
   Step(Angle, StepData);
   Angle := AngleTurn[Angle, Orient];
   Peano(Orient, Level);
   Step(Angle, StepData);
   Peano(not Orient, Level);
   Step(Angle, StepData);
   Peano(Orient, Level);
  end;

begin
 Peano(InitialOrient, InitialLevel);
end;

procedure HilbertCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
{ na podstawie "Graphic Gems II", gem I.8 }

  procedure Hilbert(Orient: boolean; Level: Cardinal);
  begin
   if Level = 0 then Exit;
   Dec(Level);

   Angle := AngleTurn[Angle, Orient];
   Hilbert(not Orient, Level);
   Step(Angle, StepData);
   Angle := AngleTurn[Angle, not Orient];
   Hilbert(Orient, Level);
   Step(Angle, StepData);
   Hilbert(Orient, Level);
   Angle := AngleTurn[Angle, not Orient];
   Step(Angle, StepData);
   Hilbert(not Orient, Level);
   Angle := AngleTurn[Angle, Orient];
  end;

begin
 Hilbert(InitialOrient, InitialLevel);
end;

{ TSpaceFillingCurve ------------------------------------------------------------ }

constructor TSpaceFillingCurve.Create(ASizeX, ASizeY: Cardinal);
begin
 inherited Create;
 FSizeX := ASizeX;
 FSizeY := ASizeY;
 FPixelsCount := SizeX*SizeY;
end;

{ TPrecalcCurve ------------------------------------------------------------ }

constructor TPrecalcCurve.Create(ASizeX, ASizeY: Cardinal);
begin
 inherited;
 if PixelsCount <> 0 then
 begin
  Pixels := GetMem(SizeOf(TVector2Cardinal) * PixelsCount);
  GeneratePixels(Pixels);
 end;
 Reset;
end;

destructor TPrecalcCurve.Destroy;
begin
 FreeMemNiling(Pixels);
end;

function TPrecalcCurve.EndOfPixels: boolean;
begin
 result := NextPixelNum >= PixelsCount;
end;

function TPrecalcCurve.NextPixel: TVector2Cardinal;
begin
 Assert(not EndOfPixels);
 result := Pixels^[NextPixelNum];
 Inc(NextPixelNum);
end;

procedure TPrecalcCurve.SkipPixels(SkipCount: Cardinal);
begin
 Assert(SkipCount <= PixelsCount-PixelsDone);
 NextPixelNum += SkipCount;
end;

procedure TPrecalcCurve.Reset;
begin
 NextPixelNum := 0;
end;

function TPrecalcCurve.PixelsDone: Cardinal;
begin
 result := NextPixelNum;
end;

{ TSwapScanCurve ------------------------------------------------------------ }

procedure TSwapScanCurve.GeneratePixels(APixels: PArray_Vector2Cardinal);
var NextPixelToWriteNum: Cardinal;

  procedure AppendPixel(x, y: Cardinal);
  begin
   APixels^[NextPixelToWriteNum, 0] := x;
   APixels^[NextPixelToWriteNum, 1] := y;
   Inc(NextPixelToWriteNum);
  end;

var x, y: Cardinal;
begin
 NextPixelToWriteNum := 0;

 for y := 0 to SizeY-1 do
 begin
  if Odd(y) then
   for x := SizeX-1 downto 0 do AppendPixel(x, y) else
   for x := 0 to SizeX-1 do AppendPixel(x, y);
 end;
end;

class function TSwapScanCurve.SFCName: string;
begin
 result := 'swapscan';
end;

{ pomoce do THilbertCurve i TPeanoCurve ---------------------------------------- }

type
  TStepData = record
    LastX, LastY, NextPixelToWriteNum, SizeX, SizeY: Cardinal;
    Pixels: PArray_Vector2Cardinal;
  end;
  PStepData=^TStepData;

procedure InitStepData(out StepData: TStepData; Pixels: PArray_Vector2Cardinal;
  const SizeX, SizeY: Cardinal);
begin
 FillChar(Pixels^[0], SizeOf(Pixels^[0]), 0);
 StepData.Pixels := Pixels;
 StepData.NextPixelToWriteNum := 1;
 StepData.LastX := 0;
 StepData.LastY := 0;
 StepData.SizeX := SizeX;
 StepData.SizeY := SizeY;
end;

procedure HilbertPeanoStep(Angle: TSFCAngle; RawData: Pointer);
{ callback dla TSFCStepFunction dla Hilbert lub PeanoCurve.
  Zapisze Pixels^[NextPixelToWriteNum], uaktualniajac tez
  LastX, LastY, NextPixelToWriteNum. Bedzie obcinal zapisywane do Pixels
  elementy do 0..SizeX-1, 0..SizeY-1. }
var Data: PStepData absolute RawData;
begin
 with Data^ do
 begin
  case Angle of
   0: Inc(LastX);
   1: Inc(LastY);
   2: Dec(LastX);
   3: Dec(LastY);
  end;

  if Between(LastX, 0, SizeX-1) and Between(LastY, 0, SizeY-1) then
  begin
   Pixels^[NextPixelToWriteNum, 0] := LastX;
   Pixels^[NextPixelToWriteNum, 1] := LastY;
   Inc(NextPixelToWriteNum);
  end;
 end;
end;

{ THilbertCurve ------------------------------------------------------------ }

procedure THilbertCurve.GeneratePixels(APixels: PArray_Vector2Cardinal);
var StepData: TStepData;
    Level: Cardinal;
begin
 Level := Smallest2Exponent(Max(SizeX, SizeY));
 Assert((Level = 0) or (NatNatPower(2, Level-1) < Max(SizeX, SizeY)));

 InitStepData(StepData, APixels, SizeX, SizeY);
 HilbertCurve(true, 0, Level,
   {$ifdef FPC_OBJFPC} @ {$endif} HilbertPeanoStep, @StepData);
end;

class function THilbertCurve.SFCName: string;
begin
 result := 'hilbert';
end;

{ TPeanoCurve ------------------------------------------------------------ }

procedure TPeanoCurve.GeneratePixels(APixels: PArray_Vector2Cardinal);
var StepData: TStepData;
    MaxSize, Power3Level, Level: Cardinal;
begin
 { calculate Level, pomagajac sobie MaxSize i Power3Level }
 MaxSize := Max(SizeX, SizeY);
 Level := 0;
 Power3Level := 1; { = zawsze 3^Level }
 while Power3Level < MaxSize do begin Inc(Level); Power3Level *= 3 end;
 Assert((Level = 0) or (NatNatPower(3, Level-1) < Max(SizeX, SizeY)));

 InitStepData(StepData, APixels, SizeX, SizeY);
 PeanoCurve(false, 0, Level,
   {$ifdef FPC_OBJFPC} @ {$endif} HilbertPeanoStep, @StepData);
end;

class function TPeanoCurve.SFCName: string;
begin
 result := 'peano';
end;

{ operacje na SFCName -------------------------------------------------------- }

function StrToSFCurveClass(const s: string): TSpaceFillingCurveClass;
var i: Integer;
begin
 for i := 0 to High(AvailableSFCurveClasses) do
  if AnsiSameText(s, AvailableSFCurveClasses[i].SFCName) then
   Exit(AvailableSFCurveClasses[i]);
 raise EInvalidSFCurveClassName.Create('Invalid space filling curve name : "'+
   s+'", allowed names are '+AllSFCurveClassesNames+'.');
end;

function AllSFCurveClassesNames: string;
var i: Integer;
begin
 result := '"'+AvailableSFCurveClasses[0].SFCName+'"';
 for i := 1 to High(AvailableSFCurveClasses) do
  result += ', "'+AvailableSFCurveClasses[i].SFCName+'"';
end;

end.
