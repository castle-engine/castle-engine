{$ifdef read_interface}

{ Kambi changes:
  Kiedys ponizsze rzeczy byly osobnym modulem - definiowal
  on typy OpenGL'a nie eksportujac zadnych funkcji z OpenGLDLL.
  Zasada bylo ze uzywanie tego modulu NIE powoduje ze program uzywa
  OpenGL.dll lub .so.

  Uzywalem tego zeby moc pisac moduly ktore mogly robic rozne
  rzeczy na typach OpenGL'a jednoczesnie nie wymagajac aby koncowy
  program ktory uzywa tych modulow musial uzywac OpenGL'a.
  Teraz jednak zarzucilem taka strategie : jezeli pisze modul ktory
  moze byc uzywany BEZ uzywania OpenGL'a to ten modul powinien
  byc takze niezalezny od aktualnych definicji typow OpenGL'a,
  prawda ? Jezeli program uzywa OpenGLTypes to tym samym staje
  sie zalezny od dokladnosci typow zmiennoprzec. na implemenatcji
  OpenGL'a na danym komputerze itp. Jezeli chce napisac modul
  ktory nie musi uzywac OpenGL'a to ten modul nie powinien tez zmieniac
  definicji swoich typow w zaleznosci od tego jaka mamy aktualnie
  implementacje OpenGL'a.
  Dlatego operacje na wektorach zostaly ogolnie i naprawde niezaleznie
  od OpenGL'a zaimplementowane w module VectorMath a niniejszy modul
  OpenGLTypes zostaje wlaczony przez $I jako integralna czesc
  OpenGLh. Jak to juz kiedys zreszta bylo.

  Notabene pozwolilo mi to na przeniesienie modulu Boxes3d takze do
  modulow niezaleznych od OpenGLa.

  Sa w tym pliku problemy z eleganckim zapisaniem wszystkiego:
  - definicje funkcji konstruktorow typow musza byc tutaj powtorzone
    (bo nie mozna przeciazac zmiennych funkcyjnych,
    wiec sztuczka jakiej uzylem zeby eksportowac np. BWColor3fvTo1st
    z VectorMath nie moze byc zawsze uzyta)
  - jezeli kiedys pod jakas implementacja OpenGL'a beda inne definicje
    typow TGLfloat itp. to trzeba bedzie zmienic nie tylko definicje
    TGLfloat ale tez TVector*f, TMatrix*f itd.
  - definicje stalych musza byc powtorzone (to jest niedogodnosc FPC
    1.0.x ktora wkrotce zostanie zniesiona gdy FPC 2.0 bedzie mial
    stabilna wersje)

  Moje zmiany w porownaniu z GeometryTypes :
  - kiedys dolaczalem niegdysiejszy unit GeometryTypes bezposrednio przez
    $Include, obecnie jego tresc jest tu literalnie wklejona.

    W oryginale GeometryTypes to byl w ogole osobny unit - u mnie jest
    wstawiony do OpenGLTypes.inc a wiec i do OpenGLh.pas bo:
    * tak jest wygodniej, to raz,
    * dwa to ze typy z GeometryTypes potrzebuja i tak definicji TGLfloat,
      TGLdouble i TGLint aby byc elegancko zdefiniowanymi
      (w oryginale pisali na poczatku GemetryTypes Float = Single i
      potem pisali rzeczy zakladajac ze Float = TGLfloat, Double = TGLdouble,
      Integer = TGLint itd.)
    Poza zniesieniem tego nonsensu i wprowadzeniem jasnego zapisu w rodzaju
    TVector3f = array[0..3]of GLfloat troszeczke ukonsekwentnilem tez inne
    rzeczy, m.in. powykanczalem rodzinki typow TVector* i dorobilem rodzinki
    typow PVector* i PMatrix*.
  - dodalem typ T/PPolygonStipple i uczynilem go parametrem glPolygonStipple
    aby troche wzmocnic pozadana kontrole typow
  - TGLBoolean zamienione z ByteBool na Byte - GL_TRUE musi byc integerem
  - rodzinki TMatrix to tablice typow TVector
  - I made every array "packed", just to be on the safe side
    (as it seems that FPC 1.9.3 introduces some serious struct aligning)
}

type
  PPointer = ^Pointer;

  { Probably I will add more definitions like that below in the future. }
  TDynGLuintArray = TDynLongWordArray;

{ GeometryTypes begins here ------------------------------------------------ }

// ***************************************************************************
// TVector1* :

type
  TVector1ub = packed array [0..0] of TGLubyte;   PVector1ub = ^TVector1ub;
  TVector1s = packed array [0..0] of TGLshort;    PVector1s = ^TVector1s;
  TVector1i = packed array [0..0] of TGLint;      PVector1i = ^TVector1i;
  TVector1f = packed array [0..0] of TGLfloat;    PVector1f = ^TVector1f;
  TVector1d = packed array [0..0] of TGLDouble;   PVector1d = ^TVector1d;
  TVector1p = packed array [0..0] of Pointer;     PVector1p = ^TVector1p;

// *****************************************************************************
// TVector2x:
// *****************************************************************************

type
  TVector2f = TVector2Single;   PVector2f = PVector2Single;
  TVector2d = TVector2Double;   PVector2d = PVector2Double;
  TVector2ub = TVector2Byte;    PVector2ub = PVector2Byte;

  TVector2s = packed array [0..1] of TGLshort;    PVector2s = ^TVector2s;
  TVector2i = packed array [0..1] of TGLint;      PVector2i = ^TVector2i;
  TVector2p = packed array [0..1] of Pointer;     PVector2p = ^TVector2p;
const
  NullVector2i : TVector2i = (0, 0);
  NullVector2f : TVector2f = (0, 0);
  NullVector2d : TVector2d = (0, 0);
  NullVector2p : TVector2p = (nil, nil);
  XVector2i : TVector2i = (1, 0);
  XVector2f : TVector2f = (1, 0);
  XVector2d : TVector2d = (1, 0);
  YVector2i : TVector2i = (0, 1);
  YVector2f : TVector2f = (0, 1);
  YVector2d : TVector2d = (0, 1);

// *****************************************************************************
// TVector3x:
// *****************************************************************************

type
  TVector3f = TVector3Single;   PVector3f = PVector3Single;
  TVector3d = TVector3Double;   PVector3d = PVector3Double;
  TVector3ub = TVector3Byte;    PVector3ub = PVector3Byte;
  TVector3us = TVector3Word;    PVector3us = PVector3Word;

  TVector3b = packed array [0..2] of TGLbyte;     PVector3b = ^TVector3b;
  TVector3i = packed array [0..2] of TGLint;      PVector3i = ^TVector3i;
  TVector3s = packed array [0..2] of TGLshort;    PVector3s = ^TVector3s;
  TVector3ui = packed array [0..2] of TGLuint;    PVector3ui = ^TVector3ui;
  TVector3p = packed array [0..2] of Pointer;     PVector3p = ^TVector3p;
const
  NullVector3i : TVector3i = (0, 0, 0);
  NullVector3f : TVector3f = (0, 0, 0);
  NullVector3d : TVector3d = (0, 0, 0);
  NullVector3p : TVector3p = (nil, nil, nil);
  XVector3i : TVector3i = (1, 0, 0);
  XVector3f : TVector3f = (1, 0, 0);
  XVector3d : TVector3d = (1, 0, 0);
  YVector3i : TVector3i = (0, 1, 0);
  YVector3f : TVector3f = (0, 1, 0);
  YVector3d : TVector3d = (0, 1, 0);
  ZVector3i : TVector3i = (0, 0, 1);
  ZVector3f : TVector3f = (0, 0, 1);
  ZVector3d : TVector3d = (0, 0, 1);

// *****************************************************************************
// TVector4x:
// *****************************************************************************

type
  TVector4f = TVector4Single;   PVector4f = PVector4Single;
  TVector4d = TVector4Double;   PVector4d = PVector4Double;
  TVector4ub = TVector4Byte;    PVector4ub = PVector4Byte;
  TVector4us = TVector4Word;    PVector4us = PVector4Word;

  TVector4b = packed array [0..3] of TGLbyte;      PVector4b = ^TVector4b;
  TVector4i = packed array [0..3] of TGLint;       PVector4i = ^TVector4i;
  TVector4s = packed array [0..3] of TGLshort;     PVector4s = ^TVector4s;
  TVector4ui = packed array [0..3] of TGLuint;     PVector4ui = ^TVector4ui;
  TVector4p = packed array [0..3] of Pointer;      PVector4p = ^TVector4p;

const
  NullVector4i : TVector4i = (0, 0, 0, 0);
  NullVector4f : TVector4f = (0, 0, 0, 0);
  NullVector4d : TVector4d = (0, 0, 0, 0);
  NullVector4p : TVector4p = (nil, nil, nil, nil);


// *****************************************************************************
// TMatrix2x:
// *****************************************************************************

type
  TMatrix2i = TMatrix2Longint; PMatrix2i = PMatrix2Longint;
  TMatrix2f = TMatrix2Single;  PMatrix2f = PMatrix2Single;
  TMatrix2d = TMatrix2Double;  PMatrix2d = PMatrix2Double;

  TMatrix3i = TMatrix3Longint; PMatrix3i = PMatrix3Longint;
  TMatrix3f = TMatrix3Single;  PMatrix3f = PMatrix3Single;
  TMatrix3d = TMatrix3Double;  PMatrix3d = PMatrix3Double;

  TMatrix4i = TMatrix4Longint; PMatrix4i = PMatrix4Longint;
  TMatrix4f = TMatrix4Single;  PMatrix4f = PMatrix4Single;
  TMatrix4d = TMatrix4Double;  PMatrix4d = PMatrix4Double;

const
  NullMatrix2f: TMatrix2f = ((0, 0), (0, 0));
  NullMatrix2d: TMatrix2d = ((0, 0), (0, 0));
  NullMatrix2i: TMatrix2i = ((0, 0), (0, 0));

  NullMatrix3f: TMatrix3f = ((0, 0, 0), (0, 0, 0), (0, 0, 0));
  NullMatrix3d: TMatrix3d = ((0, 0, 0), (0, 0, 0), (0, 0, 0));
  NullMatrix3i: TMatrix3i = ((0, 0, 0), (0, 0, 0), (0, 0, 0));

  NullMatrix4f: TMatrix4f = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));
  NullMatrix4d: TMatrix4d = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));
  NullMatrix4i: TMatrix4i = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));

  IdentityMatrix2f: TMatrix2f = ((1, 0), (0, 1));
  IdentityMatrix2d: TMatrix2d = ((1, 0), (0, 1));
  IdentityMatrix2i: TMatrix2i = ((1, 0), (0, 1));

  IdentityMatrix3f: TMatrix3f = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  IdentityMatrix3d: TMatrix3d = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  IdentityMatrix3i: TMatrix3i = ((1, 0, 0), (0, 1, 0), (0, 0, 1));

  IdentityMatrix4f: TMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
  IdentityMatrix4d: TMatrix4d = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
  IdentityMatrix4i: TMatrix4i = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

// *****************************************************************************
// TPointXy:
// *****************************************************************************

type
  TPoint2i = TVector2i;
  TPoint2f = TVector2f;
  TPoint2d = TVector2d;
  TPoint3i = TVector3i;
  TPoint3f = TVector3f;
  TPoint3d = TVector3d;
  TPoint4i = TVector4i;
  TPoint4f = TVector4f;
  TPoint4d = TVector4d;

// *****************************************************************************
// TColorXy:
// *****************************************************************************

{ by Kambi : color types are equal to vector types }
type
  TColor3ub = TVector3ub;
  TColor3f = TVector3f;
  TColor4ub = TVector4ub;
  TColor4f = TVector4f;

const
  DefaultMatAmbientColor4ub : TColor4ub = (51, 51, 51, 255);
  DefaultMatAmbientColor4f : TColor4f = (0.2, 0.2, 0.2, 1.0);
  DefaultMatDiffuseColor4ub : TColor4ub = (204, 204, 204, 255);
  DefaultMatDiffuseColor4f : TColor4f = (0.8, 0.8, 0.8, 1.0);
  DefaultMatSpecularColor4ub : TColor4ub = (0, 0, 0, 255);
  DefaultMatSpecularColor4f : TColor4f = (0, 0, 0, 1.0);

type
  TAVector2f = TVector2f;
  TAVector2d = TVector2d;

type
  TAVector3f = TVector3f;
  TAVector3d = TVector3d;
  THVector3f = TVector3f;
  THVector3d = TVector3d;
const
  NullHVector3f : THVector3f = (0, 0, 1);
  NullHVector3d : THVector3d = (0, 0, 1);
  XHVector3f : THVector3f = (1, 0, 1);
  XHVector3d : THVector3d = (1, 0, 1);
  YHVector3f : THVector3f = (0, 1, 1);
  YHVector3d : THVector3d = (0, 1, 1);

type
  TAVector4f = TVector4f;
  TAVector4d = TVector4d;
  THVector4f = TVector4f;
  THVector4d = TVector4d;
const
  NullHVector4f : THVector4f = (0, 0, 0, 1);
  NullHVector4d : THVector4d = (0, 0, 0, 1);
  XHVector4f : THVector4f = (1, 0, 0, 1);
  XHVector4d : THVector4d = (1, 0, 0, 1);
  YHVector4f : THVector4f = (0, 1, 0, 1);
  YHVector4d : THVector4d = (0, 1, 0, 1);
  ZHVector4f : THVector4f = (0, 0, 1, 1);
  ZHVector4d : THVector4d = (0, 0, 1, 1);

type
  TAPoint2f = TAVector2f;
  TAPoint2d = TAVector2d;
  TAPoint3f = TAVector3f;
  TAPoint3d = TAVector3d;
  THPoint3f = THVector3f;
  THPoint3d = THVector3d;
  THPoint4f = THVector4f;
  THPoint4d = THVector4d;

{ end of GeometryTypes ------------------------------------------------------ }

{ Kambi+ ------------------------------------------------------------------- }

type
  { more geometry types }

  TPolygonStipple = packed array[0..(32*32 div 8)-1]of TGLubyte;
  PPolygonStipple = ^TPolygonStipple;

  TTriangle3f = TTriangle3Single;
  TTriangle3d = TTriangle3Double;

  { tablice "nieskonczone" }

  TArray_GLuint = packed array[0..MaxInt div SizeOf(TGLuint)-1]of TGLuint;
  PArray_GLuint = ^TArray_GLuint;
  TArray_Vector3d = packed array[0..MaxInt div SizeOf(TVector3d)-1]of TVector3d;
  PArray_Vector3d = ^TArray_Vector3d;
  TArray_Vector3ub = packed array[0..MaxInt div SizeOf(TVector3ub)-1]of TVector3ub;
  PArray_Vector3ub = ^TArray_Vector3ub;

  { Dyn arrays and some "infinite" static arrays. }

  TDynVector2fArray = TDynVector2SingleArray;
  TArray_Vector2f = TArray_Vector2Single;
  PArray_Vector2f = PArray_Vector2Single;

  TDynVector3fArray = TDynVector3SingleArray;
  TArray_Vector3f = TArray_Vector3Single;
  PArray_Vector3f = PArray_Vector3Single;

{ Trivial operations on types below }

function Vector2f(x, y: TGLfloat): TVector2f;
function Vector3f(x, y: TGLfloat; z: TGLfloat {$IFDEF DEFPARS}=0.0{$ENDIF}): TVector3f; overload;
function Vector3f(const v3: TVector3d): TVector3f; overload;
function Vector3f(const v3: TVector3ub): TVector3f; overload;
function Vector3d(x, y: TGLdouble; z: TGLdouble {$IFDEF DEFPARS}=0.0{$ENDIF}): TVector3d; overload;
function Vector3d(const v: TVector3f): TVector3d; overload;
function Vector4f(x, y: TGLfloat; z: TGLfloat {$IFDEF DEFPARS}=0{$ENDIF}; w: TGLfloat {$IFDEF DEFPARS}=1.0{$ENDIF}): TVector4f; overload;
function Vector4f(const v3: TVector3f; w: TGLfloat {$IFDEF DEFPARS}=1.0{$ENDIF}): TVector4f; overload;
function Vector4f(const ub: TVector4ub): TVector4f; overload;

function Vector3ub(x, y, z: TGLubyte): TVector3ub;
function Vector4ub(x, y, z, w: TGLubyte): TVector4ub; overload;
function Vector4ub(const f4: TVector4f): TVector4ub; overload;

const
  Vector3fPoint: function(const v: TVector4f): TVector3f =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.Vector3SinglePoint;

  Vector3fCut: function(const v: TVector4f): TVector3f =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.Vector3SingleCut;

{ Normal3f zwraca jak Vector3f ale od razu normalizuje wartosci x, y, z }
function Normal3f(x, y: TGLfloat; z: TGLfloat {$IFDEF DEFPARS}=0{$ENDIF}): TVector3f; overload;

const
  Vector3fFromStr: function (const s: string): TVector3Single =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.Vector3SingleFromStr;

  Vector3dFromStr: function (const s: string): TVector3Double =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.Vector3DoubleFromStr;

  Triangle3d: function(const t: TTriangle3Single): TTriangle3Double =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.Triangle3Double;

  BWColor3fvTo1st: procedure(v: PVector3Single) =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.BWColor3SinglevTo1st;

  BWColor3ubvTo1st: procedure(v: PVector3Byte) =
    {$ifdef FPC_OBJFPC} @ {$endif} VectorMath.BWColor3BytevTo1st;

{$endif}

{$ifdef read_implementation}

function Vector2f(x, y: TGLfloat): TVector2f;
begin result[0] := x; result[1] := y end;

function Vector4f(x, y: TGLfloat; z: TGLfloat{=0}; w: TGLfloat{=1}): TVector4f;
begin
 result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4f(const v3: TVector3f; w: TGLfloat{=1}): TVector4f;
begin
 move(v3, result, SizeOf(TVector3f)); result[3] := w;
end;

function Vector4f(const ub: TVector4ub): TVector4f;
begin
 result[0] := ub[0]/255;
 result[1] := ub[1]/255;
 result[2] := ub[2]/255;
 result[3] := ub[3]/255;
end;

function Vector3f(x, y: TGLfloat; z: TGLfloat{=0.0}): TVector3f;
begin
 result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3d(x, y: TGLdouble; z: TGLdouble{=0.0}): TVector3d;
begin
 result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3f(const v3: TVector3d): TVector3f;
begin
 result[0] := v3[0]; result[1] := v3[1]; result[2] := v3[2];
end;

function Vector3f(const v3: TVector3ub): TVector3f; overload;
begin
 result[0] := v3[0]/255;
 result[1] := v3[1]/255;
 result[2] := v3[2]/255;
end;

function Vector3d(const v: TVector3f): TVector3d;
begin
 result[0] := v[0]; result[1] := v[1]; result[2] := v[2];
end;

function Vector3ub(x, y, z: TGLubyte): TVector3ub;
begin
 result[0] := x; result[1] := y; result[2] := z;
end;

function Vector4ub(x, y, z, w: TGLubyte): TVector4ub;
begin
 result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4ub(const f4: TVector4f): TVector4ub;
begin
 result[0] := Round(f4[0]*255);
 result[1] := Round(f4[1]*255);
 result[2] := Round(f4[2]*255);
 result[3] := Round(f4[3]*255);
end;

function Normal3f(x, y: TGLfloat; z: TGLfloat{=0}): TVector3f;
begin
 result[0] := x; result[1] := y; result[2] := z;
 NormalizeTo1st(result);
end;

{$endif}
