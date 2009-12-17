{
  Copyright 2002-2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ @abstract(Classes to handle material information in 3DS files.) }

unit Object3dsMaterial;

interface

uses VectorMath, KambiUtils, Classes, KambiClassUtils, SysUtils, Object3dsChunks;
  
{$define read_interface}

const
  { TODO: I don't know default 3ds material parameters; below I just
    use some default OpenGL and VRML 1.0 values. }
  Default3dsMatAmbient: TVector4Single = (0.2, 0.2, 0.2, 1.0);
  Default3dsMatDiffuse: TVector4Single = (0.8, 0.8, 0.8, 1.0);
  Default3dsMatSpecular: TVector4Single = (0, 0, 0, 1.0);
  Default3dsMatShininess: Single = 0.2; { in range 0..1 }
  
type
  TMaterialMap3ds = record
    Exists:boolean;
    MapFilename:string;
    UScale, VScale, UOffset, VOffset:Single; 
  end;

  TMaterial3ds = class
    FName:string;
    FInitialized:boolean;
  public
    property Name:string read FName;
    
    { jezeli not Initialized to znaczy ze ten material zostal napotkany
      podczas odczytu TTrimesh ale jeszcze nie zostal zdefiniowany w
      pliku .3ds. Tym samym jezeli podczas czytania pliku 3ds natkniemy
      sie na material o ten samej nazwie - powinnismy zainicjowac niniejszy
      obiekt. Jezeli w pliku 3ds napotkamy na material Initialized o tej
      samej nazwie to zwracamy blad (duplicate meterial spec in 3ds file).
      Po zaladowaniu calego pliku mamy pewnosc ze wszystkie uzyte w Trimeshach
      materialy sa na liscie Materials, wystarczy wiec ze sprawdzimy czy
      wszystkie sa Initialized aby miec pewnosc ze wszystkie materialy
      potrzebne do renderowania sceny zostaly odczytane z pliku 3ds. }
    property Initialized:boolean read FInitialized; { =false }

  public
    { wlasciwosci materialu. Maja pewne domyslne wartosci bo
      nikt nie mowi ze w chunku Material sa wszystkie zdefiniowane. }
    AmbientCol:TVector4Single; { = Default3dsMatAmbient }
    DiffuseCol:TVector4Single; { = Default3dsMatDiffuse }
    SpecularCol:TVector4Single; { = Default3dsMatSpecular }

    TextureMap1, TextureMap2:TMaterialMap3ds; { .Exists = false }

    { all Singles below are always read from 3ds file (they are required 
      subchunks of material chunk). They are in range 0..1. }
    Shininess:Single; { = Default3dsShininess }
    ShininessStrenth, Transparency,
      TransparencyFalloff, ReflectBlur :Single; { = 0 } 

    constructor Create(const AName:string);
    
    { czyta CHUNK_MATERIAL inicjujac sie odpowiednio i ustawia Initialized na true }
    procedure ReadFromStream(Stream:TStream; EndPos:Int64);
  end;
  
type
  TObjectsListItem_4 = TMaterial3ds;
  {$I ObjectsList_4.inc}
  TMaterial3dsListBase = TObjectsList_4;
  
  EMaterialNotInitialized = class(EInvalid3dsFile);
  
  TMaterial3dsList = class(TMaterial3dsListBase)
  public
    {zwraca indeks materialu o nazwie MatName; gwarantowane jest ze zwroci
     jakis indeks >=0 : jezeli nie istnieje MatName to go doda i bedzie
     mial material not Initialized}
    function MaterialIndex(const MatName:string):Integer;
    {rzuca wyjatek EMaterialNotInitialized jezeli jest na liscie jakis
     material not Initialized; powinienes to wywolac po odczytaniu calego
     pliku 3ds aby sprawdzic czy wszystkie materialy uzywane przez Trimeshe
     zostaly odczytane z pliku 3ds}
    procedure CheckAllInitialized;
    procedure ReadMaterial(Stream:TStream; EndPos:Int64);
  end;
  
{$undef read_interface}

implementation

uses Images;

{$define read_implementation}
{$I ObjectsList_4.inc}

{ TMaterial3ds ---------------------------------------------------------- }

constructor TMaterial3ds.Create(const AName:string);
begin
 inherited Create;
 FName:=AName;
 FInitialized:=false;
 AmbientCol:=Default3dsMatAmbient;
 DiffuseCol:=Default3dsMatDiffuse;
 SpecularCol:=Default3dsMatSpecular;
 Shininess:=Default3dsMatShininess;
 TextureMap1.Exists:=false;
 TextureMap2.Exists:=false;
end;

procedure TMaterial3ds.ReadFromStream(Stream:TStream; EndPos:Int64);

  function ReadMaterialMap(EndPos:Int64):TMaterialMap3ds;
  const 
    InitialExistingMatMap:TMaterialMap3ds =
    (Exists:true; MapFileName:''; UScale:1; VScale:1; UOffset:0; VOffset:0);
  var h:TChunkHeader;
      hEnd:Int64;
  begin
   result:=InitialExistingMatMap;

   { read MAP subchunks }
   while Stream.Position<EndPos do
   begin
    Stream.ReadBuffer(h, SizeOf(h));
    hEnd:=Stream.Position -SizeOf(TChunkHeader) +h.len;
    case h.id of
     CHUNK_MAP_FILE: result.MapFilename:=StreamReadZeroEndString(Stream);
     CHUNK_MAP_USCALE: Stream.ReadBuffer(result.UScale, SizeOf(Single));
     CHUNK_MAP_VSCALE: Stream.ReadBuffer(result.VScale, SizeOf(Single));
     CHUNK_MAP_UOFFSET: Stream.ReadBuffer(result.UOffset, SizeOf(Single));
     CHUNK_MAP_VOFFSET: Stream.ReadBuffer(result.VOffset, SizeOf(Single));
     else Stream.Position:=hEnd;
    end;
   end;
  end;

var h:TChunkHeader;
    hEnd:Int64;
begin
 { read material subchunks }
 while Stream.Position<EndPos do
 begin
  Stream.ReadBuffer(h, SizeOf(h));
  hEnd:=Stream.Position -SizeOf(TChunkHeader) +h.len;
  case h.id of
   { Colors }
   CHUNK_AMBIENT: TryReadColorInSubchunks(AmbientCol, Stream, hEnd);
   CHUNK_DIFFUSE: TryReadColorInSubchunks(DiffuseCol, Stream, hEnd);
   CHUNK_SPECULAR: TryReadColorInSubchunks(SpecularCol, Stream, hEnd);

   { Percentage values }
   CHUNK_SHININESS: 
     TryReadPercentageInSubchunks(Shininess, Stream, hEnd);    
   CHUNK_SHININESS_STRENTH: 
     TryReadPercentageInSubchunks(ShininessStrenth, Stream, hEnd);
   CHUNK_TRANSPARENCY: 
     TryReadPercentageInSubchunks(Transparency, Stream, hEnd);
   CHUNK_TRANSPARENCY_FALLOFF: 
     TryReadPercentageInSubchunks(TransparencyFalloff, Stream, hEnd);
   CHUNK_REFLECT_BLUR: 
     TryReadPercentageInSubchunks(ReflectBlur, Stream, hEnd);
   
   CHUNK_TEXMAP_1: TextureMap1:=ReadMaterialMap(hEnd);
   CHUNK_TEXMAP_2: TextureMap2:=ReadMaterialMap(hEnd);
   else Stream.Position:=hEnd;
  end;
 end;

 FInitialized:=true;
end;

{ TMaterial3dsList ----------------------------------------------------- }

function TMaterial3dsList.MaterialIndex(const MatName:string):Integer;
begin
 for result:=0 to Count-1 do
  if Items[result].Name = MatName then exit;
 {material not found ?}
 Add(TMaterial3ds.Create(MatName));
 result:=Count-1;
end;

procedure TMaterial3dsList.CheckAllInitialized;
var i:integer;
begin
 for i:=0 to Count-1 do
  if not Items[i].Initialized then
   raise EMaterialNotInitialized.Create(
    'Material "'+Items[i].Name+'" used but does not exist');
end;

procedure TMaterial3dsList.ReadMaterial(Stream:TStream; EndPos:Int64);
var ind:Integer;
    MatName:string;
    StreamStartPos:Int64;
    h:TChunkHeader;
begin
 {kazdy material musi miec chunk MATNAME inaczej material jest calkowicie
  bezuzyteczny (odwolywac sie do materiala mozna tylko poprzez nazwe).
  Ponadto o ile wiem moze byc tylko jeden chunk MATNAME (material moze miec
  tylko jedna nazwe).
  Ponizej szukamy chunka MATNAME aby zidentyfikowac material. }
 StreamStartPos:=Stream.Position;
 MatName:='';
 
 while Stream.Position<EndPos do
 begin
  Stream.ReadBuffer(h, SizeOf(h));
  if h.id = CHUNK_MATNAME then
  begin
   MatName:=StreamReadZeroEndString(Stream);
   break;
  end else
   Stream.Position:=Stream.Position -SizeOf(TChunkHeader) +h.len;
 end;

 Stream.Position:=StreamStartPos; { restore original position in Stream }
 if MatName='' then raise EInvalid3dsFile.Create('Unnamed material');
 
 { znalezlismy MatName; teraz mozemy dodac go do Items, chyba ze juz tam
   jest not Initialized (wtedy wystarczy go zainicjowac) lub Initialized
   (wtedy blad - exception) }
 ind:=MaterialIndex(MatName);
 with Items[ind] do
 begin
  Check3dsFile( not Initialized, 'Duplicate material '+MatName+' specification');
  {jesli not Initialized to znaczy ze material albo zostal dodany do listy
   przez MaterialIndex albo zostal juz gdzies uzyty w trimeshu -
   tak czy siak nie zostal jeszcze zdefiniowany w pliku 3ds.
   Wiec mozemy go teraz spokojnie zainicjowac.}
  ReadFromStream(Stream, EndPos);
 end;
end;

end.
