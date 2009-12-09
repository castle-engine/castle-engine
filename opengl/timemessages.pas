{
  Copyright 2002-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ @abstract(@link(TTimeMessagesManager) class to display some "messages"
  (in OpenGL using GLWindow) that should automatically disappear from
  screen after some short time.)

  You know, messages like "Picked up 20 ammo" or "Player Foo joined game"
  in Quake and similar games.

  Typowe uzycie :
  w glw.OnInit stworz obiekt tej klasy
    TimeMsg := TTimeMessagesManager.Create(glwin,...);
  w glw.OnClose zwolnij go
    FreeAndNil(TimeMsg);
  narysuj go w OnDraw przez Draw2d (musi byc aktywne 2d projection)
  zrob mu Idle w OnIdle
  wszedzie gdzie chcesz zglaszaj message'y przez Show('message');
}

unit TimeMessages;

interface

uses GL, GLU, GLExt, GLWindow, Classes, SysUtils, KambiUtils, KambiGLUtils,
  OpenGLBmpFonts, OpenGLFonts, KambiTimeUtils, VectorMath;

{$define read_interface}

{ ----------------------------------------------------------------------
  wewnetrzny typ TMessageStruct ktory jednak musi byc tu zdefinowany }

type
  TMessageStruct = record
    Text: string;
    Time: TMilisecTime; { czas w jakim sie pojawila }
  end;
  PMessageStruct = ^TMessageStruct;

type
  TDynArrayItem_1 = TMessageStruct;
  PDynArrayItem_1 = PMessageStruct;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I DynArray_1.inc}
  TDynMessageStructArray = TDynArray_1;

{ TTimeMessagesManager -------------------------------------------------- }

type
  THorizPosition = (hpLeft, hpMiddle, hpRight);
  TVertPosition = (vpDown, vpMiddle, vpUp);

  TTimeMessagesManager = class
  private
    { zgloszone messages. Mniejsze numery to starsze messagy (to znaczy ze nowe
      message'y sa dopisywane na koncu) }
    Messages: TDynMessageStructArray;
    FHorizMessgPosition: THorizPosition;
    FVertMessgPosition: TVertPosition;
    FDisplayPixelWidth: integer;
    FMessageFont: TGLBitmapFont_Abstract;
    procedure PostRedisplayMessages;
  private
    FColor: TVector3Single;
  public
    { ile messagy mo¿e byc maksymalnie na ekranie }
    maxMessagesCount: integer; { =10 }
    { ile czasu message mo¿e pozostac na ekranie. Message "wypada" jesli zajdzie
     choc jeden z 2 warunkow - koniec czasu lub potrzeba miejsce na nowy string. }
    messageDuration: TMilisecTime; { =5000 }
    { okienko do ktorego beda wysylane PostRedisplay gdy zajdzie potrzeba jego
      przemalowania (bo pojawi sie nowy mesage lub zniknie stary message).
      Moze byc = nil jezeli nie chcesz zeby bylo wtedy wysylane PostRedisplay
      gdziekolwiek. }
    glwin: TGLWindow;

    property HorizMessgPosition: THorizPosition read FHorizMessgPosition;
    property VertMessgPosition: TVertPosition read FVertMessgPosition;

    { DisplayPixelWidth > 0 oznacza ze znana jest z gory szerokosc PixelWidth
      obszaru na ktorym bedziemy rysowac przez Draw2d. To pozwoli nam
      automatycznie lamac zbyt dlugie message przekazane nam przez Show; }
    property DisplayPixelWidth: integer read FDisplayPixelWidth;

    { Show = zglos nowy message. W wersji (s: string) znaki nl beda automatycznie
      rozpoznane w s  wiec s moze tak naprawde oznaczac wiele linijek tekstu.
      Ponadto jezeli DisplayPixelWidth > 0 to tekst moze zostac polamany tak zeby
      zmiescil sie na DisplayPixelWidth. }
    procedure Show(const s: string); overload;
    procedure Show(s: TStrings); overload;
    procedure Clear; { wyczysc wszystkie aktualne messagy }

    { wywoluj Idle co jakis czas w programie. (typowo, w glw.OnIdle) }
    procedure Idle;
    { Draw2d, konstruktor i destruktor to jedyne metody jakie potrzebuja
      aktywnego kontekstu OpenGL'a zeby dzialac.

      Rysuj Messagy. Matrix powinna byc MODELVIEW, modyfikuje currrent matrix
      (chcesz, to otocz ta proc glPush/PopMatrix).
      Ta proc rysuje zakladajac ze dostepny ekran to 0..GLMaxX, 0..GLMaxY,
      przy czym mniejsze X-y sa w lewo a mniejsze Y-ki sa w dol.
      Jesli nie chcesz, nie musisz podawac tutaj rozmiarow calego okienka.

      Musisz podac PixelWidth/Height ktore mowia ile rzeczywistych pixli
      ma ten obszar ekranu. (Uzywamy fontow bitmapowych wiec zeby
      moc je wysuwac na srodek ekranu itp. musimy operowac nie tylko
      na wspolrzednych OpenGL'a ale tez wiedziec jak one sie maja
      do rzeczywistych pixeli).}
    procedure Draw2d(GLMaxX, GLMaxY, PixelWidth, PixelHeight: integer);

    { This returns whether Draw2d will actually draw anything.
      This just checks whether we have any messages that we want to
      display --- if not, then you don't need to call Draw2d,
      as it will not do anything.

      This function is useful only if you want to draw some background under
      TimeMessages, like in "The Castle". Then you want to know whether
      the background is needed now or not --- that's where this function
      is usefull. }
    function DrawNeeded: boolean;

    { Color used to draw messages. Default value is yellow. }
    property Color: TVector3Single read FColor write FColor;

    { Font used to draw messages. Read-only for now, in the future
      you should be allowed to change it. }
    property MessageFont: TGLBitmapFont_Abstract read FMessageFont;

    { konstrukctor i destruktor musza byc uruchomione w tym kontekscie
      OpenGL'a w ktorym pozniej maja wyswietlac message'y. (typowo -
      w glw.OnInit / OnClose. }
    constructor Create(Aglwin: TGLwindow; AHorizMessgPosition: THorizPosition;
      AVertMessgPosition: TVertPosition; ADisplayPixelWidth: integer);
    destructor Destroy; override;
  end;

{$undef read_interface}

implementation

uses BFNT_BitstreamVeraSans_Unit, KambiLog;

{$define read_implementation}
{$I dynarray_1.inc}

{ TTimeMessagesManager ------------------------------------------------------- }

const HorizMargin = 10; { marginesy wyswietlania, w pixelach }
      VertMargin = 1;

constructor TTimeMessagesManager.Create(Aglwin: TGLwindow;
  AHorizMessgPosition: THorizPosition; AVertMessgPosition: TVertPosition;
  ADisplayPixelWidth: integer);
begin
 inherited Create;
 Messages := TDynMessageStructArray.Create;
 maxMessagesCount := 10;
 messageDuration := 5000;
 glwin := Aglwin;
 FHorizMessgPosition := AHorizMessgPosition;
 FVertMessgPosition := AVertMessgPosition;
 FDisplayPixelWidth := ADisplayPixelWidth;

 FMessageFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
 FColor := Yellow3Single;
end;

destructor TTimeMessagesManager.Destroy;
begin
 FreeAndNil(FMessageFont);

 FreeAndNil(Messages);
 inherited;
end;

procedure TTimeMessagesManager.PostRedisplayMessages;
begin
 if glwin <> nil then glwin.PostRedisplay;
end;

procedure TTimeMessagesManager.Show(s: TStrings);

  procedure AddStrings(s: TStrings);
  var ms: TMessageStruct;
      i: integer;
  begin
   { ponizej jest zapisane prosto cos co teoretycznie moglibysmy znacznie
     zoptymalizowac. Nie optymalizowalem bo przynajmniej w tej chwili
     zamierzam tego uzywac tylko na malych Messages.Count i s.Count. }
   for i := 0 to s.Count-1 do
   begin
    if Messages.Count = maxMessagesCount then Messages.Delete(0, 1);
    ms.Text := s[i];
    ms.Time := GetTickCount;
    Messages.Add(ms);
   end;
  end;

var broken: TStringList;
begin
 if Log then
   WriteLog('Time message', S.Text);

 if DisplayPixelWidth > 0 then
 begin
  broken := TStringList.Create;
  try
   messageFont.BreakLines(s, broken, DisplayPixelWidth - HorizMargin*2);
   AddStrings(broken);
  finally broken.Free end;
 end else
  AddStrings(s);
 PostRedisplayMessages;
end;

procedure TTimeMessagesManager.Show(const s: string);
var strs: TStringList;
begin
 strs := TStringList.Create;
 try
  strs.Text := s;
  Show(strs);
 finally strs.Free end;
end;

procedure TTimeMessagesManager.Clear;
begin
 Messages.SetLength(0);
 PostRedisplayMessages;
end;

procedure TTimeMessagesManager.Draw2d(GLMaxX, GLMaxY, PixelWidth, PixelHeight: integer);
var i: integer;
    x, y: integer;
begin
 glLoadIdentity;
 glColorv(Color);
 for i := 0 to Messages.Count-1 do
 begin
  {ustal x wzgledem 0..PixelWidth, potem zamien to na 0..GLMaxX}
  case HorizMessgPosition of
   hpLeft: x := HorizMargin;
   hpRight: x := PixelWidth-messageFont.TextWidth(messages.Items[i].Text)-HorizMargin;
   hpMiddle: x:=(PixelWidth-messageFont.TextWidth(messages.Items[i].Text)) div 2;
  end;
  x := x * GLMaxX div PixelWidth;

  {podobnie y : najpierw ustal wzgledem 0..PixelHeight, potem zamien na 0..GLMaxY}
  case VertMessgPosition of
   vpDown: y:=(Messages.Count-i-1) * messageFont.RowHeight + messageFont.Descend + VertMargin;
   vpMiddle: y:=(PixelHeight - Messages.Count * messageFont.RowHeight) div 2 + i*messageFont.RowHeight;
   vpUp: y := PixelHeight-(i+1)*messageFont.RowHeight - VertMargin;
  end;
  y := y * GLMaxY div PixelHeight;

  {teraz wyswietl Text na pozycji x, y}
  glRasterPos2i(x, y);
  messageFont.print(messages.Items[i].Text);
 end;
end;

function TTimeMessagesManager.DrawNeeded: boolean;
begin
  Result := Messages.Count <> 0;
end;

procedure TTimeMessagesManager.Idle;
{ sprawdz time-out messagy. Sprawdzamy na podstawie GetTickCount. }
var gtc: TMilisecTime;
    i: integer;
begin
 gtc := GetTickCount;
 for i := Messages.Count-1 downto 0 do
  if TimeTickSecondLater(messages.Items[i].Time, gtc, messageDuration) then
  begin { skasuj messagy 0..i }
   Messages.Delete(0, i+1);
   PostRedisplayMessages;
   break;
  end;
end;

end.
