{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dgraph Pascal units".

  "Kambi's 3dgraph Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dgraph Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dgraph Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(@link(TMatrixNavigator) class and descendants.) }

unit MatrixNavigation;

interface

uses SysUtils, VectorMath, KambiUtils, Keys, Boxes3d;

const
  DefaultFallingDownStartSpeed = 0.1;

type
  { }
  TMatrixNavigator = class;
  TMatrixNavigatorNotifyFunc = procedure (Navigator: TMatrixNavigator) of object;

  T3BoolKeys = array[0..2, boolean]of TKey;

  { TMatrixNavigator to taka klasa ktora pozwala w specjalistyczny sposob
    operowac na macierzy. Ten specjalistyczny sposob polega na tym ze
    za pomoca tej macierzy mozna robic dosc ogolne rzeczy na hipotetycznym
    obiekcie 3d ktory bylby transformowany przez ta macierz
    - np. zeby mozna bylo obracac obiekt jakby byl modelem (TMatrixExaminer)
    albo zeby mozna bylo chodzic po modelu (TMatrixWalker).

    Jak uzywac tej klasy w typowy sposob z TGLWindow + OpenGLem ?
    - Stworz obiekt podklasy TMatrixNavigator w programie, definujac mu
      odpowiednie OnMatrixChanged (typowo robiace cos jak glwin.PostRedisplay)
    - zdefiniuj mu odpowiednie wlasciwosci startowe
      (najlepiej do tego celu uzyc metod Init zdefiniowanych w kazdej podklasie
      z innymi parametrami - dostosowanymi do tego jakiego initu wymaga
      klasa; one zainicjuja stan poczatkowy klasy, wymagajac od ciebie podania
      zasadniczych walsciwosci dla obiektu (ktore nie moga miec zbyt uzytecznych
      domyslnych stanow)).
    - jezeli navigator is TMatrixNavigatorWithIdle to w OnIdle wywoluj
      navigator.Idle
    - W OnDraw uzyj transformacji glMultMatrix(navigator.Matrix)
    - W OnKeyDown uzywaj KeyDown,
      mozesz tez uzywac w dowolnym momencie odpowiednich, specyficznych dla danej
      podklasy, procedury zmieniajacych navigatora jak Rotate, Move, ScaleBy,
      StopRotating itd.

    Po prostsza metode uzycia: patrz TGLWindowNavigated, ono zajmuje sie
    wykonaniem czesci opisanych powyzej rzeczy automatycznie. }
  TMatrixNavigator = class
  protected
    { In this class this calls OnMatrixChanged (if assigned),
      (this may also do some other internal things (e.g. recalculating
      the frustum in TMatrixWalker), so always call inherited in
      descendants). }
    procedure MatrixChanged; virtual;
  public
    { OnMatrixChanged bedzie wywolywane zawsze gdy funkcja Matrix
      zacznie zwracac inne wartosci LUB gdy jakakolwiek wartosc wlasciwosci
      podklasy ulegnie zmianie (jak np. MoveSpeed w TMatrixWalker).
      Moze byc = nil ale zazwyczaj nie powinna - CHCESZ przeciez reagowac na
      zmiany, prawda ? Pamietaj ze kazda zmiana macierzy spowoduje natychmiastowe
      wywolanie OnMatrixChanged - wiec napisz OnMatrixChanged tak zeby dzialalo
      zawsze gdy bedziesz wywolywal jakas metode na tym obiekcie. }
    OnMatrixChanged: TMatrixNavigatorNotifyFunc;
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      virtual;

    { zwraca aktualna macierz przez ktora powinienes przemnozyc kazdy
      punkt sceny po ktorej nawigujesz, tzn. kazdy punkt P powinien
      byc wyswietlony jako Matrix*P. }
    function Matrix: TMatrix4Single; virtual; abstract;
    { zwraca tylko obroty z aktualnej macierzy (bez skalowania, bez przesuniec);
      przydatne aby zastosowac nieba w rodzaju node'a Background VRMLa 97. }
    function RotationOnlyMatrix: TMatrix4Single; virtual; abstract;

    { Zwraca true jezeli jakos obsluzyl klawisz c/key.
      W tej klasie zawsze zwraca false - w podklasach powinny dzialac na
      zasadzie
        result := inherited;
        if result then exit;
        ...i dzialamy, tzn. zawsze obsluga w klasach nadrzednych ma priorytet
        nad osbluga w podklasach.
      Mozesz podac stan aktualnie wcisnietych klawiszy w KeysDown.
        Podaj nil jezeli nie chcesz tego podawac. (zawartosc podanego KeysDown
        nie bedzie zmieniana, bierzemy wskaznik tylko dlatego zebys mogl przekazac
        nil) }
    function KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean; virtual;
  end;

  TMatrixNavigatorClass = class of TMatrixNavigator;

  { ta klasa reprezentuje Navigatory ktore potrzebuja cos takiego jak Idle()
    ktore musi byc wywolywane co mozliwie krotki czas. Argument CompSpeed
    dla tego Idle powinien byc liczony jak TGLWindow.FpsCompSpeed,
    argument KeysDown okresla klawisze wcisniete w czasie idle - powinienes
    tu podac klawisze w rodzaju glwin.KeysDown. Ew. mozesz tu podac nil -
    ale pamietaj ze wtedy nie pozwalasz userowi sterowac w domyslny sposob
    navigatorem, musisz wtedy sam zadbac o odpowiednie sterowanie.
    (zawartosc KeysDown nie bedzie zmieniana) }
  TMatrixNavigatorWithIdle = class(TMatrixNavigator)
  public
    procedure Idle(const CompSpeed: Single; KeysDown: PKeysBooleans); virtual; abstract;
  end;

  { ogladanie modelu. Model jest wyswietlany wokol punktu MoveAmount,
    nastepnie obracany o RotationAngle i skalowany o ScaleFactor
    (juz wzgledem tego punktu MoveAmount). }
  TMatrixExaminer = class(TMatrixNavigatorWithIdle)
  private
    FRotationsSpeed, FRotationsAngle, FMoveAMount, FModelBoxMiddle: TVector3Single;
    FScaleFactor: Single;
    FModelBox: TBox3d;
    procedure SetRotationsSpeed(const Value: TVector3Single);
    procedure SetRotationsAngle(const Value: TVector3Single);
    procedure SetScaleFactor(const Value: Single);
    procedure SetMoveAmount(const Value: TVector3Single);
    procedure SetModelBox(const Value: TBox3d);
  public
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      override;

    function Matrix: TMatrix4Single; override;
    function RotationOnlyMatrix: TMatrix4Single; override;
    procedure Idle(const CompSpeed: Single; KeysDown: PKeysBooleans); override;
    function KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean; override;

    { wlasciwosci ------------------------------------------------------------- }

    { RotationsSpeed[0] to szybkosc z jaka zmienia sie RotationsAngle[0] w czasie
        Idle. RotationsAngle[0] wyznacza obrot wokol osi X (wektora 1, 0, 0)
        (w stopniach). Analogicznie dla indeksow [1] i [2].
        Obroty sa robione wzgledem srodka ModelBox
      MoveAmount okresla jak ma byc przetransformowany punkt (0, 0, 0) modelu
        (poczatkowo bedziesz pewnie chcial zepewne zeby bylo tak ze srodek
        modelu jest widoczny na ekranie, a wiec np. cos w rodzaju
          MoveAmount := ObjectMiddle + (0, 0, -2*ObjectSize)
      ScaleFactor okresla jak obiekt bedzie skalowany (skalowanie tez bedzie
        wokol punktu ModelCenter)
      Idea jest taka zeby ScaleFactor, MoveAmount, RotationsSpeed
        byly modyfikowane przez usera a RotationsAngle byly pozostawione sobie
        samemu. ModelBox wymaga jednokrotnej inicjalizacji i potem czesto moze
        juz byc stale do konca programu. }
    property RotationsSpeed: TVector3Single read FRotationsSpeed write SetRotationsSpeed; { =(0, 0, 0) }
    property RotationsAngle: TVector3Single read FRotationsAngle write SetRotationsAngle; { =(0, 0, 0) }
    property ScaleFactor: Single read FScaleFactor write SetScaleFactor; { =1 }
    property MoveAmount: TVector3Single read FMoveAmount write SetMoveAmount; { =(0, 0, 0) }
    property ModelBox: TBox3d read FModelBox write SetModelBox; { = EmptyBox3d }

    { ustawi ModelBox i ustawi MoveAmount zeby model byl
      dobrze widoczny gdy patrzymy z punktu (0, 0, 0) w strone (0, 0, -1),
      pozostale wartosci ustawi na ich defaultowe wartosci jakie maja
      zaraz po utworzeniu obiektu.
      Innymi slowy zrobi SetModelBox + Home. (czasem mozesz chciec
      wykonac samo SetModelBox zamiast Init, przyklad - patrz sgk_shadows) }
    {sorry-zrobic parametr MoveAmountDefaultZero: boolean, przyda sie w sgk_shadows}
    procedure Init(const AModelBox: TBox3d);
    procedure Home;

    { -------------------------------------------------------------------------
      akcje na obiekcie na ktore zazwyczaj bedziesz chcial pozwolic userowi  }

    { obiekt caly czas obraca sie o RotationsSpeed.
      StopRotating sprawia ze RotationSpeed:=(0, 0, 0).
      Rotate natomiast dodaje do RotationSpeed[coord] SpeedChange (w stopniach). }
    procedure StopRotating;
    procedure Rotate(coord: integer; const SpeedChange: Single);
    procedure Scale(const ScaleBy: Single);
    procedure Move(coord: integer; const MoveDistance: Single);

    { klawisze ---------------------------------------- }
    {sorry - tak samo jak TMatrixWalker, przydaloby sie moc podawac
      tutaj za jednym zamachem char+TKey+modifiers zamiast tylko char
      lub tylko TKey.
      W tym momencie klawisze Keys_Move dzialaja gdy ModifiersDown = [mkCtrl],
      a pozostale klawisze gdy ModifiersDown = []. }
    Keys_Move: T3BoolKeys; { = ((K_Left, K_Right), (K_Down, K_Up), (K_PgDown, K_PgUp)) }
    Keys_Rotate: T3BoolKeys; { = ((K_Up, K_Down), (K_Left, K_Right), (K_PgDown, K_PgUp)) }
    Key_ScaleLarger: TKey; { = K_Plus }
    Key_ScaleSmaller: TKey; { = K_Minus }
    Key_Home: TKey; { = K_Home }
    CharKey_StopRotating: char; { = ' ' }
  end;

  TMatrixWalker = class;

  { See @link(TMatrixWalker.DoMoveAllowed) and
    @link(TMatrixWalker.OnMoveAllowed) }
  TMoveAllowedFunc = function(Navigator: TMatrixWalker;
    const ProposedNewPos: TVector3Single;
    var NewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean;

  { See @link(TMatrixWalker.OnFallenDown). }
  TFalledDownNotifyFunc = procedure (Navigator: TMatrixNavigator;
    const FallenHeight: Single) of object;

  { Walking (DOOM-like moving) over the model.
    Camera is defined by it's position, looking direction
    and up vector, user can rotate and move camera using various keys. }
  TMatrixWalker = class(TMatrixNavigatorWithIdle)
  private
    FCameraPos, FCameraDir, FCameraUp,
    FHomeCameraPos, FHomeCameraDir, FHomeCameraUp: TVector3Single;

    FMoveSpeed, FMoveVertSpeed, FRotateSpeed: Single;

    procedure SetCameraPos(const Value: TVector3Single);
    procedure SetCameraDir(const Value: TVector3Single);
    procedure SetCameraUp(const Value: TVector3Single);

    { Private things related to frustum ---------------------------- }

    FProjectionMatrix: TMatrix4Single;
    FFrustum: TFrustum;
    procedure RecalculateFrustum;
    procedure SetProjectionMatrix(const Value: TMatrix4Single);

    { Private things related to gravity ---------------------------- }

    FCameraHeight: Single;

    FIsFallingDown: boolean;
    FFallingDownStartPos: TVector3Single;
    FOnFalledDown: TFalledDownNotifyFunc;
    FFallingDownStartSpeed: Single;
    FFallingDownSpeed: Single;
    FGravity: boolean;
  protected
    { }
    procedure MatrixChanged; override;
  public
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      override;

    function Matrix: TMatrix4Single; override;
    function RotationOnlyMatrix: TMatrix4Single; override;
    procedure Idle(const CompSpeed: Single; KeysDown: PKeysBooleans); override;
    function KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean; override;

    { This is used by @link(DoMoveAllowed), see there for description. }
    OnMoveAllowed: TMoveAllowedFunc;

    { @abstract(DoMoveAllowed will be used when user will move in the scene,
      i.e. when user will want to change CameraPos.)

      ProposedNewPos is the position where the user wants to move
      (current user position is always stored in CameraPos,
      so you can calculate move direction by ProposedNewPos-CameraPos).

      This is the place to "plug in" your collision detection
      into this object.

      DoMoveAllowed should return false if no move is allowed.
      Else it should return true and set NewPos to the position
      where user should be moved. E.g. if you're doing a simple
      test for collisions (with yes/no results), you will always
      want to set NewPos to ProposedNewPos when returning true.
      But you can also do more sophisticated calculations and
      sometimes not allow user to move to ProposedNewPos, but allow
      him to move instead to some other close position.
      E.g. look what's happening in quake (or just any first-person
      3d game) when you're trying to walk "into the wall"
      at angle like 30 degrees: you're blocked,
      i.e. you obviously can't walk into the wall, but your position
      changes a bit and you're slowly moving alongside the wall.
      That's how you can use NewPos: you can return true and set
      NewPos to something that is not exactly ProposedNewPos
      (but is close to ProposedNewPos).

      Note that it's allowed to modify NewPos when returning false.
      This is meaningless, but may be comfortable for implementor
      of DoMoveAllowed.

      BecauseOfGravity says whether this move is caused by gravity
      dragging the camera down. Can happen only if @link(Gravity)
      is @true. You can use BecauseOfGravity to control DoMoveAllowed
      behavior --- e.g. view3dscene will not allow camera to move
      outside scene's bounding box when BecauseOfGravity
      (because this would mean that camera falls down infinitely),
      on the other hand when BecauseOfGravity is @false moving
      outside bounding box is allowed (to allow viewer to look at the
      scene from "the outside").

      Basic implementation of DoMoveAllowed in this class:
      If OnMoveAllowed = nil then returns true and sets NewPos to
      ProposedNewPos (so move is always allowed).
      Else calls OnMoveAllowed. }
    function DoMoveAllowed(const ProposedNewPos: TVector3Single;
      var NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; virtual;

    { klawisze --------------------------------------------------------- }

    {sorry - przydaloby sie rozwiazac tu sprawe z modifiers jakos bardziej
      elegancko. Kazdy klawisz to powinien byc kod + flagi modifierow.
      W tej chwili klawisze wszystkie ponizsze klawisze dzialaja gdy
      wszystkie modifiery sa OFF, za wyjatkiem Key_Right/LeftRot i
      KeyUp/DownRotate ktore uzyskuja specjalne znaczenie gdy dziala modifier
      Ctrl : obracaja 10 razy wolniej. }

    Key_Forward, Key_Backward, { = K_Up, K_Down }
    Key_RightRot, Key_LeftRot, { = K_Right, K_Left }
    Key_RightStrafe, Key_LeftStrafe, { = K_Z, K_X }
    Key_UpRotate, Key_DownRotate, { = K_PgUp, K_PgDown }
    Key_UpMove, Key_DownMove, { = K_Insert, K_Delete }
    Key_HomeUp, { = K_Home }
    {Key_MoveSpeed* zmieniaja MoveSpeed i MoveVertSpeed jednoczesnie }
    Key_MoveSpeedInc, Key_MoveSpeedDec { = K_Plus, K_Minus } :TKey;

    { wlasciwosci ----------------------------------------------------- }

    { move*Speed sa na poczatku rowne 1 zebys mogl userowi wyswietlac
      te zmienne jako jakas "szybkosc" gdzie 1 oznacza ruch w/g dlugosci
      wektora dir. Pamietaj ze tak naprawde w czasie ruchu uwzgledniamy
      zawsze compSpeed i w ogole nie jest jasne jak szybki tak naprawde
      jest ten ruch - musisz i tak domyslna dlugosc HomeCameraDir dopasowywac
      eksperymentalnie. }
    property MoveSpeed: Single read FMoveSpeed; { =1 }
    property MoveVertSpeed: Single read FMoveVertSpeed; { =1 }
    property RotateSpeed: Single read FRotateSpeed write FRotateSpeed; { =3 (w stopniach) }

    { Camera position, looking direction and up vector.

      Initially (after creating this object) they are equal to initial
      values of HomeCameraPos, HomeCameraDir, HomeCameraUp.
      Also @link(Init) and @link(Home) methods reset them to respective
      HomeCameraXxx values.

      The length of CameraDir vector @bold(is significant) ---
      together with MoveSpeed and MoveVertSpeed it determines
      the moving speed.

      When setting CameraDir, CameraUp will always be automatically
      adjusted to be orthogonal to CameraDir. And vice versa ---
      when setting CameraUp, CameraDir will be adjusted.
      But don't worry, the lengths of adjusted CameraUp or CameraDir
      will always be preserved.

      @groupBegin }
    property CameraPos: TVector3Single read FCameraPos write SetCameraPos;
    property CameraDir: TVector3Single read FCameraDir write SetCameraDir;
    property CameraUp : TVector3Single read FCameraUp  write SetCameraUp;
    { @groupEnd }

    { If PreferHomeUp then various operations are done with respect
      to HomeCameraUp, otherwise they are done with
      respect to current CameraUp (that can be different than HomeCameraUp,
      e.g. after using Key_UpRotate, Key_DownRotate --- raise / bow your head).
      Currently this affects rotations (keys Key_LeftRot and Key_RightRot)
      and vertical moving (keys Key_UpMove and Key_DownMove).

      Polish: W rezultacie przy PreferHomeUp poczucie pionu
      jest jakby silniejsze dla usera bo nawet jesli zadziera/pochyla glowe
      to wektor pionu pozostaje taki sam.
      Podczas gdy z not PreferHomeUp wektor pionu moze sie latwo
      zmieniac, np. wlasnie klawisze Key_Up/DownRotate zmieniaja wektor CameraUp
      a wiec takze wektor wokol ktorego sa robione obroty.

      Podejscie pierwsze (PreferHomeUp = true) jest wygodniejsze dla usera
      jesli ogladana scena jest "silnie zorientowana" wokol wektora HomeUp.
      Natomiast jesli scena nie ma zbyt silnego poczucia pionu (np. jakas scena
      w kosmosie gdzie trudno powiedziec gdzie gora a gdzie dol) lub gdy
      wektor HomeUp niekoniecznie okresla prawidlowo pion sceny (bo np. zgadujemy
      tylko wektor HomeUp, np. w view3dscene zgadujemy go jako (0, 1, 0) bo
      taka jest konwencja w VRMLu ale to tylko konwencja, robiac scene mozna
      sobie ustawic pion dowolnie. Ja sam preferuje przeciez ustawianie
      pionu w (0, 0, 1)) --- wtedy lepiej uzyc PreferHomeUp = false zeby user
      mial szanse swobodnie zmieniac pion. }
    PreferHomeUp: boolean; { = true }

    { ustawianie home camery.
      homeCameraUp bedzie automatycznie poprawione  tak zeby wektory
        homeCameraDir i homeCameraUp definiowaly ciagle ta sama plaszczyzne
        ale zeby byly na niej ponadto prostopadle (bedziemy potrzebowac tej
        prostopadlosci). Wynika z tego warunek : homeCameraUp nie moze byc
        rownolegly do homeCameraDir. }
    property HomeCameraPos: TVector3Single read FHomeCameraPos; { = 0, 0, 0 }
    property HomeCameraDir: TVector3Single read FHomeCameraDir; { = 0, -1, 0 }
    property HomeCameraUp : TVector3Single read FHomeCameraUp;  { = 0, 1, 0 }
    procedure SetCameraHome_LookDir(const AHomeCameraPos, AHomeCameraDir,
      AHomeCameraUp: TVector3Single);
    procedure SetCameraHome_LookAt(const AHomeCameraPos, AHomeCameraCenter,
      AHomeCameraUp: TVector3Single);

    { ustawia wektory Home camery i robi Home, tzn. ustawia
      wektory CameraPos/Dir/Up na ich odpowiedniki z przedrostkiem "Home".
      Uwaga - klawisz Key_HomeUp nie wywoluje ponizszej metody Home, on
      tylko ustawia CameraUp := HomeCameraUp (nie resetuje CameraPos i Dir). }
    procedure Init(const AHomeCameraPos, AHomeCameraDir,
      AHomeCameraUp: TVector3Single); overload;
    { alternatywny Init. Wylicza HomeCameraPos, HomeCameraDir, HomeCameraUp
      takie zeby obiekt w box byl "dobrze widoczny" (cokolwiek by to nie mialo
      oznaczac) }
    procedure Init(const box: TBox3d); overload;
    procedure Home;

    { Things related to frustum ---------------------------------------- }

    { This is initially IdentityMatrix4Single.
      This is not modified anywhere from this class.
      *You* should modify it, you should set it to projection matrix
       that you use, if you want to use Frustum value.
      This is used whenever Frustum is recalculated. }
    property ProjectionMatrix: TMatrix4Single
      read FProjectionMatrix write SetProjectionMatrix;

    { This is recalculated based on @link(ProjectionMatrix) and @link(Matrix)
      whenever one of these properties change.
      This specifies the viewing frustum derived from these properties.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read FFrustum;

    { Things related to gravity ---------------------------------------- }

    { This unlocks a couple of features and automatic behaviors
      related to gravity. This is meaningfull only when PreferHomeUp
      is @true. Gravity always drags the camera down to -HomeCameraUp.

      TODO: write docs based when GravityIdle implementation
      will be finished.

      @noAutoLinkHere }
    property Gravity: boolean
      read FGravity write FGravity default false;

    { When @link(Gravity) is on, the CameraPos tries to stay CameraHeight
      above the ground. Temporary it may be lower (player can
      shortly "duck" when he falls from high).

      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(Gravity) ! 
      
      TODO: unused for now. }
    property CameraHeight: Single
      read FCameraHeight write FCameraHeight default 0.0;

    { This is called when camera was falling down for some time,
      and suddenly stopped (this means that camera "hit the ground").
      Of course this is used only when @link(Gravity) is true.

      It can be useful in games to do some things
      (maybe basing on FallenHeight parameter passed to this callback)
      like lowering player's health and/or making some effects (displaying
      "blackout" or playing sound like "Ouh!" etc.). 
      
      TODO: should work but untested. }
    property OnFalledDown: TFalledDownNotifyFunc
      read FOnFalledDown write FOnFalledDown;

    { Initial speed of falling down.
      Of course this is used only when @link(Gravity) is true.

      Note that while falling down,
      the camera will actually fall with greater and greated speed
      (this adds more realism to the gravity effect...).
      Note that this is always relative to CameraDir length.
      CameraDir determines moving speed --- and so it determines
      also falling speed. The default DefaultFallingDownStartSpeed
      is chosen to be something sensible, to usually get nice effect
      of falling.

      You can change it at any time, but note that if you change this
      while IsFallingDown is @true, then you will not change the
      "current falling down speed". You will change only the falling down
      speed used the next time. }
    property FallingDownStartSpeed: Single
      read FFallingDownStartSpeed write FFallingDownStartSpeed
      default DefaultFallingDownStartSpeed;

    property IsFallingDown: boolean read FIsFallingDown;
  end;

implementation

uses Math;

{ TMatrixNavigator ------------------------------------------------------------ }

procedure TMatrixNavigator.MatrixChanged;
begin
 if Assigned(OnMatrixChanged) then OnMatrixChanged(Self);
end;

constructor TMatrixNavigator.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
begin
 inherited Create;
 OnMatrixChanged := AOnMatrixChanged;
end;

function TMatrixNavigator.KeyDown(key: TKey; c: char;
  KeysDown: PKeysBooleans): boolean;
begin result := false end;

{ TMatrixExaminer ------------------------------------------------------------ }

const
  DefaultKeys_Move: T3BoolKeys =
    ((K_Left, K_Right), (K_Down, K_Up), (K_PgDown, K_PgUp));
  DefaultKeys_Rotate: T3BoolKeys =
    ((K_Up, K_Down), (K_Left, K_Right), (K_PgDown, K_PgUp));

constructor TMatrixExaminer.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
begin
 inherited;
 FScaleFactor := 1;
 FModelBox := EmptyBox3d;

 { default keys }
 Keys_Move := DefaultKeys_Move;
 Keys_Rotate := DefaultKeys_Rotate;
 Key_ScaleLarger := K_Plus;
 Key_ScaleSmaller := K_Minus;
 Key_Home := K_Home;
 CharKey_StopRotating := ' ';
end;

function TMatrixExaminer.Matrix: TMatrix4Single;
begin
 result := TranslationMatrix(VectorAdd(MoveAmount, FModelBoxMiddle));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[0], Vector3Single(1, 0, 0)));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[1], Vector3Single(0, 1, 0)));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[2], Vector3Single(0, 0, 1)));
 result := MultMatrices(result, ScalingMatrix(Vector3Single(ScaleFactor, ScaleFactor, ScaleFactor)));
 result := MultMatrices(result, TranslationMatrix(VectorNegate(FModelBoxMiddle)));
end;

function TMatrixExaminer.RotationOnlyMatrix: TMatrix4Single;
begin
 result := RotationMatrixDeg(RotationsAngle[0], Vector3Single(1, 0, 0));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[1], Vector3Single(0, 1, 0)));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[2], Vector3Single(0, 0, 1)));
end;

procedure TMatrixExaminer.Idle(const CompSpeed: Single; KeysDown: PKeysBooleans);
var i: integer;
    move_change, rot_speed_change, scale_change: Single;
    ModsDown: TModifierKeys;
begin
 ModsDown := ModifiersDown(KeysDown);

 { porownujemy RotationsAngle z (0, 0, 0). W ten sposob jezeli wywolales ostatnio
   StopRotating to teraz nie bedziemy w Idle ciagle generowac MatrixChanged. }
 if (RotationsSpeed[0] <> 0) or
    (RotationsSpeed[1] <> 0) or
    (RotationsSpeed[2] <> 0) then
 begin
  for i := 0 to 2 do RotationsAngle[i] += RotationsSpeed[i]* CompSpeed;
  MatrixChanged;
 end;

 if KeysDown <> nil then
 begin
  if IsEmptyBox3d(ModelBox) then
   move_change := 0.02 * CompSpeed else
   move_change := Box3dAvgSize(ModelBox) * 0.02 * CompSpeed;
  rot_speed_change := 0.1 * CompSpeed;
  scale_change := 1.1; { nie mnoz tego razy compSpeed - scale_change bedzie uzywane do mnozenia }

  if ModsDown=[mkCtrl] then
  begin
   for i := 0 to 2 do
   begin
    if KeysDown[Keys_Move[i, true ]] then Move(i, +move_change);
    if KeysDown[Keys_Move[i, false]] then Move(i, -move_change);
   end;
  end else
  if ModsDown=[] then
  begin
   for i := 0 to 2 do
   begin
    if KeysDown[Keys_Rotate[i, true]]  then Rotate(i, +rot_speed_change);
    if KeysDown[Keys_Rotate[i, false]] then Rotate(i, -rot_speed_change);
   end;
  end;

  if KeysDown[Key_ScaleLarger] then Scale(scale_change);
  if KeysDown[Key_ScaleSmaller] then Scale(1/scale_change);
 end;
end;

procedure TMatrixExaminer.StopRotating;
begin FRotationsSpeed := ZeroVector3Single; MatrixChanged; end;

procedure TMatrixExaminer.Rotate(coord: integer; const SpeedChange: Single);
begin FRotationsSpeed[coord] += SpeedChange; MatrixChanged; end;

procedure TMatrixExaminer.Scale(const ScaleBy: Single);
begin FScaleFactor *= ScaleBy; MatrixChanged; end;

procedure TMatrixExaminer.Move(coord: integer; const MoveDistance: Single);
begin FMoveAmount[coord] += MoveDistance; MatrixChanged; end;

procedure TMatrixExaminer.Init(const AModelBox: TBox3d);
begin
 ModelBox := AModelBox;
 Home;
end;

{ TMatrixExaminer.Set* properties }

procedure TMatrixExaminer.SetRotationsSpeed(const Value: TVector3Single);
begin FRotationsSpeed := Value; MatrixChanged; end;

procedure TMatrixExaminer.SetRotationsAngle(const Value: TVector3Single);
begin FRotationsAngle := Value; MatrixChanged; end;

procedure TMatrixExaminer.SetScaleFactor(const Value: Single);
begin FScaleFactor := Value; MatrixChanged; end;

procedure TMatrixExaminer.SetMoveAmount(const Value: TVector3Single);
begin FMoveAmount := Value; MatrixChanged; end;

procedure TMatrixExaminer.Home;
begin
 if IsEmptyBox3d(FModelBox) then
  FMoveAmount := Vector3Single(0, 0, 0) { any dummy value } else
  FMoveAmount := VectorAdd(
    VectorNegate(FModelBoxMiddle),
    Vector3Single(0, 0, -Box3dAvgSize(FModelBox)*2));
 FRotationsAngle := ZeroVector3Single;
 FRotationsSpeed := ZeroVector3Single;
 FScaleFactor := 1.0;

 MatrixChanged;
end;

procedure TMatrixExaminer.SetModelBox(const Value: TBox3d);
begin
 FModelBox := Value;
 if IsEmptyBox3d(FModelBox) then
  FModelBoxMiddle := Vector3Single(0, 0, 0) { any dummy value } else
  FModelBoxMiddle := Box3dMiddle(FModelBox);
 MatrixChanged;
end;

{ TMatrixExaminer Key* methods ---------------------------------------- }

function TMatrixExaminer.KeyDown(key: TKey; c: char;
  KeysDown: PKeysBooleans): boolean;
begin
 result := inherited;
 if result then Exit;

 if (c <> #0) and (c = CharKey_StopRotating) then
  StopRotating else
 if (Key <> K_None) and (ModifiersDown(KeysDown) = []) and (key = Key_Home) then
  Home else
  Exit(false);

 Result := true;
end;

{ TMatrixWalker ---------------------------------------------------------------- }

constructor TMatrixWalker.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
begin
 inherited;
 FHomeCameraPos := Vector3Single(0, 0, 0);  FCameraPos := HomeCameraPos;
 FHomeCameraDir := Vector3Single(0, 0, -1); FCameraDir := HomeCameraDir;
 FHomeCameraUp  := Vector3Single(0, 1, 0);  FCameraUp  := HomeCameraUp;

 FMoveSpeed := 1;
 FMoveVertSpeed := 1;
 FRotateSpeed := 3;
 FFallingDownStartSpeed := DefaultFallingDownStartSpeed;
 PreferHomeUp := true;

 Key_Forward := K_Up;
 Key_Backward := K_Down;
 Key_RightRot := K_Right;
 Key_LeftRot := K_Left;
 Key_RightStrafe := K_X;
 Key_LeftStrafe := K_Z;
 Key_UpRotate := K_PgUp;
 Key_DownRotate := K_PgDown;
 Key_UpMove := K_Insert;
 Key_DownMove := K_Delete;
 Key_HomeUp := K_Home;
 Key_MoveSpeedInc := K_Plus;
 Key_MoveSpeedDec := K_Minus;

 FProjectionMatrix := IdentityMatrix4Single;
end;

function TMatrixWalker.Matrix: TMatrix4Single;
begin
 result := LookDirMatrix(CameraPos, CameraDir, CameraUp);
end;

function TMatrixWalker.RotationOnlyMatrix: TMatrix4Single;
begin
 result := LookDirMatrix(ZeroVector3Single, CameraDir, CameraUp);
end;

function TMatrixWalker.DoMoveAllowed(const ProposedNewPos: TVector3Single;
  var NewPos: TVector3Single; const BecauseOfGravity: boolean): boolean;
begin
 if Assigned(OnMoveAllowed) then
  Result := OnMoveAllowed(Self, ProposedNewPos, NewPos, BecauseOfGravity) else
 begin
  Result := true;
  NewPos := ProposedNewPos;
 end;
end;

procedure TMatrixWalker.Idle(const CompSpeed: Single; KeysDown: PKeysBooleans);

  { Tries to move CameraPos to CameraPos + MoveVector.
    Returns DoMoveAllowed result. So if it returns @false,
    you know that CameraPos didn't change (on the other hand,
    if it returns @true, you don't know anything --- maybe CameraPos
    didn't change, maybe it changed to CameraPos + MoveVector,
    maybe it changed to something different). }
  function Move(const MoveVector: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  var
    ProposedNewPos, NewPos: TVector3Single;
  begin
    ProposedNewPos := VectorAdd(CameraPos, MoveVector);
    Result := DoMoveAllowed(ProposedNewPos, NewPos, BecauseOfGravity);
    if Result then
      { Note that setting CameraPos automatically calls MatrixChanged }
      CameraPos := NewPos;
  end;

  procedure MoveHorizontal(const Multiply: Integer = 1);
  begin
    Move(VectorScale(CameraDir, MoveSpeed * CompSpeed * Multiply), false);
  end;

  procedure MoveVertical(const Multiply: Integer);

    procedure MoveVerticalCore(const PreferredUpVector: TVector3Single);
    begin
      Move(VectorScale(PreferredUpVector,
        { VectorLen(CameraDir) / VectorLen(PreferredUpVector) * }
        Sqrt(VectorLenSqr(CameraDir) / VectorLenSqr(PreferredUpVector)) *
        MoveVertSpeed * CompSpeed * Multiply), false);
    end;

  begin
    if PreferHomeUp then
      MoveVerticalCore(HomeCameraUp) else
      MoveVerticalCore(CameraUp);
  end;

  procedure RotateAroundHomeUp(AngleDeg: Single);
  var Axis: TVector3Single;
  begin
   { nie obracamy cameraDir wokol cameraUp, takie obroty w polaczeniu z
     obrotami vertical moglyby sprawic ze kamera staje sie przechylona w
     stosunku do plaszczyny poziomu (plaszczyzny dla ktorej wektorem normalnym
     jest homeCameraUp) (a my chcemy zeby zawsze plaszczyzna wyznaczana przez
     wektory Dir i Up byla prostopadla do plaszczyzny poziomu - bo to po prostu
     daje wygodniejsze sterowanie (chociaz troche bardziej ograniczone -
     jestesmy wtedy w jakis sposob uwiazani do plaszczyzny poziomu)).

     Acha, i jeszcze jedno : zeby trzymac zawsze obroty w ta sama strone
     (ze np. strzalka w lewo zawsze powoduje ze swiat ze obraca w prawo
     wzgledem nas) musze czasami obracac sie wokol homeCameraUp, a czasem
     wokol -homeCameraUp.
   }
   if AngleRadBetweenVectors(CameraUp, HomeCameraUp) > Pi/2 then
    Axis := VectorNegate(HomeCameraUp) else
    Axis := HomeCameraUp;

   FCameraUp := RotatePointAroundAxisDeg(AngleDeg, CameraUp, Axis);
   FCameraDir := RotatePointAroundAxisDeg(AngleDeg, CameraDir, Axis);

   MatrixChanged;
  end;

  procedure RotateAroundUp(AngleDeg: Single);
  begin
   { W TYM MIEJSCU POTRZEBUJEMY aby cameraDir i cameraUp byly prostopadle ! }
   CameraDir := RotatePointAroundAxisDeg(AngleDeg, CameraDir, CameraUp);
  end;

  procedure RotateVertical(AngleDeg: Single);
  var Side: TVector3Single;
  begin
   Side := VectorProduct(CameraDir, CameraUp);
   {obroc cameraDir i Up wokolo wektora Side}
   FCameraDir := RotatePointAroundAxisDeg(AngleDeg, CameraDir, Side);
   FCameraUp := RotatePointAroundAxisDeg(AngleDeg, CameraUp,  Side);
   MatrixChanged;
  end;

  procedure CheckRotates(SpeedScale: Single);
  { sprawdz czy wcisnieto KeyRight/LeftRot i jesli tak to zareaguj odpowiednio.
    Uzyj SpeedScale aby skalowac szybkosc obracania sie, tzn. defaltowa
    szybkosc obracania sie = 1.0 }
  begin
   if PreferHomeUp then
   begin
    if KeysDown[Key_RightRot] then RotateAroundHomeUp(-RotateSpeed * CompSpeed * SpeedScale);
    if KeysDown[Key_LeftRot] then RotateAroundHomeUp(RotateSpeed * CompSpeed * SpeedScale);
   end else
   begin
    if KeysDown[Key_RightRot] then RotateAroundUp(-RotateSpeed * CompSpeed * SpeedScale);
    if KeysDown[Key_LeftRot] then RotateAroundUp(RotateSpeed * CompSpeed * SpeedScale);
   end;

   if KeysDown[Key_UpRotate] then RotateVertical(RotateSpeed * CompSpeed * SpeedScale);
   if KeysDown[Key_DownRotate] then RotateVertical(-RotateSpeed * CompSpeed * SpeedScale);
  end;

  { Things related to gravity --- jumping, taking into account
    falling down and keeping CameraHeight above the ground. }
  procedure GravityIdle;
  const
    Jumping = false;

    function TryJump: boolean;
    begin
      Result := false; { TODO --- see szklane_lasy }

      (*
      result:=false;
      if Podskakuje then
      begin
       result:=true;
       WysokoscPodskoku:=WysokoscPodskoku + SilaPodskoku;
       if (WysokoscPodskoku > MaxWysokoscPodskoku) or (SilaPodskoku <= 0) then
        Podskakuje:=false else
       begin
        RuchZ(SilaPodskoku,false);
        SilaPodskoku:=SilaPodskoku - TimeBasedIntChange(15);
       end;
      end;
      *)
    end;

    function TryGrow: boolean;
    begin
      Result := false; { TODO --- see szklane_lasy }

      (*
      {rosnij, tzn. stan na podlodze, jesli jest pod toba i wychodzi ze jestes za nisko
       (taki stan moze byc wynikiem np. liliputa lub konca flying). Zwroc true jezeli
       rosles (a wiec nie bylo potrzeby Spadania w tej turze, przynajmniej przed ta funkcja
       (bo moze urosnac czasem za duzo; to nic, w nasteppnej turze bedzie wtedy spadal)).  }
      var powinienUrosnac:integer;
      begin
       result:=false;
       powinienUrosnac:=wysokoscPostaci - wysGlowyNadZiemia(wysokoscPostaci);
       if powinienUrosnac>marginesSciany then
       begin
        {ponizej nakladamy kaganiec na rosniecie - nie moze urosnac za duze,
         bo zauwazmy ze w TrySpadaj moze spasc za duzo. Gdyby mogl i spasc i urosnac
         za duzo pojawialaby sie sytuacja ze gracz podskakuje w miejscu bo
         na zmiane wykrywamy ze trzeba spasc  i rosnac. }
        RuchZ(KambiUtils.min(graczPoleW div 10,powinienUrosnac),true);
        result:=true;
       end;
      end;
      *)
    end;

    function TryFallingDown: boolean;
    { TODO --- finish, see szklane_lasy }
    const
      CamRotateDeviation = 1;
    var
      CameraPosBefore: TVector3Single;
    begin
      Result := false;
      if Jumping then
      begin
        FIsFallingDown := false;
        Exit;
      end;

      { Make sure that FallingDownSpeed is initialized.
        When IsFallingDown, we know it's initialized (because setting
        "FIsFallingDown := true;" is done only in the piece of code below...),
        otherwise we make sure it's set to it's starting value. }
      if not FIsFallingDown then
        FFallingDownSpeed := FallingDownStartSpeed;

      { try to fall down }
      CameraPosBefore := CameraPos;

      if Move(VectorScale(HomeCameraUp, -
        { VectorLen(CameraDir) / VectorLen(HomeCameraUp) * }
        Sqrt(VectorLenSqr(CameraDir) / VectorLenSqr(HomeCameraUp)) *
        FFallingDownSpeed * CompSpeed), true) and
        (not VectorsPerfectlyEqual(CameraPos, CameraPosBefore)) then
      begin
        FIsFallingDown := true;

        { Note that when changing FFallingDownSpeed below I'm using CompSpeed.
          And also above when using FFallingDownSpeed, I multipled
          FFallingDownSpeed * CompSpeed. This is correct:
          - changing position based on FallingDownSpeed is a "velocity"
          - changing FallingDownSpeed below is "acceleration"
          And both acceleration and velocity must be time-based. }
        FFallingDownSpeed *= Power(13/12, CompSpeed);
      end else
        FIsFallingDown := false;
(*
        { TODO: restore this:
        if FallingDownSpeed > CameraHeight div 8 then
        begin
          if spadanie_speed>graczPoleW div 3 then
            ZmienKier(Round(RandomPlusMinus* Random(3)* glw.FpsCompSpeed));
          if CamRotate<0 then CamRotate:=CamRotate - CamRotateDeviation*glw.FpsCompSpeed else
          if CamRotate>0 then CamRotate:=CamRotate + CamRotateDeviation*glw.FpsCompSpeed else
            CamRotate:=RandomPlusMinus* CamRotateDeviation* glw.FpsCompSpeed;
        end;
        }

       RuchZ(-KambiUtils.min(spadanie_speed,
                          wysGlowyNadZiemia(spadanie_speed) - marginesSciany),true);
         { powyzej jest to min() bo spadanie_speed moze byc duze
           i w rezultacie powyzszy RuchZ moglby nie przesunac gracza w ogole
           mimo ze wysokoscNog > 0.

           Jednoczesnie, prawa liczba jest wieksza niz wysokoscNog co pozwoli
           nam na lekkie sprezynowanie gracza, tzn. gdy spadnie z duzej wysokosci
           - ugna sie pod nim nogi.

           sorry - powyzsze mozna by bylo wykonac prostym
           RuchZ(-spadanie_speed, true); gdyby ruch byl sprytny i ruszal
           sie "w danym kierunku tyle ile mozna"; to z kolei mozna by zaimplementowac
           gdyby odpowiednio rozszerzyc modul Collisions. Przy okazji, to rozszerzenie
           nadawaloby sie tez do wykorzystania w wysGlowyNadZiemia.
         }
       if not wysGlowyNadZiemiaGreater(wysokoscPostaci) then
       begin
        {acha ! to znaczy ze wlasnie skonczyl spadac.
         W tym momencie wielkosc spadanie_speed mowi nam jak dlugo spadal.}
        if spadanie_speed > graczPoleW div 2 then DoBlackOut(Red3Single);
        {sorry - tu moznaby modyfikowac jego Zycie, czy Zywotnosc, gdy juz
         zrobimy graczowi taki wskaznik}
       end else
       begin
         ... { This is already restored ... }
       end;
       result:=true; *)
    end;

    procedure StabilizeCameraHeight;
    begin
      { TODO --- see szklane_lasy }

      (*
      {ta procedura bedzie wywolywana gdy gracz bedzie spokojnie sobie stal
       na ziemi. Nie bedzie w trakcie zadnego podskoku ani nie bedzie spadal ani nie
       bedzie rosl. Wiec w tej procedurze ma chwile oddechu i moze ustabilizowac
       swoje zmienne. }
      const CamRotateNormalization = 7;
      begin
       {prostuj CamRotate}
       if CamRotate<0 then
        CamRotate:=KambiUtils.min(CamRotate + CamRotateNormalization*glw.FpsCompSpeed, 0.0) else
       if CamRotate>0 then
        CamRotate:=KambiUtils.max(CamRotate - CamRotateNormalization*glw.FpsCompSpeed, 0.0);

       {spadanie_speed moze wrocic do normalnego stanu poczatkowego}
       spadanie_speed:=spadanie_speed_normal;
      end;
      *)
    end;

  var
    OldIsFallingDown: boolean;
  begin
    OldIsFallingDown := IsFallingDown;

    if Gravity and PreferHomeUp then
      if not TryJump then
        if not TryGrow then
          if not TryFallingDown then
            StabilizeCameraHeight;

    if OldIsFallingDown and (not IsFallingDown) and Assigned(OnFalledDown) then
      OnFalledDown(Self, PointsDistance(CameraPos, FFallingDownStartPos));
  end;

var ModsDown: TModifierKeys;
begin
 ModsDown := ModifiersDown(KeysDown);

 if ModsDown=[] then
 begin
  CheckRotates(1.0);

  if KeysDown[Key_Forward] then MoveHorizontal;
  if KeysDown[Key_Backward] then MoveHorizontal(-1);

  { do strafe'ow musimy uzywac RotateAroundUp, bez wzgledu na to czego
    uzywamy do normalnych obrotow (na *rot). To dlatego ze gdy camDir
    jest pochylony (tzn. nie jest prostopadly do HomeUp) to obrocenie
    camDir o 90 wokol homeUp NIE da wektora prostopadlego do camDir
    (tylko ich rzuty na plaszczyzne wyznaczana przez wektor homeUp beda
    prostopadle). Musimy do tego uzyc RotateAroundUp. }
  if KeysDown[Key_RightStrafe] then
   begin RotateAroundUp(-90); MoveHorizontal; RotateAroundUp(90); end;
  if KeysDown[Key_LeftStrafe] then
   begin RotateAroundUp(90); MoveHorizontal; RotateAroundUp(-90); end;

  { A simple implementation of Key_UpMove was
      RotateVertical(90); Move(MoveVertSpeed * CompSpeed); RotateVertical(-90)
    Similarly, simple implementation of Key_DownMove was
      RotateVertical(-90); Move(MoveVertSpeed * CompSpeed); RotateVertical(90)
    But this is not good, because when PreferHomeUp, we want to move along the
    CameraHomeUp. }
  if KeysDown[Key_UpMove] then
    MoveVertical( 1);
  if KeysDown[Key_DownMove] then
    MoveVertical(-1);

  { zmiana szybkosci nie wplywa na Matrix (nie od razu). Ale wywolujemy
    MatrixChanged - zmienilismy swoje wlasciwosci, moze sa one np. gdzies
    wypisywane w oknie na statusie i okno potrzebuje miec PostRedisplay po zmianie
    Move*Speed ?.

    How to apply CompSpeed here ?
    I can't just ignore CompSpeed, but I can't also write
      FMoveSpeed *= 1.1 * CompSpeed;
    What I want is such (pl: ci±g³a) function that e.g.
      F(FMoveSpeed, 2) = F(F(FMoveSpeed, 1), 1)
    I.e. CompSpeed = 2 should work just like doing the same change twice.
    So F is FMoveSpeed * Power(1.1, CompSpeed)
    Easy!
  }
  if KeysDown[Key_MoveSpeedInc] then
  begin
   FMoveSpeed *= Power(1.1, CompSpeed);
   FMoveVertSpeed *= Power(1.1, CompSpeed);
   MatrixChanged;
  end;
  if KeysDown[Key_MoveSpeedDec] then
  begin
   FMoveSpeed /= Power(1.1, CompSpeed);
   FMoveVertSpeed /= Power(1.1, CompSpeed);
   MatrixChanged;
  end;
 end else
 if ModsDown = [mkCtrl] then
 begin
  CheckRotates(0.1);
 end;

 GravityIdle;
end;

procedure TMatrixWalker.Home;
begin
 { nie tylko nie powinienem tutaj poslugiwac sie propertami CameraXxx
   tylko zamiast tego polami FCameraXxx zeby zaoszczedzic na czasie
   (w ten sposob moge wywolac MatrixChanged tylko raz). Ale nawet MUSZE
   tak robic bo pamietaj ze ustawienie CameraDir i CameraUp pociaga za soba
   ew. dostosowanie wektora up (aby byl prostopadly do wektra dir).
   W zwiazku z tym wymagam aby zawsze podawany Dir i Up nie byly rownolegle -
   ale w takim razie powinienem tez ustawiac je jednoczesnie, tzn. nie
   chce ponizej ustawic CameraXxx wymuszajac aby aktualny CameraUp sie
   dostosowal (a przeciez aktualny CameraUp moze byc rownolegly do nowego
   CameraDir !). Zamiast tego chce ustawic jednoczesnie FCameraDir i FCameraUp.
   Ponadto nalezy zwrocic uwage ze w tym przypadku w ogole nie musze sie martwic
   o poprawianie CameraUp bo przeciez HomeCameraDir/Up JUZ sa odpowiednio
   poprawione. }
 FCameraPos := HomeCameraPos;
 FCameraDir := HomeCameraDir;
 FCameraUp := HomeCameraUp;
 MatrixChanged;
end;

function TMatrixWalker.KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean;
begin
 result := inherited;
 if result then exit;

 { nie interesuja nas tutaj zdarzenia z Key = K_None }
 if Key = K_None then Exit;

 if ModifiersDown(KeysDown)<>[] then Exit(false);

 if Key = Key_HomeUp then
  CameraUp := HomeCameraUp else
 {tu dopisuj inne klawisze jako
 if Key = <key> then
  <do-somthing> else}
  Exit(false);

 result := true;
end;

procedure TMatrixWalker.Init(const AHomeCameraPos, AHomeCameraDir, AHomeCameraUp: TVector3Single);
begin
 SetCameraHome_LookDir(AHomeCameraPos, AHomeCameraDir, AHomeCameraUp);
 Home;
end;

procedure TMatrixWalker.Init(const Box: TBox3d);
var Pos: TVector3Single;
    AvgSize: Single;
begin
 if IsEmptyBox3d(Box) then
  Init(Vector3Single(0, 0, 0),
       Vector3Single(0, 0, -1),
       Vector3Single(0, 1, 0)) else
 begin
  AvgSize := Box3dAvgSize(Box);
  Pos[0] := Box[0, 0]-AvgSize;
  Pos[1]:=(Box[0, 1]+Box[1, 1])/2;
  Pos[2]:=(Box[0, 2]+Box[1, 2])/2;
  Init(Pos, VectorAdjustToLength(UnitVector3Single[0], AvgSize*0.1),
    UnitVector3Single[2]);
 end;
end;

procedure TMatrixWalker.SetCameraHome_LookDir(const AHomeCameraPos,
  AHomeCameraDir, AHomeCameraUp: TVector3Single);
begin
 FHomeCameraPos := AHomeCameraPos;
 FHomeCameraDir := AHomeCameraDir;
 FHomeCameraUp := AHomeCameraUp;
 MakeVectorsAngleOnTheirPlane(FHomeCameraUp, FHomeCameraDir, 90);
 MatrixChanged;
end;

procedure TMatrixWalker.SetCameraHome_LookAt(const AHomeCameraPos,
  AHomeCameraCenter, AHomeCameraUp: TVector3Single);
begin
 SetCameraHome_LookDir(AHomeCameraPos, VectorSubtract(AHomeCameraCenter,
   AHomeCameraPos), AHomeCameraUp);
end;

procedure TMatrixWalker.SetCameraPos(const Value: TVector3Single);
begin FCameraPos := Value; MatrixChanged; end;

procedure TMatrixWalker.SetCameraDir(const Value: TVector3Single);
begin
 FCameraDir := Value;
 MakeVectorsAngleOnTheirPlane(FCameraUp, FCameraDir, 90);
 MatrixChanged;
end;

procedure TMatrixWalker.SetCameraUp(const Value: TVector3Single);
begin
 FCameraUp := Value;
 MakeVectorsAngleOnTheirPlane(FCameraDir, FCameraUp, 90);
 MatrixChanged;
end;

procedure TMatrixWalker.RecalculateFrustum;
begin
 CalculateFrustum(FFrustum, ProjectionMatrix, Matrix);
end;

procedure TMatrixWalker.MatrixChanged;
begin
 RecalculateFrustum;
 inherited;
end;

procedure TMatrixWalker.SetProjectionMatrix(const Value: TMatrix4Single);
begin
 FProjectionMatrix := Value;
 RecalculateFrustum;
end;

end.
