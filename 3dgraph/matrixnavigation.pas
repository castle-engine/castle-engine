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
  DefaultFallingDownStartSpeed = 0.5;
  DefaultGrowingSpeed = 1.0;
  DefaultHeadBobbing = 0.1;
  DefaultCrouchHeight = 0.5;
  DefaultMaxJumpHeight = 1.0;
  DefaultMinAngleRadFromHomeUp = { 10 degress } Pi / 18;
  DefaultRotationHorizontalSpeed = 3.0;
  DefaultRotationVerticalSpeed = 2.0;
  DefaultFallingDownSpeedIncrease = 13/12;
  DefaultMouseLookHorizontalSensitivity = 0.09;
  DefaultMouseLookVerticalSensitivity = 0.09;
  DefaultHeadBobbingDistance = 20.0;
  DefaultJumpSpeedMultiply = 2.0;
  DefaultJumpPower = 0.18;

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
      wykonac samo SetModelBox zamiast Init, przyklad - patrz sgk_shadows)

      TODO: zrobic parametr MoveAmountDefaultZero: boolean,
      przyda sie w sgk_shadows }
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

    { TODO: tak samo jak TMatrixWalker, przydaloby sie moc podawac
      tutaj za jednym zamachem char+TKey+modifiers zamiast tylko char
      lub tylko TKey.
      W tym momencie klawisze Keys_Move dzialaja gdy ModifiersDown = [mkCtrl],
      a pozostale klawisze gdy ModifiersDown = []. }

    Keys_Move: T3BoolKeys; { = ((K_Left, K_Right), (K_Down, K_Up), (K_PageDown, K_PageUp)) }
    Keys_Rotate: T3BoolKeys; { = ((K_Up, K_Down), (K_Left, K_Right), (K_PageDown, K_PageUp)) }
    Key_ScaleLarger: TKey; { = K_Numpad_Plus }
    Key_ScaleSmaller: TKey; { = K_Numpad_Minus }
    Key_Home: TKey; { = K_Home }
    CharKey_StopRotating: char; { = ' ' }
  end;

const
  { Default values of Key_Xxx properties of TMatrixWalker.
    @groupBegin }
  WalkerDefaultKey_Forward = K_Up;
  WalkerDefaultKey_Backward = K_Down;
  WalkerDefaultKey_LeftRot = K_Left;
  WalkerDefaultKey_RightRot = K_Right;
  WalkerDefaultKey_LeftStrafe = K_Comma;
  WalkerDefaultKey_RightStrafe = K_Period;
  WalkerDefaultKey_UpRotate = K_PageUp;
  WalkerDefaultKey_DownRotate = K_PageDown;
  WalkerDefaultKey_UpMove = K_Insert;
  WalkerDefaultKey_DownMove = K_Delete;
  WalkerDefaultKey_HomeUp = K_Home;
  WalkerDefaultKey_MoveSpeedInc = K_Numpad_Plus;
  WalkerDefaultKey_MoveSpeedDec = K_Numpad_Minus;
  WalkerDefaultKey_Jump = K_A;
  WalkerDefaultKey_Crouch = K_Z;
  { @groupEnd }

type
  TMatrixWalker = class;

  { See @link(TMatrixWalker.DoMoveAllowed) and
    @link(TMatrixWalker.OnMoveAllowed) }
  TMoveAllowedFunc = function(Navigator: TMatrixWalker;
    const ProposedNewPos: TVector3Single;
    var NewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean of object;

  { See @link(TMatrixWalker.OnFalledDown). }
  TFalledDownNotifyFunc = procedure (Navigator: TMatrixWalker;
    const FallenHeight: Single) of object;

  TGetCameraHeight = procedure (Navigator: TMatrixWalker;
    var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single)
    of object;

  { Walking (DOOM-like moving) over the model.
    Camera is defined by it's position, looking direction
    and up vector, user can rotate and move camera using various keys. }
  TMatrixWalker = class(TMatrixNavigatorWithIdle)
  private
    FCameraPos, FCameraDir, FCameraUp,
    FHomeCameraPos, FHomeCameraDir, FHomeCameraUp: TVector3Single;

    FMoveSpeed, FMoveVertSpeed: Single;
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FPreferHomeUpForRotations: boolean;
    FPreferHomeUpForMoving: boolean;

    procedure SetCameraPos(const Value: TVector3Single);
    procedure SetCameraDir(const Value: TVector3Single);
    procedure SetCameraUp(const Value: TVector3Single);

    FKey_Forward: TKey;
    FKey_Backward: TKey;
    FKey_RightRot: TKey;
    FKey_LeftRot: TKey;
    FKey_RightStrafe: TKey;
    FKey_LeftStrafe: TKey;
    FKey_UpRotate: TKey;
    FKey_DownRotate: TKey;
    FKey_UpMove: TKey;
    FKey_DownMove: TKey;
    FKey_HomeUp: TKey;
    FKey_MoveSpeedInc: TKey;
    FKey_MoveSpeedDec: TKey;
    FKey_Jump: TKey;
    FKey_Crouch: TKey;

    FAllowSlowerRotations: boolean;
    FCheckModsDown: boolean;

    FMinAngleRadFromHomeUp: Single;

    FMouseLook: boolean;
    FMouseLookHorizontalSensitivity: Single;
    FMouseLookVerticalSensitivity: Single;

    procedure RotateAroundHomeUp(const AngleDeg: Single);
    procedure RotateAroundUp(const AngleDeg: Single);
    procedure RotateHorizontal(const AngleDeg: Single);
    procedure RotateVertical(const AngleDeg: Single);

    { Private things related to frustum ---------------------------- }

    FProjectionMatrix: TMatrix4Single;
    FFrustum: TFrustum;
    procedure RecalculateFrustum;
    procedure SetProjectionMatrix(const Value: TMatrix4Single);

    { Private things related to gravity ---------------------------- }

    FCameraPreferredHeight: Single;
    FIsFallingDown: boolean;
    FFallingDownStartPos: TVector3Single;
    FOnFalledDown: TFalledDownNotifyFunc;
    FFallingDownStartSpeed: Single;
    FFallingDownSpeed: Single;
    FFallingDownSpeedIncrease: Single;
    FGravity: boolean;
    FOnGetCameraHeight: TGetCameraHeight;
    FGrowingSpeed: Single;
    { This is used by FallingDownEffect to temporary modify Matrix result
      by rotating CameraUp around CameraDir. In degress. }
    Fde_CameraUpRotate: Single;
    { This is used by FallingDownEffect to consistently rotate us.
      This is either -1, 0 or +1. }
    Fde_RotateHorizontal: Integer;
    FFallingDownEffect: boolean;

    FMaxJumpHeight: Single;
    FIsJumping: boolean;
    FJumpHeight: Single;
    FJumpPower: Single;
    FJumpSpeedMultiply: Single;

    FHeadBobbing: Single;
    HeadBobbingPosition: Single;
    FHeadBobbingDistance: Single;
    function UseHeadBobbing: boolean;

    FCrouchHeight: Single;
    FIsCrouching: boolean;

    FFallingOnTheGround: boolean;
    FFallingOnTheGroundAngleIncrease: boolean;

    FIsWalkingOnTheGround: boolean;

    function RealCameraPreferredHeightNoHeadBobbing: Single;
    function RealCameraPreferredHeightMargin: Single;
  protected
    { }
    procedure MatrixChanged; override;
  public
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      override;

    procedure DoGetCameraHeight(
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single); virtual;

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

    { Keys --------------------------------------------------------- }

    { TODO: przydaloby sie rozwiazac tu sprawe z modifiers jakos bardziej
      elegancko. Kazdy klawisz to powinien byc kod + flagi modifierow.
      W tej chwili klawisze wszystkie ponizsze klawisze dzialaja gdy
      wszystkie modifiery sa OFF, za wyjatkiem Key_Right/LeftRot i
      KeyUp/DownRotate ktore uzyskuja specjalne znaczenie gdy dziala modifier
      Ctrl (see AllowSlowerRotations). }

    { }
    property Key_Forward: TKey read FKey_Forward write FKey_Forward
      default WalkerDefaultKey_Forward;
    property Key_Backward: TKey read FKey_Backward write FKey_Backward
      default WalkerDefaultKey_Backward;
    property Key_LeftRot: TKey read FKey_LeftRot write FKey_LeftRot
      default WalkerDefaultKey_LeftRot;
    property Key_RightRot: TKey read FKey_RightRot write FKey_RightRot
      default WalkerDefaultKey_RightRot;
    property Key_LeftStrafe: TKey read FKey_LeftStrafe write FKey_LeftStrafe
      default WalkerDefaultKey_LeftStrafe;
    property Key_RightStrafe: TKey read FKey_RightStrafe write FKey_RightStrafe
      default WalkerDefaultKey_RightStrafe;
    property Key_UpRotate: TKey read FKey_UpRotate write FKey_UpRotate
      default WalkerDefaultKey_UpRotate;
    property Key_DownRotate: TKey read FKey_DownRotate write FKey_DownRotate
      default WalkerDefaultKey_DownRotate;
    property Key_UpMove: TKey read FKey_UpMove write FKey_UpMove
      default WalkerDefaultKey_UpMove;
    property Key_DownMove: TKey read FKey_DownMove write FKey_DownMove
      default WalkerDefaultKey_DownMove;
    property Key_HomeUp: TKey read FKey_HomeUp write FKey_HomeUp
      default WalkerDefaultKey_HomeUp;

    { Note that Key_MoveSpeedInc and Key_MoveSpeedDec change
      both MoveSpeed and MoveVertSpeed.
      @groupBegin }
    property Key_MoveSpeedInc: TKey
      read FKey_MoveSpeedInc write FKey_MoveSpeedInc
      default WalkerDefaultKey_MoveSpeedInc;
    property Key_MoveSpeedDec: TKey
      read FKey_MoveSpeedDec write FKey_MoveSpeedDec
      default WalkerDefaultKey_MoveSpeedDec;
    { @groupEnd }

    { Note that jumping and crouching works only when @link(Gravity) works.
      @groupBegin }
    property Key_Jump: TKey read FKey_Jump write FKey_Jump
      default WalkerDefaultKey_Jump;
    property Key_Crouch: TKey read FKey_Crouch write FKey_Crouch
      default WalkerDefaultKey_Crouch;
    { @groupEnd }

    { If @true then all rotation keys
      (Key_RightRot, Key_LeftRot, Key_UpRotate, Key_DownRotate)
      will work 10x slower when Ctrl modified is pressed. }
    property AllowSlowerRotations: boolean
      read FAllowSlowerRotations write FAllowSlowerRotations
      default true;

    { If @true then all keys work only when no modifiers are pressed,
      and additionally when Ctrl is pressed (and AllowSlowerRotations) then
      rotation keys work 10x slower.

      If @false then all keys work as usual, no matter what
      modifiers are pressed. And rotation keys never work 10x slower
      (AllowSlowerRotations is ignored). }
    property CheckModsDown: boolean
      read FCheckModsDown write FCheckModsDown
      default true;

    { General stuff ----------------------------------------------------- }

    { move*Speed sa na poczatku rowne 1 zebys mogl userowi wyswietlac
      te zmienne jako jakas "szybkosc" gdzie 1 oznacza ruch w/g dlugosci
      wektora dir. Pamietaj ze tak naprawde w czasie ruchu uwzgledniamy
      zawsze compSpeed i w ogole nie jest jasne jak szybki tak naprawde
      jest ten ruch - musisz i tak domyslna dlugosc HomeCameraDir dopasowywac
      eksperymentalnie. }
    property MoveSpeed: Single
      read FMoveSpeed write FMoveSpeed default 1.0;
    property MoveVertSpeed: Single
      read FMoveVertSpeed write FMoveVertSpeed default 1.0;

    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      default DefaultRotationHorizontalSpeed;

    property RotationVerticalSpeed: Single
      read FRotationVerticalSpeed write FRotationVerticalSpeed
      default DefaultRotationVerticalSpeed;

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

    { If PreferHomeUpForRotations or PreferHomeUpForMoving
      then various operations are done with respect
      to HomeCameraUp, otherwise they are done with
      respect to current CameraUp (that can be different than HomeCameraUp,
      e.g. after using Key_UpRotate, Key_DownRotate --- raise / bow your head).

      With PreferHomeUpForRotations, this affects rotations:
      horizontal rotations (keys Key_LeftRot and Key_RightRot)
      and rotations caused by MouseLook.
      Also vertical rotations are bounded by MinAngleRadFromHomeUp
      when PreferHomeUpForRotations.

      Note that you can change it freely at runtime,
      and when you set PreferHomeUpForRotations from @false to @true
      then in nearest Idle
      calls CameraUp will be gradually fixed, so that CameraDir and CameraUp
      and HomeCameraUp are on the same plane. Also CameraDir may be adjusted
      to honour MinAngleRadFromHomeUp.

      With PreferHomeUpForMoving, this affects moving:
      horizontal moving (forward, backward, strafe),
      and vertical moving (keys Key_UpMove and Key_DownMove).
      E.g. when PreferHomeUpForMoving then forward/backward keys are tied
      to horizontal plane defined by HomeCameraUp.
      When not PreferHomeUpForMoving then forward/backward try to move
      you just in the CameraDir. Which is usually more handy when
      e.g. simulating flying.

      It's a delicate decision how to set them, because generally
      all the decisions are "somewhat correct" --- they just sometimes
      "feel incorrect" for player.

      @unorderedList(
        @item(
          First of all, if the scene is not "naturally oriented"
          around HomeCameraUp, then you *may* set
          PreferHomeUpForRotations as @false and you *should*
          leave PreferHomeUpForMoving and @link(Gravity) to @false.

          By the scene "naturally oriented around HomeCameraUp"
          I mean that we have some proper HomeCameraUp,
          not just some guessed HomeCameraUp that may
          be incorrect. For example when view3dscene loads a VRML model
          without any camera definition then it assumes that "up vector"
          is (0, 1, 0), because this is more-or-less VRML standard
          suggested by VRML spec. But this may be very inappopriate,
          for example the scene may be actually oriented with (0, 0, 1)
          up vector in mind.

          Other examples of the scenes without any
          "naturally oriented around HomeCameraUp" may be some
          "outer space" scene without any gravity.)

        @item(
          With PreferHomeUpForRotations the "feeling" of HomeCameraUp
          is stronger for user, because HomeCameraUp, CameraUp and CameraDir
          always define the same plane in 3D space (i.e. along with the
          4th point, (0, 0, 0), for camera eye). Raising/bowing the head
          doesn't break this assumption.

          Without PreferHomeUpForRotations, we quickly start to do rotations
          in an awkward way --- once you do some vertical rotation,
          you changed CameraUp, and next horizontal rotation will be
          done versus new CameraUp.

          If your HomeCameraUp is good, then you generally should
          leave PreferHomeUpForRotations to @true. Unless you really *want*
          the player to feel movements as "awkward", e.g. when you
          want to simulate this "outer space without any gravity" feeling.)

        @item(
          If your HomeCameraUp is good, then you generally should set
          PreferHomeUpForMoving just like Gravity.

          E.g. when the player is flying / swimming etc. he will probably prefer
          PreferHomeUpForMoving = @false, because this way he will not have to
          press Key_UpMove and Key_DownMove. Simply pressing Key_Forward
          and Key_Backward and doing rotations will be enough to move
          freely in 3D space.

          When gravity works, PreferHomeUpForMoving = @true is better,
          otherwise player would unnecessarily try to jump when looking up.)

      @groupBegin }
    property PreferHomeUpForRotations: boolean
      read FPreferHomeUpForRotations write FPreferHomeUpForRotations default true;

    property PreferHomeUpForMoving: boolean
      read FPreferHomeUpForMoving write FPreferHomeUpForMoving default true;
    { @groupEnd }

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

    { This returns CameraDir vector rotated such that it is
      orthogonal to HomeCameraUp. This way it returns CameraDir projected
      on the initial horizontal plane, which neutralizes such things
      like raising / bowing your head.

      Note that when CameraDir and HomeCameraUp are parallel,
      this just returns current CameraDir --- because in such case
      we can't project CameraDir on the horizontal plane. }
    function CameraDirInHomePlane: TVector3Single;

    { ustawia wektory Home camery i robi Home, tzn. ustawia
      wektory CameraPos/Dir/Up na ich odpowiedniki z przedrostkiem "Home".
      Uwaga - klawisz Key_HomeUp nie wywoluje ponizszej metody Home, on
      tylko ustawia CameraUp := HomeCameraUp (nie resetuje CameraPos i Dir).

      It will also call CorrectCameraPreferredHeight(ACameraRadius),
      because this is important thing that's too easy to otherwise forget.
      Just pass ACameraRadius = 0.0 if you don't really want this. }
    procedure Init(const AHomeCameraPos, AHomeCameraDir,
      AHomeCameraUp: TVector3Single;
      const ACameraPreferredHeight: Single;
      const ACameraRadius: Single); overload;

    { Alternatywny Init. Wylicza HomeCameraPos, HomeCameraDir, HomeCameraUp
      i CameraPreferredHeight takie zeby obiekt w box byl "dobrze widoczny"
      (cokolwiek by to nie mialo oznaczac) i zeby CameraPreferredHeight
      zachowywalo sie "rozsadnie". }
    procedure Init(const box: TBox3d; const ACameraRadius: Single); overload;

    procedure Home;

    { This sets the minimal angle (in radians) between HomeCameraUp
      and CameraDir, and also between -HomeCameraUp and CameraDir.
      This way vertical rotations (like Key_UpRotate,
      Key_DownRotate) are "bounded" to not allow player to do something
      strange, i.e. bow your head too much and raise your head too much.

      This is used only when PreferHomeUpForRotations
      is @true and when it's <> 0.0.

      This must be always between 0 and Pi/2. Value of Pi/2 will effectively
      disallow vertical rotations (although you should rather do this in
      a "cleaner way" by setting Key_UpRotate and Key_DownRotate to K_None). }
    property MinAngleRadFromHomeUp: Single
      read FMinAngleRadFromHomeUp write FMinAngleRadFromHomeUp
      default DefaultMinAngleRadFromHomeUp;

    { If true, then player is able to rotate the view (horizontally
      and vertically, equivalent to Key_LeftRot, Key_RightRot,
      Key_UpRotate, Key_DownRotate) using the mouse.

      You have to call MouseMove of this object to achieve this.
      Also note that there are things that you have to take care
      of yourself when enabling this property:
      @unorderedList(
        @item(After calling MouseMove you should reposition your mouse
          at the middle of your window (or the screen), to avoid the situation
          when mouse movement is blocked by screen borders.)
        @item(You usually will want to hide the mouse cursor, as showing
          it always bouncing at the middle of the window/screen
          doesn't look sensible.)
      ) }
    property MouseLook: boolean read FMouseLook write FMouseLook default false;

    { These control mouse look sensitivity.
      They say how much angle change is produced by 1 pixel change
      (for MouseXChange, MouseYChange in MouseMove).
      You can change this, to better adjust to user.

      @groupBegin }
    property MouseLookHorizontalSensitivity: Single
      read FMouseLookHorizontalSensitivity write FMouseLookHorizontalSensitivity
      default DefaultMouseLookHorizontalSensitivity;
    property MouseLookVerticalSensitivity: Single
      read FMouseLookVerticalSensitivity write FMouseLookVerticalSensitivity
      default DefaultMouseLookVerticalSensitivity;
    { @groupEnd }

    { Call this to actually make MouseLook work.
      MouseXChange and MouseYChange are differences between current
      and previous window coords
      (like in TGLWindow.MouseX/MouseY, so 0,0 is top-left corner). }
    procedure MouseMove(MouseXChange, MouseYChange: Integer);

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
      related to gravity. Gravity always drags the camera down to
      -HomeCameraUp.

      Summary of things done by gravity:
      @unorderedList(
        @item(It uses OnGetCameraHeight to get camera height above the ground.)
        @item(It allows player to jump. See Key_Jump, IsJumping, MaxJumpHeight,
          JumpSpeedMultiply.)
        @item(It allows player to crouch. See Key_Crouch, CrouchHeight.)
        @item(It tries to keep CameraPos above the ground on
          CameraPreferredHeight height.)
        @item(When current height is too small --- CameraPos is moved up.
          See GrowingSpeed.)
        @item(When current height is too large --- we're falling down.
          See IsFallingDown, OnFalledDown, FallingDownStartSpeed,
          FallingDownSpeedIncrease, FallingDownEffect.)
        @item(It does head bobbing. See HeadBobbing, HeadBobbingDistance.)
      )

      While there are many properties allowing you to control
      gravity behavior, most of them have initial values that should be
      sensible in all cases. The only things that you really want to take
      care of are: OnGetCameraHeight and CameraPreferredHeight.
      Everything else should basically work auto-magically.

      Note that Gravity setting is independent from
      PreferHomeUpForRotations or PreferHomeUpForMoving settings ---
      PreferHomeUpXxx say how the player controls work,
      Gravity says what happens to player due to ... well, due to gravity.

      @noAutoLinkHere }
    property Gravity: boolean
      read FGravity write FGravity default false;

    { When @link(Gravity) is on, CameraPos tries to stay CameraPreferredHeight
      above the ground. Temporary it may be lower (player can
      shortly "duck" when he falls from high).

      This must always be >= 0.
      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(Gravity),
      and also you should set OnGetCameraHeight.

      See CorrectCameraPreferredHeight for important property
      of CameraPreferredHeight that you should keep. }
    property CameraPreferredHeight: Single
      read FCameraPreferredHeight write FCameraPreferredHeight default 0.0;

    { This procedure corrects CameraPreferredHeight based on your CameraRadius
      and on current HeadBobbing.

      Exactly what and why is done: if you do any kind of collision
      detection with some CameraRadius, then
      you should make sure that RealCameraPreferredHeight is always >= of your
      CameraRadius, otherwise strange effects may happen when crouching
      or when head bobbing forces camera to go down.

      Exactly, the required equation is
@preformatted(
  MinimumRealCameraPreferredHeight :=
    CameraPreferredHeight * CrouchHeight * (1 - HeadBobbing);
)
      and always must be
@preformatted(
  MinimumRealCameraPreferredHeight >= RealCameraPreferredHeight
)

      Reasoning: otherwise this class would "want camera to fall down"
      (because we will always be higher than RealCameraPreferredHeight)
      but your OnMoveAllowed would not allow it (because CameraRadius
      would not allow it). Note that this class doesn't keep value
      of your CameraRadius, because collision detection
      is (by design) never done by this class --- it's always
      delegated to OnGetCameraHeight and OnMoveAllowed.
      Also, it's not exactly forced *how* you should force this
      condition to hold. Sometimes the good solution is to adjust
      CameraRadius, not to adjust CameraPreferredHeight.

      Anyway, this method will make sure that this condition
      holds by eventually adjusting (making larger) CameraPreferredHeight.
      Note that for CameraRadius = 0.0 this will always leave
      CameraPreferredHeight as it is. }
    procedure CorrectCameraPreferredHeight(const CameraRadius: Single);

    { Assign here the callback (or override DoGetCameraHeight)
      to say what is the current height of camera above the ground.
      This should be calculated like collision of ray from CameraPos
      in direction -HomeCameraUp with the scene.

      This event returns IsAboveTheGround: bolean (set to @false
      to indicate that the ray doesn't hit the scene at all,
      so the camera is not above the ground at all) and
      SqrHeightAboveTheGround (meaningfull only if IsAboveTheGround;
      this is the Sqr(height of camera above the ground)).

      Implementation of DoGetCameraHeight in this class
      initializes IsAboveTheGround to @false and then calls
      OnGetCameraHeight, if assigned. }
    property OnGetCameraHeight: TGetCameraHeight
      read FOnGetCameraHeight
      write FOnGetCameraHeight;

    { This is called when camera was falling down for some time,
      and suddenly stopped (this means that camera "hit the ground").
      Of course this is used only when @link(Gravity) is @true
      (it can also be called shortly after you changed
      @link(Gravity) from @true to @false, so don't simply assert
      here that @link(Gravity) is @true).

      It can be useful in games to do some things
      (maybe basing on FallenHeight parameter passed to this callback)
      like lowering player's health and/or making some effects (displaying
      "blackout" or playing sound like "Ouh!" etc.). }
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

    { When falling down, the speed increases.
      Set this to 1.0 to fall down with constant speed
      (taken from FallingDownStartSpeed). }
    property FallingDownSpeedIncrease: Single
      read FFallingDownSpeedIncrease write FFallingDownSpeedIncrease
      default DefaultFallingDownSpeedIncrease;

    property IsFallingDown: boolean read FIsFallingDown;

    { If IsFallingDown, then this will force IsFallingDown to false
      @bold(without calling OnFallenDown). It's much like forcing
      the opinion that "camera is not falling down right now".

      Of course, if in the nearest Idle we will find out (using
      GetCameraHeight) that camera is too high above the ground,
      then we will start falling down again, setting IsFallingDown
      back to true. (but the we will start falling down from the beginning,
      starting at given CameraPos and with initial falling down speed).

      This is useful to call if you just changed CameraPos because
      e.g. the player teleported somewhere (or e.g. game levels changed).
      In this case you just want to forget the fact that camera
      was falling down --- no consequences (like lowering player's
      health, redout etc.). }
    procedure CancelFallingDown;

    { This triggers a nice effect when falling down from high.
      Camera dir rotates slightly, and camera up temporary rotates
      around camera up. This makes nice visual effect, so usually
      you will want this.

      Of course this is meaningfull only when @link(Gravity) works.

      Note that changing it from @true to @false doesn't immediately
      "cancel out" this effect if it's currently in progress.
      It only prevents this effect from starting again. }
    property FallingDownEffect: boolean
      read FFallingDownEffect write FFallingDownEffect default true;

    { When @link(Gravity) works and camera height above the ground
      is less than CameraPreferredHeight, then we try to "grow",
      i.e. camera position increases along the HomeCameraUp
      so that camera height above the ground is closer to
      CameraPreferredHeight. This property (together with length of
      CameraDir, that always determines every moving speed)
      determines the speed of this growth. }
    property GrowingSpeed: Single
      read FGrowingSpeed write FGrowingSpeed
      default DefaultGrowingSpeed;

    { How high can you jump ?
      The max jump distance is calculated as
      MaxJumpHeight * CameraPreferredHeight, see MaxJumpDistance. }
    property MaxJumpHeight: Single
      read FMaxJumpHeight write FMaxJumpHeight default DefaultMaxJumpHeight;

    { Returns just MaxJumpHeight * CameraPreferredHeight,
      see MaxJumpHeight for explanation. }
    function MaxJumpDistance: Single;

    { Camera is in the middle of a "jump" move right now. }
    property IsJumping: boolean read FIsJumping;

    property JumpSpeedMultiply: Single
      read FJumpSpeedMultiply write FJumpSpeedMultiply
      default DefaultJumpSpeedMultiply;

    property JumpPower: Single read FJumpPower write FJumpPower
      default DefaultJumpPower;

    { When you move horizontally, you get "head bobbing" effect
      --- camera position slightly changes it's vertical position,
      going a little up, then a little down, then a little up again etc.

      This property mutiplied by CameraPreferredHeight
      says how much head bobbing can move you along HomeCameraUp.
      Set this to 0 to disable head bobbing.
      This must always be < 1.0. For sensible effects, this should
      be rather close to 0.0.

      Of course this is meaningfull only when @link(Gravity) works. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing default DefaultHeadBobbing;

    { This controls head bobbing frequency.

      I increase HeadBobbingPosition such that
      HeadBobbingPosition increase of 1
      means that player moved horizontally by
        VectorLen(Direction) * MoveSpeed * HeadBobbingDistance. }
    property HeadBobbingDistance: Single
      read FHeadBobbingDistance write FHeadBobbingDistance
      default DefaultHeadBobbingDistance;

    { This defines the preferred height of camera when crouching.
      This is always mutiplied to CameraPreferredHeight.
      This should always be <= 1 (CrouchHeight = 1 effectively disables
      crouching, it's better to do this setting Key_Crouch to K_None). }
    property CrouchHeight: Single
      read FCrouchHeight write FCrouchHeight default DefaultCrouchHeight;

    { Is player crouching right now ? }
    property IsCrouching: boolean read FIsCrouching;

    { This is CameraPreferredHeight slightly modified by head bobbing
      and crouch. It can be useful for collision detection
      between camera and something. }
    function RealCameraPreferredHeight: Single;

    { This makes a visual effect of camera falling down horizontally
      on the ground. This works by gradually changing CameraUp such that
      it gets orthogonal to HomeCameraUp. }
    procedure FallOnTheGround;

    { This is set in every Idle. @true means that gravity works
      (i.e. @link(Gravity) is @true), and player
      is standing stable on the ground, and player is moving
      horizontally. The intention is that you can use this to make
      some "footsteps" sound for the player. }
    property IsWalkingOnTheGround: boolean read FIsWalkingOnTheGround;
  end;

{ See TMatrixWalker.CorrectCameraPreferredHeight.
  This is a global version, sometimes may be useful. }
procedure CorrectCameraPreferredHeight(var CameraPreferredHeight: Single;
  const CameraRadius: Single; const CrouchHeight, HeadBobbing: Single);

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
    ((K_Left, K_Right), (K_Down, K_Up), (K_PageDown, K_PageUp));
  DefaultKeys_Rotate: T3BoolKeys =
    ((K_Up, K_Down), (K_Left, K_Right), (K_PageDown, K_PageUp));

constructor TMatrixExaminer.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
begin
 inherited;
 FScaleFactor := 1;
 FModelBox := EmptyBox3d;

 { default keys }
 Keys_Move := DefaultKeys_Move;
 Keys_Rotate := DefaultKeys_Rotate;
 Key_ScaleLarger := K_Numpad_Plus;
 Key_ScaleSmaller := K_Numpad_Minus;
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
    if KeysDown^[Keys_Move[i, true ]] then Move(i, +move_change);
    if KeysDown^[Keys_Move[i, false]] then Move(i, -move_change);
   end;
  end else
  if ModsDown=[] then
  begin
   for i := 0 to 2 do
   begin
    if KeysDown^[Keys_Rotate[i, true]]  then Rotate(i, +rot_speed_change);
    if KeysDown^[Keys_Rotate[i, false]] then Rotate(i, -rot_speed_change);
   end;
  end;

  if KeysDown^[Key_ScaleLarger] then Scale(scale_change);
  if KeysDown^[Key_ScaleSmaller] then Scale(1/scale_change);
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
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FRotationVerticalSpeed := DefaultRotationVerticalSpeed;
  FFallingDownStartSpeed := DefaultFallingDownStartSpeed;
  FFallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
  FPreferHomeUpForRotations := true;
  FPreferHomeUpForMoving := true;
  FGravity := false;
  FGrowingSpeed := DefaultGrowingSpeed;
  FFallingDownEffect := true;
  FIsJumping := false;
  FHeadBobbing := DefaultHeadBobbing;
  FCrouchHeight := DefaultCrouchHeight;
  FMaxJumpHeight := DefaultMaxJumpHeight;
  FMinAngleRadFromHomeUp := DefaultMinAngleRadFromHomeUp;
  FAllowSlowerRotations := true;
  FCheckModsDown := true;
  FMouseLookHorizontalSensitivity := DefaultMouseLookHorizontalSensitivity;
  FMouseLookVerticalSensitivity := DefaultMouseLookVerticalSensitivity;
  FHeadBobbingDistance := DefaultHeadBobbingDistance;
  FJumpSpeedMultiply := DefaultJumpSpeedMultiply;
  FJumpPower := DefaultJumpPower;

  Key_Forward := WalkerDefaultKey_Forward;
  Key_Backward := WalkerDefaultKey_Backward;
  Key_LeftRot := WalkerDefaultKey_LeftRot;
  Key_RightRot := WalkerDefaultKey_RightRot;
  Key_LeftStrafe := WalkerDefaultKey_LeftStrafe;
  Key_RightStrafe := WalkerDefaultKey_RightStrafe;
  Key_UpRotate := WalkerDefaultKey_UpRotate;
  Key_DownRotate := WalkerDefaultKey_DownRotate;
  Key_UpMove := WalkerDefaultKey_UpMove;
  Key_DownMove := WalkerDefaultKey_DownMove;
  Key_HomeUp := WalkerDefaultKey_HomeUp;
  Key_MoveSpeedInc := WalkerDefaultKey_MoveSpeedInc;
  Key_MoveSpeedDec := WalkerDefaultKey_MoveSpeedDec;
  Key_Jump := WalkerDefaultKey_Jump;
  Key_Crouch := WalkerDefaultKey_Crouch;

  FProjectionMatrix := IdentityMatrix4Single;
end;

function TMatrixWalker.Matrix: TMatrix4Single;
begin
  { Yes, below we compare Fde_CameraUpRotate with 0.0 using normal
    (precise) <> operator. Don't worry --- Fde_Stabilize in Idle
    will take care of eventually setting Fde_CameraUpRotate to
    a precise 0.0. }
  if Fde_CameraUpRotate <> 0.0 then
    Result := LookDirMatrix(CameraPos, CameraDir,
      RotatePointAroundAxisDeg(Fde_CameraUpRotate, CameraUp, CameraDir)) else
    Result := LookDirMatrix(CameraPos, CameraDir, CameraUp);
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

procedure TMatrixWalker.DoGetCameraHeight(
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
begin
  IsAboveTheGround := false;
  if Assigned(OnGetCameraHeight) then
    OnGetCameraHeight(Self, IsAboveTheGround, SqrHeightAboveTheGround);
end;

function TMatrixWalker.UseHeadBobbing: boolean;
begin
  Result := Gravity and (HeadBobbing <> 0.0);
end;

function TMatrixWalker.RealCameraPreferredHeightNoHeadBobbing: Single;
begin
  Result := CameraPreferredHeight;

  if IsCrouching then
    Result *= CrouchHeight;
end;

function TMatrixWalker.RealCameraPreferredHeight: Single;
var
  BobbingModifier: Single;
begin
  Result := RealCameraPreferredHeightNoHeadBobbing;

  if UseHeadBobbing then
  begin
    { HeadBobbingPosition = 0 means that head is at lowest position.
      HeadBobbingPosition = 0.5 means that head is at highest position.
      HeadBobbingPosition = 1.0 means that head is at lowest position again.

      Larger HeadBobbingPosition work like Frac(HeadBobbingPosition)
      (i.e. function HeadBobbingPosition -> BobbingModifier
      is periodic with period = 1.0). }

    BobbingModifier := Frac(HeadBobbingPosition);

    if BobbingModifier <= 0.5 then
      BobbingModifier := MapRange(BobbingModifier, 0.0, 0.5, -1, +1) else
      BobbingModifier := MapRange(BobbingModifier, 0.5, 1.0, +1, -1);

    { Most game tutorials and codes advice that head bobbing be done with sinus,
      as below. But actually I found that the visual difference between
      sin-based head bobbing and linear-based (like above) head bobbing
      is not noticeable, so I'm using linear-based right now (as it's
      a little faster --- no trig calculation needed, although this
      could be avoided with sinus lookup table).

      If however you prefer sin-based head bobbing, uncomment line below
      and comment out 3 lines "if BobbingModifier <= 0.5 then ...." above.

    BobbingModifier := Sin(BobbingModifier * 2 * Pi);
    }

    BobbingModifier *= Result * HeadBobbing;
    Result += BobbingModifier;
  end;
end;

function TMatrixWalker.RealCameraPreferredHeightMargin: Single;
begin
  { I tried using here something smaller like
    SingleEqualityEpsilon, but this was not good. }
  Result := RealCameraPreferredHeight * 0.01;
end;

procedure TMatrixWalker.RotateAroundHomeUp(const AngleDeg: Single);
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

procedure TMatrixWalker.RotateAroundUp(const AngleDeg: Single);
begin
 { W TYM MIEJSCU POTRZEBUJEMY aby cameraDir i cameraUp byly prostopadle ! }
 CameraDir := RotatePointAroundAxisDeg(AngleDeg, CameraDir, CameraUp);
end;

procedure TMatrixWalker.RotateHorizontal(const AngleDeg: Single);
begin
  if PreferHomeUpForRotations then
    RotateAroundHomeUp(AngleDeg) else
    RotateAroundUp(AngleDeg);
end;

procedure TMatrixWalker.RotateVertical(const AngleDeg: Single);
var
  Side: TVector3Single;
  AngleRad: Single;

  procedure DoRealRotate;
  begin
    { Rotate CameraUp around Side }
    FCameraUp := RotatePointAroundAxisRad(AngleRad, CameraUp,  Side);
    { Rotate CameraDir around Side }
    FCameraDir := RotatePointAroundAxisRad(AngleRad, CameraDir, Side);
  end;

var
  AngleRadBetween: Single;
begin
  AngleRad := DegToRad(AngleDeg);

  if PreferHomeUpForRotations and (MinAngleRadFromHomeUp <> 0.0) then
  begin
    Side := VectorProduct(CameraDir, HomeCameraUp);
    if IsZeroVector(Side) then
    begin
      { Brutally adjust CameraDir and CameraUp to be correct.
        This should happen only if your code was changing values of
        PreferHomeUpForRotations and MinAngleRadFromHomeUp at runtime.
        E.g. first you let CameraDir and CameraUp to be incorrect,
        and then you set PreferHomeUpForRotations to @true and MinAngleRadFromHomeUp
        to > 0 --- and suddenly we find that CameraUp can be temporarily bad. }
      FCameraDir := HomeCameraDir;
      FCameraUp := HomeCameraUp;
    end else
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(CameraDir, HomeCameraUp);
      if AngleRadBetween - AngleRad < MinAngleRadFromHomeUp then
        AngleRad := AngleRadBetween - MinAngleRadFromHomeUp else
      if AngleRadBetween - AngleRad > Pi - MinAngleRadFromHomeUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleRadFromHomeUp);

      DoRealRotate;
    end;
  end else
  begin
    Side := VectorProduct(CameraDir, CameraUp);
    DoRealRotate;
  end;

  MatrixChanged;
end;

procedure TMatrixWalker.Idle(const CompSpeed: Single; KeysDown: PKeysBooleans);

  { Like Move, but you pass here final ProposedNewPos }
  function MoveTo(const ProposedNewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  var
    NewPos: TVector3Single;
  begin
    Result := DoMoveAllowed(ProposedNewPos, NewPos, BecauseOfGravity);
    if Result then
      { Note that setting CameraPos automatically calls MatrixChanged }
      CameraPos := NewPos;
  end;

  { Tries to move CameraPos to CameraPos + MoveVector.
    Returns DoMoveAllowed result. So if it returns @false,
    you know that CameraPos didn't change (on the other hand,
    if it returns @true, you don't know anything --- maybe CameraPos
    didn't change, maybe it changed to CameraPos + MoveVector,
    maybe it changed to something different). }
  function Move(const MoveVector: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  begin
    Result := MoveTo(VectorAdd(CameraPos, MoveVector), BecauseOfGravity);
  end;

var
  { This is initally false. It's used by MoveHorizontal while head bobbing,
    to avoid updating HeadBobbingPosition more than once in the same Idle call.

    Updating it more than once is bad --- try e.g. holding Key_Forward
    with one of the strafe keys: you move and it's very noticeable
    that HeadBobbing seems faster. That's because
    when holding both Key_Forward and Key_StrafeRight, you shouldn't
    do HeadBobbing twice in one Idle --- you should do it only Sqrt(2).
    When you will also hold Key_RotateRight at the same time --- situation
    gets a little complicated...

    The good solution seems to just do head bobbing only once.
    In some special cases this means that head bobbing will be done
    *less often* than it should be, but this doesn't hurt. }
  HeadBobbingAlreadyDone: boolean;

  { MoveHorizontal call sets this to @true to indicate that some
    horizontal move was done. }
  MoveHorizontalDone: boolean;

  { Multiply must be +1 or -1 }
  procedure MoveHorizontal(const Multiply: Integer = 1);
  var
    Direction: TVector3Single;
  var
    AJumpMultiply: Single;
  begin
    if IsJumping then
      AJumpMultiply := JumpSpeedMultiply else
      AJumpMultiply := 1.0;

    { Update HeadBobbingPosition }
    if (not IsJumping) and UseHeadBobbing and (not HeadBobbingAlreadyDone) then
    begin
      HeadBobbingPosition += CompSpeed / HeadBobbingDistance;
      HeadBobbingAlreadyDone := true;
    end;

    MoveHorizontalDone := true;

    if PreferHomeUpForMoving then
      Direction := CameraDirInHomePlane else
      Direction := CameraDir;

    Move(VectorScale(Direction,
      MoveSpeed * CompSpeed * Multiply * AJumpMultiply), false);
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
    if PreferHomeUpForMoving then
      MoveVerticalCore(HomeCameraUp) else
      MoveVerticalCore(CameraUp);
  end;

  { This is just like RotateHorizontal, but it uses
    PreferHomeUpForMoving to decide which rotation to use.
    This way when PreferHomeUpForMoving, then we rotate versus HomeCameraUp,
    move in HomeCameraUp plane, and then rotate back versus HomeCameraUp.
    If not PreferHomeUpForMoving, then we do all this versus CameraUp.
    And so everything works. }
  procedure RotateHorizontalForStrafeMove(const AngleDeg: Single);
  begin
    if PreferHomeUpForMoving then
      RotateAroundHomeUp(AngleDeg) else
      RotateAroundUp(AngleDeg);
  end;

  procedure CheckRotates(SpeedScale: Single);
  { sprawdz czy wcisnieto KeyRight/LeftRot i jesli tak to zareaguj odpowiednio.
    Uzyj SpeedScale aby skalowac szybkosc obracania sie, tzn. defaltowa
    szybkosc obracania sie = 1.0 }
  begin
    if KeysDown^[Key_RightRot] then RotateHorizontal(
      -RotationHorizontalSpeed * CompSpeed * SpeedScale);
    if KeysDown^[Key_LeftRot] then RotateHorizontal(
      +RotationHorizontalSpeed * CompSpeed * SpeedScale);

    if KeysDown^[Key_UpRotate] then RotateVertical(
      +RotationVerticalSpeed * CompSpeed * SpeedScale);
    if KeysDown^[Key_DownRotate] then RotateVertical(
      -RotationVerticalSpeed * CompSpeed * SpeedScale);
  end;

  { Things related to gravity --- jumping, taking into account
    falling down and keeping RealCameraPreferredHeight above the ground. }
  procedure GravityIdle;
  var
    IsAboveTheGround: boolean;
    SqrHeightAboveTheGround: Single;

    function TryJump: boolean;
    var
      ThisJumpHeight: Single;
    begin
      Result := IsJumping;

      if Result then
      begin
        { jump. This means:
          1. update FJumpHeight and FJumpPower and move CameraPos
          2. or set FIsJumping to false when jump ends }

        ThisJumpHeight := MaxJumpDistance * FJumpPower * CompSpeed;
        FJumpHeight += ThisJumpHeight;

        if FJumpHeight > MaxJumpDistance then
          FIsJumping := false else
        begin
          { do jumping }
          Move(VectorAdjustToLength(HomeCameraUp, ThisJumpHeight), false);

          { Initially it was my intention to decrease FJumpPower
            at each point. But this doesn't make any nice visible effect,
            moreover it can't guarentee that every jump will sooner or later
            reach MaxJumpDistance. And we want for every jump to
            sooner or later reach MaxJumpDistance.

            FJumpPower *= Power(0.95, CompSpeed);

            So the line above is commented out, and jumping is done with
            constant speed FJumpPower. So every jump sooner or later reaches
            MaxJumpDistance. }
        end;
      end;
    end;

   function TryFde_Stabilize: boolean; forward;

    { If our height above the ground is < RealCameraPreferredHeight
      then we try to "grow".

      (this may happen because of many things --- e.g. user code
      just changed CameraPreferredHeight to something larger
      (because e.g. "duck mode" ended), or we just ended falling dowm
      from high). }
    function TryGrow: boolean;
    var
      GrowingVectorLength: Single;
    begin
      Result :=
        IsAboveTheGround and
        (SqrHeightAboveTheGround <
          Sqr(RealCameraPreferredHeight - RealCameraPreferredHeightMargin));

      if Result then
      begin
        { calculate GrowingVectorLength }
        { Well, we have to resign here from operating on Sqrs,
          we need actual values. }
        GrowingVectorLength := Min(
          { TODO --- use CameraPreferredHeight here ? }
          VectorLen(CameraDir) * GrowingSpeed * CompSpeed,
          RealCameraPreferredHeight - Sqrt(SqrHeightAboveTheGround));

        Move(VectorAdjustToLength(HomeCameraUp, GrowingVectorLength), true);

        { When growing, TryFde_Stabilize also must be done.
          Otherwise when player walks horizontally on the flat surface
          for some time then "Falling down effect" activates --- because
          player is always in TryGrow or TryFallingDown. So one of them
          (TryGrow or TryFallingDown) *must* allow "Falling down effect"
          to stabilize itself. Obviously TryFallingDown can't (this would
          be against the idea of this effect) so TryGrow does it... }
        TryFde_Stabilize;
      end;
    end;

    function TryFallingDown: boolean;

      { Return +1 or -1, randomly. }
      function RandomPlusMinus: Integer;
      begin
        Result := Random(2);
        if Result = 0 then
          Result := -1;
      end;

    const
      Fde_VerticalRotateDeviation = 1.0;
      Fde_HorizontalRotateDeviation = 0.3;
    var
      CameraPosBefore: TVector3Single;
      SqrFallingDownVectorLength: Single;
    begin
      Result := false;

      { Note that if we got here, then TryGrow returned false,
        which means that (assuming OnGetCameraHeight is correctly assigned)
        we are not above the ground, or
          SqrHeightAboveTheGround >=
            Sqr(RealCameraPreferredHeight - RealCameraPreferredHeightMargin)
        However we check here for something stronger:
          SqrHeightAboveTheGround >
            Sqr(RealCameraPreferredHeight + RealCameraPreferredHeightMargin)

        This is important, because this way we avoid the unpleasant
        "bouncing" effect when in one Idle we decide that camera
        is falling down, in next Idle we decide that it's growing,
        in next Idle it falls down again etc. In TryGrow we try
        to precisely set our CameraPos, so that it hits exactly
        at RealCameraPreferredHeight -- which means that after TryGrow,
        in next Idle TryGrow should not cause growing and TryFallingDown
        should not cause falling down. }
      if IsAboveTheGround and
         (SqrHeightAboveTheGround <=
           Sqr(RealCameraPreferredHeight + RealCameraPreferredHeightMargin)) then
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

      { calculate SqrFallingDownVectorLength.

        Note that we make sure that SqrFallingDownVectorLength is no longer
        than SqrHeightAboveTheGround --- this way we avoid the problem
        that when FFallingDownSpeed would get very big,
        we couldn't fall down any more (while in fact we should then fall down
        very quickly).

        Actually, we even do more. We make sure that
        FallingDownVectorLength is no longer than
        (HeightAboveTheGround - RealCameraPreferredHeight).
        Initially I wanted to do here
          MinTo1st(SqrFallingDownVectorLength, SqrHeightAboveTheGround);
        i.e. to allow camera to fall below RealCameraPreferredHeight.

        But this didn't work like it should. Why ?
        See above for the trick that I have to do with
        RealCameraPreferredHeightMargin above (to not cause
        "unpleasant bouncing" when swapping FallingDown and TryGrow).
        If I could fall down here below RealCameraPreferredHeight then

        1. It *will not* cause the desired "nice" effect (of automatically
           "ducking" when falling down from high), because of comparison
           (the one with RealCameraPreferredHeightMargin) above.

        2. It *will* cause the undesired unpleasant swapping between
           FallingDown and TryGrow.

        So it's totally bad thing to do.

        This means that I should limit myself to not fall down
        below RealCameraPreferredHeight. And that's what I'm doing. }
      SqrFallingDownVectorLength :=
        VectorLenSqr(CameraDir) * Sqr(FFallingDownSpeed * CompSpeed);
      if IsAboveTheGround then
        MinTo1st(SqrFallingDownVectorLength,
          Sqr(Sqrt(SqrHeightAboveTheGround) - RealCameraPreferredHeight));

      if Move(VectorScale(HomeCameraUp,
         - Sqrt(SqrFallingDownVectorLength /
             VectorLenSqr(HomeCameraUp))), true) and
        (not VectorsPerfectlyEqual(CameraPos, CameraPosBefore)) then
      begin
        if not IsFallingDown then
        begin
          FFallingDownStartPos := CameraPosBefore;

          { Why do I init here FFallingDownSpeed ? A few lines above I did
              if not FIsFallingDown then
                FFallingDownSpeed := FallingDownStartSpeed;
            to init FFallingDownSpeed (I had to do it to calculate
            SqrFallingDownVectorLength). So why initing it again here ?

            Answer: Because Move above called MoveTo, that set CameraPos
            that actually called MatrixChanged that called OnMatrixChanged.
            And OnMatrixChanged is used callback and user could do there
            things like
            - Changing FallingDownStartSpeed (but still it's unspecified
              whether we have to apply this change, right ?)
            - Calling CancelFallingDown and *then* changing FallingDownStartSpeed.
              And in this case, we *must* honour it, because here user
              expects that we will use FallingDownStartSpeed if we want
              to fall down. (of course, one call to "Move" with old
              "FallingDownStartSpeed" was already done, that's unavoidable...). }
          FFallingDownSpeed := FallingDownStartSpeed;

          FIsFallingDown := true;
        end;

        Result := true;

        if SqrHeightAboveTheGround < Sqr(RealCameraPreferredHeight * 1.1) then
        begin
          { This check is needed, otherwise when you're walking down even from
            the most slight hill then you get

            1. FallingDownEffect
            2. OnFalledDown is called seldom and with large heights.

            Why ? Because MoveHorizontal calls are done between GravityIdle
            calls, and the move can be quite fast. So even though the player is
            actually quite closely following the terrain, we would constantly
            have IsFallingDown := true. Consider a large hill that is almost
            flat --- when walking down the hill, we would get IsFallingDown
            := true, FallingDownSpeed and FallingDownEffect would raise,
            and at the end OnFalledDown would be called with parameters
            like player fell down from the top of the hill to the ground
            (which can cause e.g. player losing life).

            The check for RealCameraPreferredHeight * 1.1 above and
            setting FIsFallingDown cure the situation. OnFalledDown will
            be called more often indicating very small fallen down heights,
            and FallingDownSpeed and FallingDownEffect will not be able
            to raise high as long as player follows terrain closely.

            Of course we're setting here FIsFallingDown := false even though
            the player is not exactly on the terrain --- but he's very close.
            In the next GravityIdle call we will again bring him a little
            down, set FIsFallingDown to @true, and then set it back to @false
            by line below. }
          FIsFallingDown := false;
        end else
        begin
          { Note that when changing FFallingDownSpeed below I'm using CompSpeed.
            And also above when using FFallingDownSpeed, I multipled
            FFallingDownSpeed * CompSpeed. This is correct:
            - changing position based on FallingDownSpeed is a "velocity"
            - changing FallingDownSpeed below is "acceleration"
            And both acceleration and velocity must be time-based. }
          if FallingDownSpeedIncrease <> 1.0 then
            FFallingDownSpeed *= Power(FallingDownSpeedIncrease, CompSpeed);

          { This is where we do FallingDownEffect }
          if FallingDownEffect and
             (FFallingDownSpeed > FallingDownStartSpeed * 3) then
          begin
            if FFallingDownSpeed > FallingDownStartSpeed * 5 then
            begin
              if Fde_RotateHorizontal = 0 then
                Fde_RotateHorizontal := RandomPlusMinus;
              RotateAroundHomeUp(Fde_RotateHorizontal *
                Fde_HorizontalRotateDeviation * CompSpeed);
            end;

            if Fde_CameraUpRotate < 0 then
              Fde_CameraUpRotate -= Fde_VerticalRotateDeviation * CompSpeed else
            if Fde_CameraUpRotate > 0 then
              Fde_CameraUpRotate += Fde_VerticalRotateDeviation * CompSpeed else
              Fde_CameraUpRotate := RandomPlusMinus *
                                    Fde_VerticalRotateDeviation * CompSpeed;

            MatrixChanged;
          end;
        end;
      end else
        FIsFallingDown := false;
    end;

    function TryFde_Stabilize: boolean;
    const
      Fde_VerticalRotateNormalization = 7;
    var
      Change: Single;
    begin
      Result := (Fde_RotateHorizontal <> 0) or (Fde_CameraUpRotate <> 0);

      { Bring Fde_Xxx vars back to normal (zero) values. }

      Fde_RotateHorizontal := 0;

      if Fde_CameraUpRotate <> 0.0 then
      begin
        { Note that we try to immediately bring CameraUpRotate to
          range (-360, 360) here. E.g. no need to gradually bring back
          CameraUpRotate from 360.0 to 0.0 --- this doesn't cause
          any interesting visual effect (and the only reason for
          CameraUpRotate is a visual effect)... }
        Change := Trunc(Abs(Fde_CameraUpRotate) / 360.0) * 360.0 +
          Fde_VerticalRotateNormalization * CompSpeed;

        if Fde_CameraUpRotate < 0 then
          Fde_CameraUpRotate := Min(Fde_CameraUpRotate + Change, 0.0) else
          Fde_CameraUpRotate := Max(Fde_CameraUpRotate - Change, 0.0);

        MatrixChanged;
      end;
    end;

    function TryFallingOnTheGround: boolean;
    var
      Angle, AngleRotate: Single;
    begin
      Result := FFallingOnTheGround;
      if not Result then
        Exit;

      Angle := AngleRadBetweenVectors(CameraUp, HomeCameraUp);

      if FloatsEqual(Angle, HalfPi) then
      begin
        { FallingOnTheGround effect stops here. }
        FFallingOnTheGround := false;
        Exit;
      end;

      AngleRotate := 0.1 * CompSpeed;
      MinTo1st(AngleRotate, Abs(Angle - HalfPi));
      if not FFallingOnTheGroundAngleIncrease then
        AngleRotate := -AngleRotate;

      CameraUp := RotatePointAroundAxisRad(AngleRotate, CameraUp,
        CameraDirInHomePlane);
    end;

    procedure DoFalledDown;
    var
      BeginPos, EndPos, EndToBegin: TVector3Single;
      Coord: Integer;
    begin
      if Assigned(OnFalledDown) then
      begin
        { Note that I project CameraPos and FFallingDownStartPos
          onto HomeCameraUp vector to calculate FalledHeight. }
        BeginPos := PointOnLineClosestToPoint(ZeroVector3Single, HomeCameraUp, FFallingDownStartPos);
        EndPos   := PointOnLineClosestToPoint(ZeroVector3Single, HomeCameraUp, CameraPos);
        EndToBegin := VectorSubtract(BeginPos, EndPos);

        { Now check that EndToBegin points in the same direction as HomeCameraUp.
          If not, then EndPos is actually *higher* than BeginPos,
          so we were not really falling down. That can happen, various growing
          and jumping things can cause such "false flying". For OnFalledDown
          only the real falling down (from somewhere higher to lower) should
          be reported. }
        Coord := MaxAbsVectorCoord(EndToBegin);
        if (EndToBegin[Coord] >= 0) <> (HomeCameraUp[Coord] >= 0) then
          Exit;

        OnFalledDown(Self, VectorLen(EndToBegin));
      end;
    end;

    procedure HeadBobbingGoesDown;
    const
      HeadBobbingGoingDownSpeed = 0.1;
    var
      FracHeadBobbingPosition: Single;
    begin
      if UseHeadBobbing and (not HeadBobbingAlreadyDone) then
      begin
        { If head bobbing is active, but player did not move during
          this Idle call, and no gravity effect is in work
          then player is standing still on the ground.

          This means that his head bobbing should go down as far as
          possible. This means that HeadBobbingPosition should
          go to nearest integer value.

          Note that we avoid changing HeadBobbingPosition by less
          than SingleEqualityEpsilon, just to be on the safe side
          and avoid any "corner cases", when HeadBobbingPosition
          would switch between going up and down repeatedly. }
        FracHeadBobbingPosition := Frac(HeadBobbingPosition);
        if FracHeadBobbingPosition > 0.5 then
        begin
          if 1 - FracHeadBobbingPosition > SingleEqualityEpsilon then
            HeadBobbingPosition +=
              Min(HeadBobbingGoingDownSpeed * CompSpeed,
                1 - FracHeadBobbingPosition);
        end else
        begin
          if FracHeadBobbingPosition > SingleEqualityEpsilon then
            HeadBobbingPosition -=
              Min(HeadBobbingGoingDownSpeed * CompSpeed,
                FracHeadBobbingPosition);
        end;
      end;
    end;

    function IsStandingOnTheGround: boolean;
    var
      MinHeightAboveTheGround, MaxHeightAboveTheGround, H: Single;
    begin
      H := RealCameraPreferredHeightNoHeadBobbing;
      MinHeightAboveTheGround := (H - H * HeadBobbing) * 0.99;
      MaxHeightAboveTheGround := (H + H * HeadBobbing) * 1.01;
      Result :=
        (Sqr(MinHeightAboveTheGround) <= SqrHeightAboveTheGround) and
        (SqrHeightAboveTheGround <= Sqr(MaxHeightAboveTheGround));
    end;

  var
    OldIsFallingDown: boolean;
  begin
    FIsWalkingOnTheGround := false;
    OldIsFallingDown := IsFallingDown;

    if Gravity then
    begin
      { calculate IsAboveTheGround, SqrHeightAboveTheGround }
      DoGetCameraHeight(IsAboveTheGround, SqrHeightAboveTheGround);

      FIsWalkingOnTheGround := MoveHorizontalDone and IsStandingOnTheGround;

      if not TryJump then
        if not TryGrow then
          if not TryFallingDown then
            if not TryFde_Stabilize then
              { Note that we don't do FallingOnTheGround effect until all
                other effects (jumping, growing, falling on the ground
                and stabilizing after falling on the ground) will finish
                their work. }
              if not TryFallingOnTheGround then
                HeadBobbingGoesDown;
    end else
    begin
      FIsFallingDown := false;
      TryFde_Stabilize;
    end;

    if OldIsFallingDown and (not IsFallingDown) then
      DoFalledDown;
  end;

  procedure PreferHomeUpForRotationsIdle;
  (* This is a good piece of work and seemed to work OK,
     but it's too much untested right now to let it work.

     It's needed only when you'll start to change
     PreferHomeUpForRotations from false to true in runtime,
     to avoid making player feel "awkward" rotations.

     Temporary I don't need it.

  var
    TargetPlane: TVector4Single;
    TargetPlaneDir: TVector3Single absolute TargetPlane;
    TargetCameraUp: TVector3Single;
    AngleRadBetweenTargetAndHome: Single;
    AngleRadBetweenTarget, AngleRadBetweenTargetChange: Single;
    NewCameraUp: TVector3Single;
  begin
    if PreferHomeUp then
    begin
      { TODO: Correcting MinAngleRadFromHomeUp }

      { Correct CameraUp such that HomeCameraUp, CameraDir and CameraUp
        are on the same plane.

        Math:
          TargetPlane := common plane of HomeCameraUp and CameraDir,
          given by (A, B, C) = VectorProduct(HomeCameraUp, CameraDir)
          and D = 0 (because point (0, 0, 0) is part of this plane).

          We check whether CameraUp is on this TargetPlane too.

          If not, we find TargetCameraUp = nearest point to CameraUp
          lying on this TargetPlane. We want our CameraUp be pointing
          like HomeCameraUp, not in the other way, so if the angle between
          HomeCameraUp and TargetCameraUp is > 90 degress we negate
          TargetCameraUp. If the angle is exactly 90 degress then
          TargetCameraUp is simply equal to HomeCameraUp.

          And then we make the angle between TargetCameraUp and CameraUp
          smaller. }

      TargetPlaneDir := VectorProduct(HomeCameraUp, CameraDir);
      if not IsZero(
         (TargetPlaneDir[0] * FCameraUp[0]) +
         (TargetPlaneDir[1] * FCameraUp[1]) +
         (TargetPlaneDir[2] * FCameraUp[2])) then
      begin
        TargetPlane[3] := 0;

        Writeln('corrrecting');

        { calculate TargetCameraUp }
        TargetCameraUp := PointOnPlaneClosestToPoint(TargetPlane, FCameraUp);
        AngleRadBetweenTargetAndHome :=
          AngleRadBetweenVectors(TargetCameraUp, HomeCameraUp);
        if FloatsEqual(AngleRadBetweenTargetAndHome, HalfPi) then
          TargetCameraUp := HomeCameraUp else
        if AngleRadBetweenTargetAndHome > HalfPi then
          VectorNegateTo1st(TargetCameraUp);

        AngleRadBetweenTarget :=
          AngleRadBetweenVectors(TargetCameraUp, FCameraUp);
        AngleRadBetweenTargetChange := 0.01 * CompSpeed;
        if AngleRadBetweenTarget > AngleRadBetweenTargetChange then
        begin
          NewCameraUp := FCameraUp;
          MakeVectorsAngleRadOnTheirPlane(NewCameraUp, TargetCameraUp,
            AngleRadBetweenTarget - AngleRadBetweenTargetChange);
          CameraUp := NewCameraUp;
        end else
          CameraUp := TargetCameraUp;
      end;
    end;
    *)
  begin
  end;

var
  ModsDown: TModifierKeys;
begin
  ModsDown := ModifiersDown(KeysDown);

  HeadBobbingAlreadyDone := false;
  MoveHorizontalDone := false;

  FIsCrouching := KeysDown^[Key_Crouch];

  if (not CheckModsDown) or (ModsDown = []) then
  begin
    CheckRotates(1.0);

    if KeysDown^[Key_Forward] then MoveHorizontal;
    if KeysDown^[Key_Backward] then MoveHorizontal(-1);

    if KeysDown^[Key_RightStrafe] then
    begin
      RotateHorizontalForStrafeMove(-90);
      MoveHorizontal;
      RotateHorizontalForStrafeMove(90);
    end;

    if KeysDown^[Key_LeftStrafe] then
    begin
      RotateHorizontalForStrafeMove(90);
      MoveHorizontal;
      RotateHorizontalForStrafeMove(-90);
    end;

    { A simple implementation of Key_UpMove was
        RotateVertical(90); Move(MoveVertSpeed * CompSpeed); RotateVertical(-90)
      Similarly, simple implementation of Key_DownMove was
        RotateVertical(-90); Move(MoveVertSpeed * CompSpeed); RotateVertical(90)
      But this is not good, because when PreferHomeUp, we want to move along the
      CameraHomeUp. (Also later note: RotateVertical is now bounded by
      MinAngleRadFromHomeUp). }
    if KeysDown^[Key_UpMove] then
      MoveVertical( 1);
    if KeysDown^[Key_DownMove] then
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
    if KeysDown^[Key_MoveSpeedInc] then
    begin
      FMoveSpeed *= Power(1.1, CompSpeed);
      FMoveVertSpeed *= Power(1.1, CompSpeed);
      MatrixChanged;
    end;

    if KeysDown^[Key_MoveSpeedDec] then
    begin
      FMoveSpeed /= Power(1.1, CompSpeed);
      FMoveVertSpeed /= Power(1.1, CompSpeed);
      MatrixChanged;
    end;
  end else
  if AllowSlowerRotations and (ModsDown = [mkCtrl]) then
  begin
    CheckRotates(0.1);
  end;

  PreferHomeUpForRotationsIdle;

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

  procedure Jump;
  var
    IsAboveTheGround: boolean;
    SqrHeightAboveTheGround: Single;
  begin
    if IsJumping or IsFallingDown or (not Gravity) then Exit;

    { Merely checking for IsFallingDown is not enough, because IsFallingDown
      may be triggered with some latency. E.g. consider user that holds
      Key_Jump key down: whenever jump will end (in GravityIdle),
      KeysDown[Key_Jump] = true will cause another jump to be immediately
      (before IsFallingDown will be set to true) initiated.
      This is of course bad, because user holding Key_Jump key down
      would be able to jump to any height. The only good thing to do
      is to check whether player really has some ground beneath his feet
      to be able to jump. }

    { calculate IsAboveTheGround, SqrHeightAboveTheGround }
    DoGetCameraHeight(IsAboveTheGround, SqrHeightAboveTheGround);

    if (not IsAboveTheGround) or
       (SqrHeightAboveTheGround >
          Sqr(RealCameraPreferredHeight + RealCameraPreferredHeightMargin)) then
      Exit;

    FIsJumping := true;
    FJumpHeight := 0.0;
  end;

begin
 result := inherited;
 if result then exit;

 { nie interesuja nas tutaj zdarzenia z Key = K_None }
 if Key = K_None then Exit;

 if (not CheckModsDown) or (ModifiersDown(KeysDown) = []) then
 begin
   if Key = Key_HomeUp then
    CameraUp := HomeCameraUp else
   if Key = Key_Jump then
    Jump else
   {tu dopisuj inne klawisze jako
   if Key = <key> then
    <do-something> else}
    Exit(false);

   result := true;
 end;
end;

procedure TMatrixWalker.Init(
  const AHomeCameraPos, AHomeCameraDir, AHomeCameraUp: TVector3Single;
  const ACameraPreferredHeight: Single;
  const ACameraRadius: Single);
begin
 SetCameraHome_LookDir(AHomeCameraPos, AHomeCameraDir, AHomeCameraUp);
 CameraPreferredHeight := ACameraPreferredHeight;
 CorrectCameraPreferredHeight(ACameraRadius);
 Home;
end;

procedure TMatrixWalker.Init(const Box: TBox3d; const ACameraRadius: Single);
var Pos: TVector3Single;
    AvgSize: Single;
begin
 if IsEmptyBox3d(Box) then
  Init(Vector3Single(0, 0, 0),
       Vector3Single(0, 0, -1),
       Vector3Single(0, 1, 0), 0.0 { whatever }, ACameraRadius) else
 begin
  AvgSize := Box3dAvgSize(Box);
  Pos[0] := Box[0, 0]-AvgSize;
  Pos[1] := (Box[0, 1]+Box[1, 1])/2;
  Pos[2] := (Box[0, 2]+Box[1, 2])/2;
  Init(Pos, VectorAdjustToLength(UnitVector3Single[0], AvgSize*0.1),
    UnitVector3Single[2], AvgSize * 0.1, ACameraRadius);
 end;
end;

procedure TMatrixWalker.SetCameraHome_LookDir(const AHomeCameraPos,
  AHomeCameraDir, AHomeCameraUp: TVector3Single);
begin
 FHomeCameraPos := AHomeCameraPos;
 FHomeCameraDir := AHomeCameraDir;
 FHomeCameraUp := AHomeCameraUp;
 MakeVectorsOrthoOnTheirPlane(FHomeCameraUp, FHomeCameraDir);
 MatrixChanged;
end;

procedure TMatrixWalker.SetCameraHome_LookAt(const AHomeCameraPos,
  AHomeCameraCenter, AHomeCameraUp: TVector3Single);
begin
 SetCameraHome_LookDir(AHomeCameraPos, VectorSubtract(AHomeCameraCenter,
   AHomeCameraPos), AHomeCameraUp);
end;

procedure TMatrixWalker.SetCameraPos(const Value: TVector3Single);
begin
  FCameraPos := Value;
  MatrixChanged;
end;

procedure TMatrixWalker.SetCameraDir(const Value: TVector3Single);
begin
 FCameraDir := Value;
 MakeVectorsOrthoOnTheirPlane(FCameraUp, FCameraDir);
 MatrixChanged;
end;

procedure TMatrixWalker.SetCameraUp(const Value: TVector3Single);
begin
 FCameraUp := Value;
 MakeVectorsOrthoOnTheirPlane(FCameraDir, FCameraUp);
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

procedure TMatrixWalker.CorrectCameraPreferredHeight(const CameraRadius: Single);
begin
  MatrixNavigation.CorrectCameraPreferredHeight(
    FCameraPreferredHeight, CameraRadius, CrouchHeight, HeadBobbing);
end;

function TMatrixWalker.MaxJumpDistance: Single;
begin
  Result := MaxJumpHeight * CameraPreferredHeight;
end;

function TMatrixWalker.CameraDirInHomePlane: TVector3Single;
begin
  Result := CameraDir;

  if not VectorsParallel(Result, HomeCameraUp) then
    MakeVectorsOrthoOnTheirPlane(Result, HomeCameraUp);
end;

procedure TMatrixWalker.FallOnTheGround;
begin
  FFallingOnTheGround := true;

  { Mathematically reasoning, this should be smarter.
    I mean that we should randomize FFallingOnTheGroundAngleIncrease
    *only* if CameraUp is parallel to HomeCameraUp ?
    Otherwise CameraUp could change through some strange path ?

    But current effect seems to behave good in all situations...
    In any case, CameraUp going through some strange path will only
    be noticeable for a very short time, so I don't think that's a real
    problem... unless I see some example when it looks bad. }

  FFallingOnTheGroundAngleIncrease := Random(2) = 0;
end;

procedure TMatrixWalker.CancelFallingDown;
begin
  { Fortunately implementation of this is brutally simple right now. }
  FIsFallingDown := false;
end;

procedure TMatrixWalker.MouseMove(MouseXChange, MouseYChange: Integer);
begin
  if MouseLook then
  begin
    if MouseXChange <> 0 then
      RotateHorizontal(-MouseXChange * MouseLookHorizontalSensitivity);
    if MouseYChange <> 0 then
      RotateVertical(-MouseYChange * MouseLookVerticalSensitivity);
  end;
end;

{ global ------------------------------------------------------------ }

procedure CorrectCameraPreferredHeight(var CameraPreferredHeight: Single;
  const CameraRadius: Single; const CrouchHeight, HeadBobbing: Single);
var
  NewCameraPreferredHeight: Single;
begin
  { We have requirement that
      CameraPreferredHeight * CrouchHeight * (1 - HeadBobbing) >= CameraRadius
    So
      CameraPreferredHeight >= CameraRadius / (CrouchHeight * (1 - HeadBobbing));

    I make it even a little larger (that's the reason for "* 1.01") to be
    sure to avoid floating-point rounding errors. }

  NewCameraPreferredHeight := 1.01 * CameraRadius /
    (CrouchHeight * (1 - HeadBobbing));

  if CameraPreferredHeight < NewCameraPreferredHeight then
    CameraPreferredHeight := NewCameraPreferredHeight;
end;

end.
