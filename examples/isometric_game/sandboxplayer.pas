unit SandBoxPlayer;

interface

uses SysUtils, CastleImages, CastleGLUtils, CastleGLImages;

type
  TDirection = (
    dirNorth,
    dirEast,
    dirSouth,
    dirWest,
    dirNorthEast,
    dirSouthEast,
    dirSouthWest,
    dirNorthWest);

  TPlayer = class
  private
    MoveDistance: Single;
    MoveStartTime: Single;
    NewX, NewY: Cardinal;
    NewXPixel, NewYPixel: Integer;
    FX, FY: Cardinal;
    FXPixel: Integer;
    FYPixel: Integer;
    FDirection: TDirection;
  public
    constructor Create;
    destructor Destroy; override;

    property X: Cardinal read FX;
    property Y: Cardinal read FY;
    { These are coords calculated by ViewMoveToCenterPosition from (X, Y)
      in main program. }
    property XPixel: Integer read FXPixel;
    property YPixel: Integer read FYPixel;
  public
    Image: array [TDirection] of TCastleImage;
    GLImage: array [TDirection] of TGLImage;

    property Direction: TDirection read FDirection;

    procedure Teleport(ANewX, ANewY: Cardinal; NewDirection: TDirection);
    { Note that ChangeX, ChangeY may point to invalid coords --- we will
      correct (clamp) them as needed. }
    procedure Move(ChangeX, ChangeY: Integer; NewDirection: TDirection); overload;
    procedure Move(NewDirection: TDirection); overload;
  public
    { If Moving then he moves from (X, Y) position to (NewX, NewY).
      MovingSmallMoveX, MovingSmallMoveX is the exact pixel displacement
      of the player sprite then. }
    Moving: boolean;
    MovingSmallMoveX, MovingSmallMoveY: Single;

    procedure Idle;
  end;

implementation

uses CastleFilesUtils, CastleUtils, SandBoxGame;

constructor TPlayer.Create;
const
  MoveShortcutNames: array [TDirection] of string = (
    'N',
    'E',
    'S',
    'W',
    'NE',
    'SE',
    'SW',
    'NW');
var
  Dir: TDirection;
begin
  inherited Create;

  for Dir := Low(Dir) to High(Dir) do
  begin
    Image[Dir] := LoadImage(ProgramDataPath +
      'tiles' + PathDelim + 'woldforge' + PathDelim +
      'sprites' + PathDelim + 'creatures' + PathDelim + 'observer' + PathDelim +
      'observer_float_' + MoveShortcutNames[Dir] + '_1_hh.png',
      PixelsImageClasses);
    GLImage[Dir] := TGLImage.Create(Image[Dir]);
  end;
end;

destructor TPlayer.Destroy;
var
  Dir: TDirection;
begin
  for Dir := Low(Dir) to High(Dir) do
  begin
    FreeAndNil(Image[Dir]);
    FreeAndNil(GLImage[Dir]);
  end;
  inherited;
end;

procedure TPlayer.Teleport(ANewX, ANewY: Cardinal; NewDirection: TDirection);
begin
  FX := ANewX;
  FY := ANewy;
  ViewMoveToCenterPosition(X, Y, FXPixel, FYPixel);
  FDirection := NewDirection;
end;

procedure TPlayer.Move(ChangeX, ChangeY: Integer; NewDirection: TDirection);
var
  ANewX, ANewY: Cardinal;
begin
  ANewX := Clamped(Integer(X) + ChangeX, 0, Map.Width - 1);
  ANewY := Clamped(Integer(Y) + ChangeY, 0, Map.Height - 1);
  if not Moving then
  begin
    { Change Direction even if no move will be done --- this is a nice effect
      visually when standing on the edge of the map and trying to move beside
      the edge. }
    FDirection := NewDirection;
    if ((X <> ANewX) or
        (Y <> ANewY)) then
    begin
      Moving := true;
      NewX := ANewX;
      NewY := ANewY;
      ViewMoveToCenterPosition(NewX, NewY, NewXPixel, NewYPixel);
      MoveStartTime := GameTime;
      MovingSmallMoveX := 0;
      MovingSmallMoveY := 0;
      MoveDistance := Sqrt(Sqr(XPixel - NewXPixel) +
                           Sqr(YPixel - NewYPixel));

    end;
  end;
end;

procedure TPlayer.Move(NewDirection: TDirection);
begin
  case NewDirection of
    dirNorth: Move(0,  2, NewDirection);
    dirEast : Move( 1, 0, NewDirection);
    dirSouth: Move(0, -2, NewDirection);
    dirWest : Move(-1, 0, NewDirection);
    dirNorthEast: begin if     Odd(Y) then Move(+1,  1, NewDirection) else Move(0,  1, NewDirection) end;
    dirSouthEast: begin if     Odd(Y) then Move(+1, -1, NewDirection) else Move(0, -1, NewDirection) end;
    dirSouthWest: begin if not Odd(Y) then Move(-1, -1, NewDirection) else Move(0, -1, NewDirection) end;
    dirNorthWest: begin if not Odd(Y) then Move(-1,  1, NewDirection) else Move(0,  1, NewDirection) end;
  end;
end;

procedure TPlayer.Idle;
var
  Distance: Single;
const
  PlayerMoveSpeed = 300.0;
begin
  if Moving then
  begin
    Distance := (GameTime - MoveStartTime) * PlayerMoveSpeed;
    if Distance > MoveDistance then
    begin
      FX := NewX;
      FY := NewY;
      FXPixel := NewXPixel;
      FYPixel := NewYPixel;
      Moving := false;
    end else
    begin
      MovingSmallMoveX := MapRange(Distance, 0, MoveDistance,
        { It's XPixel - NewXPixel, not
               NewXPixel - XPixel, because
          our MovingSmallMoveX work in reverse fashion. }
        0, XPixel - NewXPixel);
      MovingSmallMoveY := MapRange(Distance, 0, MoveDistance,
        0, YPixel - NewYPixel);
    end;
  end;
end;

end.