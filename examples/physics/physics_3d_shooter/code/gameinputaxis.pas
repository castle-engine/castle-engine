unit GameInputAxis;

interface

uses
  Classes, SysUtils, CastleClassUtils, CastleKeysMouse, CastleUIControls,
  CastleVectors;

type
  TMouseLookAxis = (
    mlaHorizontal,
    mlaVertical);

  TCastleInputAxis = class (TCastleComponent)
  strict private
    FPositiveKey: TKey;
    FNegativeKey: TKey;
    FMouseLook: Boolean;
    FMouseLookAxis: TMouseLookAxis;
    FMouseLookMultiplier: Single;
    FMultiplier: Single;

    FLastUpdateMousePosition: TVector2;
    FLastMousePositionIsSet: Boolean;

    function HandleMouseLook(const Container: TCastleContainer): Single;
  public
    const
      DefaultMultiplier = 1.0;
      DefaultMouseLookMultiplier = 1.0;

    constructor Create(AOwner: TComponent); override;

    { Mouse look check mouse position between calls }
    function Value(const Container: TCastleContainer): Single;
  published
    property PositiveKey: TKey read FPositiveKey write FPositiveKey;
    property NegativeKey: TKey read FNegativeKey write FNegativeKey;
    property MouseLook: Boolean read FMouseLook write FMouseLook;
    property MouseLookAxis: TMouseLookAxis read FMouseLookAxis
      write FMouseLookAxis;
    property MouseLookMultiplier: Single read FMouseLookMultiplier
      write FMouseLookMultiplier;
    property Multiplier: Single read FMultiplier write FMultiplier;
  end;


implementation

uses Math, CastleLog;

{ TCastleInputAxis  ---------------------------------------------------------- }

function TCastleInputAxis.HandleMouseLook(const Container: TCastleContainer): Single;
var
  MouseChange: TVector2;
  Event: TInputMotion;
begin
  if Container.InternalMouseLookIgnoreNextMotion then
  begin
    Result := 0;
    FLastUpdateMousePosition := Container.MousePosition;
    WritelnLog('Motion Ignored');
    Exit;
  end;

  // MouseChange := (Container.MousePosition) - FLastUpdateMousePosition;

  // TODO: try to use here - MouseChange := Container.MouseLookDelta(Event, RenderRect);
  // and prepare TMotionEvent by myself to do that
  Event.OldPosition := FLastUpdateMousePosition;
  Event.Position := Container.MousePosition;
  Event.Pressed := Container.MousePressed;

  // TODO ?
  //Event.FingerIndex := ;
  // TODO: how to set render rect?
  MouseChange := Container.MouseLookDelta(Event);

  case MouseLookAxis of
  mlaHorizontal:
    Result := MouseChange.X;
  mlaVertical:
    Result := MouseChange.Y;
  end;
  FLastUpdateMousePosition := Container.MousePosition;
end;

constructor TCastleInputAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMouseLookMultiplier := DefaultMouseLookMultiplier;
  FMultiplier := DefaultMultiplier;
  FLastUpdateMousePosition := Vector2(0, 0);
end;

function TCastleInputAxis.Value(const Container: TCastleContainer): Single;
begin
  Result := 0;
  if (PositiveKey <> keyNone) and (Container.Pressed[PositiveKey])  then
    Result := 1;
  if (NegativeKey <> keyNone) and (Container.Pressed[NegativeKey])  then
    Result := Result - 1;

  if IsZero(Result) then
  begin
    // check mouse look and add mouse look multiplier
    if FMouseLook then
    begin
      if buttonRight in Container.MousePressed then
      begin
        {if not FLastMousePositionIsSet then
        begin
          FLastUpdateMousePosition := Container.MousePosition;
          FLastMousePositionIsSet := true;
          Container.MouseLookPress;
          WritelnLog('MouseLookPress');
        end;
        Container.MouseLookUpdate;
        Result := HandleMouseLook(Container) * MouseLookMultiplier;}

        case MouseLookAxis of
        mlaHorizontal:
          Result := Container.MouseLookLastDelta.X * MouseLookMultiplier;
        mlaVertical:
          Result := Container.MouseLookLastDelta.Y * MouseLookMultiplier;
        end;

        WritelnLog('REsult: ' + FloatToStr(Result));
      end else
      begin
        FLastMousePositionIsSet := false;
      end;
    end;
  end else
    FLastMousePositionIsSet := false;

  Result := Result * Multiplier;
end;

end.

