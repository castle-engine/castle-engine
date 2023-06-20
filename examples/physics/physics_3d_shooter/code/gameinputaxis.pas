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

constructor TCastleInputAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMouseLookMultiplier := DefaultMouseLookMultiplier;
  FMultiplier := DefaultMultiplier;
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
      case MouseLookAxis of
      mlaHorizontal:
        Result := Container.MouseLookLastDelta.X * MouseLookMultiplier;
      mlaVertical:
        Result := Container.MouseLookLastDelta.Y * MouseLookMultiplier;
      end;
    end;
  end;

  Result := Result * Multiplier;
end;

end.

