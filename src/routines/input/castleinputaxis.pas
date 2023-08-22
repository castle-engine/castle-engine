unit CastleInputAxis;

interface

uses
  Classes, SysUtils, CastleClassUtils, CastleKeysMouse, CastleUIControls,
  CastleVectors;

type
  TMouseLookAxis = (
    mlaHorizontal,
    mlaVertical);

  { Input for axis, good for specifying values on some axis.
    In case of keys we have values:
    - 0 - no key pressed
    - -1.0 negative key is pressed
    - 1.0 positive key is pressed

    In case of mouse look:
    - 0 - no mouse movement
    - other value - delta from mouse look

    Valuse can be modified by MouseLookMultiplier or Multiplier.

    Setting MouseLook to true do not turn mouse look on it means that
    CastleInputAxis will read Container.MouseLookLastDelta value }
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

    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Return value keys has priority currently (maybe should be changed?) }
    function Value(const Container: TCastleContainer): Single;
  published
    { Key for positive (1.0) value }
    property PositiveKey: TKey read FPositiveKey write FPositiveKey;
    { Key for negative (-1.0) value }
    property NegativeKey: TKey read FNegativeKey write FNegativeKey;
    {Setting MouseLook to true do not turn mouse look on it means that
    CastleInputAxis will read Container.MouseLookLastDelta value.
    Use TCastleContainer.StartMouseLook or TCastleContainer.StopMouseLook. }
    property MouseLook: Boolean read FMouseLook write FMouseLook;
    { Axis for mouse look horizontal or vertical }
    property MouseLookAxis: TMouseLookAxis read FMouseLookAxis
      write FMouseLookAxis;
    { Good place for change values returned by mouse look - change mouse sensitivity }
    property MouseLookMultiplier: Single read FMouseLookMultiplier
      write FMouseLookMultiplier;
    { General value multiplier }
    property Multiplier: Single read FMultiplier write FMultiplier;
  end;


implementation

uses Math, CastleUtils;

{ TCastleInputAxis  ---------------------------------------------------------- }

constructor TCastleInputAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMouseLookMultiplier := DefaultMouseLookMultiplier;
  FMultiplier := DefaultMultiplier;
end;

function TCastleInputAxis.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'PositiveKey', 'NegativeKey', 'MouseLook', 'MouseLookAxis',
     'MouseLookMultiplier', 'Multiplier'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
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

