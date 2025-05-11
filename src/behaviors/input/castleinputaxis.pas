unit CastleInputAxis;

interface

uses
  Classes, SysUtils, CastleClassUtils, CastleKeysMouse, CastleUIControls,
  CastleVectors;

type
  TCastleInputAxis = class;
  TAxisUpdateEvent = procedure (const Sender: TCastleInputAxis;
    var Value: Single) of object;

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

    FPositiveWheelDirection: TMouseWheelDirection;
    FNegativeWheelDirection: TMouseWheelDirection;

    FMouseLook: Boolean;
    FMouseLookAxis: TMouseLookAxis;
    FMouseLookMultiplier: Single;

    FMouseDrag: Boolean;
    FMouseDragAxis: TMouseLookAxis;
    FMouseDragMultiplier: Single;

    FMultiplier: Single;

    FOnUpdate: TAxisUpdateEvent;
  public
    const
      DefaultMultiplier = 1.0;
      DefaultMouseLookMultiplier = 1.0;
      DefaultMouseDragMultiplier = 1.0;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Return value keys has priority currently (maybe should be changed?) }
    function Value(const Container: TCastleContainer): Single;
  published
    { Key for positive (1.0) value }
    property PositiveKey: TKey read FPositiveKey write FPositiveKey;
    { Key for negative (-1.0) value }
    property NegativeKey: TKey read FNegativeKey write FNegativeKey;
    { Mouse wheel (scroll) direction for positive (1.0) value }
    property PositiveWheelDirection: TMouseWheelDirection
      read FPositiveWheelDirection write FPositiveWheelDirection;
    { Mouse wheel (scroll) direction for negative (-1.0) value }
    property NegativeWheelDirection: TMouseWheelDirection
      read FNegativeWheelDirection write FNegativeWheelDirection;
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
    {Setting MouseLook to true do not turn mouse look on it means that
    CastleInputAxis will read Container.MouseDragDelta value.
    Use TCastleContainer.StartMouseDrag or TCastleContainer.StopMouseDrag. }
    property MouseDrag: Boolean read FMouseDrag write FMouseDrag;
    { Axis for mouse drag horizontal or vertical }
    property MouseDragAxis: TMouseLookAxis read FMouseDragAxis
      write FMouseDragAxis;
    { Good place for change values returned by mouse drag - change mouse sensitivity }
    property MouseDragMultiplier: Single read FMouseDragMultiplier
      write FMouseDragMultiplier;
    { General value multiplier }
    property Multiplier: Single read FMultiplier write FMultiplier;
    { callback to change returned value when standard keys, mouse is not used. }
    property OnUpdate: TAxisUpdateEvent read FOnUpdate write FOnUpdate;
  end;


implementation

uses Math, CastleUtils, CastleLog;

{ TCastleInputAxis  ---------------------------------------------------------- }

constructor TCastleInputAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMouseLookMultiplier := DefaultMouseLookMultiplier;
  FMouseDragMultiplier := DefaultMouseDragMultiplier;
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
  if ((PositiveKey <> keyNone) and (Container.Pressed[PositiveKey])) or
   ((PositiveWheelDirection <> mwNone) and
   (Container.LastUpdateMouseWheelDirection = PositiveWheelDirection)) then
    Result := 1;
  if (NegativeKey <> keyNone) and (Container.Pressed[NegativeKey]) or
   ((NegativeWheelDirection <> mwNone) and
   (Container.LastUpdateMouseWheelDirection = NegativeWheelDirection)) then
    Result := Result - 1;

  if IsZero(Result) then
  begin
    // check mouse look and add mouse look multiplier
    if FMouseLook and Container.MouseLookStarted then
    begin
      case MouseLookAxis of
      mlaHorizontal:
        Result := Container.MouseLookLastDelta.X * MouseLookMultiplier;
      mlaVertical:
        Result := Container.MouseLookLastDelta.Y * MouseLookMultiplier;
      end;
    end else
    if FMouseDrag then
    begin
      case FMouseDragAxis of
        mlaHorizontal:
          Result := Container.MouseDragDelta.X * MouseDragMultiplier;
        mlaVertical:
          Result := Container.MouseDragDelta.Y * MouseDragMultiplier;
      end;
    end;
  end;

  if Assigned (FOnUpdate) then
    FOnUpdate(Self, Result);

  Result := Result * Multiplier;
end;

end.

