unit Elevations;

interface

uses SysUtils, Classes, KambiScript;

type
  { Elevation (height for each X, Y) data. }
  TElevation = class
  public
    function Height(const X, Y: Single): Single; virtual; abstract;
  end;

  { Elevation (height for each X, Y) data calculated from KambiScript
    expression. At construction, pass FunctionExpression,
    that is KambiScript language expression calculating height
    based on X, Y. }
  TElevationKamScript = class(TElevation)
  private
    FXVariable, FYVariable: TKamScriptFloat;
    FFunction: TKamScriptExpression;
  public
    constructor Create(const FunctionExpression: string);
    destructor Destroy; override;
    function Height(const X, Y: Single): Single; override;
  end;

  { Elevation data from a grid of values with specified width * height.
    Used when your underlying data is a simple 2D array of
    GridSizeX * GridSizeY heights.
    The idea is that on such elevation, there are special grid points
    where the height data is accurate. Everything else is an interpolation
    derived from this data. }
  TElevationGrid = class(TElevation)
  public
    { Get height of the elevation at specified 2D point.

      This is implemented in TElevationGrid class, using
      the data returned by GridHeight. For float X in 0..1 range,
      we return grid values for grid points 0..GridSizeX - 1.
      Outside 0..1 range, we clamp (that is, take nearest value
      from 0..1 range) --- this way the elevation seemingly continues
      into the infinity.

      In comparison to GridHeight, it's (very slightly) slower,
      and it doesn't really present any more interesting information
      (in contrast to typical procedural terrain, where there can be always
      more and more detail at each level). }
    function Height(const X, Y: Single): Single; override;

    { GridSizeX, GridSizeY specify grid dimensions.
      Use GridHeight(0..GridSizeX - 1, 0..GridSizeY - 1) to get height
      at particular grid point.
      @groupBegin }
    function GridHeight(const X, Y: Cardinal): Single; virtual; abstract;
    function GridSizeX: Cardinal; virtual; abstract;
    function GridSizeY: Cardinal; virtual; abstract;
    { @groupEnd }
  end;

  TElevationSRTM = class(TElevationGrid)
  private
    FData: array [0..1200, 0..1200] of SmallInt;
  public
    constructor CreateFromFile(const FileName: string);

    function GridHeight(const X, Y: Cardinal): Single; override;
    function GridSizeX: Cardinal; override;
    function GridSizeY: Cardinal; override;
  end;

implementation

uses KambiUtils, KambiScriptParser;

{ TElevationKamScript -------------------------------------------------------- }

constructor TElevationKamScript.Create(const FunctionExpression: string);
begin
  inherited Create;

  FXVariable := TKamScriptFloat.Create(false);
  FXVariable.Name := 'x';
  FXVariable.OwnedByParentExpression := false;

  FYVariable := TKamScriptFloat.Create(false);
  FYVariable.Name := 'y';
  FYVariable.OwnedByParentExpression := false;

  FFunction := ParseFloatExpression(FunctionExpression, [FXVariable, FYVariable]);
end;

destructor TElevationKamScript.Destroy;
begin
  FFunction.FreeByParentExpression;
  FFunction := nil;

  FreeAndNil(FXVariable);
  FreeAndNil(FYVariable);

  inherited;
end;

function TElevationKamScript.Height(const X, Y: Single): Single;
begin
  FXVariable.Value := X;
  FYVariable.Value := Y;
  Result := (FFunction.Execute as TKamScriptFloat).Value;
end;

{ TElevationGrid ------------------------------------------------------------- }

function TElevationGrid.Height(const X, Y: Single): Single;
begin
  { TODO: for now, just take the nearest point, no bilinear filtering. }
  Result := GridHeight(
    Clamped(Round(X * (GridSizeX - 1)), 0, GridSizeX - 1),
    Clamped(Round(Y * (GridSizeY - 1)), 0, GridSizeY - 1));
end;

{ TElevationSRTM ------------------------------------------------------------- }

constructor TElevationSRTM.CreateFromFile(const FileName: string);
var
  Stream: TFileStream;
  P: PSmallInt;
  I: Cardinal;
  LastCorrectHeight: SmallInt;
begin
  inherited Create;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.ReadBuffer(FData, SizeOf(FData));
  finally FreeAndNil(Stream) end;

  LastCorrectHeight := 0; { any sensible value }
  P := @(FData[0, 0]);
  for I := 1 to 1201 * 1201 do
  begin
    {$ifdef ENDIAN_LITTLE}
    P^ := Swap(P^);
    {$endif ENDIAN_LITTLE}

    { Fix unknown data by setting to last correct seen value.
      Since we scan data cell-by-cell, in a row, this is in practice
      somewhat excusable approach. Of course, we could do something much better
      (filling unknown values by interpolating values from around). }
    if P^ = Low(SmallInt) then
      P^ := LastCorrectHeight else
      LastCorrectHeight := P^;

    Inc(P);
  end;
end;

function TElevationSRTM.GridHeight(const X, Y: Cardinal): Single;
begin
  Result := FData[X, Y];
end;

function TElevationSRTM.GridSizeX: Cardinal;
begin
  Result := 1201;
end;

function TElevationSRTM.GridSizeY: Cardinal;
begin
  Result := 1201;
end;

end.
