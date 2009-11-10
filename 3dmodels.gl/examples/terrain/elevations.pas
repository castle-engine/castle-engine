unit Elevations;

interface

uses SysUtils, Classes;

type
  { Elevation (height for each X, Y) data.

    The specification of this is a little uncertain for now,
    in the future I want to derive both TElevationSRTM (data from stored
    1201 x 1201 grid) and TElevationRandom (data calculated from random
    noise) from this. Still unsure how the final class interface should
    look like, to be comfortable and sensible. }
  TElevation = class
  public
    //TODO: function Height(const X, Y: Single): Single;
  end;

  { Elevation data from a grid of values with specified width * height.
    Used when your underlying data is a simple 2D array of
    GridSizeX * GridSizeY heights.
    The idea is that on such elevation, there are special grid points
    where the height data is accurate. Everything else is an interpolation
    derived from this data.

    GridSizeX, GridSizeY specify grid dimensions.
    Use GridHeight(0..GridSizeX - 1, 0..GridSizeY - 1) to get height
    at particular grid point.

    Normal @link(Height) method, taking float arguments, is still usable
    and implemented in this class. Just be aware that it simply uses the data
    from GridHeight, interpolating it. So it's (very slightly) slower,
    and it doesn't really present any more interesting information
    (in contrast to typical procedural terrain, where there can be always
    more and more detail at each level). }
  TElevationGrid = class(TElevation)
  public
    function GridHeight(const X, Y: Cardinal): Single; virtual; abstract;
    function GridSizeX: Cardinal; virtual; abstract;
    function GridSizeY: Cardinal; virtual; abstract;
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
