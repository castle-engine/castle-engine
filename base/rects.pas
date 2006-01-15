{ Utilities for Types unit.

  This is my only unit (from the units that are supposed to compile
  with FPC) that depends on the Types unit.

  I must admit that I don't like Types unit introduced by Delphi.
  It looks like a mess: a couple of hacks that are intended to somehow
  hide some differences between Linux and Windows (Types unit was
  invented when Kylix was made). IMHO this is bad, because you can't design
  a cross-platform unit by just throwing a couple of hacks inside
  that hide some minor part of differences between Windows and everything else.

  See the implementation of Types unit and you will know what I mean.
  Using Types unit means getting a namespace polluted with a lot of
  useless platform-specific names that you almost never need.

  For me, the only reason to use Types unit is to get convenient routines
  for operating on Windows.TRect type.
  And that's exactly what I'm using this unit, Rects, for --- only TRect type
  exported from Types and some helpful functions.
}

unit Rects;

interface

uses Types, KambiUtils;

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

procedure InflateRect(var r: TRect; dx, dy: integer);
  {$ifdef WIN32} stdcall; external userDLL name 'InflateRect'; {$endif}
function InflatedRect(const r: TRect; dx, dy: integer): TRect;

implementation

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Bounds.Left := ALeft;
  Bounds.Top := ATop;
  Bounds.Right := ALeft + AWidth;
  Bounds.Bottom := ATop + AHeight;
end;

{$ifndef WIN32}
procedure InflateRect(var r: TRect; dx, dy: integer);
begin
  r.left := r.left-dx;
  r.right := r.right+dx;
  r.top := r.top-dy;
  r.bottom := r.bottom+dy;
end;
{$endif}

function InflatedRect(const r: TRect; dx, dy: integer): TRect;
begin
  result := r;
  InflateRect(result, dx, dy);
end;

end.