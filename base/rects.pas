{ As for now this is my only unit (from the units that are supposed to compile with FPC)
  that depends on the Types unit (under FPC 1.9.x and DELPHI) or implements some
  part of Types unit functionality (under FPC 1.0.x).
  It also adds some functionality to Types unit - for now it's just InflateRect.

  I don't like Types unit because it's a MESS under Borland's Delphi and it's not
  avaiable under FPC 1.0.x. It's available with FPC 1.9.3 but, still, it's a huge mess
  (mainly to be compatible with Borland's unit - no part of FPC and rtl, fcl etc.
  use unit Types. This was just done in FPC 1.9.x to be compatible with Borland.)

  So ONLY sense I see in using Types unit is to get some convenient routines
  for operating on Windows.TRect type (and some parts of VCL and CLX).
  And that's exactly what I'm using this unit, Rects, for - only TRect type
  (exported from Types (under Delphi or newer FPC) or exported from Windows type
  (under FPC 1.0.10 and Windows) or manually declared (under FPC 1.0.10 and
  non-Windows) and some helpful functions.

  Units that use Rects and not Types (even under Delphi or newer FPC where
  Types unit is available) do not pollute their namespace by including unit
  Types with all it's trash.
}

unit Rects;

interface

{$ifndef VER1_0}
uses Types, KambiUtils;
{$else}

uses Windows, KambiUtils;

{ copy&pasted from FPC 1.9.3 Types.pp }
type
{$ifdef Win32}
  TRect = Windows.TRect;
{$else}
  TRect = packed record
    case Integer of
      0: (Left,Top,Right,Bottom : Longint);
      1: (TopLeft,BottomRight : TPoint);
    end;
{$endif}
{$endif not VER1_0}

function Bounds(ALeft,ATop,AWidth,AHeight : Integer) : TRect;

procedure InflateRect(var r:TRect; dx,dy:integer);
  {$ifdef WIN32} stdcall; external userDLL name 'InflateRect'; {$endif}
function InflatedRect(const r:TRect; dx,dy:integer):TRect;

implementation

{ copy&pasted from FPC 1.9.3 Types.pp }
function Bounds(ALeft,ATop,AWidth,AHeight : Integer) : TRect;
begin
  Bounds.Left:=ALeft;
  Bounds.Top:=ATop;
  Bounds.Right:=ALeft+AWidth;
  Bounds.Bottom:=ATop+AHeight;
end;

{$ifndef WIN32}
procedure InflateRect(var r:TRect; dx,dy:integer);
begin
 r.left:=r.left-dx;
 r.right:=r.right+dx;
 r.top:=r.top-dy;
 r.bottom:=r.bottom+dy;
end;
{$endif}

function InflatedRect(const r:TRect; dx,dy:integer):TRect;
begin
 result:=r;
 InflateRect(result,dx,dy);
end;

end.