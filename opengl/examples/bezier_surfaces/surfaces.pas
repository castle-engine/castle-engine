{
  Copyright 2006-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Bezier surface. }
unit Surfaces;

interface

uses Math, Curve;

type
  TSurface = class
  private
    FXBegin, FXEnd: Float;
    FYBegin, FYEnd: Float;
  public
    { @noAutoLinkHere }
    constructor Create(const AXBegin, AXEnd, AYBegin, AYEnd: Float);

    { @noAutoLinkHere }
    destructor Destroy; override;

    { Curves list.

      Note that each curve may be any TCurve descendant
      (with control points or without). Note that for control points-based
      curves (like Bezier ones) there is no requiment for a number
      of control points (each curve may have different number of
      control points).

      The only requirement is that for each curve, it's TBegin must
      be equal to our XBegin and TEnd must be equal to our XEnd.

      This list is owned by this class, and in destructor we will
      free @italic(with contents) (i.e. freeing also the items inside)
      this list.

      @noAutoLinkHere }
    Curves: TCurvesList;

    { XBegin/XEnd determine the valid range of t.
      Must be XBegin <= XEnd.
      @groupBegin }
    property XBegin: Float read FXBegin write FXBegin;
    property XEnd  : Float read FXEnd   write FXEnd;
    property YBegin: Float read FYBegin write FYBegin;
    property YEnd  : Float read FYEnd   write FYEnd;
    { @groupEnd }

    { This renders given surface.

      Note that it uses TRationalBezierCurve to construct the vertical
      curves used to calculate the surface. (Still, it doesn't require
      curves on the @link(Curves) list to be of any particular descendant
      of TCurve). In the future it may be extended to use any TCurve
      descendant.

      @noAutoLinkHere }
    procedure Render(const XSegments, YSegments: Cardinal);

    { This renders quads depicting all control points.

      For this, each curve on @link(Curves) must be a TControlPointsCurve
      descendant and must have equal number of control points.

      Each quad is rendered with a correct normal vector. }
    procedure RenderControlPoints;
  end;

implementation

uses KambiClassUtils, GL, GLU, KambiGLUtils, VectorMath, SysUtils,
  BezierCurve, KambiUtils;

constructor TSurface.Create(const AXBegin, AXEnd, AYBegin, AYEnd: Float);
begin
  inherited Create;
  Curves := TCurvesList.Create;
  FXBegin := AXBegin;
  FXEnd   := AXEnd;
  FYBegin := AYBegin;
  FYEnd   := AYEnd;
end;

destructor TSurface.Destroy;
begin
  FreeWithContentsAndNil(Curves);
  inherited;
end;

procedure TSurface.Render(const XSegments, YSegments: Cardinal);

  { Make a vertical curve with control points taken from
    all normal Curves at given X (in the range XBegin ... XEnd). }
  function MakeCurve(const X: Single): TCurve;
  var
    I: Cardinal;
  begin
    Result := TRationalBezierCurve.Create(YBegin, YEnd);
    for I := 0 to Curves.High do
    begin
      TRationalBezierCurve(Result).ControlPoints.AppendItem(
        Curves.Items[I].Point(X));
      TRationalBezierCurve(Result).Weights.AppendItem(1.0);
    end;
    TRationalBezierCurve(Result).UpdateControlPoints;
  end;

var
  I, J: Integer;
  X, Y: Single;
  NewPointPrev, NewPointNow, LastPointPrev, LastPointNow: TVector3Single;
  CurvePrev, CurveNow: TCurve;
begin
  CurvePrev := MakeCurve(XBegin);
  for I := 1 to XSegments do
  begin
    X := MapRange(I, 0, XSegments, XBegin, XEnd);
    CurveNow := MakeCurve(X);

    glBegin(GL_QUAD_STRIP);
      LastPointPrev := CurvePrev.Point(YBegin);
      LastPointNow := CurveNow.Point(YBegin);

      glVertexv(LastPointPrev);
      glVertexv(LastPointNow);

      for J := 0 to YSegments do
      begin
        Y := MapRange(J, 0, YSegments, YBegin, YEnd);

        NewPointPrev := CurvePrev.Point(Y);
        NewPointNow := CurveNow.Point(Y);

        glNormalv(TriangleNormal(LastPointPrev, LastPointNow, NewPointNow));
        glVertexv(NewPointPrev);
        glVertexv(NewPointNow);

        LastPointPrev := NewPointPrev;
        LastPointNow := NewPointNow;
      end;
    glEnd;

    FreeAndNil(CurvePrev);
    CurvePrev := CurveNow;
  end;

  FreeAndNil(CurvePrev);
end;

procedure TSurface.RenderControlPoints;
var
  CurveControlPointsCount, I, J: Cardinal;
  Curve0, CurvePrev, CurveNow: TControlPointsCurve;
  LastPrev, LastNow, NewPrev, NewNow: TVector3Single;
begin
  Curve0 := Curves.Items[0] as TControlPointsCurve;
  CurveControlPointsCount := Curve0.ControlPoints.Count;

  for I := 1 to Curves.High do
  begin
    CurvePrev := Curves.Items[I-1] as TControlPointsCurve;
    CurveNow := Curves.Items[I] as TControlPointsCurve;
    Assert(CurveControlPointsCount = Cardinal(CurvePrev.ControlPoints.Count));
    Assert(CurveControlPointsCount = Cardinal(CurveNow.ControlPoints.Count));

    glBegin(GL_QUAD_STRIP);
      LastPrev := CurvePrev.ControlPoints.Items[0];
      LastNow := CurveNow.ControlPoints.Items[0];
      glVertexv(LastPrev);
      glVertexv(LastNow);

      for J := 1 to CurveControlPointsCount - 1 do
      begin
        NewPrev := CurvePrev.ControlPoints.Items[J];
        NewNow := CurveNow.ControlPoints.Items[J];

        glNormalv(TriangleNormal(LastPrev, LastNow, NewNow));
        glVertexv(NewPrev);
        glVertexv(NewNow);

        LastPrev := NewPrev;
        LastNow := NewNow;
      end;
    glEnd;
  end;
end;

end.
