{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Some more interpolation filters for TFPCanvas.StretchDraw:

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit extinterpolation;

{
Some more interpolation filters for TFPCanvas.StretchDraw:
Bessel, Gaussian and Sinc are infinite impulse response (IIR),
the other are finite impulse response (FIR). The implementation
of Bessel and Sinc are windowed with Blackman filter.
}

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, FPImage, FPCanvas;

type

  { TBlackmanInterpolation }

  TBlackmanInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TBlackmanSincInterpolation }

  TBlackmanSincInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TBlackmanBesselInterpolation }

  TBlackmanBesselInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TGaussianInterpolation }

  TGaussianInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TBoxInterpolation }

  TBoxInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { THermiteInterpolation }

  THermiteInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TLanczosInterpolation }

  TLanczosInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TQuadraticInterpolation }

  TQuadraticInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TCubicInterpolation }

  TCubicInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TCatromInterpolation }

  TCatromInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TBilineairInterpolation }

  TBilineairInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { THanningInterpolation }

  THanningInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { THammingInterpolation }

  THammingInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

implementation


// BesselOrderOne: computes Bessel function of x in the first kind of order 0

function J1 (x : double) : double;
const Pone : array[0..8] of double =
    (  0.581199354001606143928050809e+21,
      -0.6672106568924916298020941484e+20,
       0.2316433580634002297931815435e+19,
      -0.3588817569910106050743641413e+17,
       0.2908795263834775409737601689e+15,
      -0.1322983480332126453125473247e+13,
       0.3413234182301700539091292655e+10,
      -0.4695753530642995859767162166e+7,
       0.270112271089232341485679099e+4
    );
    Qone : array [0..8] of double =
    ( 0.11623987080032122878585294e+22,
      0.1185770712190320999837113348e+20,
      0.6092061398917521746105196863e+17,
      0.2081661221307607351240184229e+15,
      0.5243710262167649715406728642e+12,
      0.1013863514358673989967045588e+10,
      0.1501793594998585505921097578e+7,
      0.1606931573481487801970916749e+4,
      0.1e+1
    );
var p,q : double;
    r : 0..8;
begin
  p := Pone[8];
  q := Qone[8];
  for r := 7 downto 0 do
    begin
    p := p*x*x+pOne[r];
    q := q*X*X+Qone[r];
    end;
  result := p / q;
end;

function P1 (x : double) : double;
const Pone : array[0..5] of double =
    ( 0.352246649133679798341724373e+5,
      0.62758845247161281269005675e+5,
      0.313539631109159574238669888e+5,
      0.49854832060594338434500455e+4,
      0.2111529182853962382105718e+3,
      0.12571716929145341558495e+1
    );
    Qone : array [0..5] of double =
    ( 0.352246649133679798068390431e+5,
      0.626943469593560511888833731e+5,
      0.312404063819041039923015703e+5,
      0.4930396490181088979386097e+4,
      0.2030775189134759322293574e+3,
      0.1e+1
    );
var x8,p,q : double;
    r : 0..5;
begin
  p := Pone[5];
  q := Qone[5];
  x8 := 8.0 / x;
  for r := 4 downto 0 do
    begin
    p := p*x8*x8+pOne[r];
    q := q*x8*x8+Qone[r];
    end;
  result := p / q;
end;

function Q1 (x : double) : double;
const Pone : array[0..5] of double =
    ( 0.3511751914303552822533318e+3,
      0.7210391804904475039280863e+3,
      0.4259873011654442389886993e+3,
      0.831898957673850827325226e+2,
      0.45681716295512267064405e+1,
      0.3532840052740123642735e-1
    );
    Qone : array [0..5] of double =
    ( 0.74917374171809127714519505e+4,
      0.154141773392650970499848051e+5,
      0.91522317015169922705904727e+4,
      0.18111867005523513506724158e+4,
      0.1038187585462133728776636e+3,
      0.1e+1
    );
var x8,p,q : double;
    r : 0..5;
begin
  p := Pone[5];
  q := Qone[5];
  x8 := 8.0 / x;
  for r := 4 downto 0 do
    begin
    p := p*x8*x8+pOne[r];
    q := q*x8*x8+Qone[r];
    end;
  result := p / q;
end;

function BesselOrderOne (x : double) : double;
var p,OneOverSqrt2,sinx,cosx : double;
begin
  if x = 0.0 then
    result := 0.0
  else
    begin
    p := x;
    if x < 0.0 then
      x := -x;
    if x < 8.0 then
      result := p * J1(x)
    else
      begin
      OneOverSqrt2 := 1.0 / sqrt(2.0);
      sinx := sin(x);
      cosx := cos(x);
      result := sqrt(2.0/(PI*x)) *
           ( P1(x)*(OneOverSqrt2*(sinx-cosx))
             - 8.0/x*Q1(x)*(-OneOverSqrt2*(sinx+cosx))
           );
      if p < 0.0 then
        result := -result;
      end
    end;
end;

// Functions to aid calculations

function Bessel (x : double) : double;
begin
  if x = 0.0 then
    result := PI / 4.0
  else
    result := BesselOrderOne(PI * x) / (2.0 * x);
end;

function Sinc (x : double) : double;
var xx : double;
begin
  if x = 0.0 then
    result := 1.0
  else
    begin
    xx := PI*x;
    result := sin(xx) / (xx);
    end;
end;

function Blackman (x : double) : double;
var xpi : double;
begin
  xpi := PI * x;
  result := 0.42 + 0.5 * cos(xpi) + 0.08 * cos(2*xpi);
end;

{ THermiteInterpolation }

function THermiteInterpolation.Filter(x: double): double;
begin
  if x < -1.0 then
    result := 0.0
  else if x < 0.0 then
    result := (2.0*(-x)-3.0)*(-x)*(-x)+1.0
  else if x < 1.0 then
    result := (2.0*x-3.0)*x*x+1.0
  else
    result := 0;
end;

function THermiteInterpolation.MaxSupport: double;
begin
  result := 1.0;
end;

{ TLanczosInterpolation }

function TLanczosInterpolation.Filter(x: double): double;
begin
  if x < -3.0 then
    result := 0.0
  else if x < 0.0 then
    result := sinc(-x)*sinc(-x/3.0)
  else if x < 3.0 then
    result := sinc(x)*sinc(x/3.0)
  else
    result := 0.0;
end;

function TLanczosInterpolation.MaxSupport: double;
begin
  result := 3.0;
end;

{ TQuadraticInterpolation }

function TQuadraticInterpolation.Filter(x: double): double;
begin
  if x < -1.5 then
    result := 0.0
  else if x < -0.5 then
    begin
    x := x + 1.5;
    result := 0.5*x*x;
    end
  else if x < 0.5 then
    result := 0.75 - x*x
  else if x < 1.5 then
    begin
    x := x - 1.5;
    result := 0.5*x*x;
    end
  else
    result := 0.0;
end;

function TQuadraticInterpolation.MaxSupport: double;
begin
  result := 1.5;
end;

{ TCubicInterpolation }

function TCubicInterpolation.Filter(x: double): double;
begin
  if x < -2.0 then
    result := 0.0
  else if x < -1.0 then
    begin
    x := x +2.0;
    result := x*x*x / 6.0;
    end
  else if x < 0.0 then
    result := (4.0+x*x*(-6.0-3.0*x)) / 6.0
  else if x < 1.0 then
    result := (4.0+x*x*(-6.0+3.0*x)) / 6.0
  else if x < 2.0 then
    begin
    x := 2.0 - x;
    result := x*x*x / 6.0;
    end
  else
    result := 0.0;
end;

function TCubicInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;

{ TCatromInterpolation }

function TCatromInterpolation.Filter(x: double): double;
begin
  if x < -2.0 then
    result := 0.0
  else if x < -1.0 then
    result := 0.5*(4.0+x*(8.0+x*(5.0+x)))
  else if x < 0.0 then
    result := 0.5*(2.0+x*x*(-5.0-3.0*x))
  else if x < 1.0 then
    result := 0.5*(2.0+x*x*(-5.0+3.0*x))
  else if x < 2.0 then
    result := 0.5*(4.0+x*(-8.0+x*(5.0-x)))
  else
    result := 0.0;
end;

function TCatromInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;

{ THanningInterpolation }

function THanningInterpolation.Filter(x: double): double;
begin
  if x < -1.0 then
    result := 0.0
  else if x <= 1.0 then
    result := 0.5+0.5*cos(PI*x)
  else
    result := 0.0;
end;

function THanningInterpolation.MaxSupport: double;
begin
  result := 1.0;
end;

{ THammingInterpolation }

function THammingInterpolation.Filter(x: double): double;
begin
  if x < -1.0 then
    result := 0.0
  else if x <= 1.0 then
    result := 0.54+0.46*cos(PI*x)
  else
    result := 0.0;
end;

function THammingInterpolation.MaxSupport: double;
begin
  result := 1.0;
end;

{ TBilineairInterpolation }

function TBilineairInterpolation.Filter(x: double): double;
begin
  if x < -1.0 then
    result := 0.0
  else if x < 0.0 then
    result := 1 + x
  else if x < 1.0 then
    result := 1 - x
  else
    result := 0.0;
end;

function TBilineairInterpolation.MaxSupport: double;
begin
  result := 1.0;
end;

{ TBoxInterpolation }

function TBoxInterpolation.Filter(x: double): double;
begin
  if x < -0.5 then
    result := 0.0
  else if x < 0.5 then
    result := 1.0
  else
    result := 0.0;
end;

function TBoxInterpolation.MaxSupport: double;
begin
  result := 0.5;
end;

{ TGaussianInterpolation }

function TGaussianInterpolation.Filter(x: double): double;
begin
  result := exp(-2.0*x*x) * sqrt(2.0/PI);
end;

function TGaussianInterpolation.MaxSupport: double;
begin
  result := 1.25;
end;

{ TBlackmanBesselInterpolation }

function TBlackmanBesselInterpolation.Filter(x: double): double;
begin
  result := Blackman(x/MaxSupport) * Bessel (x);
end;

function TBlackmanBesselInterpolation.MaxSupport: double;
begin
  Result := 3.2383;
end;

{ TBlackmanSincInterpolation }

function TBlackmanSincInterpolation.Filter(x: double): double;
begin
  Result := Blackman(x/MaxSupport) * Sinc(x);
end;

function TBlackmanSincInterpolation.MaxSupport: double;
begin
  Result := 4.0;
end;

{ TBlackmanInterpolation }

function TBlackmanInterpolation.Filter(x: double): double;
begin
  Result := Blackman (x);
end;

function TBlackmanInterpolation.MaxSupport: double;
begin
  Result := 1.0;
end;

end.


