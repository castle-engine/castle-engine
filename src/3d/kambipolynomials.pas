{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Some polynomials handling.
  Currently useful only for @link(Curve) unit.) }

unit KambiPolynomials;

interface

uses KambiUtils;

type
  { Natural cubic spline.
    In Polish this is "naturalna funkcja sklejana III stopnia"
    -- hopefully this is proper translation ?

    May be periodic or not. }
  TNaturalCubicSpline = class
  private
    FMinX, FMaxX: Float;
    FOwnsX, FOwnsY: boolean;
    FPeriodic: boolean;
    FX, FY: TDynFloatArray;
    M: TDynFloatArray;
  public
    property MinX: Float read FMinX;
    property MaxX: Float read FMaxX;
    property Periodic: boolean read FPeriodic;

    { Constructs natural cubic spline such that for every i in [0; X.Count-1]
      s(X[i]) = Y[i]. Must be X.Count = Y.Count.
      X must be already sorted.
      MinX = X[0], MaxX = X[X.Count-1].

      Warning: we will copy references to X and Y ! So make sure that these
      objects are available for the life of this object.
      We will free in destructor X if OwnsX and free Y if OwnsY. }
    constructor Create(X, Y: TDynFloatArray; AOwnsX, AOwnsY, APeriodic: boolean);
    destructor Destroy; override;

    { Evaluate value of natural cubic spline at x.
      Must be MinX <= x <= MaxX. }
    function Evaluate(x: Float): Float;
  end;

implementation

{ SLE := Stanislaw Lewanowicz }

constructor TNaturalCubicSpline.Create(X, Y: TDynFloatArray;
  AOwnsX, AOwnsY, APeriodic: boolean);

var
  { n = X.High. Integer, not Cardinal, to avoid some overflows in n-2. }
  n: Integer;

  { [Not]PeriodicDK licza wspolczynnik d_k. k jest na pewno w [1; n-1] }
  function PeriodicDK(k: Integer): Float;
  var h_k: Float;
      h_k1: Float;
  begin
   h_k  := X[k] - X[k-1];
   h_k1 := X[k+1] - X[k];
   Result := ( 6 / (h_k + h_k1) ) *
             ( (Y[k+1] - Y[k]) / h_k1 -
               (Y[k] - Y[k-1]) / h_k
             );
  end;

  { special version, works like PeriodicDK(n) should work
    (but does not, PeriodicDK works only for k < n.) }
  function PeriodicDN: Float;
  var h_n: Float;
      h_n1: Float;
  begin
   h_n  := X[n] - X[n-1];
   h_n1 := X[1] - X[0];
   Result := ( 6 / (h_n + h_n1) ) *
             ( (Y[1] - Y[n]) / h_n1 -
               (Y[n] - Y[n-1]) / h_n
             );
  end;

  function IlorazRoznicowy(const Start, Koniec: Cardinal): Float;
  { liczenie pojedynczego ilorazu roznicowego wzorem rekurencyjnym.
    Poniewaz do algorytmu bedziemy potrzebowali tylko ilorazow stopnia 3
    (lub mniej) i to tylko na chwile - wiec taka implementacja
    (zamiast zabawa w tablice) bedzie prostsza i wystarczajaca. }
  begin
   if Start = Koniec then
    Result := Y[Start] else
    Result := (IlorazRoznicowy(Start + 1, Koniec) -
               IlorazRoznicowy(Start, Koniec - 1))
               / (X[Koniec] - X[Start]);
  end;

  function NotPeriodicDK(k: Integer): Float;
  begin
   Result := 6 * IlorazRoznicowy(k-1, k+1);
  end;

var u, q, s, t, v: TDynFloatArray;
    hk, dk, pk, delta_k, delta_n, h_n, h_n1: Float;
    k: Integer;
begin
 inherited Create;
 Assert(X.Count = Y.Count);
 FMinX := X[0];
 FMaxX := X[X.High];
 FOwnsX := AOwnsX;
 FOwnsY := AOwnsY;
 FX := X;
 FY := Y;
 FPeriodic := APeriodic;

 { prepare to calculate M }
 n := X.High;
 M := TDynFloatArray.Create(n+1);

 { Algorytm obliczania wartosci M[0..n] z notatek SLE, te same oznaczenia.
   Sa tutaj polaczone algorytmy na Periodic i not Perdiodic, zeby mozliwie
   nie duplikowac kodu (i uniknac pomylek z copy&paste).
   Tracimy przez to troche czasu (wielokrotne testy "if Periodic ..."),
   ale kod jest prostszy i bardziej elegancki.

   Notka: W notatkach SLE dla Periodic = true w jednym miejscu uzyto
   M[n+1] ale tak naprawde nie musimy go liczyc ani uzywac. }

 u := nil;
 q := nil;
 s := nil;
 try
  u := TDynFloatArray.Create(n);
  q := TDynFloatArray.Create(n);
  if Periodic then s := TDynFloatArray.Create(n);

  { calculate u[0], q[0], s[0] }
  u[0] := 0;
  q[0] := 0;
  if Periodic then s[0] := 1;

  for k := 1 to n - 1 do
  begin
   { calculate u[k], q[k], s[k] }

   hk := X[k] - X[k-1];
   { delta[k] = h[k] / (h[k] + h[k+1])
              = h[k] / (X[k] - X[k-1] + X[k+1] - X[k])
              = h[k] / (X[k+1] - X[k-1])
   }
   delta_k := hk / (X[k+1] - X[k-1]);
   pk := delta_k * q[k-1] + 2;
   q[k]:=(delta_k - 1) / pk;
   if Periodic then s[k] := - delta_k * s[k-1] / pk;
   if Periodic then
    dk := PeriodicDK(k) else
    dk := NotPeriodicDK(k);
   u[k]:=(dk - delta_k * u[k-1]) / pk;
  end;

  { teraz wyliczamy wartosci M[0..n] }
  if Periodic then
  begin
   t := nil;
   v := nil;
   try
    t := TDynFloatArray.Create(n+1);
    v := TDynFloatArray.Create(n+1);

    t[n] := 1;
    v[n] := 0;

    { z notatek SLE wynika ze t[0], v[0] nie sa potrzebne (i k moze robic
      "downto 1" zamiast "downto 0") ale t[0], v[0] MOGA byc potrzebne:
      przy obliczaniu M[n] dla n = 1. }
    for k := n-1 downto 0 do
    begin
     t[k] := q[k] * t[k+1] + s[k];
     v[k] := q[k] * v[k+1] + u[k];
    end;

    h_n  := X[n] - X[n-1];
    h_n1 := X[1] - X[0];
    delta_n := h_n / (h_n + h_n1);
    M[n] := (PeriodicDN - (1-delta_n) * v[1] - delta_n * v[n-1]) /
            (2          + (1-delta_n) * t[1] + delta_n * t[n-1]);
    M[0] := M[n];
    for k := n-1 downto 1 do  M[k] := v[k] + t[k] * M[n];
   finally
    t.Free;
    v.Free;
   end;
  end else
  begin
   { zawsze M[0] = M[n] = 0, zeby latwo bylo zapisac obliczenia w Evaluate }
   M[0] := 0;
   M[n] := 0;
   M[n-1] := u[n-1];
   for k := n-2 downto 1 do M[k] := u[k] + q[k] * M[k+1];
  end;
 finally
  u.Free;
  q.Free;
  s.Free;
 end;
end;

destructor TNaturalCubicSpline.Destroy;
begin
 if FOwnsX then FX.Free;
 if FOwnsY then FY.Free;
 M.Free;
 inherited;
end;

function TNaturalCubicSpline.Evaluate(x: Float): Float;

  function Power3rd(const a: Float): Float;
  begin
   Result := a * a * a;
  end;

var k, KMin, KMax, KMiddle: Cardinal;
    hk: Float;
begin
 Clamp(x, MinX, MaxX);

 { calculate k: W ktorym przedziale x[k-1]..x[k] jest argument ?
   TODO: nalezoloby pomyslec o wykorzystaniu faktu
   ze czesto wiadomo iz wezly x[i] sa rownoodlegle. }
 KMin := 1;
 KMax := FX.High;
 repeat
  KMiddle:=(KMin + KMax) div 2;
  { jak jest ulozony x w stosunku do przedzialu FX[KMiddle-1]..FX[KMiddle] ? }
  if x < FX[KMiddle-1] then KMax := KMiddle-1 else
   if x > FX[KMiddle] then KMin := KMiddle+1 else
    begin
     KMin := KMiddle; { set only KMin, KMax is meaningless from now }
     break;
    end;
 until KMin = KMax;

 k := KMin;

 Assert(Between(x, FX[k-1], FX[k]));

 { obliczenia uzywaja tych samych symboli co w notatkach SLE }
 { teraz obliczam wartosc s(x) gdzie s to postac funkcji sklejanej
   na przedziale FX[k-1]..FX[k] }
 hk := FX[k] - FX[k-1];
 Result := ( ( M[k-1] * Power3rd(FX[k] - x) + M[k] * Power3rd(x - FX[k-1]) )/6 +
             ( FY[k-1] - M[k-1]*Sqr(hk)/6 )*(FX[k] - x)                        +
             ( FY[k]   - M[k  ]*Sqr(hk)/6 )*(x - FX[k-1])
           ) / hk;
end;

end.