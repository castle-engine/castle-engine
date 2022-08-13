{
  Copyright 2009-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating noise. }
unit CastleInternalNoise;

{$I castleconf.inc}

interface

{ Noise for 2D coords, resulting in float 0..1 range.

  This is the interpolated integer noise. That is,
  values on integer grid points (when X, Y are ints)
  come from the really independent "integer noise".
  Values between integer points are interpolated.

  Suffix describes interpolation method.

  @groupBegin }
function InterpolatedNoise2D_None(const X, Y: Single; const Seed: Cardinal): Single;
function InterpolatedNoise2D_Linear(const X, Y: Single; const Seed: Cardinal): Single;
function InterpolatedNoise2D_Cosine(const X, Y: Single; const Seed: Cardinal): Single;
function InterpolatedNoise2D_Spline(const X, Y: Single; const Seed: Cardinal): Single;
{ @groupEnd }

{ Noise for 2D coords, resulting in float 0..1 range, additionally blurred.

  Blurring smooths the noise by averaging values a little with neighbors
  (just like blur on normal 2D images). This helps for 2D noise
  to be less vertical/horizontal oriented.

  BlurredInterpolatedNoise* functions first blur
  the IntegerNoise, and then blurred noise is interpolated.
  Results are the same as the other way around (first interpolate IntegerNoise,
  and only then blur it), and marginally faster.

  @groupBegin }
function BlurredInterpolatedNoise2D_None(const X, Y: Single; const Seed: Cardinal): Single;
function BlurredInterpolatedNoise2D_Linear(const X, Y: Single; const Seed: Cardinal): Single;
function BlurredInterpolatedNoise2D_Cosine(const X, Y: Single; const Seed: Cardinal): Single;
function BlurredInterpolatedNoise2D_Spline(const X, Y: Single; const Seed: Cardinal): Single;
{ @groupEnd }

implementation

uses Math, CastleCurves;

{ Integer noise -------------------------------------------------------------- }

{ Make integer noise: for 3 given LongInts, generate random LongWord value.

  It is designed to fill uniformly the LongWord range.
  So if you want float result in 0 ... 1, just divide by High(LongWord),
  which is exactly what IntegerNoise (without Core suffix) does.

  If you want noise based on 2D coords, just pass Z = 0 -- it's Ok,
  noise will still nicely fill whole range.

  Seed chooses prime numbers that are "seeds" for this noise.
  Any Cardinal value is Ok, although actually we have only ~100 sets
  of prime numbers prepared, so Seed = i*100+k may be equal
  to Seed = k. }

{ This integer noise implementation is very close to the one on
  http://freespace.virgin.net/hugo.elias/models/m_perlin.htm,
  and it follows Blender's cellNoiseU. Especially Seed = 0
  values are equal. }

const
  Primes: array [0..99] of array [0..4] of UInt32 = (
    (1301, 314159, 15731, 789221, 1376312589),
    { Following rows were generated. See gen_primes.lpr to know how }
    (1847, 423307, 10139, 1249603, 1665650897),
    (1987, 728471, 14081, 2490529, 1392026473),
    (1523, 311111, 12149, 4335337, 1415538263),
    (1097, 727427, 18637, 3364037, 1150632649),
    (1279, 535991, 19889, 1354247, 1372899589),
    (1637, 546841, 15859, 4067467, 1842430829),
    (1801, 609101, 14699, 1722163, 1301061851),
    (1571, 678157, 12569, 1476983, 1425758801),
    (1277, 770447, 12569, 3671453, 1160210171),
    (1693, 247337, 14533, 2917181, 1096009073),
    (1997, 472631, 15101, 2206927, 1308414101),
    (1381, 243343, 13399, 3593581, 1473006989),
    (1087, 343787, 19121, 1666303, 1827806587),
    (1511, 432457, 19889, 4688323, 1140647509),
    (1423, 638423, 10859, 2792261, 1388850959),
    (1693, 306941, 12227, 2114197, 1486745171),
    (1811, 459623, 12097, 1701967, 1789608661),
    (1931, 333997, 16453, 1909399, 1963309847),
    (1471, 663031, 19333, 3723271, 1123370333),
    (1181, 511991, 12527, 4514173, 1844831603),
    (1847, 752681, 10061, 3083083, 1107775621),
    (1279, 209371, 10597, 4558963, 1469510213),
    (1129, 255349, 12071, 3496249, 1937407343),
    (1361, 275201, 19751, 2685509, 1380662681),
    (1979, 258697, 10781, 971171, 1622753981),
    (1993, 279269, 12301, 3882007, 1789168301),
    (1511, 756191, 13121, 2518079, 1145597183),
    (1907, 476849, 14947, 739759, 1025324617),
    (1187, 783269, 18679, 1988659, 1565677307),
    (1693, 522127, 16127, 643619, 1325160337),
    (1973, 282377, 10657, 4793471, 1182127627),
    (1061, 206519, 19687, 2211817, 1290819697),
    (1487, 483139, 13399, 3299017, 1620163709),
    (1733, 399353, 18131, 3896413, 1782206801),
    (1823, 539837, 17509, 2924893, 1579678643),
    (1279, 662281, 11117, 3868649, 1460807707),
    (1307, 556573, 18701, 4909981, 1020040097),
    (1399, 783227, 11617, 1585261, 1683233333),
    (1511, 489329, 16417, 977507, 1076020889),
    (1861, 535663, 15527, 3241877, 1316557103),
    (1493, 576151, 15493, 1359857, 1995392447),
    (1741, 690583, 15451, 4715257, 1544226283),
    (1549, 597643, 15391, 2167259, 1505011861),
    (1049, 230189, 16741, 4954721, 1113820843),
    (1721, 492799, 13757, 4611911, 1099761457),
    (1709, 731681, 10831, 1725359, 1372268377),
    (1319, 221773, 15013, 587341, 1923567649),
    (1823, 755861, 17891, 3275137, 1842351779),
    (1087, 624467, 10499, 4473671, 1755489077),
    (1259, 410587, 10723, 827941, 1921405481),
    (1987, 215921, 16217, 1807891, 1523633599),
    (1879, 409379, 13043, 2535179, 1004391049),
    (1033, 629711, 13469, 3431009, 1323242267),
    (1597, 636739, 12517, 541339, 1590396649),
    (1091, 770207, 10987, 3032399, 1573672669),
    (1931, 729037, 10007, 2094601, 1848962197),
    (1567, 722983, 12377, 2413097, 1767850663),
    (1663, 511991, 14173, 1883237, 1388814047),
    (1069, 351359, 10937, 2999231, 1701603977),
    (1361, 376171, 17359, 3707813, 1047300581),
    (1319, 248509, 17729, 1920613, 1897665247),
    (1453, 423649, 11971, 3806837, 1481850761),
    (1481, 441307, 14591, 1897277, 1468411541),
    (1061, 341027, 16319, 4835749, 1510244311),
    (1259, 629059, 13613, 3925729, 1965058607),
    (1409, 602593, 18287, 878651, 1292754097),
    (1289, 488729, 16889, 2448613, 1527555457),
    (1597, 555419, 18119, 2529953, 1988884747),
    (1973, 205267, 15199, 684599, 1125570263),
    (1949, 665069, 16229, 4194847, 1152730927),
    (1657, 299857, 16979, 1749569, 1194919007),
    (1361, 740303, 13577, 3384569, 1406711543),
    (1277, 431521, 11527, 2202329, 1269718669),
    (1783, 767093, 11897, 4346663, 1887886951),
    (1091, 534739, 13859, 3246707, 1197746239),
    (1747, 218549, 18787, 4891483, 1205730121),
    (1201, 693697, 13249, 4457093, 1947146767),
    (1847, 703819, 14627, 2416369, 1977370267),
    (1019, 423103, 16691, 811861, 1843870841),
    (1559, 301703, 13877, 2401253, 1568621947),
    (1117, 585691, 14387, 635729, 1326978659),
    (1151, 374837, 16823, 4959883, 1427600791),
    (1181, 428863, 18587, 1630987, 1781740633),
    (1997, 766439, 12781, 826663, 1219929743),
    (1213, 486971, 17929, 2152307, 1617558353),
    (1657, 513101, 12527, 2052311, 1951551751),
    (1277, 657061, 12583, 4566997, 1855642709),
    (1831, 585283, 10243, 549169, 1251554477),
    (1973, 381103, 17231, 3439193, 1736060657),
    (1193, 541349, 18583, 1150117, 1746547661),
    (1607, 635483, 12721, 1968679, 1468711331),
    (1087, 792443, 19949, 2665717, 1775479577),
    (1709, 238529, 10091, 3150529, 1260068023),
    (1319, 639299, 16901, 3629719, 1706624761),
    (1873, 629567, 11261, 1476551, 1447471297),
    (1979, 608459, 18637, 4424353, 1941211913),
    (1237, 631753, 11329, 3096277, 1995626081),
    (1531, 588011, 19597, 4746457, 1208747233),
    (1201, 219977, 10937, 2862599, 1111733503)
  );

function IntegerNoiseCore(const X, Y, Z: LongInt; const Seed: Cardinal): UInt32;
type
  TUInt32Array = array [0..High(Integer) div SizeOf(UInt32) - 1] of UInt32;
  PUInt32Array = ^TUInt32Array;
var
  N: LongWord;
  PPrimes: PUInt32Array;
{$I norqcheckbegin.inc}
begin
  { Choose our primes row from Primes table. }
  PPrimes := @Primes[Seed mod (High(Primes)+1)];

  N := UInt32(x) + UInt32(y) * PPrimes^[0] + UInt32(z) * PPrimes^[1];
  N := N xor (N shl 13);
  Result := (n * (n * n * PPrimes^[2] + PPrimes^[3]) + PPrimes^[4]);
end;
{$I norqcheckend.inc}

function IntegerNoise(const X, Y, Z: LongInt; const Seed: Cardinal): Single; overload;
begin
  Result := IntegerNoiseCore(X, Y, Z, Seed) / High(UInt32);
end;

function IntegerNoise(const X, Y: LongInt; const Seed: Cardinal): Single; overload;
begin
  Result := IntegerNoiseCore(X, Y, 0, Seed) / High(UInt32);
end;

{ Interpolated noise for 2D coords ------------------------------------------- }

function InterpolatedNoise2D_None(const X, Y: Single; const Seed: Cardinal): Single;
begin
  Result := IntegerNoise(Round(X), Round(Y), Seed);
end;

function InterpolatedNoise2D_Linear(const X, Y: Single; const Seed: Cardinal): Single;
{$I castleinternalnoise_interpolatednoise2d_linear_cosine.inc}

function InterpolatedNoise2D_Cosine(const X, Y: Single; const Seed: Cardinal): Single;
{$define InterpolatedNoise2D_Cosine}
{$I castleinternalnoise_interpolatednoise2d_linear_cosine.inc}
{$undef InterpolatedNoise2D_Cosine}

function InterpolatedNoise2D_Spline(const X, Y: Single; const Seed: Cardinal): Single;
{$I castleinternalnoise_interpolatednoise2d_spline.inc}

{ BlurredInterpolatedNoise* -------------------------------------------------- }

function BlurredIntegerNoise(const X, Y: LongInt; const Seed: Cardinal): Single;
begin
  Result :=
      IntegerNoise(X    , Y    , Seed) / 4 +
    ( IntegerNoise(X - 1, Y    , Seed)  +
      IntegerNoise(X + 1, Y    , Seed)  +
      IntegerNoise(X    , Y - 1, Seed)  +
      IntegerNoise(X    , Y + 1, Seed)  ) / 8 +
    ( IntegerNoise(X - 1, Y - 1, Seed)  +
      IntegerNoise(X - 1, Y + 1, Seed)  +
      IntegerNoise(X + 1, Y - 1, Seed)  +
      IntegerNoise(X + 1, Y + 1, Seed)  ) / 16;
end;

function BlurredInterpolatedNoise2D_None(const X, Y: Single; const Seed: Cardinal): Single;
begin
  Result := BlurredIntegerNoise(Round(X), Round(Y), Seed);
end;

{$define IntegerNoise := BlurredIntegerNoise}

function BlurredInterpolatedNoise2D_Linear(const X, Y: Single; const Seed: Cardinal): Single;
{$I castleinternalnoise_interpolatednoise2d_linear_cosine.inc}

function BlurredInterpolatedNoise2D_Cosine(const X, Y: Single; const Seed: Cardinal): Single;
{$define InterpolatedNoise2D_Cosine}
{$I castleinternalnoise_interpolatednoise2d_linear_cosine.inc}
{$undef InterpolatedNoise2D_Cosine}

function BlurredInterpolatedNoise2D_Spline(const X, Y: Single; const Seed: Cardinal): Single;
{$I castleinternalnoise_interpolatednoise2d_spline.inc}

{$undef IntegerNoise}

end.
