{
  Copyright 2003,2004 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Ray ---------------------------------------------------------------------- }

{$define TrySimplePlaneRayIntersection_IMPLEMENT:=
var MaybeT:TYPE_SCALAR;
begin
 result:=not IsLineParallelToSimplePlane(RayVector, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-Ray0[PlaneConstCoord]) / RayVector[PlaneConstCoord];
  result:=MaybeT>=0;
  if result then 
  begin
   {$ifdef HAS_T}T:=MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection:=VectorAdd(Ray0, VectorScale(RayVector, MaybeT));{$endif}
  end; 
 end;
end;}

function TrySimplePlaneRayIntersection(var Intersection:TYPE_VECTOR3;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Ray0, RayVector:TYPE_VECTOR3):boolean;
{$define HAS_INTR}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneRayIntersection(var Intersection:TYPE_VECTOR3; var T:TYPE_SCALAR;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Ray0, RayVector:TYPE_VECTOR3):boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneRayIntersection(var T:TYPE_SCALAR;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Ray0, RayVector:TYPE_VECTOR3):boolean;
{$define HAS_T}
TrySimplePlaneRayIntersection_IMPLEMENT  
{$undef HAS_T}

{$undef TrySimplePlaneRayIntersection_IMPLEMENT}

{ OdcinekDir ------------------------------------------------------------ } 

{$define TrySimplePlaneOdcinekDirIntersection_IMPLEMENT:=
var MaybeT:TYPE_SCALAR;
begin
 result:=not IsLineParallelToSimplePlane(OdcVector, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-Odc0[PlaneConstCoord]) / OdcVector[PlaneConstCoord];
  result:=(MaybeT>=0) and (MaybeT<=1);
  if result then 
  begin
   {$ifdef HAS_T}T:=MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection:=VectorAdd(Odc0, VectorScale(OdcVector, MaybeT));{$endif}
  end; 
 end;
end;}

function TrySimplePlaneOdcinekDirIntersection(var Intersection:TYPE_VECTOR3;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Odc0, OdcVector:TYPE_VECTOR3):boolean;
{$define HAS_INTR}
TrySimplePlaneOdcinekDirIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneOdcinekDirIntersection(var Intersection:TYPE_VECTOR3; var T:TYPE_SCALAR;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Odc0, OdcVector:TYPE_VECTOR3):boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneOdcinekDirIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneOdcinekDirIntersection(var T:TYPE_SCALAR;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Odc0, OdcVector:TYPE_VECTOR3):boolean;
{$define HAS_T}
TrySimplePlaneOdcinekDirIntersection_IMPLEMENT  
{$undef HAS_T}  

{$undef TrySimplePlaneOdcinekDirIntersection_IMPLEMENT}

{ Odcinek ----------------------------------------------------------------- }

{$define TrySimplePlaneOdcinekIntersection_IMPLEMENT:=
var MaybeT:TYPE_SCALAR;
    Odc0:TYPE_VECTOR3 absolute Pos1;
    OdcVector:TYPE_VECTOR3;
begin
 OdcVector:=VectorSubtract(Pos2, Pos1);
 
 result:=not IsLineParallelToSimplePlane(OdcVector, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-Odc0[PlaneConstCoord]) / OdcVector[PlaneConstCoord];
  result:=(MaybeT>=0) and (MaybeT<=1);
  if result then 
  begin
   {$ifdef HAS_T}T:=MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection:=VectorAdd(Odc0, VectorScale(OdcVector, MaybeT));{$endif}
  end; 
 end;
end;}

function TrySimplePlaneOdcinekIntersection(var Intersection:TYPE_VECTOR3;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Pos1, Pos2:TYPE_VECTOR3):boolean;
{$define HAS_INTR}
TrySimplePlaneOdcinekIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneOdcinekIntersection(var Intersection:TYPE_VECTOR3; var T:TYPE_SCALAR;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Pos1, Pos2:TYPE_VECTOR3):boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneOdcinekIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneOdcinekIntersection(var T:TYPE_SCALAR;
  const PlaneConstCoord:integer; const PlaneConstValue:TYPE_SCALAR;
  const Pos1, Pos2:TYPE_VECTOR3):boolean;
{$define HAS_T}
TrySimplePlaneOdcinekIntersection_IMPLEMENT  
{$undef HAS_T}  

{$undef TrySimplePlaneOdcinekIntersection_IMPLEMENT}

{ eof ------------------------------------------------------------ }