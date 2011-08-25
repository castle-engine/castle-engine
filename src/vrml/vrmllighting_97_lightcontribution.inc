{ zapisana w jednym pliku implementacja dwoch funkcji ktore w skomplikowany
  sposob wspoldziela swoj kod : VRML97LightContribution i
  VRML97LightContribution_CameraIndependent. Symbol CAMERA_INDEP
  oznacza ze mamy zrobic druga funkcje. }

var Spot: Single;
    Attenuat: Single;

    LightNode: TAbstractLightNode;
    PosLightNode: TAbstractPositionalLightNode absolute LightNode;

    Factors: TVector3Single;
    { kierunek od Intersection do zrodla swiatla, normalized }
    LightDirNorm: TVector3Single;
    { wektor normalny do powierzchni (skierowany w strone Light) }
    Normal: TVector3Single;

    {$ifndef CAMERA_INDEP}
    M1: TMaterialNode_1;
    M2: TMaterialNode;

    { srednia z wektora do cameraa i LightDirNorm, potrzebna do liczenia
      odbicia Specular }
    CameraAndLightHalfway: TVector3Single;

    MaterialAmbientIntensity: Single;
    MaterialAmbientColor: TVector3Single;
    MaterialDiffuseColor: TVector3Single;
    MaterialSpecularColor: TVector3Single;
    MaterialShininessExp: Single;

    {$define Point := Intersection}
    {$define PointPlaneNormal := IntersectNode.World.Plane}
    {$endif}

  function CalculateSpot_1(SpotLight: TSpotLightNode_1): Single;
  var
    CosSpotAngle, SpotAngle: Single;
  begin
    { Uzywamy dropOffRate VRMLa 1.0 zeby sprawic by spot byl
      bardziej "skupiony" zgodnie z rownaniami swiatla OpenGLa.
      W VRMLu 97 skupienie spota jest okreslane inaczej, przez beamWidth. }
    CosSpotAngle := (-LightDirNorm) ** Light.Direction;
    { TODO: optimize: no need to calc ArcCos here if we would precalc
      CosCutOffAngle for every light. }
    SpotAngle := ArcCos(CosSpotAngle);
    if SpotAngle <= SpotLight.FdCutOffAngle.Value then
      Result := Power(CosSpotAngle, SpotLight.SpotExp) else
      Result := 0;
  end;

  function CalculateSpot(SpotLight: TSpotLightNode): Single;
  var
    CosSpotAngle, SpotAngle, SpotCO, SpotBW: Single;
  begin
    CosSpotAngle := (-LightDirNorm) ** Light.Direction;
    SpotAngle := ArcCos(CosSpotAngle);
    SpotCO := SpotLight.FdCutOffAngle.Value;
    SpotBW := SpotLight.FdBeamWidth.Value;

    if SpotAngle < SpotCO then
    begin
      if SpotAngle > SpotBW then
        Result := (SpotAngle - SpotCO) / (SpotBW - SpotCO) else
        Result := 1;
    end else
      Result := 0;
  end;

begin
 { wyciagnij ze struktur kilka zmiennych ktorych bedziemy intensywnie uzywac }
 {$ifndef CAMERA_INDEP}
 M1 := nil;
 M2 := nil;
 if IntersectNode.State.ShapeNode <> nil then
 begin
   M2 := IntersectNode.State.ShapeNode.Material;
   if M2 <> nil then
   begin
     MaterialAmbientIntensity := M2.FdAmbientIntensity.Value;
     MaterialDiffuseColor := M2.FdDiffuseColor.Value;
     MaterialSpecularColor := M2.FdSpecularColor.Value;
     MaterialShininessExp := M2.ShininessExp;
   end else
   begin
     { Default VRML 2.0 lighting properties. }
     MaterialAmbientIntensity := DefaultMaterialAmbientIntensity;
     MaterialDiffuseColor := DefaultMaterialDiffuseColor;
     MaterialSpecularColor := DefaultMaterialSpecularColor;
     MaterialShininessExp := DefaultMaterialShininess * 128.0;
   end;
 end else
 begin
   M1 := IntersectNode.State.LastNodes.Material;
   MaterialAmbientColor := M1.AmbientColor3Single(0);
   MaterialDiffuseColor := M1.DiffuseColor3Single(0);
   MaterialSpecularColor := M1.SpecularColor3Single(0);
   MaterialShininessExp := M1.ShininessExp(0);
 end;
 {$endif}
 LightNode := TAbstractLightNode(Light.Node);

 if (not LightNode.FdOn.Value) then
 begin
   Result := ZeroVector3Single;
   Exit;
 end;

 { TODO: arg dla attenuation powinien byc w ukladzie transformacji swiatla.
   For now lights with non-trivial attenuation put under some transformation
   that does distance scaling will not render as required by VRML 2.0 spec.
   Czyli musze implementowac InvertTransformation ? }
 if LightNode is TAbstractPositionalLightNode then
 begin
  if PosLightNode.DistanceNeededForAttenuation then
   Attenuat := PosLightNode.Attenuation( PointsDistance(Point, Light.Location) ) else
   Attenuat := PosLightNode.Attenuation( Single(0.0) );
 end else
  Attenuat := 1;

 { LightDirNorm is called "L" in the equation in VRML spec }
 if LightNode is TAbstractPositionalLightNode then
  LightDirNorm := Normalized(Light.Location - Point) else
  LightDirNorm := -Light.Direction;
 { wektory Normal i LightDirNorm musza wychodzic z tej samej strony
   powierzchni TriNormPlaneSingle (musimy przeciez zrobic cos co w OpenGLu
   jest okreslane jako two-sided lighting, czyli wziac albo Normal albo
   -Normal, w zaleznosci od pozycji swiatla.). }

 { Normal is called "N" in the equation in VRML spec }
 Normal := PlaneDirInDirection(PointPlaneNormal, LightDirNorm);

 {$ifndef CAMERA_INDEP}
 { niestety musimy znormalizowac (CamPosition - Point)
   (zeby mial taki sam wklad w sume co LightDirNorm) i wynik
   (ktory musi byc znormalizowany zeby nadawal sie do wyliczania
   cosinusa jako prostego dot product; jak zwykle, nalezy pamietac
   ze suma dwoch wektorow dlugosci 1 niekoniecznie (rzadko) jest wektorem
   dlugosci 2 (wiec skalowanie przez 1/2 nie rozwiazaloby tu problemu)) }
 CameraAndLightHalfway := Normalized(
   Normalized(CamPosition - Point) + LightDirNorm);
 {$endif}

 { oblicz Spot - dla SpotLight okresla on jak bardzo punkt jest w spocie,
   dla innych swiatel jest zawsze = 1. }
 if LightNode is TSpotLightNode_1 then
   Spot := CalculateSpot_1(TSpotLightNode_1(LightNode)) else
 if LightNode is TSpotLightNode then
   Spot := CalculateSpot(TSpotLightNode(LightNode)) else
   Spot := 1;

 Result := LightNode.FdColor.Value * (Attenuat * Spot);

 { Now we calculate "ambient_i" part of equation.

   Note special things for VRML 1.0:
   - when light.ambientIntensity < 0 then we assume we have to
     be compatible to our initial VRML 1.0 behavior and
     ambient factor is just zero.
   - when light.ambientIntensity >= 0 but this is still VRML 1.0 material
     then instead of material.ambientIntensity we take
     material.ambientColor. Default material.ambientIntensity
     in VRML 2.0 is 0.2 and default material.ambientColor in VRML 1.0
     is (0.2, 0.2, 0.2), so results should match for VRML 1.0 and 2.0
     for default situations. }
 {$ifdef CAMERA_INDEP}
 Factors := ZeroVector3Single;
 {$else}
 if LightNode.FdAmbientIntensity.Value < 0 then
   Factors := ZeroVector3Single else
 if M1 <> nil then
   Factors := (MaterialDiffuseColor * MaterialAmbientColor) *
     LightNode.FdAmbientIntensity.Value else
   Factors := MaterialDiffuseColor *
     (LightNode.FdAmbientIntensity.Value * MaterialAmbientIntensity);
 {$endif}

 { Factors += diffuse factor }
 Factors += MaterialDiffuseColor *
   (LightNode.FdIntensity.Value * (Normal ** LightDirNorm));

 {$ifndef CAMERA_INDEP}
 { Factors += specular factor }
 Factors += MaterialSpecularColor *
   (LightNode.FdIntensity.Value *
     Power( Max(Normal ** CameraAndLightHalfway, Single(0.0)),
       MaterialShininessExp));
 {$endif}

 Result *= Factors;
end;

{$ifndef CAMERA_INDEP}
  {$undef Point}
  {$undef PointPlaneNormal}
  {$undef MaterialDiffuseColor}
{$endif}
