{ zapisana w jednym pliku implementacja dwoch funkcji ktore w skomplikowany
  sposob wspoldziela swoj kod : VRML97LightContribution i
  VRML97LightContribution_CameraIndependent. Symbol CAMERA_INDEP
  oznacza ze mamy zrobic druga funkcje. }

var Spot, CosSpotAngle: Single;
    Attenuat: Single;

    LightNode: TNodeGeneralLight;
    PosLightNode: TNodeGeneralPositionalLight absolute LightNode;

    Factors: TVector3Single;
    { kierunek od Intersection do zrodla swiatla, normalized }
    LightDirNorm: TVector3Single;
    { wektor normalny do powierzchni (skierowany w strone Light) }
    Normal: TVector3Single;

    {$ifndef CAMERA_INDEP}
    M1: TNodeMaterial_1;
    M2: TNodeMaterial_2;

    { srednia z wektora do viewera i LightDirNorm, potrzebna do liczenia
      odbicia Specular }
    ViewerAndLightHalfway: TVector3Single;

    MaterialDiffuseColor: TVector3Single;
    MaterialSpecularColor: TVector3Single;
    MaterialShininessExp: Single;

    {$define Point := Intersection}
    {$define PointPlaneNormal := IntersectNode.TriangleNormPlane}
    {$endif}

begin
 { wyciagnij ze struktur kilka zmiennych ktorych bedziemy intensywnie uzywac }
 {$ifndef CAMERA_INDEP}
 if IntersectNode.State.ParentShape <> nil then
 begin
   M2 := IntersectNode.State.ParentShape.Material;
   if M2 <> nil then
   begin
     MaterialDiffuseColor := M2.FdDiffuseColor.Value;
     MaterialSpecularColor := M2.FdSpecularColor.Value;
     MaterialShininessExp := M2.ShininessExp;
   end else
   begin
     { Default VRML 2.0 lighting properties. }
     MaterialDiffuseColor := Vector3Single(0.8, 0.8, 0.8);
     MaterialSpecularColor := ZeroVector3Single;
     MaterialShininessExp := 0.2 * 128.0;
   end;
 end else
 begin
   M1 := IntersectNode.State.LastNodes.Material;
   MaterialDiffuseColor := M1.DiffuseColor3Single(IntersectNode.MatNum);
   MaterialSpecularColor := M1.SpecularColor3Single(IntersectNode.MatNum);
   MaterialShininessExp := M1.ShininessExp(IntersectNode.MatNum);
 end;
 {$endif}
 LightNode := TNodeGeneralLight(Light.LightNode);

 if (not LightNode.FdOn.Value) then Exit(ZeroVector3Single);

 { TODO: arg dla attenuation powinien byc w ukladzie transformacji swiatla.
   aaargh - czy to znaczy ze musze implementowac InvertTransformation ? }
 if LightNode is TNodeGeneralPositionalLight then
 begin
  if PosLightNode.DistanceNeededForAttenuation then
   Attenuat := PosLightNode.Attenuation( PointsDistance(Point, Light.TransfLocation) ) else
   Attenuat := PosLightNode.Attenuation( Single(0.0) );
 end else
  Attenuat := 1;

 if LightNode is TNodeGeneralPositionalLight then
  LightDirNorm := Normalized(VectorSubtract(Light.TransfLocation, Point)) else
  LightDirNorm := VectorNegate(Light.TransfNormDirection);
 { wektory Normal i LightDirNorm musza wychodzic z tej samej strony
   powierzchni TriNormPlaneSingle (musimy przeciez zrobic cos co w OpenGLu
   jest okreslane jako two-sided lighting, czyli wziac albo Normal albo
   -Normal, w zaleznosci od pozycji swiatla.). }
 Normal := PlaneDirInDirection(PointPlaneNormal, LightDirNorm);

 {$ifndef CAMERA_INDEP}
 { niestety musimy znormalizowac VectorSubtract(CamPosition, Point)
   (zeby mial taki sam wklad w sume co LightDirNorm) i wynik
   (ktory musi byc znormalizowany zeby nadawal sie do wyliczania
   cosinusa jako prostego VectorDotProduct; jak zwykle, nalezy pamietac
   ze suma dwoch wektorow dlugosci 1 niekoniecznie (rzadko) jest wektorem
   dlugosci 2 (wiec skalowanie przez 1/2 nie rozwiazaloby tu problemu)) }
 ViewerAndLightHalfway := Normalized( VectorAdd(
   Normalized(VectorSubtract(CamPosition, Point)),
   LightDirNorm));
 {$endif}

 { oblicz Spot - dla SpotLight okresla on jak bardzo punkt jest w spocie,
   dla innych swiatel jest zawsze = 1. Notka - uzywamy dropOffRate VRMLa 1.0
   zeby sprawic by spot byl bardziej "skupiony" zgodnie z rownaniami swiatla
   OpenGLa. W VRMLu 97 skupienie spota jest okreslane inaczej, przez beamWidth. }
 if LightNode is TNodeSpotLight_1 then
 begin
  CosSpotAngle := VectorDotProduct( VectorNegate(LightDirNorm),
    Light.TransfNormDirection);
  if ArcCos(CosSpotAngle) <= TNodeSpotLight_1(LightNode).FdCutOffAngle.Value then
   Spot := Power(CosSpotAngle, TNodeSpotLight_1(LightNode).SpotExp) else
   Spot := 0;
 end else
 if LightNode is TNodeSpotLight_2 then
 begin
   { TODO }
 end else
  Spot := 1;

 result := VectorScale(LightNode.FdColor.Value, Attenuat * Spot);

 { AmbientFactor = (0, 0,0). W VRML 97 dochodzi ambientIntensity
   (domyslnie rowne 0) i AmbientFactor = LightNode.ambientIntensity *
   IntersectNode.State.LastNodes.Material.MaterialColor3Single.
   To jest sensowne, przeciez nie chcemy zeby ambientIntensity mialo
   zbyt duzy wplyw na kolor (bo kolor ambient to jest cos juz
   ZUPELNIE nierealistycznego w takim prostym modelu oswietlenia),
   chcemy raczej zeby dominowaly komponenty diffuse i specular. }
 { Factors := ambient factor, TODO }
 Factors := ZeroVector3Single;

 { Factors += diffuse factor }
 VectorAddTo1st(Factors, VectorScale(
   MaterialDiffuseColor,
   LightNode.FdIntensity.Value * VectorDotProduct( Normal, LightDirNorm)));

 {$ifndef CAMERA_INDEP}
 { Factors += specular factor }
 VectorAddTo1st(Factors, VectorScale(
   MaterialSpecularColor,
   LightNode.FdIntensity.Value *
     Power( max(VectorDotProduct(Normal, ViewerAndLightHalfway), Single(0.0)),
       MaterialShininessExp)));
 {$endif}

 VectorMultEachPosTo1st(result, Factors);
end;

{$ifndef CAMERA_INDEP}
  {$undef Point}
  {$undef PointPlaneNormal}
  {$undef MaterialDiffuseColor}
{$endif}
