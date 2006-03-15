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
    MaterialNode: TNodeMaterial; 
    
    { srednia z wektora do viewera i LightDirNorm, potrzebna do liczenia 
      odbicia Specular }
    ViewerAndLightHalfway: TVector3Single;
    
    {$define Point := Intersection}
    {$define PointPlaneNormal := IntersectNode.TriangleNormPlane}
    {$define MaterialDiffuseColor := MaterialNode.DiffuseColor3Single
      (IntersectNode.MatNum)}
    {$endif}
    
begin 
 { wyciagnij ze struktur kilka zmiennych ktorych bedziemy intensywnie uzywac } 
 {$ifndef CAMERA_INDEP} 
 MaterialNode := IntersectNode.State.LastNodes.Material; 
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
 if LightNode is TNodeSpotLight then
 begin
  CosSpotAngle := VectorDotProduct( VectorNegate(LightDirNorm),
    Light.TransfNormDirection);
  if ArcCos(CosSpotAngle) <= TNodeSpotLight(LightNode).FdCutOffAngle.Value then
   Spot := Power(CosSpotAngle, TNodeSpotLight(LightNode).SpotExp) else
   Spot := 0;
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
   MaterialNode.SpecularColor3Single(IntersectNode.MatNum),
   LightNode.FdIntensity.Value *
     Power( max(VectorDotProduct(Normal, ViewerAndLightHalfway), Single(0.0)),
       MaterialNode.ShininessExp(IntersectNode.MatNum))));
 {$endif} 

 VectorMultEachPosTo1st(result, Factors);
end;

{$ifndef CAMERA_INDEP}   
  {$undef Point}
  {$undef PointPlaneNormal}
  {$undef MaterialDiffuseColor}
{$endif}
