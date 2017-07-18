{ Implement
  TLightInstance.Contribution and
  TLightInstance.ContributionCameraIndependent.

  Note: In some cases we have to do something different than VRML 2.0/X3D spec:

  @unorderedList(
    @item(
      For VRML 1.0 SpotLight, we have to calculate spot light differently
      (because VRML 1.0 SpotLight gives me dropOffRate instead of
      beamWidth), so we use spot factor equation following OpenGL equations.)

    @item(
      For VRML 1.0, we have to calculate ambientFactor in a little different way:
      see [http://castle-engine.sourceforge.net/x3d_extensions.php#ext_light_attenuation],
      when light's ambientIntensity is < 0 then we just return 0 and
      otherwise we use material's ambientColor.)

    @item(
      VRML 97 lighting equations suggest one-sided lighting, only where
      the normal points out. In my opinion, one-sided lighting is not useful,
      and also our OpenGL rendering uses two-sides lighting.
      (One reason for OpenGL rendering is to integrate nicely with flat mirrors,
      where you have to flip normals. So OpenGL renderer always gives
      vectors from CCW, and so uses two-side to not favor any side of the face.))
  )
}

var
  Spot: Single;
  Attenuat: Single;

  Factors: TVector3;
  { kierunek od Point do zrodla swiatla, normalized }
  LightDirNorm: TVector3;
  { wektor normalny do powierzchni (skierowany w strone Light) }
  Normal: TVector3;
  PosLightNode: TAbstractPositionalLightNode;

  {$ifndef CAMERA_INDEP}
  M: TMaterialInfo;

  { srednia z wektora do camera i LightDirNorm, potrzebna do liczenia
    odbicia Specular }
  CameraAndLightHalfway: TVector3;

  MaterialAmbientColor: TVector3;
  MaterialDiffuseColor: TVector3;
  MaterialSpecularColor: TVector3;
  MaterialShininessExp: Single;
  {$endif}

  function CalculateSpot_1(SpotLight: TSpotLightNode_1): Single;
  var
    CosSpotAngle, SpotAngle: Single;
  begin
    { Uzywamy dropOffRate VRMLa 1.0 zeby sprawic by spot byl
      bardziej "skupiony" zgodnie z rownaniami swiatla OpenGLa.
      W VRMLu 97 skupienie spota jest okreslane inaczej, przez beamWidth. }
    CosSpotAngle := TVector3.DotProduct(-LightDirNorm, Direction);
    { TODO: optimize: no need to calc ArcCos here if we would precalc
      CosCutOffAngle for every light. }
    SpotAngle := ArcCos(CosSpotAngle);
    if SpotAngle <= SpotLight.FdCutOffAngle.Value then
      Result := Power(CosSpotAngle, SpotLight.SpotExponent) else
      Result := 0;
  end;

  function CalculateSpot(SpotLight: TSpotLightNode): Single;
  var
    CosSpotAngle, SpotAngle, SpotCO, SpotBW: Single;
  begin
    CosSpotAngle := TVector3.DotProduct(-LightDirNorm, Direction);
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
  M := State.MaterialInfo;
  if M <> nil then
  begin
    MaterialAmbientColor := M.AmbientColor;
    MaterialDiffuseColor := M.DiffuseColor;
    MaterialSpecularColor := M.SpecularColor;
    MaterialShininessExp := M.ShininessExp;
  end else
  begin
    MaterialAmbientColor := TMaterialInfo.DefaultAmbientColor;
    MaterialDiffuseColor := TMaterialInfo.DefaultDiffuseColor;
    MaterialSpecularColor := TMaterialInfo.DefaultSpecularColor;
    MaterialShininessExp := TMaterialInfo.DefaultShininessExp;
  end;
  {$endif}

  if (not Node.FdOn.Value) then
  begin
    Result := TVector3.Zero;
    Exit;
  end;

  { TODO: arg dla attenuation powinien byc w ukladzie transformacji swiatla.
    For now lights with non-trivial attenuation put under some transformation
    that does distance scaling will not render as required by VRML 2.0 spec.
    Czyli musze implementowac InvertTransformation ? }
  if Node is TAbstractPositionalLightNode then
  begin
   PosLightNode := TAbstractPositionalLightNode(Node);
   if PosLightNode.DistanceNeededForAttenuation then
    Attenuat := PosLightNode.CalculateAttenuation( PointsDistance(Point, Location) ) else
    Attenuat := PosLightNode.CalculateAttenuation( Single(0.0) );
  end else
   Attenuat := 1;

  { LightDirNorm is called "L" in the equation in VRML spec }
  if Node is TAbstractPositionalLightNode then
   LightDirNorm := Normalized(Location - Point) else
   LightDirNorm := -Direction;
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
  if Node is TSpotLightNode_1 then
    Spot := CalculateSpot_1(TSpotLightNode_1(Node)) else
  if Node is TSpotLightNode then
    Spot := CalculateSpot(TSpotLightNode(Node)) else
    Spot := 1;

  Result := Node.FdColor.Value * (Attenuat * Spot);

  { Factors := ambient factor }
  {$ifdef CAMERA_INDEP}
  Factors := TVector3.Zero;
  {$else}
  if Node.FdAmbientIntensity.Value < 0 then
    Factors := TVector3.Zero
  else
    Factors := MaterialAmbientColor * Node.FdAmbientIntensity.Value;
  {$endif}

  { Factors += diffuse factor }
  Factors += TVector3.MultiplyComponents(MaterialDiffuseColor, DiffuseTextureColor) *
    (Node.FdIntensity.Value * TVector3.DotProduct(Normal, LightDirNorm));

  {$ifndef CAMERA_INDEP}
  { Factors += specular factor }
  Factors += MaterialSpecularColor *
    (Node.FdIntensity.Value *
      Power( Max(TVector3.DotProduct(Normal, CameraAndLightHalfway), Single(0.0)),
        MaterialShininessExp));
  {$endif}

  Result := TVector3.MultiplyComponents(Result, Factors);
end;
