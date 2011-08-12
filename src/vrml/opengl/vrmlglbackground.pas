{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering backgrounds, sky and such (TVRMLGLBackground). }
unit VRMLGLBackground;

{$I kambiconf.inc}

interface

uses VectorMath, SysUtils, GL, GLExt, KambiGLUtils, KambiUtils, Images,
  VRMLNodes;

type
  { Background rendering sky, ground and such around the camera.
    Background defined here has the same features as VRML/X3D Background:

    @unorderedList(
      @itemSpacing Compact
      @item a cube with each face textured (textures may have alpha channel)
      @item a ground sphere around this, with color rings for ground colors
      @item a sky sphere around this, with color rings for sky colors
    )

    See [http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/enveffects.html#Background]
    for the detailed meaning of constructor parameters.

    Conceptually, the background is infinitely far from the camera,
    regardless of the camera position. So actually, we just ignore camera
    position, and render like the camera was always in the middle
    of the background box/sphere. But still we take into acccount camera
    rotations. This makes convincing sky look. }
  TVRMLGLBackground = class
  private
    szescianNieba_list: TGLuint;
    nieboTex: packed array [TBackgroundSide] of TGLuint;
  public
    Transform: TMatrix4Single;

    { Render background around.

      Current modelview matrix should contain only the camera rotation.
      Uses one OpenGL attrib stack place.
      Automatically creates and uses a display list.
      Assumes that the user is standing in the middle of background,
      so we can use backface culling.

      We render without GL_DEPTH_TEST to cover everyhing on the screen
      (so rendering a background should be a first thing you render,
      no point in even doing glClear yourself).
      When possible (we have only one sky color), we even use
      glClear(GL_COLOR_BUFFER) to set initial color. }
    procedure Render;

    { Calculate (or just confirm that Proposed value is still OK)
      the sky sphere radius that fits nicely in your projection near/far.

      Background spheres (for sky and ground) are rendered at given radius.
      And inside these spheres, we have a cube (to apply background textures).
      Both spheres and cube must fit nicely within your projection near/far
      to avoid any artifacts.

      We first check is Proposed a good result value (it satisfies
      the conditions, with some safety margin). If yes, then we return
      exactly the Proposed value. Otherwise, we calculate new value
      as an average in our range.
      This way, if you already had sky sphere radius calculated
      (and prepared some OpenGL resources for it),
      and projection near/far changes very slightly
      (e.g. because bounding box slightly changed), then you don't have
      to recreate background --- if the old sky sphere radius is still OK,
      then the old background resources are still OK.

      Just pass Proposed = 0 (or anything else that is always outside
      the range) if you don't need this feature. }
    class function NearFarToSkySphereRadius(const zNear, zFar: Single;
      const Proposed: Single = 0): Single;

    { Construct background. Prepares OpenGL resources for rendering.

      Parameters correspond to VRML/X3D Background node,
      see [http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/enveffects.html#Background].
      For example SkyColorCount > 0 and GroundColorCount > GroundAngleCount.

      Any of the TBackgroundImages passed here may be @nil,
      or of a class that can be rendered as OpenGL textures (TextureImageClasses). }
    constructor Create(
      GroundAngle: PArray_Single; GroundAngleCount: Integer;
      GroundColor: PArray_Vector3Single; GroundColorCount: Integer;
      const Imgs: TBackgroundImages;
      SkyAngle: PArray_Single; SkyAngleCount: Integer;
      SkyColor: PArray_Vector3Single; SkyColorCount: Integer;
      SkySphereRadius: Single);
    destructor Destroy; override;
  end;

implementation

uses KambiWarnings, GLImages;

const
  { Relation of a cube size and a radius of it's bounding sphere.

    Sphere surrounds the cube, such that 6 cube corners touch the sphere.
    So cube diameter = 2 * sphere radius.
    Cube diameter = sqrt(sqr(cube size) + sqr(cube face diameter)),
    and cube face diameter = sqrt(2) * cube size.
    This gives constants below. }
  SphereRadiusToCubeSize = 2/Sqrt(3);
  CubeSizeToSphereRadius = Sqrt(3)/2;

  BGAllSides: TBackgroundSides = [Low(TBackgroundSide) .. High(TBackgroundSide)];

{ TVRMLGLBackground ------------------------------------------------------------ }

procedure TVRMLGLBackground.Render;
begin
  glPushMatrix;
    glMultMatrix(Transform);
    glCallList(szescianNieba_list);
  glPopMatrix;
end;

class function TVRMLGLBackground.NearFarToSkySphereRadius(const zNear, zFar: Single;
  const Proposed: Single): Single;

{ Conditions are ZNear < CubeSize/2, ZFar > SphereRadius.
  So conditions for radius are

    ZNear * 2 * CubeSizeToSphereRadius < SphereRadius < ZFar

  Note that 2 * CubeSizeToSphereRadius is Sqrt(3) =~ 1.7,
  so it's possible to choose
  ZNear <= ZFar that still yield no possible radius.

  It would be possible to avoid whole need for this method
  by setting projection matrix in our own render. But then,
  you'd have to pass fovy and such parameters to the background renderer.
}

var
  Min, Max, SafeMin, SafeMax: Single;
begin
  Min := zNear * 2 * CubeSizeToSphereRadius;
  Max := zFar;

  { The new sphere radius should be in [Min...Max].
    For maximum safety (from floating point troubles), we require
    that it's within slightly smaller "safe" range. }

  SafeMin := Lerp(0.1, Min, Max);
  SafeMax := Lerp(0.9, Min, Max);

  if (Proposed >= SafeMin) and
     (Proposed <= SafeMax) then
    Result := Proposed else
    Result := (Min + Max) / 2;
end;

constructor TVRMLGLBackground.Create(
  GroundAngle: PArray_Single; GroundAngleCount: Integer;
  GroundColor: PArray_Vector3Single; GroundColorCount: Integer;
  const Imgs: TBackgroundImages;
  SkyAngle: PArray_Single; SkyAngleCount: Integer;
  SkyColor: PArray_Vector3Single; SkyColorCount: Integer;
  SkySphereRadius: Single);

var
  CubeSize, CubeSize2: Single;

  procedure RenderTextureSide(bs: TBackgroundSide);
  const
    { wspolrzedne tekstury beda zawsze nakladane na te coords w kolejnosci
      (0, 0), (1, 0), (1, 1), (0, 1). }
    Coords: array[TBackgroundSide, 0..3]of TVector3Integer =
    ( ((1, 0, 1), (0, 0, 1), (0, 1, 1), (1, 1, 1)), {back}
      ((0, 0, 1), (1, 0, 1), (1, 0, 0), (0, 0, 0)), {bottom}
      ((0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)), {front}
      ((0, 0, 1), (0, 0, 0), (0, 1, 0), (0, 1, 1)), {left}
      ((1, 0, 0), (1, 0, 1), (1, 1, 1), (1, 1, 0)), {right}
      ((0, 1, 0), (1, 1, 0), (1, 1, 1), (0, 1, 1))  {top}
    );
    TexCoords: array [0..3] of TVector2f = ((0, 0), (1, 0), (1, 1), (0, 1));
  var
    i: Integer;
  begin
    if nieboTex[bs] = 0 then Exit;

    { If nieboTex[bs] <> 0 to for sure Imgs[bs] <> nil,
      so I can safely do here checks "Imgs[bs] is ..." }

    if Imgs.Images[bs].HasAlpha then
    begin
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
    end;

    glBindTexture(GL_TEXTURE_2D, nieboTex[bs]);
    glBegin(GL_QUADS);
      for i := 0 to 3 do
      begin
        glTexCoordv(TexCoords[i]);
        glVertex3f( (Coords[bs, i, 0]*2-1)*CubeSize2,
                    (Coords[bs, i, 1]*2-1)*CubeSize2,
                    (Coords[bs, i, 2]*2-1)*CubeSize2);
      end;
    glEnd;

    if Imgs.Images[bs].HasAlpha then
      glDisable(GL_BLEND);
  end;

  { For given Angle (meaning: 0 = zenith, Pi = nadir), calculate the height
    and radius of given circle of sky sphere. }
  procedure StackCircleCalc(const Angle: Single; var Y, Radius: Single);
  begin
    Radius := sin(Angle) * SkySphereRadius;
    Y := cos(Angle) * SkySphereRadius;
  end;

  { Render*Stack: render one stack of sky/ground sphere.
    Angles are given in the sky connvention : 0 is zenith, Pi is nadir.
    Colors are nterpolated from upper to lower angle from upper to lower color.
    RenderUpper/LowerStack do not need upper/lower angle: it is implicitly
    understood to be the zenith/nadir.

    TODO: ustalic we wszystkich Render*Stack ze sciany do wewnatrz sa
    zawsze CCW (albo na odwrot) i uzyc backface culling ? Czy cos na
    tym zyskamy ?
  }

  const
    { slices of rings rendered in Render*Stack }
    Slices = 24;

  procedure RenderStack(
    const UpperColor: TVector3Single; const UpperAngle: Single;
    const LowerColor: TVector3Single; const LowerAngle: Single);
  var
    UpperY, UpperRadius, LowerY, LowerRadius: Single;

    procedure Bar(const SliceAngle: Single);
    var
      SinSliceAngle, CosSliceAngle: Single;
    begin
      SinSliceAngle := Sin(SliceAngle);
      CosSliceAngle := Cos(SliceAngle);
      glColorv(LowerColor);
      glVertex3f(SinSliceAngle*LowerRadius, LowerY, CosSliceAngle*LowerRadius);
      glColorv(UpperColor);
      glVertex3f(SinSliceAngle*UpperRadius, UpperY, CosSliceAngle*UpperRadius);
    end;

  var
    i: Integer;
  begin
    StackCircleCalc(UpperAngle, UpperY, UpperRadius);
    StackCircleCalc(LowerAngle, LowerY, LowerRadius);
    glBegin(GL_QUAD_STRIP);
      Bar(0);
      for i := 1 to Slices-1 do Bar(i* 2*Pi/Slices);
      Bar(0);
    glEnd;
  end;

  procedure RenderUpperStack(
    const UpperColor: TVector3Single;
    const LowerColor: TVector3Single; const LowerAngle: Single);
  { Easy but not optimal implementation of this is
    RenderStack(UpperColor, 0, LowerColor, LowerAngle); }
  var
    LowerY, LowerRadius: Single;

    procedure Pt(const SliceAngle: Single);
    begin
      glVertex3f(Sin(SliceAngle)*LowerRadius, LowerY, Cos(SliceAngle)*LowerRadius);
    end;

  var
    i: Integer;
  begin
    StackCircleCalc(LowerAngle, LowerY, LowerRadius);
    glBegin(GL_TRIANGLE_FAN);
      glColorv(UpperColor);
      glVertex3f(0, SkySphereRadius, 0);
      glColorv(LowerColor);
      Pt(0);
      for i := 1 to Slices-1 do Pt(i* 2*Pi/Slices);
      Pt(0);
    glEnd;
  end;

  procedure RenderLowerStack(
    const UpperColor: TVector3Single; const UpperAngle: Single;
    const LowerColor: TVector3Single);
  { Easy but not optimal implementation of this is
    RenderStack(UpperColor, UpperAngle, LowerColor, Pi); }
  var
    UpperY, UpperRadius: Single;

    procedure Pt(const SliceAngle: Single);
    begin
      glVertex3f(Sin(SliceAngle)*UpperRadius, UpperY, Cos(SliceAngle)*UpperRadius);
    end;

  var
    i: Integer;
  begin
    StackCircleCalc(UpperAngle, UpperY, UpperRadius);
    glBegin(GL_TRIANGLE_FAN);
      glColorv(LowerColor);
      glVertex3f(0, -SkySphereRadius, 0);
      glColorv(UpperColor);
      Pt(0);
      for i := 1 to Slices-1 do Pt(i* 2*Pi/Slices);
      Pt(0);
    glEnd;
  end;

var
  bs: TBackgroundSide;
  TexturedSides: TBackgroundSides;
  i: Integer;
  GroundHighestAngle: Single;
  SomeTexturesWithAlpha: boolean;
begin
  inherited Create;

  { caly konstruktor sprowadza sie do skonstruowania display listy
    szescianNieba_list, no i do zaladowania tekstur nieboTex zeby pozniej
    ta display lista mogla ich uzyc. }

  { calculate nieboTex and SomeTexturesWithAlpha }
  SomeTexturesWithAlpha := false;
  TexturedSides := [];
  for bs := Low(bs) to High(bs) do
  begin
    nieboTex[bs] := 0;
    if (Imgs.Images[bs] <> nil) and (not Imgs.Images[bs].IsNull) then
    begin
      try
        nieboTex[bs] := LoadGLTexture(Imgs.Images[bs], GL_LINEAR, GL_LINEAR,
          { poniewaz rozciagamy teksture przy pomocy GL_LINEAR a nie chce nam
            sie robic teksturze borderow - musimy uzyc GL_CLAMP_TO_EDGE
            aby uzyskac dobry efekt na krancach }
          Texture2DClampToEdge);
      except
        { Although texture image is already loaded in Imgs[bs],
          still texture loading may fail, e.g. with ECannotLoadS3TCTexture
          when OpenGL doesn't have proper extensions. Secure against this by
          making nice OnWarning. }
        on E: ETextureLoadError do
        begin
          OnWarning(wtMinor, 'Texture', 'Texture load error: ' + E.Message);
          Continue;
        end;
      end;

      Include(TexturedSides, bs);
      if Imgs.Images[bs].HasAlpha then SomeTexturesWithAlpha := true;
    end;
  end;

  CubeSize := SkySphereRadius * SphereRadiusToCubeSize;
  CubeSize2 := CubeSize / 2;

  szescianNieba_list := glGenListsCheck(1, 'TVRMLGLBackground.Create');

  glNewList(szescianNieba_list, GL_COMPILE);
  glPushAttrib(GL_ENABLE_BIT or GL_TEXTURE_BIT or GL_COLOR_BUFFER_BIT);
  try
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_FOG);
    glDisable(GL_BLEND);

    if GLUseMultiTexturing then
      glActiveTexture(GL_TEXTURE0);
    glDisable(GL_TEXTURE_2D);
    if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
    if GL3DTextures <> gsNone  then glDisable(GL_TEXTURE_3D);

    { wykonujemy najbardziej elementarna optymalizacje : jesli mamy 6 tekstur
      i zadna nie ma kanalu alpha (a w praktyce jest to chyba najczestsza sytuacja)
      to nie ma sensu sie w ogole przejmowac sky i ground, tekstury je zaslonia. }
    if (TexturedSides <> BGAllSides) or SomeTexturesWithAlpha then
    begin
      { calculate GroundHighestAngle, will be usable to optimize rendering sky.
        GroundHighestAngle is measured in sky convention (0 = zenith, Pi = nadir).
        If there is no sky I simply set GroundHighestAngle to sthg > Pi. }
      if GroundAngleCount <> 0 then
        GroundHighestAngle := Pi-GroundAngle^[GroundAngleCount-1] else
        GroundHighestAngle := Pi + 1;

      { render sky }
      Assert(SkyColorCount >= 1, 'Sky must have at least one color');
      Assert(SkyAngleCount+1 = SkyColorCount, 'Sky must have exactly one more Color than Angles');

      if SkyColorCount = 1 then
      begin
        { alpha ponizszego koloru nie ma znaczenia dla nas. Uzywamy 0 bo jest
          standardem (standardowo glClearColor ma alpha = wlasnie 0). }
        glClearColorv(SkyColor^[0], 0);
        glClear(GL_COLOR_BUFFER_BIT);
      end else
      begin
        { wiec SkyColorCount >= 2. W zasadzie rendering przebiega na zasadzie
            RenderUpperStack
            RenderStack iles razy
            RenderLowerStack
          Probujemy jednak przerwac robote w trakcie ktoregos RenderStack
          lub RenderLowerStack zeby nie tracic czasu na malowanie obszaru
          ktory i tak zamalujemy przez ground. Uzywamy do tego GroundHighestAngle.
        }
        RenderUpperStack(SkyColor^[0], SkyColor^[1], SkyAngle^[0]);
        for i := 1 to SkyAngleCount-1 do
        begin
          if SkyAngle^[i-1] > GroundHighestAngle then Break;
          RenderStack(SkyColor^[i]  , SkyAngle^[i-1],
                      SkyColor^[i+1], SkyAngle^[i]);
        end;
        { TODO: jesli ostatni stack ma SkyAngle bliskie Pi to powinnismy renderowac
          juz ostatni stack przy uzyciu RenderLowerStack. }
        if SkyAngle^[SkyAngleCount-1] <= GroundHighestAngle then
          RenderLowerStack(
            SkyColor^[SkyColorCount-1], SkyAngle^[SkyAngleCount-1],
            SkyColor^[SkyColorCount-1]);
      end;

      { render ground }
      if GroundAngleCount <> 0 then
      begin
        { jesli GroundAngleCount = 0 to nie ma ground wiec nie wymagamy wtedy
          zeby GroundColorCount = 1 (a wiec jest to wyjatek od zasady
          GroundAngleCount + 1 = GroundColorCount) }
        Assert(GroundAngleCount+1 = GroundColorCount, 'Ground must have exactly one more Color than Angles');

        RenderLowerStack(GroundColor^[1], Pi-GroundAngle^[0],
                         GroundColor^[0]);
        for i := 1 to GroundAngleCount-1 do
         RenderStack(GroundColor^[i+1], Pi-GroundAngle^[i],
                     GroundColor^[i]  , Pi-GroundAngle^[i-1]);
      end;
    end;

    { render cube with six textured faces }

    glEnable(GL_TEXTURE_2D);
    { Wybieramy GL_REPLACE bo scianki szescianu beda zawsze cale teksturowane
      i chcemy olac zupelnie kolor/material jaki bedzie na tych sciankach.
      Chcemy wziac to z tekstury (dlatego standardowe GL_MODULATE nie jest dobre).
      Ponadto, kanal alpha tez chcemy wziac z tekstury, tzn. szescian
      nieba ma byc przeswitujacy gdy tekstura bedzie przeswitujaca
      (dlatego GL_DECAL nie jest odpowiedni). }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

    for bs := Low(bs) to High(bs) do RenderTextureSide(bs);
  finally
    glPopAttrib;
    glEndList;
  end;
end;

destructor TVRMLGLBackground.Destroy;
begin
  glDeleteLists(szescianNieba_list, 1);
  glDeleteTextures(6, @nieboTex);
  inherited;
end;

end.
