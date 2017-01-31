{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  TColladaEffect = class
    Appearance: TAppearanceNode;
    { If this effect contains a texture for diffuse, this is the name
      of texture coordinates in Collada.
      For now not used (we always take first input with TEXCOORD semantic
      to control the main texture, that affects the look (after lighting
      calculation, so not only diffuse). }
    DiffuseTexCoordName: string;
    destructor Destroy; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

destructor TColladaEffect.Destroy;
begin
  FreeIfUnusedAndNil(Appearance);
  inherited;
end;

type
  TColladaEffectList = class(specialize TFPGObjectList<TColladaEffect>)
    { Find a TColladaEffect with given Name, @nil if not found. }
    function Find(const Name: string): TColladaEffect;
  end;

function TColladaEffectList.Find(const Name: string): TColladaEffect;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Appearance.X3DName = Name then
      Exit(Items[I]);
  Result := nil;
end;

{$endif read_implementation}

{$ifdef read_implementation_nested}

var
  { List of Collada effects. Each contains an X3D Appearance node,
    with a name equal to Collada effect name. }
  Effects: TColladaEffectList;

{ Read elements of type "common_color_or_texture_type" in Collada >= 1.4.x.
  If we have <color>, return this color (texture and coord names are then set to empty).
  If we have <texture>, return white color (and the appropriate texture
  and coord names; if texture name was empty, you want to treat it like not
  existing anyway). }
function ReadColorOrTexture(Element: TDOMElement; out TextureName, TexCoordName: string): TCastleColorRGB;
var
  Child: TDOMElement;
begin
  TextureName := '';
  TexCoordName := '';
  Result := WhiteRGB;
  Child := Element.ChildElement('color', false);
  if Child <> nil then
  begin
    { I simply drop 4th color component, I don't know what's the use of this
      (alpha is exposed by effect/materials parameter transparency, so color
      alpha is supposed to mean something else ?). }
    Result := Vector3SingleCut(Vector4SingleFromStr(Child.TextData));
  end else
  begin
    Child := Element.ChildElement('texture', false);
    if Child <> nil then
    begin
      Child.AttributeString('texture', TextureName);
      Child.AttributeString('texcoord', TexCoordName);
    end;
  end;
end;

{ Read elements of type "common_color_or_texture_type" in Collada >= 1.4.x,
  but allow only color specification. }
function ReadColor(Element: TDOMElement): TVector3Single;
var
  IgnoreTextureName, IgnoreTexCoordName: string;
begin
  Result := ReadColorOrTexture(Element, IgnoreTextureName, IgnoreTexCoordName);
end;

{ Read elements of type "common_float_or_param_type" in Collada >= 1.4.x. }
function ReadFloatOrParam(Element: TDOMElement): Float;
var
  FloatElement: TDOMElement;
begin
  FloatElement := Element.ChildElement('float', false);
  if FloatElement <> nil then
  begin
    Result := StrToFloat(FloatElement.TextData);
  end else
    { We don't support anything else than <float> here, just use
      default 1 eventually. }
    Result := 1.0;
end;

{ Read <effect>. Only for Collada >= 1.4.x.
  Adds effect to the Effects list. }
procedure ReadEffect(EffectElement: TDOMElement);
var
  { Effect instance and nodes, available to local procedures inside ReadEffect. }
  Effect: TColladaEffect;
  Appearance: TAppearanceNode;
  Mat: TMaterialNode;

  { Map of <surface> names to images (references from Images list) }
  Surfaces: TStringTextureNodeMap;
  { Map of <sampler2D> names to images (references from Images list) }
  Samplers2D: TStringTextureNodeMap;

  procedure ReadTechnique(TechniqueElement: TDOMElement);
  var
    Image: TAbstractTextureNode;
    TechniqueChild: TDOMElement;
    I: TXMLElementIterator;
    DiffuseTextureName, DiffuseTexCoordName: string;
    Transparency: Single;
    HasTransparency: boolean;
  begin
    { We actually treat <phong> and <blinn> and even <lambert> elements the same.
      X3D lighting equations specify that always Blinn
      (half-vector) technique is used. What's much more practically
      important, OpenGL uses Blinn method. So actually I always do
      blinn method (at least for real-time rendering). }
    TechniqueChild := TechniqueElement.ChildElement('phong', false);
    if TechniqueChild = nil then
      TechniqueChild := TechniqueElement.ChildElement('blinn', false);
    if TechniqueChild = nil then
      TechniqueChild := TechniqueElement.ChildElement('lambert', false);

    if TechniqueChild <> nil then
    begin
      { <transparent> (color) and <transparency> (float) should
        be multiplied with each other. So says the Collada spec
        ("Determining Transparency (Opacity)" in Chapter 7,
        and forum https://collada.org/public_forum/viewtopic.php?t=386).
        When only one is specified, the other is like 1.
        However, when neither one is specified, we obviously want to leave
        the model opaque (transparency = 0) so we need boolean HasTransparency. }
      Transparency := 1;
      HasTransparency := false;

      I := TechniqueChild.ChildrenIterator;
      try
        while I.GetNext do
        begin
          if I.Current.TagName = 'emission' then
            Mat.FdEmissiveColor.Value :=  ReadColor(I.Current) else
          if I.Current.TagName = 'ambient' then
            Mat.FdAmbientIntensity.Value := VectorAverage(ReadColor(I.Current)) else
          if I.Current.TagName = 'diffuse' then
          begin
            Mat.FdDiffuseColor.Value := ReadColorOrTexture(I.Current,
              DiffuseTextureName, DiffuseTexCoordName);
            if DiffuseTextureName <> '' then
            begin
              Image := Samplers2D.Find(DiffuseTextureName);
              if Image <> nil then
              begin
                Effect.DiffuseTexCoordName := DiffuseTexCoordName;
                Appearance.FdTexture.Value := Image;
              end else
              begin
                Image := Images.FindName(DiffuseTextureName) as TAbstractTextureNode;
                if Image <> nil then
                begin
                  Effect.DiffuseTexCoordName := DiffuseTexCoordName;
                  Appearance.FdTexture.Value := Image;
                  { Happens e.g. on "Private Section/Faerie_Forrest_DAY/Faerie_Forrest_DAY.dae"
                    from collada.org/owl models. }
                  WritelnWarning('Collada', Format('<diffuse> texture refers to missing sampler2D name "%s". Found <image> with the same id, will use it',
                    [DiffuseTextureName]));
                end else
                  WritelnWarning('Collada', Format('<diffuse> texture refers to missing sampler2D name "%s"',
                    [DiffuseTextureName]));
              end;
            end;
          end else
          if I.Current.TagName = 'specular' then
            Mat.FdSpecularColor.Value := ReadColor(I.Current) else
          if I.Current.TagName = 'shininess' then
            Mat.FdShininess.Value := ReadFloatOrParam(I.Current) / 128.0 else
          if I.Current.TagName = 'reflective' then
            {Mat.FdMirrorColor.Value := } ReadColor(I.Current) else
          if I.Current.TagName = 'reflectivity' then
          begin
            if AllowKambiExtensions then
              Mat.FdMirror.Value := ReadFloatOrParam(I.Current) else
              ReadFloatOrParam(I.Current);
          end else
          if I.Current.TagName = 'transparent' then
          begin
            Transparency *= VectorAverage(ReadColor(I.Current));
            HasTransparency := true;
          end else
          if I.Current.TagName = 'transparency' then
          begin
            Transparency *= ReadFloatOrParam(I.Current);
            HasTransparency := true;
          end else
          if I.Current.TagName = 'index_of_refraction' then
            {Mat.FdIndexOfRefraction.Value := } ReadFloatOrParam(I.Current);
        end;
      finally FreeAndNil(I) end;

      if HasTransparency then
        Mat.FdTransparency.Value := Transparency;
    end;
  end;

  { Read <newparam>. }
  procedure ReadNewParam(Element: TDOMElement);
  var
    Child: TDOMElement;
    Name, RefersTo: string;
    Image: TAbstractTextureNode;
  begin
    if Element.AttributeString('sid', Name) then
    begin
      Child := Element.ChildElement('surface', false);
      if Child <> nil then
      begin
        { Read <surface>. It has <init_from>, referring to name on Images. }
        RefersTo := ReadChildText(Child, 'init_from');
        Image := Images.FindName(RefersTo) as TAbstractTextureNode;
        if Image <> nil then
          Surfaces[Name] := Image else
          WritelnWarning('Collada', Format('<surface> refers to missing image name "%s"',
            [RefersTo]));
      end else
      begin
        Child := Element.ChildElement('sampler2D', false);
        if Child <> nil then
        begin
          { Read <sampler2D>. It has <source>, referring to name on Surfaces. }
          RefersTo := ReadChildText(Child, 'source');
          Image := Surfaces.Find(RefersTo);
          if Image <> nil then
            Samplers2D[Name] := Image else
            WritelnWarning('Collada', Format('<sampler2D> refers to missing surface name "%s"',
              [RefersTo]));
        end; { else not handled <newparam> }
      end;
    end;
  end;

var
  Id: string;
  I: TXMLElementIterator;
  ProfileElement: TDOMElement;
begin
  if not EffectElement.AttributeString('id', Id) then
    Id := '';

  Effect := TColladaEffect.Create;
  Effects.Add(Effect);

  Appearance := TAppearanceNode.Create(Id, BaseUrl);
  Effect.Appearance := Appearance;

  Mat := TMaterialNode.Create('', BaseUrl);
  Appearance.FdMaterial.Value := Mat;

  ProfileElement := EffectElement.ChildElement('profile_COMMON', false);
  if ProfileElement <> nil then
  begin
    Surfaces := TStringTextureNodeMap.Create;
    Samplers2D := TStringTextureNodeMap.Create;
    try
      I := ProfileElement.ChildrenIterator;
      try
        while I.GetNext do
          if I.Current.TagName = 'technique' then
            { Actually only one <technique> within <profile_COMMON> is allowed.
              But, since we loop anyway, it's not a problem to handle many. }
            ReadTechnique(I.Current) else
          if I.Current.TagName = 'newparam' then
            ReadNewParam(I.Current);
      finally FreeAndNil(I) end;
    finally
      FreeAndNil(Surfaces);
      FreeAndNil(Samplers2D);
    end;
  end;
end;

{ Read <library_effects>. Only for Collada >= 1.4.x.
  All effects are added to the Effects list. }
procedure ReadLibraryEffects(LibraryElement: TDOMElement);
var
  I: TXMLElementIterator;
begin
  I := LibraryElement.ChildrenIterator('effect');
  try
    while I.GetNext do
      ReadEffect(I.Current);
      { other I.Current.TagName not supported for now }
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
