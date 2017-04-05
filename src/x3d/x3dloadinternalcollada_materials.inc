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

{$ifdef read_interface_nested}

var
  { List of Collada materials. Collada material is just a reference
    to Collada effect with a name.
    This way we handle instance_effect in <material> node.
    Many materials may refer to a single effect. }
  Materials: TColladaMaterialsMap;

procedure ReadLibraryMaterials(LibraryElement: TDOMElement); forward;

{$endif read_interface_nested}

{$ifdef read_implementation_nested}

{ Read <material>. It is added to the Materials list. }
procedure ReadMaterial(MatElement: TDOMElement);

  function ReadParamAsVector3(Element: TDOMElement): TVector3Single;
  var
    AType: string;
  begin
    if not Element.AttributeString('type', AType) then
    begin
      WritelnWarning('Collada', '<param> has no type attribute');
      Result := ZeroVector3Single;
    end else
    if AType <> 'float3' then
    begin
      WritelnWarning('Collada', 'Expected <param> with type "float3"');
      Result := ZeroVector3Single;
    end else
       Result := Vector3SingleFromStr(Element.TextData);
   end;

   function ReadParamAsFloat(Element: TDOMElement): Float;
   var
     AType: string;
   begin
     if not Element.AttributeString('type', AType) then
     begin
       WritelnWarning('Collada', '<param> has no type attribute');
       Result := 0;
     end else
     if AType <> 'float' then
     begin
       WritelnWarning('Collada', 'Expected <param> with type "float"');
       Result := 0;
     end else
       Result := StrToFloat(Element.TextData);
  end;

var
  MatId: string;

  { For Collada < 1.4.x }
  procedure TryCollada13;
  var
    ShaderElement, TechniqueElement, PassElement, ProgramElement: TDOMElement;
    ParamName: string;
    I: TXMLElementIterator;
    Effect: TColladaEffect;
    Appearance: TAppearanceNode;
    Mat: TMaterialNode;
  begin
    Effect := TColladaEffect.Create;
    Effects.Add(Effect);

    Appearance := TAppearanceNode.Create(MatId, BaseUrl);
    Effect.Appearance := Appearance;

    Mat := TMaterialNode.Create('', BaseUrl);
    Appearance.FdMaterial.Value := Mat;

    { Collada 1.3 doesn't really have a concept of effects used by materials.
      But to be consistent, we add one effect (to Effects list)
      and one reference to it (to Materials list). }
    Materials[MatId] := Effect;

    ShaderElement := MatElement.ChildElement('shader', false);
    if ShaderElement <> nil then
    begin
       TechniqueElement := ShaderElement.ChildElement('technique', false);
       if TechniqueElement <> nil then
       begin
         PassElement := TechniqueElement.ChildElement('pass', false);
         if PassElement <> nil then
         begin
           ProgramElement := PassElement.ChildElement('program', false);
           if ProgramElement <> nil then
           begin
             I := ProgramElement.ChildrenIterator('param');
             try
               while I.GetNext do
                 if I.Current.AttributeString('name', ParamName) then
                 begin
                   if ParamName = 'EMISSION' then
                     Mat.FdEmissiveColor.Value := ReadParamAsVector3(I.Current) else
                   if ParamName = 'AMBIENT' then
                     Mat.FdAmbientIntensity.Value := VectorAverage(ReadParamAsVector3(I.Current)) else
                   if ParamName = 'DIFFUSE' then
                     Mat.FdDiffuseColor.Value := ReadParamAsVector3(I.Current) else
                   if ParamName = 'SPECULAR' then
                     Mat.FdSpecularColor.Value := ReadParamAsVector3(I.Current) else
                   if ParamName = 'SHININESS' then
                     Mat.FdShininess.Value := ReadParamAsFloat(I.Current) / 128.0 else
                   if ParamName = 'REFLECTIVE' then
                     {Mat.FdMirrorColor.Value := } ReadParamAsVector3(I.Current) else
                   if ParamName = 'REFLECTIVITY' then
                   begin
                     if AllowKambiExtensions then
                       Mat.FdMirror.Value := ReadParamAsFloat(I.Current) else
                       ReadParamAsFloat(I.Current);
                   end else
                   (*
                   Blender Collada 1.3.1 exporter bug: it sets
                   type of TRANSPARENT param as "float".
                   Although content inicates "float3",
                   like Collada 1.3.1 spec requires (page 129),
                   and consistently with what is in Collada 1.4.1 spec.

                   I don't handle this anyway, so I just ignore it for now.
                   Should be reported to Blender.

                   if ParamName = 'TRANSPARENT' then
                     {Mat.FdTransparencyColor.Value := } ReadParamAsVector3(I.Current) else
                   *)
                   if ParamName = 'TRANSPARENCY' then
                     Mat.FdTransparency.Value := ReadParamAsFloat(I.Current);
                   { other ParamName not handled }
                 end;
             finally FreeAndNil(I) end;
           end;
         end;
       end;
    end;
  end;

  { For Collada >= 1.4.x }
  procedure TryCollada14;
  var
    InstanceEffect: TDOMElement;
    EffectId: string;
    Effect: TColladaEffect;
  begin
    if MatId = '' then Exit;

    InstanceEffect := MatElement.ChildElement('instance_effect', false);
    if InstanceEffect <> nil then
    begin
      if InstanceEffect.AttributeString('url', EffectId) and
         SCharIs(EffectId, 1, '#') then
      begin
        Delete(EffectId, 1, 1); { delete initial '#' char }
        Effect := Effects.Find(EffectId);
        if Effect <> nil then
          Materials[MatId] := Effect else
          WritelnWarning('Collada', Format('Material "%s" references ' +
            'non-existing effect "%s"', [MatId, EffectId]));
      end;
    end;
  end;

begin
  if not MatElement.AttributeString('id', MatId) then
    MatId := '';

  if Version14 then
    TryCollada14 else
    TryCollada13;
end;

{ Read <library_materials> (Collada >= 1.4.x) or
  <library type="MATERIAL"> (Collada < 1.4.x). }
procedure ReadLibraryMaterials(LibraryElement: TDOMElement);
var
  I: TXMLElementIterator;
begin
  I := LibraryElement.ChildrenIterator('material');
  try
    while I.GetNext do
      ReadMaterial(I.Current);
      { other I.Current.TagName not supported for now }
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
