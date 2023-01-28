{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_implementation_nested}

var
  Lights: TX3DChildrenList;

{ Read <library_lights> (Collada 1.4.x). Fills Lights list. }
procedure ReadLibraryLights(LibraryE: TDOMElement);

  function ReadAttenuation(E: TDOMElement): TVector3;
  begin
    Result := Vector3(1, 0, 0);
    ReadChildFloat(E, 'constant_attenuation', Result.X);
    ReadChildFloat(E, 'linear_attenuation', Result.Y);
    ReadChildFloat(E, 'quadratic_attenuation', Result.Z);
  end;

var
  I: TXMLElementIterator;
  Light: TAbstractLightNode;
  Point: TPointLightNode;
  Directional: TDirectionalLightNode;
  Spot: TSpotLightNode;
  Id, Profile: string;
  TechniqueE, LightE, ExtraE: TDOMElement;
  FalloffAngle, Radius: Float;
  LightColor: TVector3;
begin
  I := LibraryE.ChildrenIterator('light');
  try
    while I.GetNext do
      if I.Current.AttributeString('id', Id) then
      begin
        Light := nil;
        LightE := nil;

        TechniqueE := I.Current.ChildElement('technique_common', false);
        if TechniqueE <> nil then
        begin
          LightE := TechniqueE.ChildElement('point', false);
          if LightE <> nil then
          begin
            Point := TPointLightNode.Create(Id, BaseUrl);
            Point.FdAttenuation.Value := ReadAttenuation(LightE);
            Light := Point;
          end else
          begin
            LightE := TechniqueE.ChildElement('directional', false);
            if LightE <> nil then
            begin
              Directional := TDirectionalLightNode.Create(Id, BaseUrl);
              { default X3D light direction is -Z, matches Collada }
              Light := Directional;
            end else
            begin
              LightE := TechniqueE.ChildElement('spot', false);
              if LightE <> nil then
              begin
                Spot := TSpotLightNode.Create(Id, BaseUrl);
                Spot.FdAttenuation.Value := ReadAttenuation(LightE);
                { default X3D spot direction is -Z, matches Collada }
                if not ReadChildFloat(LightE, 'falloff_angle', FalloffAngle) then
                  FalloffAngle := 180; { Collada default }
                Spot.FdCutOffAngle.Value := DegToRad(FalloffAngle);
                { falloff_exponent cannot be nicely translated to X3D beamWidth,
                  see notes about SpotLight.beamWidth at VRML/X3D renderer. }
                Light := Spot;
              end else
              begin
                LightE := TechniqueE.ChildElement('ambient', false);
                if LightE <> nil then
                begin
                  Point := TPointLightNode.Create(Id, BaseUrl);
                  { ambient light can be translated to normal PointLight with
                    intensity = 0 (this scales diffuse and specular to zero). }
                  Point.FdIntensity.Value := 0;
                  Point.FdAmbientIntensity.Value := 1;
                  Light := Point;
                end else
                  WritelnWarning('Collada', 'No supported light inside <technique_common>');
              end;
            end;
          end;
        end else
          WritelnWarning('Collada', 'No supported technique inside <light>');

        if Light <> nil then
        begin
          Light.FdGlobal.Value := true;
          if ReadChildVector(LightE, 'color', LightColor) then
            Light.FdColor.Value := LightColor;
          if Light is TAbstractPositionalLightNode then
          begin
            { calculate light radius }
            Radius := MaxSingle;
            ExtraE := I.Current.ChildElement('extra', false);
            if ExtraE <> nil then
            begin
              TechniqueE := ExtraE.ChildElement('technique', false);
              if (TechniqueE <> nil) and
                 TechniqueE.AttributeString('profile', Profile) and
                 (Profile = 'blender') then
                ReadChildFloat(TechniqueE, 'dist', Radius);
            end;
            TAbstractPositionalLightNode(Light).FdRadius.Value := Radius;
          end;
          Lights.Add(Light);
        end;
      end;
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
