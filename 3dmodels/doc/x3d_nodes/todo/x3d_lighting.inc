{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  TNodeX3DLightNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdglobal: TSFBool;
    public property Fdglobal: TSFBool read FFdglobal;
  end;

  TNodeDirectionalLight = class(TNodeX3DLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    { # TODO: global by default is FALSE}
  end;

  TNodePointLight = class(TNodeX3DLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdattenuation: TSFVec3f;
    public property Fdattenuation: TSFVec3f read FFdattenuation;

    { # TODO: global by default is TRUE}

    private FFdlocation: TSFVec3f;
    public property Fdlocation: TSFVec3f read FFdlocation;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;
  end;

  TNodeSpotLight = class(TNodeX3DLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdattenuation: TSFVec3f;
    public property Fdattenuation: TSFVec3f read FFdattenuation;

    private FFdbeamWidth: TSFFloat;
    public property FdbeamWidth: TSFFloat read FFdbeamWidth;

    private FFdcutOffAngle: TSFFloat;
    public property FdcutOffAngle: TSFFloat read FFdcutOffAngle;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    { # TODO: global by default is TRUE}

    private FFdlocation: TSFVec3f;
    public property Fdlocation: TSFVec3f read FFdlocation;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DLightNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdglobal := TSFBool.Create('global', false);
  Fields.Add(FFdglobal);

  DefaultContainerField := 'children';
end;

constructor TNodeDirectionalLight.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddirection := TSFVec3f.Create('direction', Vector3Single(0, 0, -1));
  Fields.Add(FFddirection);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeDirectionalLight.ClassNodeTypeName: string;
begin
  Result := 'DirectionalLight';
end;

function TNodeDirectionalLight.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeDirectionalLight.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePointLight.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdattenuation := TSFVec3f.Create('attenuation', Vector3Single(1, 0, 0));
  Fields.Add(FFdattenuation);
  { X3D specification comment: [0,Inf) }

  FFdlocation := TSFVec3f.Create('location', Vector3Single(0, 0, 0));
  Fields.Add(FFdlocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdradius := TSFFloat.Create('radius', 100);
  Fields.Add(FFdradius);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodePointLight.ClassNodeTypeName: string;
begin
  Result := 'PointLight';
end;

function TNodePointLight.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePointLight.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSpotLight.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdattenuation := TSFVec3f.Create('attenuation', Vector3Single(1, 0, 0));
  Fields.Add(FFdattenuation);
  { X3D specification comment: [0,Inf) }

  FFdbeamWidth := TSFFloat.Create('beamWidth', Pi/2);
  Fields.Add(FFdbeamWidth);
  { X3D specification comment: (0,Pi/2] }

  FFdcutOffAngle := TSFFloat.Create('cutOffAngle', Pi/4);
  Fields.Add(FFdcutOffAngle);
  { X3D specification comment: (0,Pi/2] }

  FFddirection := TSFVec3f.Create('direction', Vector3Single(0, 0, -1));
  Fields.Add(FFddirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdlocation := TSFVec3f.Create('location', Vector3Single(0, 0, 0));
  Fields.Add(FFdlocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdradius := TSFFloat.Create('radius', 100);
  Fields.Add(FFdradius);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeSpotLight.ClassNodeTypeName: string;
begin
  Result := 'SpotLight';
end;

function TNodeSpotLight.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSpotLight.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterLightingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeDirectionalLight,
    TNodePointLight,
    TNodeSpotLight
  ]);
end;

{$endif read_implementation}
