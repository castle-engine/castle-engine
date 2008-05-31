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
  TNodeX3DBackgroundNode = class(TNodeX3DBindableNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdgroundAngle: TMFFloat;
    public property FdgroundAngle: TMFFloat read FFdgroundAngle;

    private FFdgroundColor: TMFColor;
    public property FdgroundColor: TMFColor read FFdgroundColor;

    private FFdskyAngle: TMFFloat;
    public property FdskyAngle: TMFFloat read FFdskyAngle;

    private FFdskyColor: TMFColor;
    public property FdskyColor: TMFColor read FFdskyColor;

    private FFdtransparency: TSFFloat;
    public property Fdtransparency: TSFFloat read FFdtransparency;
  end;

  INodeX3DFogObject = interface(IVRMLNode)
    property Fdcolor: TSFColor { read GetFdcolor }; { }
    property FdfogType: TSFString { read GetFdfogType }; { }
    property FdvisibilityRange: TSFFloat { read GetFdvisibilityRange }; { }
  end;

  TNodeBackground = class(TNodeX3DBackgroundNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbackUrl: TMFString;
    public property FdbackUrl: TMFString read FFdbackUrl;

    private FFdbottomUrl: TMFString;
    public property FdbottomUrl: TMFString read FFdbottomUrl;

    private FFdfrontUrl: TMFString;
    public property FdfrontUrl: TMFString read FFdfrontUrl;

    private FFdleftUrl: TMFString;
    public property FdleftUrl: TMFString read FFdleftUrl;

    private FFdrightUrl: TMFString;
    public property FdrightUrl: TMFString read FFdrightUrl;

    private FFdtopUrl: TMFString;
    public property FdtopUrl: TMFString read FFdtopUrl;
  end;

  TNodeFog = class(TNodeX3DBindableNode, INodeX3DFogObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdfogType: TSFString;
    public property FdfogType: TSFString read FFdfogType;

    private FFdvisibilityRange: TSFFloat;
    public property FdvisibilityRange: TSFFloat read FFdvisibilityRange;
  end;

  TNodeFogCoordinate = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddepth: TMFFloat;
    public property Fddepth: TMFFloat read FFddepth;
  end;

  TNodeLocalFog = class(TNodeX3DChildNode, INodeX3DFogObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdfogType: TSFString;
    public property FdfogType: TSFString read FFdfogType;

    private FFdvisibilityRange: TSFFloat;
    public property FdvisibilityRange: TSFFloat read FFdvisibilityRange;
  end;

  TNodeTextureBackground = class(TNodeX3DBackgroundNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbackTexture: TSFNode;
    public property FdbackTexture: TSFNode read FFdbackTexture;

    private FFdbottomTexture: TSFNode;
    public property FdbottomTexture: TSFNode read FFdbottomTexture;

    private FFdfrontTexture: TSFNode;
    public property FdfrontTexture: TSFNode read FFdfrontTexture;

    private FFdleftTexture: TSFNode;
    public property FdleftTexture: TSFNode read FFdleftTexture;

    private FFdrightTexture: TSFNode;
    public property FdrightTexture: TSFNode read FFdrightTexture;

    private FFdtopTexture: TSFNode;
    public property FdtopTexture: TSFNode read FFdtopTexture;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DBackgroundNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdgroundAngle := TMFFloat.Create('groundAngle', []);
  Fields.Add(FFdgroundAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdgroundColor := TMFColor.Create('groundColor', []);
  Fields.Add(FFdgroundColor);
  { X3D specification comment: [0,1] }

  FFdskyAngle := TMFFloat.Create('skyAngle', []);
  Fields.Add(FFdskyAngle);
  { X3D specification comment: [0,Pi] }

  FFdskyColor := TMFColor.Create('skyColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdskyColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create('transparency', 0);
  Fields.Add(FFdtransparency);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

constructor TNodeBackground.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbackUrl := TMFString.Create('backUrl', []);
  Fields.Add(FFdbackUrl);
  { X3D specification comment: [URI] }

  FFdbottomUrl := TMFString.Create('bottomUrl', []);
  Fields.Add(FFdbottomUrl);
  { X3D specification comment: [URI] }

  FFdfrontUrl := TMFString.Create('frontUrl', []);
  Fields.Add(FFdfrontUrl);
  { X3D specification comment: [URI] }

  FFdleftUrl := TMFString.Create('leftUrl', []);
  Fields.Add(FFdleftUrl);
  { X3D specification comment: [URI] }

  FFdrightUrl := TMFString.Create('rightUrl', []);
  Fields.Add(FFdrightUrl);
  { X3D specification comment: [URI] }

  FFdtopUrl := TMFString.Create('topUrl', []);
  Fields.Add(FFdtopUrl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';
end;

class function TNodeBackground.ClassNodeTypeName: string;
begin
  Result := 'Background';
end;

function TNodeBackground.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBackground.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeFog.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcolor := TSFColor.Create('color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdfogType := TSFString.Create('fogType', 'LINEAR');
  Fields.Add(FFdfogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdvisibilityRange := TSFFloat.Create('visibilityRange', 0);
  Fields.Add(FFdvisibilityRange);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeFog.ClassNodeTypeName: string;
begin
  Result := 'Fog';
end;

function TNodeFog.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFog.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeFogCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddepth := TMFFloat.Create('depth', []);
  Fields.Add(FFddepth);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

class function TNodeFogCoordinate.ClassNodeTypeName: string;
begin
  Result := 'FogCoordinate';
end;

function TNodeFogCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFogCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLocalFog.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcolor := TSFColor.Create('color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdenabled := TSFBool.Create('enabled', true);
  Fields.Add(FFdenabled);

  FFdfogType := TSFString.Create('fogType', 'LINEAR');
  Fields.Add(FFdfogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdvisibilityRange := TSFFloat.Create('visibilityRange', 0);
  Fields.Add(FFdvisibilityRange);
  { X3D specification comment: [0,-Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeLocalFog.ClassNodeTypeName: string;
begin
  Result := 'LocalFog';
end;

function TNodeLocalFog.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLocalFog.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureBackground.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbackTexture := TSFNode.Create(Self, 'backTexture', [X3DTextureNode]);
  Fields.Add(FFdbackTexture);

  FFdbottomTexture := TSFNode.Create(Self, 'bottomTexture', [X3DTextureNode]);
  Fields.Add(FFdbottomTexture);

  FFdfrontTexture := TSFNode.Create(Self, 'frontTexture', [X3DTextureNode]);
  Fields.Add(FFdfrontTexture);

  FFdleftTexture := TSFNode.Create(Self, 'leftTexture', [X3DTextureNode]);
  Fields.Add(FFdleftTexture);

  FFdrightTexture := TSFNode.Create(Self, 'rightTexture', [X3DTextureNode]);
  Fields.Add(FFdrightTexture);

  FFdtopTexture := TSFNode.Create(Self, 'topTexture', [X3DTextureNode]);
  Fields.Add(FFdtopTexture);

  DefaultContainerField := 'children';
end;

class function TNodeTextureBackground.ClassNodeTypeName: string;
begin
  Result := 'TextureBackground';
end;

function TNodeTextureBackground.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureBackground.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterEnvironmentalEffectsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBackground,
    TNodeFog,
    TNodeFogCoordinate,
    TNodeLocalFog,
    TNodeTextureBackground
  ]);
end;

{$endif read_implementation}
