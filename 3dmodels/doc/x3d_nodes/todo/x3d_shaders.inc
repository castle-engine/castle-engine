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
  TNodeX3DShaderNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFBool, in } { }
    private FEventactivate: TVRMLEvent;
    public property Eventactivate: TVRMLEvent read FEventactivate;

    { Event: SFBool, out } { }
    private FEventisSelected: TVRMLEvent;
    public property EventisSelected: TVRMLEvent read FEventisSelected;

    { Event: SFBool, out } { }
    private FEventisValid: TVRMLEvent;
    public property EventisValid: TVRMLEvent read FEventisValid;

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;
  end;

  TNodeX3DVertexAttributeNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;
  end;

  TNodeComposedShader = class(TNodeX3DShaderNode, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdparts: TMFNode;
    public property Fdparts: TMFNode read FFdparts;

    { # And any number of:}
  end;

  TNodeFloatVertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFFloat;
    public property Fdvalue: TMFFloat read FFdvalue;

    private FFdnumComponents: TSFInt32;
    public property FdnumComponents: TSFInt32 read FFdnumComponents;
  end;

  TNodeMatrix3VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFMatrix3f;
    public property Fdvalue: TMFMatrix3f read FFdvalue;
  end;

  TNodeMatrix4VertexAttribute = class(TNodeX3DVertexAttributeNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvalue: TMFMatrix4f;
    public property Fdvalue: TMFMatrix4f read FFdvalue;
  end;

  TNodePackagedShader = class(TNodeX3DShaderNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    { # And any number of:}
  end;

  TNodeProgramShader = class(TNodeX3DShaderNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdprograms: TMFNode;
    public property Fdprograms: TMFNode read FFdprograms;
  end;

  TNodeShaderPart = class(TNodeX3DNode, INodeX3DUrlObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFString;
    public property Fdtype: TSFString read FFdtype;
  end;

  TNodeShaderProgram = class(TNodeX3DNode, INodeX3DUrlObject, INodeX3DProgrammableShaderObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtype: TSFString;
    public property Fdtype: TSFString read FFdtype;

    { # And any number of:}
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DShaderNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventactivate := TVRMLEvent.Create('activate', TSFBool, true);
  Events.Add(FEventactivate);

  FEventisSelected := TVRMLEvent.Create('isSelected', TSFBool, false);
  Events.Add(FEventisSelected);

  FEventisValid := TVRMLEvent.Create('isValid', TSFBool, false);
  Events.Add(FEventisValid);

  FFdlanguage := TSFString.Create('language', '');
  FFdlanguage.Exposed := false;
  Fields.Add(FFdlanguage);
  { X3D specification comment: ["CG"|"GLSL"|"HLSL"|...] }

  DefaultContainerField := 'children';
end;

constructor TNodeX3DVertexAttributeNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create('name', '');
  FFdname.Exposed := false;
  Fields.Add(FFdname);

  DefaultContainerField := 'children';
end;

constructor TNodeComposedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdparts := TMFNode.Create(Self, 'parts', [ShaderPart]);
  Fields.Add(FFdparts);

  DefaultContainerField := 'children';
end;

class function TNodeComposedShader.ClassNodeTypeName: string;
begin
  Result := 'ComposedShader';
end;

function TNodeComposedShader.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeComposedShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeFloatVertexAttribute.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TMFFloat.Create('value', []);
  Fields.Add(FFdvalue);
  { X3D specification comment: (-Inf,Inf) }

  FFdnumComponents := TSFInt32.Create('numComponents', 4);
  FFdnumComponents.Exposed := false;
  Fields.Add(FFdnumComponents);
  { X3D specification comment: [1..4] }

  DefaultContainerField := 'children';
end;

class function TNodeFloatVertexAttribute.ClassNodeTypeName: string;
begin
  Result := 'FloatVertexAttribute';
end;

function TNodeFloatVertexAttribute.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFloatVertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMatrix3VertexAttribute.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TMFMatrix3f.Create('value', []);
  Fields.Add(FFdvalue);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeMatrix3VertexAttribute.ClassNodeTypeName: string;
begin
  Result := 'Matrix3VertexAttribute';
end;

function TNodeMatrix3VertexAttribute.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMatrix3VertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMatrix4VertexAttribute.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TMFMatrix4f.Create('value', []);
  Fields.Add(FFdvalue);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeMatrix4VertexAttribute.ClassNodeTypeName: string;
begin
  Result := 'Matrix4VertexAttribute';
end;

function TNodeMatrix4VertexAttribute.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMatrix4VertexAttribute.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePackagedShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';
end;

class function TNodePackagedShader.ClassNodeTypeName: string;
begin
  Result := 'PackagedShader';
end;

function TNodePackagedShader.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePackagedShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeProgramShader.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdprograms := TMFNode.Create(Self, 'programs', [ShaderProgram]);
  Fields.Add(FFdprograms);

  DefaultContainerField := 'children';
end;

class function TNodeProgramShader.ClassNodeTypeName: string;
begin
  Result := 'ProgramShader';
end;

function TNodeProgramShader.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeProgramShader.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeShaderPart.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtype := TSFString.Create('type', 'VERTEX');
  FFdtype.Exposed := false;
  Fields.Add(FFdtype);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'children';
end;

class function TNodeShaderPart.ClassNodeTypeName: string;
begin
  Result := 'ShaderPart';
end;

function TNodeShaderPart.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeShaderPart.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeShaderProgram.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtype := TSFString.Create('type', 'VERTEX');
  FFdtype.Exposed := false;
  Fields.Add(FFdtype);
  { X3D specification comment: ["VERTEX"|"FRAGMENT"] }

  DefaultContainerField := 'children';
end;

class function TNodeShaderProgram.ClassNodeTypeName: string;
begin
  Result := 'ShaderProgram';
end;

function TNodeShaderProgram.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeShaderProgram.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterShadersNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeComposedShader,
    TNodeFloatVertexAttribute,
    TNodeMatrix3VertexAttribute,
    TNodeMatrix4VertexAttribute,
    TNodePackagedShader,
    TNodeProgramShader,
    TNodeShaderPart,
    TNodeShaderProgram
  ]);
end;

{$endif read_implementation}
