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
  TNodeX3DEnvironmentTextureNode = class(TNodeX3DTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeComposedCubeMapTexture = class(TNodeX3DEnvironmentTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdback: TSFNode;
    public property Fdback: TSFNode read FFdback;

    private FFdbottom: TSFNode;
    public property Fdbottom: TSFNode read FFdbottom;

    private FFdfront: TSFNode;
    public property Fdfront: TSFNode read FFdfront;

    private FFdleft: TSFNode;
    public property Fdleft: TSFNode read FFdleft;

    private FFdright: TSFNode;
    public property Fdright: TSFNode read FFdright;

    private FFdtop: TSFNode;
    public property Fdtop: TSFNode read FFdtop;
  end;

  TNodeGeneratedCubeMapTexture = class(TNodeX3DEnvironmentTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdupdate: TSFString;
    public property Fdupdate: TSFString read FFdupdate;

    private FFdsize: TSFInt32;
    public property Fdsize: TSFInt32 read FFdsize;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;
  end;

  TNodeImageCubeMapTexture = class(TNodeX3DEnvironmentTextureNode, INodeX3DUrlObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DEnvironmentTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
end;

constructor TNodeComposedCubeMapTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdback := TSFNode.Create(Self, 'back', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdback);

  FFdbottom := TSFNode.Create(Self, 'bottom', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdbottom);

  FFdfront := TSFNode.Create(Self, 'front', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdfront);

  FFdleft := TSFNode.Create(Self, 'left', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdleft);

  FFdright := TSFNode.Create(Self, 'right', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdright);

  FFdtop := TSFNode.Create(Self, 'top', [TNodeX3DTexture2DNode]);
  Fields.Add(FFdtop);
end;

class function TNodeComposedCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'ComposedCubeMapTexture';
end;

function TNodeComposedCubeMapTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeComposedCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeGeneratedCubeMapTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdupdate := TSFString.Create(Self, 'update', 'NONE');
  Fields.Add(FFdupdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdsize := TSFInt32.Create(Self, 'size', 128);
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);
end;

class function TNodeGeneratedCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'GeneratedCubeMapTexture';
end;

function TNodeGeneratedCubeMapTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeGeneratedCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeImageCubeMapTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
  Fields.Add(FFdtextureProperties);
end;

class function TNodeImageCubeMapTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageCubeMapTexture';
end;

function TNodeImageCubeMapTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeImageCubeMapTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterCubeMapTexturingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeComposedCubeMapTexture,
    TNodeGeneratedCubeMapTexture,
    TNodeImageCubeMapTexture
  ]);
end;

{$endif read_implementation}
