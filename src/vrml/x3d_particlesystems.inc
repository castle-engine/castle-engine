{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DParticleEmitterNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdvariation: TSFFloat;
    public property Fdvariation: TSFFloat read FFdvariation;

    private FFdmass: TSFFloat;
    public property Fdmass: TSFFloat read FFdmass;

    private FFdsurfaceArea: TSFFloat;
    public property FdsurfaceArea: TSFFloat read FFdsurfaceArea;
  end;

  TNodeX3DParticlePhysicsModelNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;
  end;

  TNodeBoundedPhysicsModel = class(TNodeX3DParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdgeometry: TSFNode;
    public property Fdgeometry: TSFNode read FFdgeometry;
  end;

  TNodeConeEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdangle: TSFFloat;
    public property Fdangle: TSFFloat read FFdangle;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;
  end;

  TNodeExplosionEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;
  end;

  TNodeGravityPhysicsModel = class(TNodeX3DParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdforce: TSFVec3f;
    public property Fdforce: TSFVec3f read FFdforce;
  end;

  TNodeParticleSystem = class(TNodeX3DShapeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcreateParticles: TSFBool;
    public property FdcreateParticles: TSFBool read FFdcreateParticles;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdlifetimeVariation: TSFFloat;
    public property FdlifetimeVariation: TSFFloat read FFdlifetimeVariation;

    private FFdmaxParticles: TSFInt32;
    public property FdmaxParticles: TSFInt32 read FFdmaxParticles;

    private FFdparticleLifetime: TSFFloat;
    public property FdparticleLifetime: TSFFloat read FFdparticleLifetime;

    private FFdparticleSize: TSFVec2f;
    public property FdparticleSize: TSFVec2f read FFdparticleSize;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    private FFdcolorRamp: TSFNode;
    public property FdcolorRamp: TSFNode read FFdcolorRamp;

    private FFdcolorKey: TMFFloat;
    public property FdcolorKey: TMFFloat read FFdcolorKey;

    private FFdemitter: TSFNode;
    public property Fdemitter: TSFNode read FFdemitter;

    private FFdgeometryType: TSFString;
    public property FdgeometryType: TSFString read FFdgeometryType;

    private FFdphysics: TMFNode;
    public property Fdphysics: TMFNode read FFdphysics;

    private FFdtexCoordRamp: TSFNode;
    public property FdtexCoordRamp: TSFNode read FFdtexCoordRamp;

    private FFdtexCoordKey: TMFFloat;
    public property FdtexCoordKey: TMFFloat read FFdtexCoordKey;
  end;

  TNodePointEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;
  end;

  TNodePolylineEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, in } { }
    private FEventset_coordinate: TVRMLEvent;
    public property Eventset_coordinate: TVRMLEvent read FEventset_coordinate;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;
  end;

  TNodeSurfaceEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, in } { }
    private FEventset_coordinate: TVRMLEvent;
    public property Eventset_coordinate: TVRMLEvent read FEventset_coordinate;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

    private FFdsurface: TSFNode;
    public property Fdsurface: TSFNode read FFdsurface;
  end;

  TNodeVolumeEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, in } { }
    private FEventset_coordinate: TVRMLEvent;
    public property Eventset_coordinate: TVRMLEvent read FEventset_coordinate;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

    private FFdinternal: TSFBool;
    public property Fdinternal: TSFBool read FFdinternal;
  end;

  TNodeWindPhysicsModel = class(TNodeX3DParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdgustiness: TSFFloat;
    public property Fdgustiness: TSFFloat read FFdgustiness;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdturbulence: TSFFloat;
    public property Fdturbulence: TSFFloat read FFdturbulence;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DParticleEmitterNode.CreateNode;
begin
  inherited;

  FFdspeed := TSFFloat.Create(Self, 'speed', 0);
  Fields.Add(FFdspeed);
  { X3D specification comment: [0,Inf) }

  FFdvariation := TSFFloat.Create(Self, 'variation', 0.25);
  Fields.Add(FFdvariation);
  { X3D specification comment: [0,Inf) }

  FFdmass := TSFFloat.Create(Self, 'mass', 0);
  FFdmass.Exposed := false;
  Fields.Add(FFdmass);
  { X3D specification comment: [0,Inf) }

  FFdsurfaceArea := TSFFloat.Create(Self, 'surfaceArea', 0);
  FFdsurfaceArea.Exposed := false;
  Fields.Add(FFdsurfaceArea);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'emitter';
end;

procedure TNodeX3DParticlePhysicsModelNode.CreateNode;
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  DefaultContainerField := 'physics';
end;

procedure TNodeBoundedPhysicsModel.CreateNode;
begin
  inherited;

  FFdgeometry := TSFNode.Create(Self, 'geometry', [TNodeX3DGeometryNode]);
  Fields.Add(FFdgeometry);
end;

class function TNodeBoundedPhysicsModel.ClassNodeTypeName: string;
begin
  Result := 'BoundedPhysicsModel';
end;

class function TNodeBoundedPhysicsModel.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeConeEmitter.CreateNode;
begin
  inherited;

  FFdangle := TSFFloat.Create(Self, 'angle', Pi/4);
  Fields.Add(FFdangle);
  { X3D specification comment: [0,Pi] }

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFddirection);

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdposition);
end;

class function TNodeConeEmitter.ClassNodeTypeName: string;
begin
  Result := 'ConeEmitter';
end;

class function TNodeConeEmitter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeExplosionEmitter.CreateNode;
begin
  inherited;

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdposition);
end;

class function TNodeExplosionEmitter.ClassNodeTypeName: string;
begin
  Result := 'ExplosionEmitter';
end;

class function TNodeExplosionEmitter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGravityPhysicsModel.CreateNode;
begin
  inherited;

  FFdforce := TSFVec3f.Create(Self, 'force', Vector3Single(0, -9.8, 0));
  Fields.Add(FFdforce);
  { X3D specification comment: (Inf,Inf) }
end;

class function TNodeGravityPhysicsModel.ClassNodeTypeName: string;
begin
  Result := 'GravityPhysicsModel';
end;

class function TNodeGravityPhysicsModel.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeParticleSystem.CreateNode;
begin
  inherited;

  FFdcreateParticles := TSFBool.Create(Self, 'createParticles', true);
  Fields.Add(FFdcreateParticles);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdlifetimeVariation := TSFFloat.Create(Self, 'lifetimeVariation', 0.25);
  Fields.Add(FFdlifetimeVariation);
  { X3D specification comment: [0,1] }

  FFdmaxParticles := TSFInt32.Create(Self, 'maxParticles', 200);
  Fields.Add(FFdmaxParticles);
  { X3D specification comment: [0,Inf) }

  FFdparticleLifetime := TSFFloat.Create(Self, 'particleLifetime', 5);
  Fields.Add(FFdparticleLifetime);
  { X3D specification comment: [0,Inf) }

  FFdparticleSize := TSFVec2f.Create(Self, 'particleSize', Vector2Single(0.02, 0.02));
  Fields.Add(FFdparticleSize);
  { X3D specification comment: [0,Inf) }

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FFdcolorRamp := TSFNode.Create(Self, 'colorRamp', [TNodeX3DColorNode]);
  FFdcolorRamp.Exposed := false;
  Fields.Add(FFdcolorRamp);

  FFdcolorKey := TMFFloat.Create(Self, 'colorKey', []);
  FFdcolorKey.Exposed := false;
  Fields.Add(FFdcolorKey);
  { X3D specification comment: [0,Inf) }

  FFdemitter := TSFNode.Create(Self, 'emitter', [TNodeX3DParticleEmitterNode]);
  FFdemitter.Exposed := false;
  Fields.Add(FFdemitter);

  FFdgeometryType := TSFString.Create(Self, 'geometryType', 'QUAD');
  FFdgeometryType.Exposed := false;
  Fields.Add(FFdgeometryType);
  { X3D specification comment: ["LINE"|"POINT"|"QUAD"|"SPRITE"|"TRIANGLE"|"GEOMETRY"|...] }

  FFdphysics := TMFNode.Create(Self, 'physics', [TNodeX3DParticlePhysicsModelNode]);
  FFdphysics.Exposed := false;
  Fields.Add(FFdphysics);

  FFdtexCoordRamp := TSFNode.Create(Self, 'texCoordRamp', [TNodeTextureCoordinate]);
  FFdtexCoordRamp.Exposed := false;
  Fields.Add(FFdtexCoordRamp);

  FFdtexCoordKey := TMFFloat.Create(Self, 'texCoordKey', []);
  FFdtexCoordKey.Exposed := false;
  Fields.Add(FFdtexCoordKey);
  { X3D specification comment: [0,Inf) }
end;

class function TNodeParticleSystem.ClassNodeTypeName: string;
begin
  Result := 'ParticleSystem';
end;

class function TNodeParticleSystem.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePointEmitter.CreateNode;
begin
  inherited;

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFddirection);

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdposition);
end;

class function TNodePointEmitter.ClassNodeTypeName: string;
begin
  Result := 'PointEmitter';
end;

class function TNodePointEmitter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePolylineEmitter.CreateNode;
begin
  inherited;

  FEventset_coordinate := TVRMLEvent.Create(Self, 'set_coordinate', TSFInt32, true);
  Events.Add(FEventset_coordinate);

  FFdcoord := TSFNode.Create(Self, 'coord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFddirection);
  { X3D specification comment: [-1,1] }

  FFdcoordIndex := TMFInt32.Create(Self, 'coordIndex', [-1]);
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }
end;

class function TNodePolylineEmitter.ClassNodeTypeName: string;
begin
  Result := 'PolylineEmitter';
end;

class function TNodePolylineEmitter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeSurfaceEmitter.CreateNode;
begin
  inherited;

  FEventset_coordinate := TVRMLEvent.Create(Self, 'set_coordinate', TSFInt32, true);
  Events.Add(FEventset_coordinate);

  FFdcoordIndex := TMFInt32.Create(Self, 'coordIndex', [-1]);
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdsurface := TSFNode.Create(Self, 'surface', [TNodeX3DGeometryNode]);
  FFdsurface.Exposed := false;
  Fields.Add(FFdsurface);
end;

class function TNodeSurfaceEmitter.ClassNodeTypeName: string;
begin
  Result := 'SurfaceEmitter';
end;

class function TNodeSurfaceEmitter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeVolumeEmitter.CreateNode;
begin
  inherited;

  FEventset_coordinate := TVRMLEvent.Create(Self, 'set_coordinate', TSFInt32, true);
  Events.Add(FEventset_coordinate);

  FFdcoord := TSFNode.Create(Self, 'coord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFddirection);
  { X3D specification comment: [-1,1] }

  FFdcoordIndex := TMFInt32.Create(Self, 'coordIndex', [-1]);
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdinternal := TSFBool.Create(Self, 'internal', true);
  FFdinternal.Exposed := false;
  Fields.Add(FFdinternal);
end;

class function TNodeVolumeEmitter.ClassNodeTypeName: string;
begin
  Result := 'VolumeEmitter';
end;

class function TNodeVolumeEmitter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeWindPhysicsModel.CreateNode;
begin
  inherited;

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, 0));
  Fields.Add(FFddirection);
  { X3D specification comment: (Inf,Inf) }

  FFdgustiness := TSFFloat.Create(Self, 'gustiness', 0.1);
  Fields.Add(FFdgustiness);
  { X3D specification comment: [0,Inf) }

  FFdspeed := TSFFloat.Create(Self, 'speed', 0.1);
  Fields.Add(FFdspeed);
  { X3D specification comment: [0,Inf) }

  FFdturbulence := TSFFloat.Create(Self, 'turbulence', 0);
  Fields.Add(FFdturbulence);
  { X3D specification comment: [0,1] }
end;

class function TNodeWindPhysicsModel.ClassNodeTypeName: string;
begin
  Result := 'WindPhysicsModel';
end;

class function TNodeWindPhysicsModel.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterParticleSystemsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBoundedPhysicsModel,
    TNodeConeEmitter,
    TNodeExplosionEmitter,
    TNodeGravityPhysicsModel,
    TNodeParticleSystem,
    TNodePointEmitter,
    TNodePolylineEmitter,
    TNodeSurfaceEmitter,
    TNodeVolumeEmitter,
    TNodeWindPhysicsModel
  ]);
end;

{$endif read_implementation}
