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

    private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    private FFdVariation: TSFFloat;
    public property FdVariation: TSFFloat read FFdVariation;

    private FFdMass: TSFFloat;
    public property FdMass: TSFFloat read FFdMass;

    private FFdSurfaceArea: TSFFloat;
    public property FdSurfaceArea: TSFFloat read FFdSurfaceArea;
  end;

  TNodeX3DParticlePhysicsModelNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;
  end;

  TNodeBoundedPhysicsModel = class(TNodeX3DParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeometry: TSFNode;
    public property FdGeometry: TSFNode read FFdGeometry;
  end;

  TNodeConeEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAngle: TSFFloat;
    public property FdAngle: TSFFloat read FFdAngle;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;
  end;

  TNodeExplosionEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;
  end;

  TNodeGravityPhysicsModel = class(TNodeX3DParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdForce: TSFVec3f;
    public property FdForce: TSFVec3f read FFdForce;
  end;

  TNodeParticleSystem = class(TNodeX3DShapeNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCreateParticles: TSFBool;
    public property FdCreateParticles: TSFBool read FFdCreateParticles;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdLifetimeVariation: TSFFloat;
    public property FdLifetimeVariation: TSFFloat read FFdLifetimeVariation;

    private FFdMaxParticles: TSFInt32;
    public property FdMaxParticles: TSFInt32 read FFdMaxParticles;

    private FFdParticleLifetime: TSFFloat;
    public property FdParticleLifetime: TSFFloat read FFdParticleLifetime;

    private FFdParticleSize: TSFVec2f;
    public property FdParticleSize: TSFVec2f read FFdParticleSize;

    { Event: SFBool, out } { }
    private FEventIsActive: TVRMLEvent;
    public property EventIsActive: TVRMLEvent read FEventIsActive;

    private FFdColorRamp: TSFNode;
    public property FdColorRamp: TSFNode read FFdColorRamp;

    private FFdColorKey: TMFFloat;
    public property FdColorKey: TMFFloat read FFdColorKey;

    private FFdEmitter: TSFNode;
    public property FdEmitter: TSFNode read FFdEmitter;

    private FFdGeometryType: TSFString;
    public property FdGeometryType: TSFString read FFdGeometryType;

    private FFdPhysics: TMFNode;
    public property FdPhysics: TMFNode read FFdPhysics;

    private FFdTexCoordRamp: TSFNode;
    public property FdTexCoordRamp: TSFNode read FFdTexCoordRamp;

    private FFdTexCoordKey: TMFFloat;
    public property FdTexCoordKey: TMFFloat read FFdTexCoordKey;
  end;

  TNodePointEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;
  end;

  TNodePolylineEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, in } { }
    private FEventSet_coordinate: TVRMLEvent;
    public property EventSet_coordinate: TVRMLEvent read FEventSet_coordinate;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;
  end;

  TNodeSurfaceEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, in } { }
    private FEventSet_coordinate: TVRMLEvent;
    public property EventSet_coordinate: TVRMLEvent read FEventSet_coordinate;

    private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    private FFdSurface: TSFNode;
    public property FdSurface: TSFNode read FFdSurface;
  end;

  TNodeVolumeEmitter = class(TNodeX3DParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, in } { }
    private FEventSet_coordinate: TVRMLEvent;
    public property EventSet_coordinate: TVRMLEvent read FEventSet_coordinate;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    private FFdInternal: TSFBool;
    public property FdInternal: TSFBool read FFdInternal;
  end;

  TNodeWindPhysicsModel = class(TNodeX3DParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdGustiness: TSFFloat;
    public property FdGustiness: TSFFloat read FFdGustiness;

    private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    private FFdTurbulence: TSFFloat;
    public property FdTurbulence: TSFFloat read FFdTurbulence;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DParticleEmitterNode.CreateNode;
begin
  inherited;

  FFdSpeed := TSFFloat.Create(Self, 'speed', 0);
  Fields.Add(FFdSpeed);
  { X3D specification comment: [0,Inf) }

  FFdVariation := TSFFloat.Create(Self, 'variation', 0.25);
  Fields.Add(FFdVariation);
  { X3D specification comment: [0,Inf) }

  FFdMass := TSFFloat.Create(Self, 'mass', 0);
  FFdMass.Exposed := false;
  Fields.Add(FFdMass);
  { X3D specification comment: [0,Inf) }

  FFdSurfaceArea := TSFFloat.Create(Self, 'surfaceArea', 0);
  FFdSurfaceArea.Exposed := false;
  Fields.Add(FFdSurfaceArea);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'emitter';
end;

procedure TNodeX3DParticlePhysicsModelNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdEnabled);

  DefaultContainerField := 'physics';
end;

procedure TNodeBoundedPhysicsModel.CreateNode;
begin
  inherited;

  FFdGeometry := TSFNode.Create(Self, 'geometry', [TNodeX3DGeometryNode]);
  Fields.Add(FFdGeometry);
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

  FFdAngle := TSFFloat.Create(Self, 'angle', Pi/4);
  Fields.Add(FFdAngle);
  { X3D specification comment: [0,Pi] }

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFdDirection);

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdPosition);
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

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdPosition);
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

  FFdForce := TSFVec3f.Create(Self, 'force', Vector3Single(0, -9.8, 0));
  Fields.Add(FFdForce);
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

  FFdCreateParticles := TSFBool.Create(Self, 'createParticles', true);
  Fields.Add(FFdCreateParticles);

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdEnabled);

  FFdLifetimeVariation := TSFFloat.Create(Self, 'lifetimeVariation', 0.25);
  Fields.Add(FFdLifetimeVariation);
  { X3D specification comment: [0,1] }

  FFdMaxParticles := TSFInt32.Create(Self, 'maxParticles', 200);
  Fields.Add(FFdMaxParticles);
  { X3D specification comment: [0,Inf) }

  FFdParticleLifetime := TSFFloat.Create(Self, 'particleLifetime', 5);
  Fields.Add(FFdParticleLifetime);
  { X3D specification comment: [0,Inf) }

  FFdParticleSize := TSFVec2f.Create(Self, 'particleSize', Vector2Single(0.02, 0.02));
  Fields.Add(FFdParticleSize);
  { X3D specification comment: [0,Inf) }

  FEventIsActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventIsActive);

  FFdColorRamp := TSFNode.Create(Self, 'colorRamp', [TNodeX3DColorNode]);
  FFdColorRamp.Exposed := false;
  Fields.Add(FFdColorRamp);

  FFdColorKey := TMFFloat.Create(Self, 'colorKey', []);
  FFdColorKey.Exposed := false;
  Fields.Add(FFdColorKey);
  { X3D specification comment: [0,Inf) }

  FFdEmitter := TSFNode.Create(Self, 'emitter', [TNodeX3DParticleEmitterNode]);
  FFdEmitter.Exposed := false;
  Fields.Add(FFdEmitter);

  FFdGeometryType := TSFString.Create(Self, 'geometryType', 'QUAD');
  FFdGeometryType.Exposed := false;
  Fields.Add(FFdGeometryType);
  { X3D specification comment: ["LINE"|"POINT"|"QUAD"|"SPRITE"|"TRIANGLE"|"GEOMETRY"|...] }

  FFdPhysics := TMFNode.Create(Self, 'physics', [TNodeX3DParticlePhysicsModelNode]);
  FFdPhysics.Exposed := false;
  Fields.Add(FFdPhysics);

  FFdTexCoordRamp := TSFNode.Create(Self, 'texCoordRamp', [TNodeTextureCoordinate]);
  FFdTexCoordRamp.Exposed := false;
  Fields.Add(FFdTexCoordRamp);

  FFdTexCoordKey := TMFFloat.Create(Self, 'texCoordKey', []);
  FFdTexCoordKey.Exposed := false;
  Fields.Add(FFdTexCoordKey);
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

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFdDirection);

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdPosition);
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

  FEventSet_coordinate := TVRMLEvent.Create(Self, 'set_coordinate', TSFInt32, true);
  Events.Add(FEventSet_coordinate);

  FFdCoord := TSFNode.Create(Self, 'coord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdCoord);

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFdDirection);
  { X3D specification comment: [-1,1] }

  FFdCoordIndex := TMFInt32.Create(Self, 'coordIndex', [-1]);
  FFdCoordIndex.Exposed := false;
  Fields.Add(FFdCoordIndex);
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

  FEventSet_coordinate := TVRMLEvent.Create(Self, 'set_coordinate', TSFInt32, true);
  Events.Add(FEventSet_coordinate);

  FFdCoordIndex := TMFInt32.Create(Self, 'coordIndex', [-1]);
  FFdCoordIndex.Exposed := false;
  Fields.Add(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdSurface := TSFNode.Create(Self, 'surface', [TNodeX3DGeometryNode]);
  FFdSurface.Exposed := false;
  Fields.Add(FFdSurface);
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

  FEventSet_coordinate := TVRMLEvent.Create(Self, 'set_coordinate', TSFInt32, true);
  Events.Add(FEventSet_coordinate);

  FFdCoord := TSFNode.Create(Self, 'coord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdCoord);

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 1, 0));
  Fields.Add(FFdDirection);
  { X3D specification comment: [-1,1] }

  FFdCoordIndex := TMFInt32.Create(Self, 'coordIndex', [-1]);
  FFdCoordIndex.Exposed := false;
  Fields.Add(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdInternal := TSFBool.Create(Self, 'internal', true);
  FFdInternal.Exposed := false;
  Fields.Add(FFdInternal);
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

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, 0));
  Fields.Add(FFdDirection);
  { X3D specification comment: (Inf,Inf) }

  FFdGustiness := TSFFloat.Create(Self, 'gustiness', 0.1);
  Fields.Add(FFdGustiness);
  { X3D specification comment: [0,Inf) }

  FFdSpeed := TSFFloat.Create(Self, 'speed', 0.1);
  Fields.Add(FFdSpeed);
  { X3D specification comment: [0,Inf) }

  FFdTurbulence := TSFFloat.Create(Self, 'turbulence', 0);
  Fields.Add(FFdTurbulence);
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
