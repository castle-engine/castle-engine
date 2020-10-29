{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractParticleEmitterNode = class(TAbstractNode)
  public
    procedure CreateNode; override;

    strict private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    strict private FFdVariation: TSFFloat;
    public property FdVariation: TSFFloat read FFdVariation;

    strict private FFdMass: TSFFloat;
    public property FdMass: TSFFloat read FFdMass;

    strict private FFdSurfaceArea: TSFFloat;
    public property FdSurfaceArea: TSFFloat read FFdSurfaceArea;

    {$I auto_generated_node_helpers/x3dnodes_x3dparticleemitternode.inc}
  end;

  TAbstractParticlePhysicsModelNode = class(TAbstractNode)
  public
    procedure CreateNode; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    {$I auto_generated_node_helpers/x3dnodes_x3dparticlephysicsmodelnode.inc}
  end;

  TBoundedPhysicsModelNode = class(TAbstractParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdGeometry: TSFNode;
    public property FdGeometry: TSFNode read FFdGeometry;

    {$I auto_generated_node_helpers/x3dnodes_boundedphysicsmodel.inc}
  end;

  TConeEmitterNode = class(TAbstractParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAngle: TSFFloat;
    public property FdAngle: TSFFloat read FFdAngle;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    {$I auto_generated_node_helpers/x3dnodes_coneemitter.inc}
  end;

  TExplosionEmitterNode = class(TAbstractParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    {$I auto_generated_node_helpers/x3dnodes_explosionemitter.inc}
  end;

  TForcePhysicsModelNode = class(TAbstractParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdForce: TSFVec3f;
    public property FdForce: TSFVec3f read FFdForce;

    {$I auto_generated_node_helpers/x3dnodes_forcephysicsmodel.inc}
  end;

  TParticleSystemNode = class(TAbstractShapeNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCreateParticles: TSFBool;
    public property FdCreateParticles: TSFBool read FFdCreateParticles;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdLifetimeVariation: TSFFloat;
    public property FdLifetimeVariation: TSFFloat read FFdLifetimeVariation;

    strict private FFdMaxParticles: TSFInt32;
    public property FdMaxParticles: TSFInt32 read FFdMaxParticles;

    strict private FFdParticleLifetime: TSFFloat;
    public property FdParticleLifetime: TSFFloat read FFdParticleLifetime;

    strict private FFdParticleSize: TSFVec2f;
    public property FdParticleSize: TSFVec2f read FFdParticleSize;

    { Event out } { }
    strict private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    strict private FFdColorRamp: TSFNode;
    public property FdColorRamp: TSFNode read FFdColorRamp;

    strict private FFdColorKey: TMFFloat;
    public property FdColorKey: TMFFloat read FFdColorKey;

    strict private FFdEmitter: TSFNode;
    public property FdEmitter: TSFNode read FFdEmitter;

    strict private FFdGeometryType: TSFString;
    public property FdGeometryType: TSFString read FFdGeometryType;

    strict private FFdPhysics: TMFNode;
    public property FdPhysics: TMFNode read FFdPhysics;

    strict private FFdTexCoordRamp: TSFNode;
    public property FdTexCoordRamp: TSFNode read FFdTexCoordRamp;

    strict private FFdTexCoordKey: TMFFloat;
    public property FdTexCoordKey: TMFFloat read FFdTexCoordKey;

    {$I auto_generated_node_helpers/x3dnodes_particlesystem.inc}
  end;

  TPointEmitterNode = class(TAbstractParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    {$I auto_generated_node_helpers/x3dnodes_pointemitter.inc}
  end;

  TPolylineEmitterNode = class(TAbstractParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_coordinate: TSFInt32Event;
    public property EventSet_coordinate: TSFInt32Event read FEventSet_coordinate;

    strict private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    {$I auto_generated_node_helpers/x3dnodes_polylineemitter.inc}
  end;

  TSurfaceEmitterNode = class(TAbstractParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_coordinate: TSFInt32Event;
    public property EventSet_coordinate: TSFInt32Event read FEventSet_coordinate;

    strict private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    strict private FFdSurface: TSFNode;
    public property FdSurface: TSFNode read FFdSurface;

    {$I auto_generated_node_helpers/x3dnodes_surfaceemitter.inc}
  end;

  TVolumeEmitterNode = class(TAbstractParticleEmitterNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_coordinate: TSFInt32Event;
    public property EventSet_coordinate: TSFInt32Event read FEventSet_coordinate;

    strict private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    strict private FFdInternal: TSFBool;
    public property FdInternal: TSFBool read FFdInternal;

    {$I auto_generated_node_helpers/x3dnodes_volumeemitter.inc}
  end;

  TWindPhysicsModelNode = class(TAbstractParticlePhysicsModelNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdGustiness: TSFFloat;
    public property FdGustiness: TSFFloat read FFdGustiness;

    strict private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    strict private FFdTurbulence: TSFFloat;
    public property FdTurbulence: TSFFloat read FFdTurbulence;

    {$I auto_generated_node_helpers/x3dnodes_windphysicsmodel.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractParticleEmitterNode.CreateNode;
begin
  inherited;

  FFdSpeed := TSFFloat.Create(Self, true, 'speed', 0);
  AddField(FFdSpeed);
  { X3D specification comment: [0,Inf) }

  FFdVariation := TSFFloat.Create(Self, true, 'variation', 0.25);
  AddField(FFdVariation);
  { X3D specification comment: [0,Inf) }

  FFdMass := TSFFloat.Create(Self, false, 'mass', 0);
  AddField(FFdMass);
  { X3D specification comment: [0,Inf) }

  FFdSurfaceArea := TSFFloat.Create(Self, false, 'surfaceArea', 0);
  AddField(FFdSurfaceArea);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'emitter';
end;

procedure TAbstractParticlePhysicsModelNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  DefaultContainerField := 'physics';
end;

procedure TBoundedPhysicsModelNode.CreateNode;
begin
  inherited;

  FFdGeometry := TSFNode.Create(Self, true, 'geometry', [TAbstractGeometryNode]);
  AddField(FFdGeometry);
end;

class function TBoundedPhysicsModelNode.ClassX3DType: string;
begin
  Result := 'BoundedPhysicsModel';
end;

procedure TConeEmitterNode.CreateNode;
begin
  inherited;

  FFdAngle := TSFFloat.Create(Self, true, 'angle', Pi/4);
  AddField(FFdAngle);
  { X3D specification comment: [0,Pi] }

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 1, 0));
  AddField(FFdDirection);

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 0));
  AddField(FFdPosition);
end;

class function TConeEmitterNode.ClassX3DType: string;
begin
  Result := 'ConeEmitter';
end;

procedure TExplosionEmitterNode.CreateNode;
begin
  inherited;

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 0));
  AddField(FFdPosition);
end;

class function TExplosionEmitterNode.ClassX3DType: string;
begin
  Result := 'ExplosionEmitter';
end;

procedure TForcePhysicsModelNode.CreateNode;
begin
  inherited;

  FFdForce := TSFVec3f.Create(Self, true, 'force', Vector3(0, -9.8, 0));
  AddField(FFdForce);
  { X3D specification comment: (Inf,Inf) }
end;

class function TForcePhysicsModelNode.ClassX3DType: string;
begin
  Result := 'ForcePhysicsModel';
end;

procedure TParticleSystemNode.CreateNode;
begin
  inherited;

  FFdCreateParticles := TSFBool.Create(Self, true, 'createParticles', true);
  AddField(FFdCreateParticles);

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdLifetimeVariation := TSFFloat.Create(Self, true, 'lifetimeVariation', 0.25);
  AddField(FFdLifetimeVariation);
  { X3D specification comment: [0,1] }

  FFdMaxParticles := TSFInt32.Create(Self, true, 'maxParticles', 200);
  AddField(FFdMaxParticles);
  { X3D specification comment: [0,Inf) }

  FFdParticleLifetime := TSFFloat.Create(Self, true, 'particleLifetime', 5);
  AddField(FFdParticleLifetime);
  { X3D specification comment: [0,Inf) }

  FFdParticleSize := TSFVec2f.Create(Self, true, 'particleSize', Vector2(0.02, 0.02));
  AddField(FFdParticleSize);
  { X3D specification comment: [0,Inf) }

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);

  FFdColorRamp := TSFNode.Create(Self, false, 'colorRamp', [TAbstractColorNode]);
  AddField(FFdColorRamp);

  FFdColorKey := TMFFloat.Create(Self, false, 'colorKey', []);
  AddField(FFdColorKey);
  { X3D specification comment: [0,Inf) }

  FFdEmitter := TSFNode.Create(Self, false, 'emitter', [TAbstractParticleEmitterNode]);
  AddField(FFdEmitter);

  FFdGeometryType := TSFString.Create(Self, false, 'geometryType', 'QUAD');
  AddField(FFdGeometryType);
  { X3D specification comment: ["LINE"|"POINT"|"QUAD"|"SPRITE"|"TRIANGLE"|"GEOMETRY"|...] }

  FFdPhysics := TMFNode.Create(Self, false, 'physics', [TAbstractParticlePhysicsModelNode]);
  AddField(FFdPhysics);

  FFdTexCoordRamp := TSFNode.Create(Self, false, 'texCoordRamp', [TTextureCoordinateNode]);
  AddField(FFdTexCoordRamp);

  FFdTexCoordKey := TMFFloat.Create(Self, false, 'texCoordKey', []);
  AddField(FFdTexCoordKey);
  { X3D specification comment: [0,Inf) }
end;

class function TParticleSystemNode.ClassX3DType: string;
begin
  Result := 'ParticleSystem';
end;

procedure TPointEmitterNode.CreateNode;
begin
  inherited;

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 1, 0));
  AddField(FFdDirection);

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 0));
  AddField(FFdPosition);
end;

class function TPointEmitterNode.ClassX3DType: string;
begin
  Result := 'PointEmitter';
end;

procedure TPolylineEmitterNode.CreateNode;
begin
  inherited;

  FEventSet_coordinate := TSFInt32Event.Create(Self, 'set_coordinate', true);
  AddEvent(FEventSet_coordinate);

  FFdCoord := TSFNode.Create(Self, true, 'coord', [TAbstractCoordinateNode]);
  AddField(FFdCoord);

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 1, 0));
  AddField(FFdDirection);
  { X3D specification comment: [-1,1] }

  FFdCoordIndex := TMFInt32.Create(Self, false, 'coordIndex', [-1]);
  AddField(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }
end;

class function TPolylineEmitterNode.ClassX3DType: string;
begin
  Result := 'PolylineEmitter';
end;

procedure TSurfaceEmitterNode.CreateNode;
begin
  inherited;

  FEventSet_coordinate := TSFInt32Event.Create(Self, 'set_coordinate', true);
  AddEvent(FEventSet_coordinate);

  FFdCoordIndex := TMFInt32.Create(Self, false, 'coordIndex', [-1]);
  AddField(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdSurface := TSFNode.Create(Self, false, 'surface', [TAbstractGeometryNode]);
  AddField(FFdSurface);
end;

class function TSurfaceEmitterNode.ClassX3DType: string;
begin
  Result := 'SurfaceEmitter';
end;

procedure TVolumeEmitterNode.CreateNode;
begin
  inherited;

  FEventSet_coordinate := TSFInt32Event.Create(Self, 'set_coordinate', true);
  AddEvent(FEventSet_coordinate);

  FFdCoord := TSFNode.Create(Self, true, 'coord', [TAbstractCoordinateNode]);
  AddField(FFdCoord);

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 1, 0));
  AddField(FFdDirection);
  { X3D specification comment: [-1,1] }

  FFdCoordIndex := TMFInt32.Create(Self, false, 'coordIndex', [-1]);
  AddField(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdInternal := TSFBool.Create(Self, false, 'internal', true);
  AddField(FFdInternal);
end;

class function TVolumeEmitterNode.ClassX3DType: string;
begin
  Result := 'VolumeEmitter';
end;

procedure TWindPhysicsModelNode.CreateNode;
begin
  inherited;

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 0, 0));
  AddField(FFdDirection);
  { X3D specification comment: (Inf,Inf) }

  FFdGustiness := TSFFloat.Create(Self, true, 'gustiness', 0.1);
  AddField(FFdGustiness);
  { X3D specification comment: [0,Inf) }

  FFdSpeed := TSFFloat.Create(Self, true, 'speed', 0.1);
  AddField(FFdSpeed);
  { X3D specification comment: [0,Inf) }

  FFdTurbulence := TSFFloat.Create(Self, true, 'turbulence', 0);
  AddField(FFdTurbulence);
  { X3D specification comment: [0,1] }
end;

class function TWindPhysicsModelNode.ClassX3DType: string;
begin
  Result := 'WindPhysicsModel';
end;

procedure RegisterParticleSystemsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBoundedPhysicsModelNode,
    TConeEmitterNode,
    TExplosionEmitterNode,
    TForcePhysicsModelNode,
    TParticleSystemNode,
    TPointEmitterNode,
    TPolylineEmitterNode,
    TSurfaceEmitterNode,
    TVolumeEmitterNode,
    TWindPhysicsModelNode
  ]);
end;

{$endif read_implementation}
