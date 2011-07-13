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

{ Nodes that are defined by Avalon extensions, see
  [http://instant-reality.com/] and in particular the specifications on
  [http://instant-reality.com/documentation/nodetype/]. }

{$ifdef read_interface}
  { MatrixTransform node for VRML >= 2.0. From Avalon extension,
    see [http://instant-reality.com/documentation/nodetype/MatrixTransform/]. }
  TNodeMatrixTransform_2 = class(TNodeX3DGroupingNode, INodeTransform)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdmatrix: TSFMatrix4f;
    public property Fdmatrix: TSFMatrix4f read FFdmatrix;
  end;

  TNodeTeapot = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdmanifold: TSFBool;
    public property Fdmanifold: TSFBool read FFdmanifold;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function TexCoordField: TSFNode; override;
  end;

  TNodeLogger = class(TNodeX3DChildNode)
  private
    WriteStream: TStream;
    OwnsWriteStream: boolean;
    WriteStreamInitialized: boolean;
    procedure EventWriteReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdlevel: TSFInt32;
    public property Fdlevel: TSFInt32 read FFdlevel;

    private FFdlogFile: TSFString;
    public property FdlogFile: TSFString read FFdlogFile;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: XFAny, in } { }
    private FEventwrite: TVRMLEvent;
    public property Eventwrite: TVRMLEvent read FEventwrite;
  end;

  TNodeConverter = class(TNodeX3DChildNode)
  private
    procedure EventInReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: XFAny, in } { }
    private FEventIn: TVRMLEvent;
    public property EventIn: TVRMLEvent read FEventIn;

    { Event: XFAny, out } { }
    private FEventOut: TVRMLEvent;
    public property EventOut: TVRMLEvent read FEventOut;
  end;

  TNodeRenderedTexture = class(TNodeX3DTextureNode)
  private
    FGeneratedTextureHandler: TGeneratedTextureHandler;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;

    private FFdviewpoint: TSFNode;
    public property Fdviewpoint: TSFNode read FFdviewpoint;

    private FFdbackground: TSFNode;
    public property Fdbackground: TSFNode read FFdbackground;

    private FFdfog: TSFNode;
    public property Fdfog: TSFNode read FFdfog;

    private FFdenvironment: TSFNode;
    public property Fdenvironment: TSFNode read FFdenvironment;

    private FFdscene: TSFNode;
    public property Fdscene: TSFNode read FFdscene;

    private FFddimensions: TMFInt32;
    public property Fddimensions: TMFInt32 read FFddimensions;

    private FFddepthMap: TMFBool;
    public property FddepthMap: TMFBool read FFddepthMap;

    private FFdforeground: TSFNode;
    public property Fdforeground: TSFNode read FFdforeground;

    private FFdzOffset: TMFInt32;
    public property FdzOffset: TMFInt32 read FFdzOffset;

    private FFdtargets: TMFNode;
    public property Fdtargets: TMFNode read FFdtargets;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    { Event: SFMatrix4f, out } { }
    private FEventviewing: TVRMLEvent;
    public property Eventviewing: TVRMLEvent read FEventviewing;

    { Event: SFMatrix4f, out } { }
    private FEventprojection: TVRMLEvent;
    public property Eventprojection: TVRMLEvent read FEventprojection;

    private FFdupdate: TSFString;
    public property Fdupdate: TSFString read FFdupdate;

    private FFdframeBufferMode: TSFString;
    public property FdframeBufferMode: TSFString read FFdframeBufferMode;

    private FFdexcludeNodes: TMFNode;
    public property FdexcludeNodes: TMFNode read FFdexcludeNodes;

    private FFdtriggerName: TSFString;
    public property FdtriggerName: TSFString read FFdtriggerName;

    { Event: SFTime, in } { }
    private FEventtriggerSlot: TVRMLEvent;
    public property EventtriggerSlot: TVRMLEvent read FEventtriggerSlot;

    { Event: SFImage, out } { }
    private FEventimage_changed: TVRMLEvent;
    public property Eventimage_changed: TVRMLEvent read FEventimage_changed;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

    private FFdrepeatR: TSFBool;
    public property FdrepeatR: TSFBool read FFdrepeatR;

    { Event: SFBool, out } { }
    private FEventrendering: TVRMLEvent;
    public property Eventrendering: TVRMLEvent read FEventrendering;
  end;

  TNodePlane = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec2f;
    public property Fdsize: TSFVec2f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TNodeToggler = class(TNodeX3DChildNode)
  private
    procedure EventStatusReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure EventNotStatusReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure EventToggleReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure EventSetReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure EventResetReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdstatus: TSFBool;
    public property Fdstatus: TSFBool read FFdstatus;

    private FFdnotStatus: TSFBool;
    public property FdnotStatus: TSFBool read FFdnotStatus;

    { Event: XFAny, in } { }
    private FEventtoggle: TVRMLEvent;
    public property Eventtoggle: TVRMLEvent read FEventtoggle;

    { Event: XFAny, in } { }
    private FEventset: TVRMLEvent;
    public property Eventset: TVRMLEvent read FEventset;

    { Event: XFAny, in } { }
    private FEventreset: TVRMLEvent;
    public property Eventreset: TVRMLEvent read FEventreset;

    { Event: SFBool, out } { }
    private FEventchanged: TVRMLEvent;
    public property Eventchanged: TVRMLEvent read FEventchanged;

    { Event: SFBool, out } { }
    private FEventon: TVRMLEvent;
    public property Eventon: TVRMLEvent read FEventon;

    { Event: SFBool, out } { }
    private FEventoff: TVRMLEvent;
    public property Eventoff: TVRMLEvent read FEventoff;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;
  end;
{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeMatrixTransform_2.CreateNode;
begin
  inherited;

  FFdmatrix := TSFMatrix4f.Create(Self, 'matrix', IdentityMatrix4Single);
   Fdmatrix.ChangesAlways := [chTransform];
  Fields.Add(FFdmatrix);

  DefaultContainerField := 'children';
end;

class function TNodeMatrixTransform_2.ClassNodeTypeName: string;
begin
  Result := 'MatrixTransform';
end;

class function TNodeMatrixTransform_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeMatrixTransform_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TNodeMatrixTransform_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

procedure TNodeMatrixTransform_2.ApplyTransform(State: TVRMLGraphTraverseState);
var
  InvertedMatrix: TMatrix4Single;
begin
  inherited;

  { Inherited TVRMLGroupingNode already saved State.Transform and such. }

  State.Transform := MatrixMult(State.Transform, FdMatrix.Value);

  if TryMatrixInverse(FdMatrix.Value, InvertedMatrix) then
  begin
    State.InvertedTransform :=
      MatrixMult(InvertedMatrix, State.InvertedTransform);
  end else
  begin
    if Log then
      WritelnLogMultiline('Matrix',
        'Cannot invert matrix:' + NL + MatrixToRawStr(FdMatrix.Value, '  '));

    { When determinant is zero, we treat inverted matrix
      like identity (like in TNodeMatrixTransform_1.Transformation).
      So no need to multiply State.InvertedTransform by anything. }
  end;

  State.TransformScale *= FdMatrix.TransformScale;
end;

function TNodeMatrixTransform_2.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

function TNodeMatrixTransform_2.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TNodeTeapot.CreateNode;
begin
  inherited;

  FFdsize := TSFVec3f.Create(Self, 'size', Vector3Single(3, 3, 3));
   Fdsize.Exposed := false;
   Fdsize.ChangesAlways := [chGeometry];
  Fields.Add(FFdsize);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdmanifold := TSFBool.Create(Self, 'manifold', false);
   Fdmanifold.Exposed := false;
   Fdmanifold.ChangesAlways := [chGeometry];
  Fields.Add(FFdmanifold);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);
end;

class function TNodeTeapot.ClassNodeTypeName: string;
begin
  Result := 'Teapot';
end;

function TNodeTeapot.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeTeapot.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodeTeapot.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
const
{ TeapotCoord: array [0..0] of TVector3Single = ((0, 0, 0));
  TeapotCoordIndex: array [0..0] of LongInt = (0);
  TeapotManifoldCoord: array [0..0] of TVector3Single = ((0, 0, 0));
  TeapotManifoldCoordIndex: array [0..0] of LongInt = (0);
}
  {$I teapot/teapot.inc}
var
  CoordNode: TNodeCoordinate;
  IFS: TNodeIndexedFaceSet_2 absolute Result;
begin
  IFS := TNodeIndexedFaceSet_2.Create(NodeName, WWWBasePath);
  try
    CoordNode := TNodeCoordinate.Create('', WWWBasePath);
    IFS.FdCoord.Value := CoordNode;
    if FdManifold.Value then
      CoordNode.FdPoint.Items.AssignArray(TeapotManifoldCoord) else
      CoordNode.FdPoint.Items.AssignArray(TeapotCoord);

    { Scale Coords according to "size" field.
      Assumes that original coords are good for default size (3, 3, 3). }
    if not FdSize.EqualsDefaultValue then
    begin
      CoordNode.FdPoint.Items.MultiplyComponents(
        VectorScale(FdSize.Value, 1/3));
    end;

    if FdManifold.Value then
      IFS.FdCoordIndex.Items.AssignArray(TeapotManifoldCoordIndex) else
      IFS.FdCoordIndex.Items.AssignArray(TeapotCoordIndex);

    IFS.FdSolid.Value := FdSolid.Value;

    if FdTexCoord.CurrentChildAllowed then
      IFS.FdTexCoord.Value := FdTexCoord.Value;

    { Set CreaseAngle to anything larger than Pi, to make this completely
      smooth later using fast CreateSmoothNormalsCoordinateNode routine. }
    IFS.FdCreaseAngle.Value := 4;
  except FreeAndNil(Result); raise end;
end;

function TNodeTeapot.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TNodeTeapot.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  inherited;
  FdtexCoord.EnumerateValid(Func);
end;

function TNodeTeapot.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

procedure TNodeLogger.CreateNode;
begin
  inherited;

  FFdlevel := TSFInt32.Create(Self, 'level', 1);
  Fields.Add(FFdlevel);

  FFdlogFile := TSFString.Create(Self, 'logFile', '');
  FFdlogFile.Exposed := false;
  Fields.Add(FFdlogFile);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FEventwrite := TVRMLEvent.Create(Self, 'write', TVRMLField, true);
  Events.Add(FEventwrite);
  FEventwrite.OnReceive.Add(@EventWriteReceive);
end;

destructor TNodeLogger.Destroy;
begin
  if WriteStreamInitialized then
  begin
    if OwnsWriteStream then
      FreeAndNil(WriteStream);
  end;

  inherited;
end;

procedure TNodeLogger.EventWriteReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);

  procedure StdOutNotAvail;
  begin
    VRMLWarning(vwSerious,
      'Logger node wants to write on standard output, but it seems that stdout (standard output) ' +
      'is not available. Under Windows you should explicitly ' +
      'redirect program''s stdout to make it available, e.g. ' +
      'run "' + ProgramName + ' > ' + ProgramName + '.log".');
  end;

  procedure WritelnLogMessage;
  var
    S: string;
    Writer: TX3DWriter;
    NodeNames: TVRMLNodeNames;
  begin
    S := Format('Logger "%s": received field "%s" (%s). Time: %f.',
      [NodeName, Value.Name, Value.VRMLTypeName, Time.Seconds]);

    if FdLevel.Value = 1 then
    begin
      WritelnStr(WriteStream, S);
    end else
    begin
      Assert(FdLevel.Value >= 2);

      if (FdLevel.Value >= 3) and (Value.ParentNode <> nil) then
      begin
        S += Format(' Sending node: "%s" (%s).',
          [(Value.ParentNode as TVRMLNode).NodeName,
           (Value.ParentNode as TVRMLNode).NodeTypeName]);
      end;

      WriteStr(WriteStream, S + ' Value: ');

      NodeNames := TVRMLNodeNames.Create(false);
      Writer := TX3DWriter.Create(WriteStream);
      try
        { Just use latest format. }
        Writer.Version := X3DVersion;
        Value.FieldSaveToStream(Writer, NodeNames, xeClassic, true);
      finally
        FreeAndNil(Writer);
        FreeAndNil(NodeNames);
      end;
    end;
  end;

begin
  if FdEnabled.Value and (Fdlevel.Value > 0) then
  begin
    { First reception of "write" event causes initialization of
      WriteStream, based on logFile field value. This is Ok, since
      logFile is not exposed, so it cannot change after the file is loaded. }
    if not WriteStreamInitialized then
    begin
      Assert(WriteStream = nil);

      WriteStreamInitialized := true;

      if FdLogFile.Value <> '' then
      begin
        WriteStream := TFileStream.Create(
          FileNameAutoInc(ProgramName + '_logger_' +
            ExtractOnlyFileName(FdLogFile.Value) + '_%d.log'), fmCreate);
        OwnsWriteStream := true;
      end else
      begin
        WriteStream := StdOutStream;
        OwnsWriteStream := false;
        if WriteStream = nil then
        begin
          { report stdout not available, leave WriteStream = nil and
            WriteStreamInitialized = true. This way we will not try to
            reinit this again. }
          StdOutNotAvail;
          Exit;
        end;
      end;
    end;

    if WriteStream <> nil then
    begin
      try
        WritelnLogMessage;
      except
        on E: EWriteError do
        begin
          if not OwnsWriteStream then
          begin
            { This means that we have stdout, and it just failed...
              This is possible on Windows.

              Ideally, check for "StdOutStream = nil" should be all that is needed,
              and wrapping WritelnStr inside try...except should not be needed.
              But... see StdOutStream comments: you cannot
              depend on the fact that "StdOutStream <> nil means that stdout
              is actually available (because user redirected stdout etc.). }
            StdOutNotAvail;
          end else
            raise;
        end;
      end;
    end;
  end;
end;

class function TNodeLogger.ClassNodeTypeName: string;
begin
  Result := 'Logger';
end;

function TNodeLogger.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeLogger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeConverter.CreateNode;
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FEventIn := TVRMLEvent.Create(Self, 'in', TVRMLField, true);
  Events.Add(FEventIn);
  FEventIn.OnReceive.Add(@EventInReceive);

  FEventOut := TVRMLEvent.Create(Self, 'out', TVRMLField, true);
  Events.Add(FEventOut);
end;

procedure TNodeConverter.EventInReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdEnabled.Value then
    { TODO: uhhhmmmm, I cannot convert here (as I don't know to what I should
      convert... Convertion must be done along the route, when destination
      is known. }
    EventOut.Send(Value, Time);
end;

class function TNodeConverter.ClassNodeTypeName: string;
begin
  Result := 'Converter';
end;

function TNodeConverter.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeConverter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeRenderedTexture.CreateNode;
begin
  inherited;

  FFdviewpoint := TSFNode.Create(Self, 'viewpoint', [TVRMLViewpointNode]);
   Fdviewpoint.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdviewpoint);

  FFdbackground := TSFNode.Create(Self, 'background', [TNodeX3DBackgroundNode]);
   FFdbackground.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdbackground);

  FFdfog := TSFNode.Create(Self, 'fog', [TNodeFog]);
   Fdfog.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdfog);

  FFdenvironment := TSFNode.Create(Self, 'environment', [TVRMLNode] { nothing sensible in core X3D for this });
   Fdenvironment.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdenvironment);

  FFdscene := TSFNode.Create(Self, 'scene', [TVRMLNode]);
   Fdscene.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdscene);

  FFddimensions := TMFInt32.Create(Self, 'dimensions',
    [ DefaultRenderedTextureWidth,
      DefaultRenderedTextureHeight, 4, 1, 1]);
   Fddimensions.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFddimensions);

  FFddepthMap := TMFBool.Create(Self, 'depthMap', []);
   FddepthMap.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFddepthMap);

  FFdforeground := TSFNode.Create(Self, 'foreground', [TVRMLNode] { nothing sensible in core X3D for this });
   Fdforeground.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdforeground);

  FFdzOffset := TMFInt32.Create(Self, 'zOffset', []);
   FdzOffset.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdzOffset);

  FFdtargets := TMFNode.Create(Self, 'targets', [TNodeX3DTextureNode]);
   Fdtargets.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdtargets);

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FEventviewing := TVRMLEvent.Create(Self, 'viewing', TSFMatrix4f, false);
  Events.Add(FEventviewing);

  FEventprojection := TVRMLEvent.Create(Self, 'projection', TSFMatrix4f, false);
  Events.Add(FEventprojection);

  FFdupdate := TSFStringUpdate.Create(Self, 'update', 'NONE');
  Fields.Add(FFdupdate);

  FFdframeBufferMode := TSFString.Create(Self, 'frameBufferMode', 'auto');
   FdframeBufferMode.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdframeBufferMode);

  FFdexcludeNodes := TMFNode.Create(Self, 'excludeNodes', [TVRMLNode]);
   FdexcludeNodes.Exposed := false;
   FdexcludeNodes.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdexcludeNodes);

  FFdtriggerName := TSFString.Create(Self, 'triggerName', 'Render');
   FdtriggerName.Exposed := false;
  Fields.Add(FFdtriggerName);

  FEventtriggerSlot := TVRMLEvent.Create(Self, 'triggerSlot', TSFTime, true);
  Events.Add(FEventtriggerSlot);

  FEventimage_changed := TVRMLEvent.Create(Self, 'image_changed', TSFImage, false);
  Events.Add(FEventimage_changed);

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
   FdtextureProperties.Exposed := false;
   FdtextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdtextureProperties);

  FFdrepeatS := TSFBool.Create(Self, 'repeatS', true);
   FdrepeatS.Exposed := false;
   FdrepeatS.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create(Self, 'repeatT', true);
   FdrepeatT.Exposed := false;
   FdrepeatT.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatT);

  FFdrepeatR := TSFBool.Create(Self, 'repeatR', true);
   FdrepeatR.Exposed := false;
   FdrepeatR.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatR);

  FEventrendering := TVRMLEvent.Create(Self, 'rendering', TSFBool, false);
  Events.Add(FEventrendering);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FdUpdate := FdUpdate;
end;

destructor TNodeRenderedTexture.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TNodeRenderedTexture.ClassNodeTypeName: string;
begin
  Result := 'RenderedTexture';
end;

function TNodeRenderedTexture.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeRenderedTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodePlane.CreateNode;
begin
  inherited;

  FFdsize := TSFVec2f.Create(Self, 'size', Vector2Single(2, 2));
   Fdsize.Exposed := false;
   Fdsize.ChangesAlways := [chGeometry];
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
end;

class function TNodePlane.ClassNodeTypeName: string;
begin
  Result := 'Plane';
end;

class function TNodePlane.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodePlane.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := Rectangle2DProxy(Self, FdSize.Value, FdSolid.Value);
end;

function TNodePlane.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

type
  TSFBoolIgnorable = class(TSFBool)
  protected
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
    procedure ExposedEventReceive(Event: TVRMLEvent; AValue: TVRMLField;
      const Time: TVRMLTime); override;
  end;

function TSFBoolIgnorable.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TSFBool;
end;

procedure TSFBoolIgnorable.ExposedEventReceive(Event: TVRMLEvent; AValue: TVRMLField;
  const Time: TVRMLTime);
begin
  { This makes status and notStatus input events ignored,
    and output events not generated when enabled = FALSE. }
  if (ParentNode <> nil) and
     (not (ParentNode as TNodeToggler).FdEnabled.Value) then Exit;

  inherited;
end;

procedure TNodeToggler.CreateNode;
begin
  inherited;

  FFdstatus := TSFBoolIgnorable.Create(Self, 'status', false);
   Fdstatus.OnReceive.Add(@EventStatusReceive);
  Fields.Add(FFdstatus);

  FFdnotStatus := TSFBoolIgnorable.Create(Self, 'notStatus', true);
   Fdnotstatus.OnReceive.Add(@EventNotStatusReceive);
  Fields.Add(FFdnotStatus);

  FEventtoggle := TVRMLEvent.Create(Self, 'toggle', TVRMLField, true);
  FEventtoggle.OnReceive.Add(@EventToggleReceive);
  Events.Add(FEventtoggle);

  FEventset := TVRMLEvent.Create(Self, 'set', TVRMLField, true);
   Eventset.OnReceive.Add(@EventSetReceive);
  Events.Add(FEventset);

  FEventreset := TVRMLEvent.Create(Self, 'reset', TVRMLField, true);
   Eventreset.OnReceive.Add(@EventResetReceive);
  Events.Add(FEventreset);

  FEventchanged := TVRMLEvent.Create(Self, 'changed', TSFBool, false);
  Events.Add(FEventchanged);

  FEventon := TVRMLEvent.Create(Self, 'on', TSFBool, false);
  Events.Add(FEventon);

  FEventoff := TVRMLEvent.Create(Self, 'off', TSFBool, false);
  Events.Add(FEventoff);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);
end;

class function TNodeToggler.ClassNodeTypeName: string;
begin
  Result := 'Toggler';
end;

class function TNodeToggler.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeToggler.EventStatusReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  { Do not send event to notStatus, if it already has correct value
    (so our own value didn't actually changed).
    This avoids calling status and notStatus events in a loop.
    By the way, also avoid sending excessive changed/on/off. }
  if FdnotStatus.Value <> not FdStatus.Value then
  begin
    FdnotStatus.EventIn.Send(not FdStatus.Value, Time);

    EventChanged.Send(true, Time);
    if FdStatus.Value then EventOn.Send(true, Time);
    if not FdStatus.Value then EventOff.Send(true, Time);
  end;
end;

procedure TNodeToggler.EventNotStatusReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  { Do not send event to status, if it already has correct value
    (so our own value didn't actually changed).
    This avoids calling status and notStatus events in a loop.
    By the way, also avoid sending excessive changed/on/off. }
  if FdnotStatus.Value <> not FdStatus.Value then
  begin
    Fdstatus.EventIn.Send(not FdnotStatus.Value, Time);

    EventChanged.Send(true, Time);
    if FdStatus.Value then EventOn.Send(true, Time);
    if not FdStatus.Value then EventOff.Send(true, Time);
  end;
end;

procedure TNodeToggler.EventToggleReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdEnabled.Value then
    FdStatus.EventIn.Send(not FdStatus.Value, Time);
end;

procedure TNodeToggler.EventSetReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdEnabled.Value then
    FdStatus.EventIn.Send(true, Time);
end;

procedure TNodeToggler.EventResetReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdEnabled.Value then
    FdStatus.EventIn.Send(false, Time);
end;

procedure RegisterAvalonNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeMatrixTransform_2,
    TNodeTeapot,
    TNodeLogger,
    TNodeConverter,
    TNodeRenderedTexture,
    TNodePlane,
    TNodeToggler
  ]);
end;
{$endif read_implementation}
