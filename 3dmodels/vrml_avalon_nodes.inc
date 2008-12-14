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

{ Nodes that are defined by Avalon extensions, see
  [http://instant-reality.com/] and in particular the specifications on
  [http://instant-reality.com/documentation/nodetype/]. }

{$ifdef read_interface}
  { MatrixTransform node for VRML >= 2.0 from Avalon extension,
    see [http://instant-reality.com/documentation/nodetype/MatrixTransform/]. }
  TNodeMatrixTransform_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    private FFdmatrix: TSFMatrix4f;
    public property Fdmatrix: TSFMatrix4f read FFdmatrix;
  end;

  TNodeTeapot = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    function Proxy: TVRMLGeometryNode; override;
  end;

  TNodeLogger = class(TNodeX3DChildNode)
  private
    WriteStream: TStream;
    OwnsWriteStream: boolean;
    WriteStreamInitialized: boolean;
    procedure EventWriteReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeMatrixTransform_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdmatrix := TSFMatrix4f.Create(Self, 'matrix', IdentityMatrix4Single);
  Fields.Add(FFdmatrix);
  Fdmatrix.Transform := true;

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

class function TNodeMatrixTransform_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeMatrixTransform_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

procedure TNodeMatrixTransform_2.BeforeTraverse(var State: TVRMLGraphTraverseState);
var
  InvertedMatrix: TMatrix4Single;
begin
  inherited;

  { Inherited TVRMLGroupingNode already saved State.Transform and such. }

  State.Transform := MatrixMult(State.Transform, FdMatrix.Value);

  if TryMatrixInverse(FdMatrix.Value, InvertedMatrix) then
  begin
    State.InvertedTransform :=
      MatrixMult(InvertedMatrix, State.InvertedTransform)
  end else
  begin
    if Log then
      WritelnLogMultiline('Matrix',
        'Cannot invert matrix:' + NL + MatrixToRawStr(FdMatrix.Value, '  '));

    { When determinant is zero, we treat inverted matrix
      like identity (like in TNodeMatrixTransform_1.Transformation).
      So no need to multiply State.InvertedTransform by anything. }
  end;

  State.AverageScaleTransform *= FdMatrix.AverageScaleTransform;
end;

function TNodeMatrixTransform_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeTeapot.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdsize := TSFVec3f.Create(Self, 'size', Vector3Single(3, 3, 3));
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeTeapot.ClassNodeTypeName: string;
begin
  Result := 'Teapot';
end;

function TNodeTeapot.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTeapot.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodeTeapot.Proxy: TVRMLGeometryNode;
const
  TeapotCoord {$I teapot_data/teapot_coord.inc};
  TeapotCoordIndex {$I teapot_data/teapot_coord_index.inc};
var
  CoordNode: TNodeCoordinate;
  IFS: TNodeIndexedFaceSet_2 absolute Result;
begin
  IFS := TNodeIndexedFaceSet_2.Create(NodeName, WWWBasePath);
  try
    CoordNode := TNodeCoordinate.Create('', WWWBasePath);
    IFS.FdCoord.Value := CoordNode;
    CoordNode.FdPoint.Items.AssignArray(TeapotCoord);

    { Scale Coords according to "size" field.
      Assumes that original coords are good for default size (3, 3, 3). }
    if not FdSize.EqualsDefaultValue then
    begin
      CoordNode.FdPoint.Items.MultiplyComponents(
        VectorScale(FdSize.Value, 1/3));
    end;

    IFS.FdCoordIndex.Items.AssignArray(TeapotCoordIndex);

    IFS.FdSolid.Value := FdSolid.Value;

    { Set CreaseAngle to anything larger than Pi, to make this completely
      smooth later using fast CreateSmoothNormalsCoordinateNode routine. }
    IFS.FdCreaseAngle.Value := 4;
  except FreeAndNil(Result); raise end;
end;

constructor TNodeLogger.Create(const ANodeName: string;
  const AWWWBasePath: string);
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
  FEventwrite.OnReceive.AppendItem(@EventWriteReceive);
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
    VRMLNonFatalError(
      'Logger node wants to write on standard output, but it seems that stdout (standard output) ' +
      'is not available. Under Windows you should explicitly ' +
      'redirect program''s stdout to make it available, e.g. ' +
      'run "' + ProgramName + ' > ' + ProgramName + '.log".');
  end;

  procedure WritelnLogMessage;
  var
    S: string;
    SaveProperties: TVRMLSaveToStreamProperties;
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

      SaveProperties := TVRMLSaveToStreamProperties.Create(WriteStream);
      try
        { Just use X3D 3.2 format. }
        SaveProperties.VerMajor := 3;
        SaveProperties.VerMinor := 2;

        Value.FieldSaveToStream(SaveProperties, true);
      finally FreeAndNil(SaveProperties) end;
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
          FNameAutoInc(ProgramName + '_logger_' +
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
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeLogger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

constructor TNodeConverter.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FEventIn := TVRMLEvent.Create(Self, 'in', TVRMLField, true);
  Events.Add(FEventIn);
  FEventIn.OnReceive.AppendItem(@EventInReceive);

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
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeConverter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure RegisterAvalonNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeMatrixTransform_2,
    TNodeTeapot,
    TNodeLogger,
    TNodeConverter
  ]);
end;
{$endif read_implementation}
