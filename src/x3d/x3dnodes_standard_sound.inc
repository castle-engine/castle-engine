{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base for all sound nodes. }
  TAbstractSoundNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dsoundnode.inc}
  end;

  { Interface for all nodes that can emit audio data. }
  IAbstractSoundSourceNode = interface(IAbstractTimeDependentNode)
  ['{5E3D46A0-71B5-4561-B6B4-43A5BE5896EF}']
    property FdDescription: TSFString { read FFdDescription } { };
    property FdPitch: TSFFloat { read FFdPitch } { };
    property EventDuration_changed: TX3DEvent { read FEventDuration_changed } { };
  end;

  { Base class for (most) nodes that can emit audio data. }
  TAbstractSoundSourceNode = class(TAbstractTimeDependentNode, IAbstractSoundSourceNode)
  public
    procedure CreateNode; override;

    strict private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    strict private FFdPitch: TSFFloat;
    public property FdPitch: TSFFloat read FFdPitch;

    { Event out } { }
    strict private FEventDuration_changed: TSFTimeEvent;
    public property EventDuration_changed: TSFTimeEvent read FEventDuration_changed;

    { Pitch, corrected to always be > 0. }
    function SafePitch: Single;

    {$I auto_generated_node_helpers/x3dnodes_x3dsoundsourcenode.inc}
  end;

  { Buffer for sound data, which can be played by the @link(TSoundNode). }
  TAudioClipNode = class(TAbstractSoundSourceNode, IAbstractUrlObject)
  strict private
    FBufferLoaded: boolean;
    Buffer: TSoundBuffer;
    FDuration: TFloatTime;
    InsideLoadBuffer: Cardinal;
    procedure SetBufferLoaded(const Value: boolean);
    procedure SoundRelease(ASound: TSound);
    procedure UpdatePlayingEvent(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
    procedure UpdatePlaying;
    procedure SoundOpenClose(Sender: TObject);

    { Did we attempt loading Buffer. You still have to check Buffer <> 0 to know
      if it's successfully loaded. }
    property BufferLoaded: boolean read FBufferLoaded write SetBufferLoaded;
  private
    Sound: TSound;
  protected
    procedure ParseAfter(Reader: TX3DReaderNames); override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function GetCycleInterval: TFloatTime; override;

    strict private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    {$I auto_generated_node_helpers/x3dnodes_audioclip.inc}
  end;

  { 3D sound emitter. }
  TSoundNode = class(TAbstractSoundNode)
  private
    FTransform: TMatrix4;
    FTransformScale: Single;
    procedure UpdateSourcePosition;
    procedure EventLocationReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    strict private FFdLocation: TSFVec3f;
    public property FdLocation: TSFVec3f read FFdLocation;

    strict private FFdMaxBack: TSFFloat;
    public property FdMaxBack: TSFFloat read FFdMaxBack;

    strict private FFdMaxFront: TSFFloat;
    public property FdMaxFront: TSFFloat read FFdMaxFront;

    strict private FFdMinBack: TSFFloat;
    public property FdMinBack: TSFFloat read FFdMinBack;

    strict private FFdMinFront: TSFFloat;
    public property FdMinFront: TSFFloat read FFdMinFront;

    strict private FFdPriority: TSFFloat;
    public property FdPriority: TSFFloat read FFdPriority;

    strict private FFdSource: TSFNode;
    public property FdSource: TSFNode read FFdSource;

    strict private FFdSpatialize: TSFBool;
    public property FdSpatialize: TSFBool read FFdSpatialize;

    {$I auto_generated_node_helpers/x3dnodes_sound.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractSoundNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TAbstractSoundSourceNode.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, true, 'description', '');
  AddField(FFdDescription);

  FFdPitch := TSFFloat.Create(Self, true, 'pitch', 1.0);
  AddField(FFdPitch);
  { X3D specification comment: (0,Inf) }

  FEventDuration_changed := TSFTimeEvent.Create(Self, 'duration_changed', false);
  AddEvent(FEventDuration_changed);

  DefaultContainerField := 'children';
end;

function TAbstractSoundSourceNode.SafePitch: Single;
begin
  Result := Pitch;
  if Result <= 0 then
    Result := 1; { assign default pitch in case of trouble }
end;

procedure TAudioClipNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, true, 'url', []);
  AddField(FFdUrl);
  { X3D specification comment: [urn] }

  EventIsActive.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} UpdatePlayingEvent);
  EventIsPaused.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} UpdatePlayingEvent);

  { Although X3D XML encoding spec says that "children" is default
    containerField, common sense says it should be "source".
    (Sound.source field is where AudioClip should be placed.)
    Testcase: FISH.x3d }
  DefaultContainerField := 'source';

  FDuration := -1;

  SoundEngine.OnOpenClose.Add({$ifdef CASTLE_OBJFPC}@{$endif} SoundOpenClose);
end;

destructor TAudioClipNode.Destroy;
begin
  if Sound <> nil then
  begin
    Sound.Release;
    Assert(Sound = nil);
  end;
  BufferLoaded := false;
  SoundEngine.OnOpenClose.Remove({$ifdef CASTLE_OBJFPC}@{$endif} SoundOpenClose);
  inherited;
end;

class function TAudioClipNode.ClassX3DType: string;
begin
  Result := 'AudioClip';
end;

class function TAudioClipNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TAudioClipNode.ParseAfter(Reader: TX3DReaderNames);
begin
  inherited;

  { We want to load buffer early, not on-demand (inside UpdatePlaying):
    - This avoids sudden delay when 1st playing sound,
    - It's necessary to make FDuration good, making CycleInterval good,
      which is necessary for time-dependent mechanism (in
      TInternalTimeDependentHandler) to always work Ok (see e.g. at thunder.x3dv,
      when AudioClip.startTime is set by script to future time.

    But don't do it if URL field is empty.
    This is important in case "url" field value
    has an IS clause, and it will be really known only after expanding
    the PROTO, see thunder.x3dv. }
  if FdUrl.Count <> 0 then
    BufferLoaded := true;
end;

procedure TAudioClipNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  { Reasons for doing this: see ParseAfter }
  if FdUrl.Count <> 0 then
    BufferLoaded := true;
end;

function TAudioClipNode.GetCycleInterval: TFloatTime;
begin
  { Secure from invalid FDuration (when sound file is not loaded, or duration
    is just unknown). }
  if FDuration > 0 then
    Result := FDuration / SafePitch
  else
    Result := 0;
end;

procedure TAudioClipNode.SoundRelease(ASound: TSound);
begin
  Sound := nil;
end;

procedure TAudioClipNode.UpdatePlayingEvent(Event: TX3DEvent; Value: TX3DField;
  const Time: TX3DTime);
begin
  UpdatePlaying;
end;

procedure TAudioClipNode.UpdatePlaying;

  function GetSoundNode: TSoundNode;
  begin
    if (ParentFieldsCount <> 0) and
       (ParentFields[0].ParentNode <> nil) and
       (ParentFields[0].ParentNode is TSoundNode) then
      Result := TSoundNode(ParentFields[0].ParentNode);
  end;

var
  SoundNode: TSoundNode;
  Parameters: TSoundParameters;
begin
  if IsActive and not IsPaused then
  begin
    { if not playing, try to start playing }
    if Sound = nil then
    begin
      BufferLoaded := true;
      SoundNode := GetSoundNode;
      if (Buffer <> nil) and (SoundNode <> nil) then
      begin
        Parameters := TSoundParameters.Create;
        try
          Parameters.Buffer := Buffer;
          Parameters.Spatial := SoundNode.Spatialize;
          Parameters.Looping := Loop;
          Parameters.Importance := Clamped(Round(SoundNode.Priority * 1000), 0, 1000);
          Parameters.Gain := SoundNode.Intensity;
          Parameters.Position := SoundNode.FTransform.MultPoint(SoundNode.Location);
          Parameters.Pitch := SafePitch;
          { We pass min/maxFront as OpenAL reference distance/max distance,
            as their meanings match for distance model = dmLinearDistanceClamped. }
          Parameters.ReferenceDistance := SoundNode.FTransformScale * SoundNode.MinFront;
          Parameters.MaxDistance       := SoundNode.FTransformScale * SoundNode.MaxFront;
          // TODO: we could set here Parameters.Offset to not always start playing from start
          Sound := SoundEngine.PlaySound(Parameters);
        finally FreeAndNil(Parameters) end;
        if Sound <> nil then
          Sound.OnRelease := {$ifdef CASTLE_OBJFPC}@{$endif} SoundRelease;
      end;
    end;
  end else
  begin
    { if playing, try to stop playing }
    if Sound <> nil then
      Sound.Release;
  end;
end;

procedure TAudioClipNode.SetBufferLoaded(const Value: boolean);
var
  I: Integer;
  FullUrl: string;
begin
  if FBufferLoaded <> Value then
  begin
    FBufferLoaded := Value;

    if Value then
    begin
      for I := 0 to FdUrl.Items.Count - 1 do
      begin
        FullUrl := PathFromBaseUrl(FdUrl.Items[I]);
        try
          { LoadBuffer inside may cause OpenAL open, and we don't want to
            do SoundOpenClose then.

            Doing OnOpenClose.Remove/Add now would be bad, as this code
            may be called when inside iterating over OnOpenClose:
            (- SetEnable calls ALContextOpen
             - that calls OnOpenClose.ExecuteAll
             - that calls Self.SoundOpenClose
             - that calls UpdatePlaying
             - that loads the buffer
            ). Shuffling now OnOpenClose items (Add always adds at the end)
            would mean that the same callback is again processed,
            so Self.SoundOpenClose is called with IsContextOpenSuccess = true and Buffer <> 0,
            so ALContextOpen releases a buffer for a playing sound source
            so OpenAL raises error.

            Testcase:
            - start view3dscene with sound = on
            - click CW on all 4 speakers in sound_final.x3dv
            - turn sound off
            - turn sound back on
            - click on lower-bottom speakers }
          Inc(InsideLoadBuffer);
          try
            Buffer := SoundEngine.LoadBuffer(FullUrl, slComplete);
            FDuration := Buffer.Duration;
          finally Dec(InsideLoadBuffer) end;

          EventDuration_Changed.Send(FDuration);
          Break;
        except
          on E: Exception do
            { Remember that WritelnWarning *may* raise an exception. }
            WritelnWarning('Audio', Format(SLoadError,
              [E.ClassName, 'sound file', URIDisplay(FullUrl), E.Message]));
        end;
      end;
    end else
    begin
      Assert(Sound = nil, 'You can unload a buffer only when Sound is not playing');
      SoundEngine.FreeBuffer(Buffer);
    end;
  end;
end;

procedure TAudioClipNode.SoundOpenClose(Sender: TObject);
begin
  if InsideLoadBuffer <> 0 then Exit;

  if (Sender as TSoundEngine).IsContextOpenSuccess then
  begin
    { Maybe we should start playing? }
    BufferLoaded := false; //< force reloading buffer next time
    UpdatePlaying;
  end;
end;

procedure TSoundNode.CreateNode;
begin
  inherited;

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 0, 1));
  AddField(FFdDirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdIntensity := TSFFloat.Create(Self, true, 'intensity', 1);
  AddField(FFdIntensity);
  { X3D specification comment: [0,1] }

  FFdLocation := TSFVec3f.Create(Self, true, 'location', Vector3(0, 0, 0));
   FdLocation.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventLocationReceive);
  AddField(FFdLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdMaxBack := TSFFloat.Create(Self, true, 'maxBack', 10);
  AddField(FFdMaxBack);
  { X3D specification comment: [0,Inf) }

  FFdMaxFront := TSFFloat.Create(Self, true, 'maxFront', 10);
  AddField(FFdMaxFront);
  { X3D specification comment: [0,Inf) }

  FFdMinBack := TSFFloat.Create(Self, true, 'minBack', 1);
  AddField(FFdMinBack);
  { X3D specification comment: [0,Inf) }

  FFdMinFront := TSFFloat.Create(Self, true, 'minFront', 1);
  AddField(FFdMinFront);
  { X3D specification comment: [0,Inf) }

  FFdPriority := TSFFloat.Create(Self, true, 'priority', 0);
  AddField(FFdPriority);
  { X3D specification comment: [0,1] }

  FFdSource := TSFNode.Create(Self, true, 'source', IAbstractSoundSourceNode);
  AddField(FFdSource);

  FFdSpatialize := TSFBool.Create(Self, false, 'spatialize', true);
  AddField(FFdSpatialize);

  DefaultContainerField := 'children';

  FTransform := TMatrix4.Identity;
  FTransformScale := 1;
end;

function TSoundNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  { Necessary in order to call TAudioClipNode.BeforeTraverse,
    testcase: thunder.x3dv. }
  Result := FdSource.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TSoundNode.UpdateSourcePosition;
var
  AudioClip: TAudioClipNode;
begin
  if (FdSource.Value <> nil) and
     (FdSource.Value is TAudioClipNode) then
  begin
    AudioClip := TAudioClipNode(FdSource.Value);
    if AudioClip.Sound <> nil then
      AudioClip.Sound.Position := FTransform.MultPoint(FdLocation.Value);
  end;
end;

procedure TSoundNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  FTransform := StateStack.Top.Transform;
  FTransformScale := StateStack.Top.TransformScale;
  UpdateSourcePosition;
end;

procedure TSoundNode.EventLocationReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  UpdateSourcePosition;
end;

class function TSoundNode.ClassX3DType: string;
begin
  Result := 'Sound';
end;

class function TSoundNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterSoundNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAudioClipNode,
    TSoundNode
  ]);
end;

{$endif read_implementation}
