{
  Copyright 2002-2011 Michalis Kamburelis.

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
  TNodeX3DSoundNode = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
  end;

  INodeX3DSoundSourceNode = interface(INodeX3DTimeDependentNode)
  ['{5E3D46A0-71B5-4561-B6B4-43A5BE5896EF}']
    property FdDescription: TSFString { read FFdDescription } { };
    property FdPitch: TSFFloat { read FFdPitch } { };
    property EventDuration_changed: TVRMLEvent { read FEventDuration_changed } { };
  end;

  TNodeX3DSoundSourceNode = class(TNodeX3DTimeDependentNode, INodeX3DSoundSourceNode)
  public
    procedure CreateNode; override;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdPitch: TSFFloat;
    public property FdPitch: TSFFloat read FFdPitch;

    { Event: SFTime, out } { }
    private FEventDuration_changed: TVRMLEvent;
    public property EventDuration_changed: TVRMLEvent read FEventDuration_changed;

    { Pitch, corrected to always be > 0. }
    function Pitch: Single;
  end;

  TNodeAudioClip = class(TNodeX3DSoundSourceNode, INodeX3DUrlObject)
  private
    FBufferLoaded: boolean;
    Buffer: TALBuffer;
    FDuration: TKamTime;
    Sound: TALSound;
    InsideLoadBuffer: Cardinal;
    procedure SetBufferLoaded(const Value: boolean);
    procedure SoundUsingEnd(ASound: TALSound);
    procedure UpdatePlayingEvent(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
    procedure UpdatePlaying;
    procedure ALOpenClose(Sender: TObject);

    { Did we attempt loading Buffer. You still have to check Buffer <> 0 to know
      if it's successfully loaded. }
    property BufferLoaded: boolean read FBufferLoaded write SetBufferLoaded;
  protected
    function CycleInterval: TKamTime; override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;
  end;

  TNodeSound = class(TNodeX3DSoundNode)
  private
    FTransform: TMatrix4Single;
    FTransformScale: Single;
    procedure UpdateSourcePosition;
    procedure EventLocationReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    private FFdLocation: TSFVec3f;
    public property FdLocation: TSFVec3f read FFdLocation;

    private FFdMaxBack: TSFFloat;
    public property FdMaxBack: TSFFloat read FFdMaxBack;

    private FFdMaxFront: TSFFloat;
    public property FdMaxFront: TSFFloat read FFdMaxFront;

    private FFdMinBack: TSFFloat;
    public property FdMinBack: TSFFloat read FFdMinBack;

    private FFdMinFront: TSFFloat;
    public property FdMinFront: TSFFloat read FFdMinFront;

    private FFdPriority: TSFFloat;
    public property FdPriority: TSFFloat read FFdPriority;

    private FFdSource: TSFNode;
    public property FdSource: TSFNode read FFdSource;

    private FFdSpatialize: TSFBool;
    public property FdSpatialize: TSFBool read FFdSpatialize;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DSoundNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TNodeX3DSoundSourceNode.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFdDescription);

  FFdPitch := TSFFloat.Create(Self, 'pitch', 1.0);
  Fields.Add(FFdPitch);
  { X3D specification comment: (0,Inf) }

  FEventDuration_changed := TVRMLEvent.Create(Self, 'duration_changed', TSFTime, false);
  Events.Add(FEventDuration_changed);

  DefaultContainerField := 'children';
end;

function TNodeX3DSoundSourceNode.Pitch: Single;
begin
  Result := FdPitch.Value;
  if Result <= 0 then
    Result := 1; { assign default pitch in case of trouble }
end;

procedure TNodeAudioClip.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdUrl);
  { X3D specification comment: [urn] }

  EventIsActive.OnReceive.Add(@UpdatePlayingEvent);
  EventIsPaused.OnReceive.Add(@UpdatePlayingEvent);

  { Although X3D XML encoding spec says that "children" is default
    containerField, common sense says it should be "source".
    (Sound.source field is where AudioClip should be placed.)
    Testcase: FISH.x3d }
  DefaultContainerField := 'source';

  FDuration := -1;

  SoundEngine.OnOpenClose.Add(@ALOpenClose);
end;

destructor TNodeAudioClip.Destroy;
begin
  if Sound <> nil then
  begin
    Sound.DoUsingEnd;
    Assert(Sound = nil);
  end;
  BufferLoaded := false;
  SoundEngine.OnOpenClose.Remove(@ALOpenClose);
  inherited;
end;

class function TNodeAudioClip.ClassNodeTypeName: string;
begin
  Result := 'AudioClip';
end;

class function TNodeAudioClip.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeAudioClip.CycleInterval: TKamTime;
begin
  { Secure from invalid FDuration (when sound file is not loaded, or duration
    is just unknown). }
  if FDuration > 0 then
    Result := FDUration / Pitch else
    Result := 0;
end;

procedure TNodeAudioClip.SoundUsingEnd(ASound: TALSound);
begin
  Sound := nil;
end;

procedure TNodeAudioClip.UpdatePlayingEvent(Event: TVRMLEvent; Value: TVRMLField;
  const Time: TVRMLTime);
begin
  UpdatePlaying;
end;

procedure TNodeAudioClip.UpdatePlaying;

  function GetSoundNode: TNodeSound;
  begin
    if (ParentFieldsCount <> 0) and
       (ParentFields[0].ParentNode <> nil) and
       (ParentFields[0].ParentNode is TNodeSound) then
      Result := TNodeSound(ParentFields[0].ParentNode);
  end;

var
  SoundNode: TNodeSound;
begin
  if TimeDependentNodeHandler.IsActive and not TimeDependentNodeHandler.IsPaused then
  begin
    { if not playing, try to start playing }
    if Sound = nil then
    begin
      BufferLoaded := true;
      SoundNode := GetSoundNode;
      if (Buffer <> 0) and (SoundNode <> nil) then
      begin
        Sound := SoundEngine.PlaySound(Buffer,
          SoundNode.FdSpatialize.Value,
          FdLoop.Value,
          Clamped(Round(SoundNode.FdPriority.Value * 1000), 0, 1000),
          SoundNode.FdIntensity.Value, 0, 1,
          MatrixMultPoint(SoundNode.FTransform, SoundNode.FdLocation.Value),
          Pitch,
          { We pass min/maxFront as OpenAL reference distance/max distance,
            as their meanings match for distance model = dmLinearDistanceClamped. }
          SoundNode.FTransformScale * SoundNode.FdMinFront.Value,
          SoundNode.FTransformScale * SoundNode.FdMaxFront.Value);
        if Sound <> nil then
          Sound.OnUsingEnd := @SoundUsingEnd;
      end;
    end;
  end else
  begin
    { if playing, try to stop playing }
    if Sound <> nil then
      Sound.DoUsingEnd;
  end;
end;

procedure TNodeAudioClip.SetBufferLoaded(const Value: boolean);
var
  I: Integer;
  FullUrl: string;
  LoadedDuration: TKamTime;
begin
  if FBufferLoaded <> Value then
  begin
    FBufferLoaded := Value;

    if Value then
    begin
      for I := 0 to FdUrl.Items.Count - 1 do
      begin
        FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
        try
          { LoadBuffer inside may cause OpenAL open, and we don't want to
            do ALOpenClose then.

            Doing OnOpenClose.Remove/Add now would be bad, as this code
            may be called when inside iterating over OnOpenClose:
            (- SetEnable calls ALContextOpen
             - that calls OnOpenClose.ExecuteAll
             - that calls Self.ALOpenClose
             - that calls UpdatePlaying
             - that loads the buffer
            ). Shuffling now OnOpenClose items (Add always adds at the end)
            would mean that the same callback is again processed,
            so Self.ALOpenClose is called with ALActive = true and Buffer <> 0,
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
            Buffer := SoundEngine.LoadBuffer(FullUrl, LoadedDuration);
          finally Dec(InsideLoadBuffer) end;

          FDuration := LoadedDuration;
          EventDuration_Changed.Send(FDuration);
          Break;
        except
          on E: Exception do
            { Remember that OnWarning *may* raise an exception. }
            OnWarning(wtMinor, 'VRML/X3D', Format(SLoadError,
              [E.ClassName, 'sound file', FullUrl, E.Message]));
        end;
      end;
    end else
    begin
      Assert(Sound = nil, 'You can unload a buffer only when Sound is not playing');
      SoundEngine.FreeBuffer(Buffer);
    end;
  end;
end;

procedure TNodeAudioClip.ALOpenClose(Sender: TObject);
begin
  if InsideLoadBuffer <> 0 then Exit;

  if (Sender as TALSoundEngine).ALActive then
  begin
    { Maybe we should start playing? }
    BufferLoaded := false; //< force reloading buffer next time
    UpdatePlaying;
  end else
  begin
    { Zero Buffer, since closing of OpenAL already freed it (and stopped the
      Sound). }
    Buffer := 0;
  end;
end;

procedure TNodeSound.CreateNode;
begin
  inherited;

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, 1));
  Fields.Add(FFdDirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdIntensity := TSFFloat.Create(Self, 'intensity', 1);
  Fields.Add(FFdIntensity);
  { X3D specification comment: [0,1] }

  FFdLocation := TSFVec3f.Create(Self, 'location', Vector3Single(0, 0, 0));
   FdLocation.OnReceive.Add(@EventLocationReceive);
  Fields.Add(FFdLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdMaxBack := TSFFloat.Create(Self, 'maxBack', 10);
  Fields.Add(FFdMaxBack);
  { X3D specification comment: [0,Inf) }

  FFdMaxFront := TSFFloat.Create(Self, 'maxFront', 10);
  Fields.Add(FFdMaxFront);
  { X3D specification comment: [0,Inf) }

  FFdMinBack := TSFFloat.Create(Self, 'minBack', 1);
  Fields.Add(FFdMinBack);
  { X3D specification comment: [0,Inf) }

  FFdMinFront := TSFFloat.Create(Self, 'minFront', 1);
  Fields.Add(FFdMinFront);
  { X3D specification comment: [0,Inf) }

  FFdPriority := TSFFloat.Create(Self, 'priority', 0);
  Fields.Add(FFdPriority);
  { X3D specification comment: [0,1] }

  FFdSource := TSFNode.Create(Self, 'source', INodeX3DSoundSourceNode);
  Fields.Add(FFdSource);

  FFdSpatialize := TSFBool.Create(Self, 'spatialize', true);
  FFdSpatialize.Exposed := false;
  Fields.Add(FFdSpatialize);

  DefaultContainerField := 'children';

  FTransform := IdentityMatrix4Single;
  FTransformScale := 1;
end;

procedure TNodeSound.UpdateSourcePosition;
var
  AudioClip: TNodeAudioClip;
begin
  if (FdSource.Value <> nil) and
     (FdSource.Value is TNodeAudioClip) then
  begin
    AudioClip := TNodeAudioClip(FdSource.Value);
    if AudioClip.Sound <> nil then
      AudioClip.Sound.Position := MatrixMultPoint(FTransform, FdLocation.Value);
  end;
end;

procedure TNodeSound.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  FTransform := StateStack.Top.Transform;
  FTransformScale := StateStack.Top.TransformScale;
  UpdateSourcePosition;
end;

procedure TNodeSound.EventLocationReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  UpdateSourcePosition;
end;

class function TNodeSound.ClassNodeTypeName: string;
begin
  Result := 'Sound';
end;

class function TNodeSound.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterSoundNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeAudioClip,
    TNodeSound
  ]);
end;

{$endif read_implementation}
