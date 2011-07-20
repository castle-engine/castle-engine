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
    property Fddescription: TSFString { read FFddescription } { };
    property Fdpitch: TSFFloat { read FFdpitch } { };
    property Eventduration_changed: TVRMLEvent { read FEventduration_changed } { };
  end;

  TNodeX3DSoundSourceNode = class(TNodeX3DTimeDependentNode, INodeX3DSoundSourceNode)
  public
    procedure CreateNode; override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdpitch: TSFFloat;
    public property Fdpitch: TSFFloat read FFdpitch;

    { Event: SFTime, out } { }
    private FEventduration_changed: TVRMLEvent;
    public property Eventduration_changed: TVRMLEvent read FEventduration_changed;

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

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
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

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdintensity: TSFFloat;
    public property Fdintensity: TSFFloat read FFdintensity;

    private FFdlocation: TSFVec3f;
    public property Fdlocation: TSFVec3f read FFdlocation;

    private FFdmaxBack: TSFFloat;
    public property FdmaxBack: TSFFloat read FFdmaxBack;

    private FFdmaxFront: TSFFloat;
    public property FdmaxFront: TSFFloat read FFdmaxFront;

    private FFdminBack: TSFFloat;
    public property FdminBack: TSFFloat read FFdminBack;

    private FFdminFront: TSFFloat;
    public property FdminFront: TSFFloat read FFdminFront;

    private FFdpriority: TSFFloat;
    public property Fdpriority: TSFFloat read FFdpriority;

    private FFdsource: TSFNode;
    public property Fdsource: TSFNode read FFdsource;

    private FFdspatialize: TSFBool;
    public property Fdspatialize: TSFBool read FFdspatialize;
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

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdpitch := TSFFloat.Create(Self, 'pitch', 1.0);
  Fields.Add(FFdpitch);
  { X3D specification comment: (0,Inf) }

  FEventduration_changed := TVRMLEvent.Create(Self, 'duration_changed', TSFTime, false);
  Events.Add(FEventduration_changed);

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

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
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
    Result := FDuration / Pitch else
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
            { Remember that VRMLWarning *may* raise an exception. }
            VRMLWarning(vwIgnorable, Format(SLoadError,
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

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, 1));
  Fields.Add(FFddirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdintensity := TSFFloat.Create(Self, 'intensity', 1);
  Fields.Add(FFdintensity);
  { X3D specification comment: [0,1] }

  FFdlocation := TSFVec3f.Create(Self, 'location', Vector3Single(0, 0, 0));
   Fdlocation.OnReceive.Add(@EventLocationReceive);
  Fields.Add(FFdlocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdmaxBack := TSFFloat.Create(Self, 'maxBack', 10);
  Fields.Add(FFdmaxBack);
  { X3D specification comment: [0,Inf) }

  FFdmaxFront := TSFFloat.Create(Self, 'maxFront', 10);
  Fields.Add(FFdmaxFront);
  { X3D specification comment: [0,Inf) }

  FFdminBack := TSFFloat.Create(Self, 'minBack', 1);
  Fields.Add(FFdminBack);
  { X3D specification comment: [0,Inf) }

  FFdminFront := TSFFloat.Create(Self, 'minFront', 1);
  Fields.Add(FFdminFront);
  { X3D specification comment: [0,Inf) }

  FFdpriority := TSFFloat.Create(Self, 'priority', 0);
  Fields.Add(FFdpriority);
  { X3D specification comment: [0,1] }

  FFdsource := TSFNode.Create(Self, 'source', INodeX3DSoundSourceNode);
  Fields.Add(FFdsource);

  FFdspatialize := TSFBool.Create(Self, 'spatialize', true);
  FFdspatialize.Exposed := false;
  Fields.Add(FFdspatialize);

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
