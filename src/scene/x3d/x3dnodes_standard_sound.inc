{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

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
    {$I auto_generated_node_helpers/x3dnodes_x3dsoundnode.inc}
  end;

  { Base class for most (but not all) nodes that can emit audio data.
    We may introduce TSoundSourceFunctionality is needed to express exactly X3DSoundSourceObject. }
  TAbstractSoundSourceNode = class(TAbstractTimeDependentNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dsoundsourcenode.inc}
  end;

  { Buffer for sound data, which can be played by the @link(TSoundNode). }
  TAudioClipNode = class(TAbstractSoundSourceNode)
  strict private
    FBufferLoaded: Boolean;
    InsideLoadBuffer: Cardinal;
    PlayingSound: TCastlePlayingSoundSource;
    procedure SetBufferLoaded(const Value: boolean);
    procedure UpdatePlayingEvent(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
    procedure UpdatePlaying;
    procedure SoundOpenClose(Sender: TObject);

    { Did we attempt loading Buffer. You still have to check Buffer <> 0 to know
      if it's successfully loaded. }
    property BufferLoaded: boolean read FBufferLoaded write SetBufferLoaded;
    { Final length (in seconds) of this sound, adjusted by pitch.
      See @link(TTimeDependentFunctionality.CycleInterval),
      this must return the relevant value. }
    function GetCycleInterval: TFloatTime;
    { Pitch, corrected to always be > 0. }
    function SafePitch: Single;
  protected
    procedure ParseAfter(Reader: TX3DReaderNames); override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;

    {$I auto_generated_node_helpers/x3dnodes_audioclip.inc}
  end;

  { 3D sound emitter. }
  TSoundNode = class(TAbstractSoundNode)
  private
    FTransform: TMatrix4;
    FTransformScale: Single;
    SoundSource: TCastleSoundSource;
    SoundSourceTransform: TCastleTransform;
    procedure UpdateSoundSource;
    procedure EventLocationReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    { Location expressed in Scene coordinate-space, that is: transformed
      by X3D transformations in this X3D scene graph.

      TODO: This may need to be renamed to WorldLocation and transformed by scene transformation.
      Test how it behaves in TCastleScene that has some transformation. }
    function SceneLocation: TVector3;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;

    {$I auto_generated_node_helpers/x3dnodes_sound.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAudioClipNode ------------------------------------------------------------- }

constructor TAudioClipNode.Create(const AX3DName: string; const ABaseUrl: string);
begin
  inherited;

  FTimeFunctionality.OnCycleInterval := {$ifdef FPC}@{$endif} GetCycleInterval;

  PlayingSound := TCastlePlayingSoundSource.Create(nil);
  PlayingSound.Sound := TCastleSound.Create(PlayingSound);

  EventIsActive.AddNotification({$ifdef FPC}@{$endif} UpdatePlayingEvent);
  EventIsPaused.AddNotification({$ifdef FPC}@{$endif} UpdatePlayingEvent);

  SoundEngine.OnOpenClose.Add({$ifdef FPC}@{$endif} SoundOpenClose);

  AddFunctionality(TUrlFunctionality.Create(Self));
end;

destructor TAudioClipNode.Destroy;
begin
  FreeAndNil(PlayingSound);
  SoundEngine.OnOpenClose.Remove({$ifdef FPC}@{$endif} SoundOpenClose);
  inherited;
end;

procedure TAudioClipNode.ParseAfter(Reader: TX3DReaderNames);
begin
  inherited;

  { We want to load buffer early, not on-demand (inside UpdatePlaying):
    - This avoids sudden delay when 1st playing sound,
    - It's necessary to make PlayingSound.Sound.Duration good,
      making CycleInterval good,
      which is necessary for time-dependent mechanism (in
      TTimeDependentFunctionality) to always work Ok (see e.g. at thunder.x3dv,
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

function TAudioClipNode.SafePitch: Single;
begin
  Result := Pitch;
  if Result <= 0 then
    Result := 1; { assign default pitch in case of trouble }
end;

function TAudioClipNode.GetCycleInterval: TFloatTime;
begin
  { Secure from invalid PlayingSound.Sound.Duration
    (when sound file is not loaded, or duration is just unknown). }
  if PlayingSound.Sound.Duration > 0 then
    Result := PlayingSound.Sound.Duration / SafePitch
  else
    Result := 0;
end;

procedure TAudioClipNode.UpdatePlayingEvent(const Event: TX3DEvent; const Value: TX3DField;
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
      Result := TSoundNode(ParentFields[0].ParentNode)
    else
      Result := nil;
  end;

var
  SoundNode: TSoundNode;
begin
  if FTimeFunctionality.IsActive and (not FTimeFunctionality.IsPaused) then
  begin
    { if not playing, try to start playing }
    if not PlayingSound.Playing then
    begin
      BufferLoaded := true;
      SoundNode := GetSoundNode;
      if SoundNode <> nil then
      begin
        PlayingSound.Loop := Loop;
        PlayingSound.Sound.Priority := SoundNode.Priority;
        PlayingSound.Sound.Volume := SoundNode.Intensity;
        PlayingSound.Sound.Pitch := SafePitch;
        { We pass min/maxFront as ReferenceDistance/MaxDistance
          as their meaning matches for SoundEngine.DistanceModel = dmLinear. }
        PlayingSound.Sound.ReferenceDistance := SoundNode.FTransformScale * SoundNode.MinFront;
        PlayingSound.Sound.MaxDistance       := SoundNode.FTransformScale * SoundNode.MaxFront;
        // TODO: we could set here Parameters.Offset to not always start playing from start
        SoundNode.SoundSource.Play(PlayingSound);
      end;
    end;
  end else
  begin
    { if playing, try to stop playing }
    PlayingSound.Stop;
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
            PlayingSound.Sound.URL := FullUrl;
          finally Dec(InsideLoadBuffer) end;

          EventDuration_Changed.Send(PlayingSound.Sound.Duration);
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
      Assert(not PlayingSound.Playing, 'You can unload a buffer only when Sound is not playing');
      PlayingSound.Sound.URL := '';
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

{ TSoundNode ----------------------------------------------------------------- }

constructor TSoundNode.Create(const AX3DName: string; const ABaseUrl: string);
begin
  inherited;
  FdLocation.AddNotification({$ifdef FPC}@{$endif} EventLocationReceive);
  FTransform := TMatrix4.Identity;
  FTransformScale := 1;

  SoundSource := TCastleSoundSource.Create(nil);
  SoundSource.SetTransient;

  SoundSourceTransform := TCastleTransform.Create(nil);
  SoundSourceTransform.SetTransient;
  SoundSourceTransform.AddBehavior(SoundSource);
end;

destructor TSoundNode.Destroy;
begin
  FreeAndNil(SoundSource);
  FreeAndNil(SoundSourceTransform);
  inherited;
end;

function TSoundNode.SceneLocation: TVector3;
begin
  Result := FTransform.MultPoint(Location);
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

procedure TSoundNode.UpdateSoundSource;
// var
//   AudioClip: TAudioClipNode;
begin
  { TODO: this should be called also when
    - Scene changes,
    - Scene.Parent changes
    - Spatialize value changes
  }

  if Scene <> SoundSourceTransform.Parent then
  begin
    if SoundSourceTransform.Parent <> nil then
      SoundSourceTransform.Parent.Remove(SoundSourceTransform);
    if Scene <> nil then
      Scene.Add(SoundSourceTransform);
  end;

  SoundSourceTransform.Translation := SceneLocation;
  SoundSource.Spatial := Spatialize;

  // if FdSource.Value is TAudioClipNode then // also checks FdSource.Value <> nil
  // begin
  //   AudioClip := TAudioClipNode(FdSource.Value);
  //   //AudioClip.PlayingSound ...
  // end;
end;

procedure TSoundNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  FTransform := StateStack.Top.Transformation.Transform;
  FTransformScale := StateStack.Top.Transformation.Scale;
  UpdateSoundSource;
end;

procedure TSoundNode.EventLocationReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  UpdateSoundSource;
end;

procedure RegisterSoundNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAudioClipNode,
    TSoundNode
  ]);
end;

{$endif read_implementation}
