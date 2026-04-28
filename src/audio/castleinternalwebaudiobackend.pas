{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine backend using Web Audio API (for WebAssembly target).

  About Web Audio API: https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API

  Implementation uses FPC Job.Js (JavaScript Object Bridge) to call the browser's
  Web Audio API from Pascal/WebAssembly code.
  The Web Audio API types (like IJSAudioContext, IJSGainNode, IJSPannerNode
  etc.) are auto-generated from WebIDL definitions in CastleInternalJobWeb. }
unit CastleInternalWebAudioBackend;

{$I castleconf.inc}

{$ifndef WASI}
  {$message fatal 'This unit is only for WASI (WebAssembly) target.'}
{$endif}

interface

{ Use this to set sound engine backend to Web Audio. }
procedure UseWebAudioSoundBackend;

implementation

uses SysUtils, Classes, Math, Job.Js,
  CastleVectors, CastleTimeUtils, CastleLog, CastleUtils,
  CastleStringUtils, CastleInternalSoundFile, CastleInternalJobWeb,
  CastleUriUtils,
  CastleInternalAbstractSoundBackend, CastleSoundBase, CastleSoundEngine;

{ Sound backend classes ------------------------------------------------------ }

type
  TWebAudioSoundEngineBackend = class;

  { CGE sound buffer maps to WebAudio AudioBuffer, which is created from a TSoundFile.
    See https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/createBuffer . }
  TWebAudioSoundBufferBackend = class(TSoundBufferBackendFromSoundFile)
  private
    { JavaScript AudioBuffer object. }
    FAudioBuffer: IJSAudioBuffer;
  protected
    procedure ContextOpenFromSoundFile(const SoundFile: TSoundFile); override;
  public
    procedure ContextClose; override;
  end;

  { CGE sound source maps to WebAudio
    - AudioBufferSourceNode (recreated each time we play),
      see https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode
    - GainNode,
      see https://developer.mozilla.org/en-US/docs/Web/API/GainNode
    - PannerNode,
      see https://developer.mozilla.org/en-US/docs/Web/API/PannerNode . }
  TWebAudioSoundSourceBackend = class(TSoundSourceBackend)
  strict private
    FBuffer: TWebAudioSoundBufferBackend;

    { Current active source node, nil when not playing.
      Recreated on each Play because
      """An AudioBufferSourceNode can only be played once;"""
      (see https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode ) }
    SourceNode: IJSAudioBufferSourceNode;

    { Nodes created in ContextOpen. }
    FGainNode: IJSGainNode;
    FPannerNode: IJSPannerNode;

    { Current SourceNode propeties, stored also in fields because SourceNode
      is ephemeral. }
    FLoop: Boolean;
    FPitch: Single;

    { Whether this sound is spatial (3D) or not. If false, the PannerNode is
      bypassed and the sound is always centered.
      This determines how NodesConnect connects the nodes. }
    FSpatial: Boolean;

    { For tracking playback state. }
    FPlayStartTime: Double;
    FPlayOffset: Single;
    FPlaying: Boolean;

    function Backend: TWebAudioSoundEngineBackend;
    function GetAudioContext: IJSAudioContext;
    { Connect gain and panner nodes, based on FSpatial, to the master gain node.
      This is not dependent on SourceNode, which is recreated on each Play. }
    procedure NodesConnect;
    { Undo the work of NodesDisconnect. }
    procedure NodesDisconnect;
  public
    procedure ContextOpen; override;
    procedure ContextClose; override;
    function PlayingOrPaused: Boolean; override;
    procedure Play(const BufferChangedRecently: Boolean); override;
    procedure Stop; override;
    procedure SetPosition(const Value: TVector3); override;
    procedure SetVelocity(const Value: TVector3); override;
    procedure SetLoop(const Value: Boolean); override;
    procedure SetSpatial(const Value: Boolean); override;
    procedure SetVolume(const Value: Single); override;
    procedure SetMinGain(const Value: Single); override;
    procedure SetMaxGain(const Value: Single); override;
    procedure SetBuffer(const Value: TSoundBufferBackend); override;
    procedure SetPitch(const Value: Single); override;
    procedure SetReferenceDistance(const Value: Single); override;
    procedure SetMaxDistance(const Value: Single); override;
    procedure SetPriority(const Value: Single); override;
    function GetOffset: Single; override;
    procedure SetOffset(const Value: Single); override;
  end;

  { CGE sound engine maps to WebAudio AudioContext.
    See https://developer.mozilla.org/en-US/docs/Web/API/AudioContext . }
  TWebAudioSoundEngineBackend = class(TSoundEngineBackend)
  strict private
    FJSAudioContext: IJSAudioContext;
    FMasterGainNode: IJSGainNode;
    FDistanceModel: TSoundDistanceModel;
    function UserInteractionResumeAccepted(const aValue: Variant): Variant;
    function UserInteractionResumeRejected(const aValue: Variant): Variant;
  public
    property JSAudioContext: IJSAudioContext read FJSAudioContext;
    property MasterGainNode: IJSGainNode read FMasterGainNode;
    property DistanceModel: TSoundDistanceModel read FDistanceModel;

    function ContextOpen(const ADevice: String;
      out Information, InformationSummary: String): Boolean; override;
    procedure ContextClose; override;
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; override;
    function CreateSource: TSoundSourceBackend; override;
    procedure SetVolume(const Value: Single); override;
    procedure SetDistanceModel(const Value: TSoundDistanceModel); override;
    procedure SetDopplerFactor(const Value: Single); override;
    procedure SetListener(const Position, Direction, Up: TVector3); override;

    { Call this to resume playback when user interacts with the page.
      This is needed to comply with browser autoplay policies,
      which require a user interaction to start audio playback.
      See https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Best_practices#autoplay_policy }
    procedure UserInteraction; override;
  end;

{ TWebAudioSoundBufferBackend ------------------------------------------------ }

procedure TWebAudioSoundBufferBackend.ContextOpenFromSoundFile(const SoundFile: TSoundFile);
const
  NumChannelsForFormat: array [TSoundDataFormat] of Cardinal = (1, 1, 2, 2);
  SampleSizeForFormat: array [TSoundDataFormat] of Cardinal = (1, 2, 2, 4);
var
  NumChannels, NumFrames, SampleSize: Cardinal;
  Backend: TWebAudioSoundEngineBackend;
  ChannelData: IJSFloat32Array;
  ChannelDataSingle: TSingleList;
  I: Integer;
  Channel: Integer;
  SampleInt8: Int8;
  SampleInt16: Int16;
begin
  inherited;

  Backend := SoundEngine as TWebAudioSoundEngineBackend;
  NumChannels := NumChannelsForFormat[SoundFile.DataFormat];
  SampleSize := SampleSizeForFormat[SoundFile.DataFormat];

  NumFrames := SoundFile.DataSize div SampleSize;
  if NumFrames = 0 then
    raise Exception.CreateFmt('Sound file "%s" has no audio frames', [UriDisplay(Url)]);

  { See https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/createBuffer }
  FAudioBuffer := Backend.JSAudioContext.CreateBuffer(
    NumChannels, NumFrames, SoundFile.Frequency);

  { For each channel, to TSingleList and copy to JavaScript Float32Array }
  ChannelDataSingle := TSingleList.Create;
  try
    ChannelDataSingle.Count := NumFrames;

    for Channel := 0 to NumChannels - 1 do
    begin
      case SoundFile.DataFormat of
        sfMono8, sfStereo8:
          begin
            // Int8 for each channel, convert to float in range [-1.0, 1.0]
            for I := 0 to NumFrames - 1 do
            begin
              SampleInt8 := PInt8(SoundFile.Data)[I * SampleSize + Channel * SizeOf(Int8)];
              ChannelDataSingle[I] := SampleInt8 / High(Int8);
            end;
          end;
        sfMono16, sfStereo16:
          begin
            // Int16 for each channel, convert to float in range [-1.0, 1.0]
            for I := 0 to NumFrames - 1 do
            begin
              SampleInt16 := PInt16(SoundFile.Data)[I * SampleSize + Channel * SizeOf(Int16)];
              ChannelDataSingle[I] := SampleInt16 / High(Int16);
            end;
          end;
        else raise Exception.CreateFmt('Unsupported sound data format %d', [Ord(SoundFile.DataFormat)]);
      end;
    end;

    { Get the Float32Array for this channel }
    ChannelData := FAudioBuffer.getChannelData(Channel);
    ChannelData.CopyFromMemory(PByte(ChannelDataSingle.L), NumFrames * SizeOf(Single));
  finally
    FreeAndNil(ChannelDataSingle);
  end;
end;

procedure TWebAudioSoundBufferBackend.ContextClose;
begin
  FAudioBuffer := nil;
  inherited;
end;

{ TWebAudioSoundSourceBackend ------------------------------------------------ }

function TWebAudioSoundSourceBackend.Backend: TWebAudioSoundEngineBackend;
begin
  Result := SoundEngine as TWebAudioSoundEngineBackend;
end;

function TWebAudioSoundSourceBackend.GetAudioContext: IJSAudioContext;
begin
  Result := Backend.JSAudioContext;
end;

procedure TWebAudioSoundSourceBackend.NodesConnect;
begin
  Assert(FGainNode <> nil);
  Assert(FPannerNode <> nil);

  if FSpatial then
  begin
    { GainNode -> PannerNode -> MasterGainNode }
    FGainNode.Connect(FPannerNode);
    FPannerNode.Connect(Backend.MasterGainNode);
  end else
  begin
    { GainNode -> MasterGainNode (bypass PannerNode) }
    FGainNode.Connect(Backend.MasterGainNode);
  end;
end;

procedure TWebAudioSoundSourceBackend.NodesDisconnect;
begin
  { This may be done from finalization, so take into account that nodes may
    already be nil. }
  if FGainNode <> nil then
    FGainNode.Disconnect;
  if FPannerNode <> nil then
    FPannerNode.Disconnect;
end;

procedure TWebAudioSoundSourceBackend.ContextOpen;
const
  DistanceModelStr: array [TSoundDistanceModel] of UnicodeString = (
    'inverse',
    'linear'
  );
var
  AudioCtx: IJSAudioContext;
begin
  AudioCtx := GetAudioContext;

  { Create per-source GainNode for volume control }
  FGainNode := AudioCtx.CreateGain;

  { Create PannerNode for spatial audio }
  FPannerNode := AudioCtx.CreatePanner;

  { Set distance model on PannerNode based on engine setting }
  FPannerNode.DistanceModel := DistanceModelStr[Backend.DistanceModel];

  { Set defaults }
  FPitch := 1.0;
  FSpatial := true;
  FLoop := false;
  FPlaying := false;
  FPlayOffset := 0;

  NodesConnect;
end;

procedure TWebAudioSoundSourceBackend.ContextClose;
begin
  Stop;

  NodesDisconnect;
  FGainNode := nil;
  FPannerNode := nil;
end;

function TWebAudioSoundSourceBackend.PlayingOrPaused: Boolean;
var
  CurrentTime, Elapsed, BufferDuration: Double;
begin
  if (not FPlaying) or (FBuffer = nil) then
    Exit(false);

  { Looping sounds never stop on their own }
  if FLoop then
    Exit(true);

  { Check if the sound has finished playing based on elapsed time.
    The code below possibly changes FPlaying from true to false,
    if the sound has finished.
    And then we always return the current value of FPlaying. }

  CurrentTime := GetAudioContext.CurrentTime;
  Elapsed := CurrentTime - FPlayStartTime;

  BufferDuration := FBuffer.Duration;
  { Account for pitch: actual duration = bufferDuration / pitch }
  if FPitch > 0.001 then
    BufferDuration := BufferDuration / FPitch;

  FPlaying := Elapsed < BufferDuration;
  Result := FPlaying;
end;

procedure TWebAudioSoundSourceBackend.Play(const BufferChangedRecently: Boolean);
var
  AudioCtx: IJSAudioContext;
begin
  Stop;

  if (FBuffer = nil) or (FBuffer.FAudioBuffer = nil) then
    Exit;

  AudioCtx := GetAudioContext;

  { Create a new AudioBufferSourceNode (they are single-use in WebAudio) }
  SourceNode := AudioCtx.CreateBufferSource;
  SourceNode.Buffer := FBuffer.FAudioBuffer;
  SourceNode.Loop := FLoop;
  SourceNode.PlaybackRate.Value := FPitch;

  { Connect: SourceNode -> our GainNode (which is already connected to the rest
    of the audio graph by NodesConnect) }
  SourceNode.Connect(FGainNode);

  { Start playback }
  if FPlayOffset > 0 then
    SourceNode.Start(0, FPlayOffset)
  else
    SourceNode.Start;

  { Record start time for PlayingOrPaused tracking }
  FPlayStartTime := AudioCtx.CurrentTime;
  FPlaying := true;
  FPlayOffset := 0;
end;

procedure TWebAudioSoundSourceBackend.Stop;
begin
  if FPlaying and (SourceNode <> nil) then
  begin
    SourceNode.Stop;
    SourceNode.Disconnect;
  end;
  SourceNode := nil;
  FPlaying := false;
end;

procedure TWebAudioSoundSourceBackend.SetPosition(const Value: TVector3);
begin
  Assert(FPannerNode <> nil);
  FPannerNode.SetPosition(Value.X, Value.Y, Value.Z);
end;

procedure TWebAudioSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
  { Doppler was removed from the Web Audio API specification.
\
    See
    https://github.com/emscripten-core/emscripten/issues/4587
    https://github.com/WebAudio/web-audio-api/issues/372
    https://github.com/WebAudio/web-audio-api/issues/455
    Removed in Firefox 63:
    https://developer.mozilla.org/en-US/docs/Mozilla/Firefox/Releases/63
    """
    Since it was deprecated in the specification anyway, the limited support
    for Doppler effects on PannerNode has been removed from the Web Audio API.
    The AudioListener properties dopplerFactor and speedOfSound have been removed,
    along with the PannerNode method setVelocity() (Firefox bug 1148354).
    """

    TODO: implement doppler on our side.
  }
end;

procedure TWebAudioSoundSourceBackend.SetLoop(const Value: Boolean);
begin
  FLoop := Value;
  if SourceNode <> nil then // SourceNode may be nil if not currently playing
    SourceNode.Loop := Value;
end;

procedure TWebAudioSoundSourceBackend.SetSpatial(const Value: Boolean);
begin
  if FSpatial <> Value then
  begin
    FSpatial := Value;
    NodesDisconnect;
    NodesConnect;
  end;
end;

procedure TWebAudioSoundSourceBackend.SetVolume(const Value: Single);
begin
  Assert(FGainNode <> nil);
  FGainNode.Gain.Value := Value;
end;

procedure TWebAudioSoundSourceBackend.SetMinGain(const Value: Single);
begin
  { WebAudio does not have a direct minGain equivalent on GainNode.
    We could clamp in SetVolume, but for now this is a no-op
    like in the SOX backend. }
end;

procedure TWebAudioSoundSourceBackend.SetMaxGain(const Value: Single);
begin
  { No direct equivalent in WebAudio. }
end;

procedure TWebAudioSoundSourceBackend.SetBuffer(const Value: TSoundBufferBackend);
begin
  FBuffer := Value as TWebAudioSoundBufferBackend;
end;

procedure TWebAudioSoundSourceBackend.SetPitch(const Value: Single);
begin
  FPitch := Value;
  if SourceNode <> nil then // SourceNode may be nil if not currently playing
    SourceNode.PlaybackRate.Value := Value;
end;

procedure TWebAudioSoundSourceBackend.SetReferenceDistance(const Value: Single);
begin
  Assert(FPannerNode <> nil);
  FPannerNode.RefDistance := Value;
end;

procedure TWebAudioSoundSourceBackend.SetMaxDistance(const Value: Single);
begin
  Assert(FPannerNode <> nil);
  FPannerNode.MaxDistance := Value;
end;

procedure TWebAudioSoundSourceBackend.SetPriority(const Value: Single);
begin
  { Priority is handled by the WebAudio allocator, not the backend. }
end;

function TWebAudioSoundSourceBackend.GetOffset: Single;
var
  CurrentTime, Elapsed: Double;
begin
  if FPlaying then
  begin
    CurrentTime := GetAudioContext.currentTime;
    Elapsed := CurrentTime - FPlayStartTime + FPlayOffset;
    if (FBuffer <> nil) and (FBuffer.Duration > 0) and FLoop then
    begin
      { For looping sounds, wrap around }
      Result := FloatModulo(Elapsed, FBuffer.Duration);
    end else
      Result := Elapsed;
  end else
    Result := 0;
end;

procedure TWebAudioSoundSourceBackend.SetOffset(const Value: Single);
begin
  FPlayOffset := Value;
  { If currently playing, restart with new offset }
  if FPlaying then
    Play(false);
end;

{ TWebAudioSoundEngineBackend ------------------------------------------------ }

function TWebAudioSoundEngineBackend.ContextOpen(const ADevice: String;
  out Information, InformationSummary: String): Boolean;
begin
  FJSAudioContext := TJSAudioContext.Create;

  { Create master gain node for global volume control }
  FMasterGainNode := FJSAudioContext.CreateGain;

  { Connect master gain to destination }
  FMasterGainNode.Connect(FJSAudioContext.Destination);

  { See https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Best_practices#autoplay_policy
    for a description of browser autoplay policies. }
  // TODO: Expose SoundEngine.Resume, doing FJSAudioContext.Resume
  if FJSAudioContext.State = 'suspended' then
    WritelnWarning('Web Audio context is suspended. You must call "SoundEngine.Resume" from an event considered "user interaction" by browsers (like a mouse click) to resume playback.');

  Result := true;
  Information := FormatDot('Web Audio context initialized successfully.' + NL +
    'Sample rate: %f', [
      FJSAudioContext.SampleRate
    ]);
  InformationSummary := 'Web Audio';
end;

procedure TWebAudioSoundEngineBackend.UserInteraction;
var
  ResumePromise: IJSPromise;
begin
  if (FJSAudioContext <> nil) and (FJSAudioContext.State = 'suspended') then
  begin
    WritelnLog('Web Audio context is suspended. Attempting to resume due to user interaction.');
    ResumePromise := FJSAudioContext.Resume;
    ResumePromise._Then(
      @UserInteractionResumeAccepted,
      @UserInteractionResumeRejected
    );
  end;
end;

function TWebAudioSoundEngineBackend.UserInteractionResumeAccepted(const aValue: Variant): Variant;
begin
  WritelnLog('Web Audio context resumed successfully after user interaction.');
  Result := aValue;
end;

function TWebAudioSoundEngineBackend.UserInteractionResumeRejected(const aValue: Variant): Variant;
begin
  WritelnWarning('Failed to resume Web Audio context after user interaction. Sound may not play until the user interacts with the page again.');
  Result := aValue;
end;

procedure TWebAudioSoundEngineBackend.ContextClose;
begin
  if FMasterGainNode <> nil then
  begin
    FMasterGainNode.Disconnect;
    FMasterGainNode := nil;
  end;
  if FJSAudioContext <> nil then
  begin
    FJSAudioContext.Close;
    FJSAudioContext := nil;
  end;
end;

function TWebAudioSoundEngineBackend.CreateBuffer(
  const SoundLoading: TSoundLoading): TSoundBufferBackend;
begin
  { Ignoring SoundLoading -- WebAudio loads everything completely,
    no streaming support in this backend. }
  Result := TWebAudioSoundBufferBackend.Create(Self);
end;

function TWebAudioSoundEngineBackend.CreateSource: TSoundSourceBackend;
begin
  Result := TWebAudioSoundSourceBackend.Create(Self);
end;

procedure TWebAudioSoundEngineBackend.SetVolume(const Value: Single);
begin
  Assert(FMasterGainNode <> nil);
  FMasterGainNode.Gain.Value := Value;
end;

procedure TWebAudioSoundEngineBackend.SetDistanceModel(
  const Value: TSoundDistanceModel);
begin
  FDistanceModel := Value;
  { When creating source (in TWebAudioSoundSourceBackend.ContextOpen),
    we will configure the PannerNode's distanceModel based on this value.
    TODO: Changing DistanceModel while sources are active does not update
    existing PannerNodes. }
end;

procedure TWebAudioSoundEngineBackend.SetDopplerFactor(const Value: Single);
begin
  // TODO
end;

procedure TWebAudioSoundEngineBackend.SetListener(
  const Position, Direction, Up: TVector3);
var
  Listener: IJSAudioListener;
begin
  Assert(FJSAudioContext <> nil);
  Listener := FJSAudioContext.Listener;

  { Use the deprecated SetPosition/SetOrientation methods for maximum compatibility,
    as https://developer.mozilla.org/en-US/docs/Web/API/AudioListener
    says
    """
    Although these methods are deprecated they are currently the only way to
    set the orientation and position in Firefox (see Firefox bug 1283029).
    """
    and refers to https://bugzilla.mozilla.org/show_bug.cgi?id=1283029
  }
  Listener.SetPosition(Position.X, Position.Y, Position.Z);
  Listener.SetOrientation(
    Direction.X, Direction.Y, Direction.Z,
    Up.X, Up.Y, Up.Z);
end;

{ globals -------------------------------------------------------------------- }

procedure UseWebAudioSoundBackend;
begin
  SoundEngine.InternalBackend := TWebAudioSoundEngineBackend.Create;
end;

end.
