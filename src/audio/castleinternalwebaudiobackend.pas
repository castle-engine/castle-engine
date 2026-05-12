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

var
  { Set to @true for extra logging. }
  WebAudioVerboseLog: Boolean = false;

{ Use this to set sound engine backend to Web Audio. }
procedure UseWebAudioSoundBackend;

implementation

uses SysUtils, Classes, Math, Job.Js, Generics.Collections,
  CastleVectors, CastleTimeUtils, CastleLog, CastleUtils,
  CastleStringUtils, CastleInternalSoundFile, CastleInternalJobWeb,
  CastleUriUtils, CastleDownload, CastleInternalWebUtils,
  CastleInternalAbstractSoundBackend, CastleSoundBase, CastleSoundEngine;

{ Sound backend classes ------------------------------------------------------ }

type
  TWebAudioSoundEngineBackend = class;
  TWebAudioSoundSourceBackend = class;
  TWebAudioSoundSourceBackendList = {$ifdef FPC}specialize{$endif} TObjectList<TWebAudioSoundSourceBackend>;

  { CGE sound buffer maps to WebAudio AudioBuffer, which is created from a TSoundFile.
    See https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/createBuffer .
    We descend from TSoundBufferBackend , not TSoundBufferBackendFromSoundFile,
    because we use TSoundFile only for some formats (WAV)
    and rely on browser's support for other formats (like OGG, MP3). }
  TWebAudioSoundBufferBackend = class(TSoundBufferBackend)
  strict private
    FDuration: TFloatTime;
    FChannels: Cardinal;
    FFrequency: TSoundFrequency;
    { JavaScript AudioBuffer object. }
    FAudioBuffer: IJSAudioBuffer;
    FPendingDecode: Boolean;
    FSources: TWebAudioSoundSourceBackendList;
    FDecodingPromise: IJSPromise;
    function DecodeAudioDataSuccess(const aValue: Variant): Variant;
    function DecodeAudioDataError(const aValue: Variant): Variant;
  public
    constructor Create(const ASoundEngine: TSoundEngineBackend);
    destructor Destroy; override;

    procedure ContextOpen(const AUrl: String); override;
    procedure ContextClose; override;

    function Duration: TFloatTime; override;
    function Channels: Cardinal; override;
    function Frequency: TSoundFrequency; override;

    property AudioBuffer: IJSAudioBuffer read FAudioBuffer;

    { Decoding of non-WAV data is asynchronous in WebAudio,
      this property is @true while decoding is in-progress. }
    property PendingDecode: Boolean read FPendingDecode;

    { List of TWebAudioSoundSourceBackend that are associated with this buffer.
      Maintained to call their BufferDecodingFinishedXxx. }
    property Sources: TWebAudioSoundSourceBackendList read FSources;
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

    { For tracking playback state.

      This already takes into account FPlayOffset at the moment of starting
      playback, i.e. this is shifted from AudioContext.CurrentTime
      to pretend we started playing earlier when FPlayOffset > 0. }
    FPlayStartTime: TFloatTime;

    { Initial playback offset.

      This is set by SetOffset, which is done by:

      - When not currently playing.
        Happens from TSoundEngine.InternalPlay, by
        "Params.Offset := PlayingSound.InitialOffset"
        to indicate initial offset.

      - Or when currently playing.
        Happens from TCastlePlayingSound.SetOffset.
        This changes playing sound offset, and we implement it by starting
        sound again. }
    FPlayOffset: Single;
    FPlaying: Boolean;
    FPendingPlaying: Boolean;

    function Backend: TWebAudioSoundEngineBackend;
    function GetAudioContext: IJSAudioContext;
    { Connect gain and panner nodes, based on FSpatial, to the master gain node.
      This is not dependent on SourceNode, which is recreated on each Play. }
    procedure NodesConnect;
    { Undo the work of NodesDisconnect. }
    procedure NodesDisconnect;
    function SourceNodeEnded(Event: IJSEvent): Variant;
  private
    procedure BufferDecodingFinishedSuccess(Sender: TObject);
    procedure BufferDecodingFinishedError(Sender: TObject);
  public
    destructor Destroy; override;
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
    FResumePromise: IJSPromise;
    function UserInteractionResumeAccepted(const aValue: Variant): Variant;
    function UserInteractionResumeRejected(const aValue: Variant): Variant;
  public
    property JSAudioContext: IJSAudioContext read FJSAudioContext;
    property MasterGainNode: IJSGainNode read FMasterGainNode;
    property DistanceModel: TSoundDistanceModel read FDistanceModel;

    destructor Destroy; override;
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

constructor TWebAudioSoundBufferBackend.Create(const ASoundEngine: TSoundEngineBackend);
begin
  inherited;
  FSources := TWebAudioSoundSourceBackendList.Create(false);
end;

destructor TWebAudioSoundBufferBackend.Destroy;
begin
  if FSources <> nil then
  begin
    { Make sure to clear TWebAudioSoundSourceBackend.FBuffer references to us.
      Note that SetBuffer(nil) also modifies this FSources list. }
    while FSources.Count > 0 do
      FSources.First.SetBuffer(nil);
    FreeAndNil(FSources);
  end;
  UnregisterPromiseAndNil(FDecodingPromise);
  inherited;
end;

procedure TWebAudioSoundBufferBackend.ContextOpen(const AUrl: String);

  function Backend: TWebAudioSoundEngineBackend;
  begin
    Result := SoundEngine as TWebAudioSoundEngineBackend;
  end;

  procedure ContextOpenFromSoundFile(const SoundFile: TSoundFile);
  const
    SampleSize: array [TSoundSampleFormat] of Cardinal = (1, 2);
  var
    NumChannels, NumFrames, FrameSizeInBytes: Cardinal;
    ChannelData: IJSFloat32Array;
    ChannelDataSingle: TSingleList;
    I: Integer;
    Channel: Integer;
    SampleUInt8: UInt8;
    SampleInt16: Int16;
  begin
    NumChannels := SoundFile.Channels;
    FrameSizeInBytes := SampleSize[SoundFile.SampleFormat] * NumChannels;

    NumFrames := SoundFile.DataSize div FrameSizeInBytes;
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
        case SoundFile.SampleFormat of
          sfPcm8:
            begin
              // UInt8 for each channel, convert to float in range [-1.0, 1.0]
              for I := 0 to NumFrames - 1 do
              begin
                SampleUInt8 := PUInt8(SoundFile.Data)[I * NumChannels + Channel];
                ChannelDataSingle[I] := (SampleUInt8 - 128) / 128;
              end;
            end;
          sfPcm16:
            begin
              // Int16 for each channel, convert to float in range [-1.0, 1.0]
              for I := 0 to NumFrames - 1 do
              begin
                SampleInt16 := PInt16(SoundFile.Data)[I * NumChannels + Channel];
                ChannelDataSingle[I] := SampleInt16 / High(Int16);
              end;
            end;
          else raise Exception.CreateFmt('Unsupported sound data format %d', [Ord(SoundFile.SampleFormat)]);
        end;
      end;

      { Get the Float32Array for this channel }
      ChannelData := FAudioBuffer.getChannelData(Channel);
      ChannelData.CopyFromMemory(PByte(ChannelDataSingle.L), NumFrames * SizeOf(Single));
    finally
      FreeAndNil(ChannelDataSingle);
    end;
  end;

  { Load Url using TSoundFile, pass it to ContextOpenFromSoundFile. }
  procedure UseSoundFile;
  var
    F: TSoundFile;
  begin
    F := TSoundFile.Create(Url);
    try
      FDuration := F.Duration;
      FChannels := F.Channels;
      FFrequency := F.Frequency;
      ContextOpenFromSoundFile(F);
    finally FreeAndNil(F) end;
  end;

  { Load Url contents to JS ArrayBuffer. }
  function UrlToArrayBuffer(const Url: String): IJSArrayBuffer;
  var
    MemStream: TMemoryStream;
  begin
    MemStream := Download(Url, [soForceMemoryStream]) as TMemoryStream;
    try
      Result := TJSArrayBuffer.Create(MemStream.Size);
      Result.CopyFromMemory(MemStream.Memory, MemStream.Size);
    finally FreeAndNil(MemStream) end;
  end;

  { Decode audio data using decodeAudioData, see
    https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/decodeAudioData }
  procedure UseDecodeAudioData;
  var
    ArrayBuffer: IJSArrayBuffer;
  begin
    ArrayBuffer := UrlToArrayBuffer(Url);
    FPendingDecode := true;

    // clear previous FDecodingPromise, if existed
    UnregisterPromiseAndNil(FDecodingPromise);

    FDecodingPromise := Backend.JSAudioContext.DecodeAudioData(ArrayBuffer);
    RegisterPromiseCallbacks(FDecodingPromise,
      @DecodeAudioDataSuccess,
      @DecodeAudioDataError
    );
  end;

var
  SoundMimeType: String;
begin
  inherited;

  SoundMimeType := UriMimeType(Url);
  if (SoundMimeType = 'audio/wav') or
     (SoundMimeType = 'audio/x-wav') then
    UseSoundFile
  else
    UseDecodeAudioData;
end;

procedure TWebAudioSoundBufferBackend.ContextClose;
begin
  FAudioBuffer := nil;
  UnregisterPromiseAndNil(FDecodingPromise);
  inherited;
end;

function TWebAudioSoundBufferBackend.DecodeAudioDataSuccess(const aValue: Variant): Variant;
var
  Source: TWebAudioSoundSourceBackend;
begin
  FPendingDecode := false;

  FAudioBuffer := TJSAudioBuffer.Cast(aValue);
  FDuration := FAudioBuffer.Duration;
  FFrequency := FAudioBuffer.SampleRate;
  FChannels := FAudioBuffer.NumberOfChannels;

  WritelnLog('Successfully decoded audio data for "%s": channels: %d, frequency: %f, duration: %f', [
    UriDisplay(Url),
    FChannels,
    FFrequency,
    FDuration
  ]);

  // note: iteration like this assumes that BufferDecodingFinishedSuccess never modifies FSources list
  for Source in FSources do
    Source.BufferDecodingFinishedSuccess(Self);
  Result := aValue;
end;

function TWebAudioSoundBufferBackend.DecodeAudioDataError(const aValue: Variant): Variant;
var
  Source: TWebAudioSoundSourceBackend;
begin
  FPendingDecode := false;
  WritelnWarning('Failed to decode audio data for "%s"', [UriDisplay(Url)]);

  // note: iteration like this assumes that BufferDecodingFinishedError never modifies FSources list
  for Source in FSources do
    Source.BufferDecodingFinishedError(Self);
  Result := aValue;
end;

function TWebAudioSoundBufferBackend.Duration: TFloatTime;
begin
  Result := FDuration;
end;

function TWebAudioSoundBufferBackend.Channels: Cardinal;
begin
  Result := FChannels;
end;

function TWebAudioSoundBufferBackend.Frequency: TSoundFrequency;
begin
  Result := FFrequency;
end;

{ TDummyEventHandler --------------------------------------------------------- }

type
  TDummyEventHandler = class
    function DummySourceNodeEnded(Event: IJSEvent): Variant;
  end;

var
  DummyEventHandler: TDummyEventHandler;

function TDummyEventHandler.DummySourceNodeEnded(Event: IJSEvent): Variant;
begin
  { This is a dummy handler that does nothing, used to avoid crashes when
    SourceNode.OnEnded is called but we can no longer handle it,
    maybe even the TWebAudioSoundSourceBackend instance is destroyed. }
  Result := Event;
end;

{ TWebAudioSoundSourceBackend ------------------------------------------------ }

destructor TWebAudioSoundSourceBackend.Destroy;
begin
  { Avoid FBuffer.FSources having dangling reference to this instance. }
  if FBuffer <> nil then
    FBuffer.Sources.Remove(Self);
  if SourceNode <> nil then
    Stop; // stop playing, and make sure SourceNode.OnEnded doesn't refer to us
  inherited;
end;

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
  { Note that sources with FPendingPlaying (but not FPlaying) must still
    return PlayingOrPaused = true, otherwise they will be reused for other
    playback (sound engine will assume the playback finished super-quickly). }

  if (not (FPlaying or FPendingPlaying)) or (FBuffer = nil) then
    Exit(false);

  { Looping sounds never stop on their own }
  if FLoop then
    Exit(true);

  { Don't check time for pending play, it didn't yet set FPlayStartTime }
  if FPendingPlaying then
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

procedure TWebAudioSoundSourceBackend.BufferDecodingFinishedSuccess(Sender: TObject);
begin
  if FPendingPlaying then
  begin
    WritelnLog('Buffer "%s" finished decoding, starting playback that was pending.', [UriDisplay(FBuffer.Url)]);
    Play(false);
    { Note that Play calls Stop which sets FPendingPlaying to false, so we don't need to set it here. }
  end else
  begin
    if WebAudioVerboseLog then
      WritelnWarning('Buffer "%s" finished decoding, but there is no pending playback.', [UriDisplay(FBuffer.Url)]);
  end;
end;

procedure TWebAudioSoundSourceBackend.BufferDecodingFinishedError(Sender: TObject);
begin
  // Not logging anything, as the error is already logged in TWebAudioSoundBufferBackend.DecodeAudioDataError.
  FPendingPlaying := false;
end;

procedure TWebAudioSoundSourceBackend.Play(const BufferChangedRecently: Boolean);
var
  AudioCtx: IJSAudioContext;
begin
  Stop;

  if FBuffer = nil then
  begin
    WritelnWarning('Cannot play sound because no buffer is set.');
    Exit;
  end;
  if FBuffer.PendingDecode then // note that this usually implies "FBuffer.AudioBuffer = nil" too
  begin
    if WebAudioVerboseLog then
      WritelnLog('Sound "%s" is still decoding, will play once decoding finishes. This is normal for 1st play of non-WAV files.', [
        UriDisplay(FBuffer.Url)
      ]);
    FPendingPlaying := true;
    Exit;
  end;
  if FBuffer.AudioBuffer = nil then
  begin
    WritelnWarning('Cannot play sound because buffer is not initialized. This may happen if audio data failed to decode for some reason. Buffer URL: "%s".', [
      UriDisplay(FBuffer.Url)
    ]);
    Exit;
  end;

  AudioCtx := GetAudioContext;

  { Create a new AudioBufferSourceNode (they are single-use in WebAudio) }
  SourceNode := AudioCtx.CreateBufferSource;
  SourceNode.Buffer := FBuffer.AudioBuffer;
  SourceNode.Loop := FLoop;
  SourceNode.PlaybackRate.Value := FPitch;
  SourceNode.OnEnded := {$ifdef FPC}@{$endif} SourceNodeEnded;

  { Connect: SourceNode -> our GainNode (which is already connected to the rest
    of the audio graph by NodesConnect) }
  SourceNode.Connect(FGainNode);

  { Start playback }
  if FPlayOffset > 0 then
    SourceNode.Start(0, FPlayOffset)
  else
    SourceNode.Start;

  { Record start time for PlayingOrPaused tracking,
    pretending that we started without any offset. }
  FPlayStartTime := AudioCtx.CurrentTime - FPlayOffset / Max(FPitch, 0.001);
  FPlaying := true;
end;

function TWebAudioSoundSourceBackend.SourceNodeEnded(Event: IJSEvent): Variant;

  function JsObjectsEqual(const A, B: IJSObject): Boolean;
  begin
    { Compare using GetJSObjectCastSrc, since comparing just by reference
      ("Result := A = B;") will be wrong, Job.Js will map to different interfaces.
      Comparing by GetJSObjectID is also wrong. }
    Result :=
      ((A = nil) and (B = nil)) or
      ((A <> nil) and (B <> nil) and (A.GetJSObjectCastSrc = B.GetJSObjectCastSrc));
  end;

begin
  Result := Event; // return value of this does not seem to matter

  if not JsObjectsEqual(Event.Target, SourceNode) then
  begin
    WritelnWarning('Received SourceNodeEnded event, but for wrong SourceNode. This should never happen, as we disconnect SourceNode.OnEnded.');
    Exit;
  end;

  if WebAudioVerboseLog then
    WritelnLog('Sound finished playing (notified about it by OnEnded). This is normal if the sound reached its end.');

  FPlaying := false; // means that Stop will not call SourceNode.Stop, which should not be necessary
  Stop;
end;

procedure TWebAudioSoundSourceBackend.Stop;
begin
  if SourceNode <> nil then
  begin
    { Avoid SourceNodeEnded being called after we set SourceNode := nil below.
      This would be correct... but it crashes now, it seems Job.Js is not ready
      to assign nil to SourceNode.OnEnded.
      So instead we assign a dummy handler that does nothing,
      and will work even if TWebAudioSoundSourceBackend instance is destroyed
      by the time it is called. }
    //SourceNode.OnEnded := nil;
    SourceNode.OnEnded := {$ifdef FPC}@{$endif} DummyEventHandler.DummySourceNodeEnded;

    if FPlaying then
      SourceNode.Stop; // should not be necessary if already FPlaying = false

    { Disconnect without parameters:
      "If no parameters are provided, all outgoing connections are disconnected."
      https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/disconnect }
    SourceNode.Disconnect;
    SourceNode := nil;
  end;

  FPlaying := false;
  FPendingPlaying := false;
end;

procedure TWebAudioSoundSourceBackend.SetPosition(const Value: TVector3);
begin
  Assert(FPannerNode <> nil);
  FPannerNode.SetPosition(Value.X, Value.Y, Value.Z);
end;

procedure TWebAudioSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
  { Doppler was removed from the Web Audio API specification.

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
  if FBuffer <> Value then
  begin
    if FBuffer <> nil then
      FBuffer.Sources.Remove(Self);
    FBuffer := Value as TWebAudioSoundBufferBackend;
    FPendingPlaying := false; // cancel any pending play that was waiting for decoding previous buffer
    if FBuffer <> nil then
      FBuffer.Sources.Add(Self);
  end;
end;

procedure TWebAudioSoundSourceBackend.SetPitch(const Value: Single);
var
  CurrentOffset: Single;
begin
  if FPlaying and (SourceNode <> nil) then
  begin
    { Save CurrentOffset and later change FPlayStartTime because:

      Our GetOffset implementation calculates offset based on elapsed time
      and current pitch.
      Changing pitch during playback, without updating FPlayStartTime,
      would cause GetOffset to be wrong.

      It would be easier of WebAudio's AudioBufferSourceNode had a method to get
      current playback position, but it doesn't (the only solution seems
      to be AudioWorklet with custom processing, which is too complex for our
      needs), so we have to track it ourselves. }
    CurrentOffset := GetOffset;
    FPitch := Value;
    FPlayStartTime := GetAudioContext.CurrentTime - CurrentOffset / Max(FPitch, 0.001);
    SourceNode.PlaybackRate.Value := Value;
  end else
    FPitch := Value;
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
    CurrentTime := GetAudioContext.CurrentTime;
    Elapsed := (CurrentTime - FPlayStartTime) * FPitch;
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

destructor TWebAudioSoundEngineBackend.Destroy;
begin
  UnregisterPromiseAndNil(FResumePromise);
  inherited;
end;

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
begin
  if (FJSAudioContext <> nil) and (FJSAudioContext.State = 'suspended') then
  begin
    WritelnLog('Web Audio context is suspended. Attempting to resume due to user interaction.');

    // clear previous FResumePromise, if existed
    UnregisterPromiseAndNil(FResumePromise);

    FResumePromise := FJSAudioContext.Resume;
    RegisterPromiseCallbacks(FResumePromise,
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
  UnregisterPromiseAndNil(FResumePromise);
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

initialization
  DummyEventHandler := TDummyEventHandler.Create;
finalization
  { Let DummyEventHandler leak, in case some references to it remain from
    JS objects and will be called after finalization. }
  // FreeAndNil(DummyEventHandler);
end.
