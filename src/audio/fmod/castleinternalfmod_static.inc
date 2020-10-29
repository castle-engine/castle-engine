{ FMOD API expressed in Pascal, for Castle Game Engine.
  See https://github.com/castle-engine/castle-engine/wiki/FMOD
  about using FMOD with CGE.

  Based on FMOD 2.00.01, fmod.h and fmod_common.h header files.

  Translation to Pascal copyright by Michalis Kamburelis 2019-2019.

  ----------------------------------------------------------------------------
}
{ Link to FMOD at compile-time.
  Allows to link to static or dynamic libraries (.a, .so on Linux).
  On Unix platforms, the library must exist at compile-time
  (on Windows it is not necessary). }

{$define extdecl := extdecl_callback; external}

{ FMOD global system functions (optional). }
function FMOD_Memory_Initialize           (poolmem: Pointer; poollen: CInt; useralloc: TFMOD_MEMORY_ALLOC_CALLBACK; userrealloc: TFMOD_MEMORY_REALLOC_CALLBACK; userfree: TFMOD_MEMORY_FREE_CALLBACK; memtypeflags: TFMOD_MEMORY_TYPE): TFMOD_RESULT; extdecl;
function FMOD_Memory_GetStats             (currentalloced: PCInt; maxalloced: PCInt; blocking: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Debug_Initialize            (flags: TFMOD_DEBUG_FLAGS; mode: TFMOD_DEBUG_MODE; callback: TFMOD_DEBUG_CALLBACK; filename: PChar): TFMOD_RESULT; extdecl;
function FMOD_File_SetDiskBusy            (busy: CInt): TFMOD_RESULT; extdecl;
function FMOD_File_GetDiskBusy            (busy: PCInt): TFMOD_RESULT; extdecl;

{ FMOD System factory functions.
  Use this to create an FMOD System Instance.
  below you will see FMOD_System_Init/Close to get started. }
function FMOD_System_Create               (system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_Release              (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ 'System' API }

{ Setup functions. }
function FMOD_System_SetOutput                 (system: PFMOD_SYSTEM; output: TFMOD_OUTPUTTYPE): TFMOD_RESULT; extdecl;
function FMOD_System_GetOutput                 (system: PFMOD_SYSTEM; output: PFMOD_OUTPUTTYPE): TFMOD_RESULT; extdecl;
function FMOD_System_GetNumDrivers             (system: PFMOD_SYSTEM; numdrivers: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetDriverInfo             (system: PFMOD_SYSTEM; id: CInt; name: PChar; namelen: CInt; guid: PFMOD_GUID; systemrate: PCInt; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetDriver                 (system: PFMOD_SYSTEM; driver: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetDriver                 (system: PFMOD_SYSTEM; driver: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetSoftwareChannels       (system: PFMOD_SYSTEM; numsoftwarechannels: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetSoftwareChannels       (system: PFMOD_SYSTEM; numsoftwarechannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetSoftwareFormat         (system: PFMOD_SYSTEM; samplerate: CInt; speakermode: TFMOD_SPEAKERMODE; numrawspeakers: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetSoftwareFormat         (system: PFMOD_SYSTEM; samplerate: PCInt; speakermode: PFMOD_SPEAKERMODE; numrawspeakers: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetDSPBufferSize          (system: PFMOD_SYSTEM; bufferlength: CUInt; numbuffers: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetDSPBufferSize          (system: PFMOD_SYSTEM; bufferlength: PCUInt; numbuffers: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetFileSystem             (system: PFMOD_SYSTEM; useropen: TFMOD_FILE_OPEN_CALLBACK; userclose: TFMOD_FILE_CLOSE_CALLBACK; userread: TFMOD_FILE_READ_CALLBACK; userseek: TFMOD_FILE_SEEK_CALLBACK; userasyncread: TFMOD_FILE_ASYNCREAD_CALLBACK; userasynccancel: TFMOD_FILE_ASYNCCANCEL_CALLBACK; blockalign: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_AttachFileSystem          (system: PFMOD_SYSTEM; useropen: TFMOD_FILE_OPEN_CALLBACK; userclose: TFMOD_FILE_CLOSE_CALLBACK; userread: TFMOD_FILE_READ_CALLBACK; userseek: TFMOD_FILE_SEEK_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_System_SetAdvancedSettings       (system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): TFMOD_RESULT; extdecl;
function FMOD_System_GetAdvancedSettings       (system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): TFMOD_RESULT; extdecl;
function FMOD_System_SetCallback               (system: PFMOD_SYSTEM; callback: TFMOD_SYSTEM_CALLBACK; callbackmask: TFMOD_SYSTEM_CALLBACK_TYPE): TFMOD_RESULT; extdecl;

{ Plug-in support. }
(* TODO: translate
function FMOD_System_SetPluginPath             (system: PFMOD_SYSTEM; path: PChar): TFMOD_RESULT; extdecl;
function FMOD_System_LoadPlugin                (system: PFMOD_SYSTEM; filename: PChar; handle: PCUInt; priority: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_UnloadPlugin              (system: PFMOD_SYSTEM; handle: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNumNestedPlugins       (system: PFMOD_SYSTEM; handle: CUInt; count: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNestedPlugin           (system: PFMOD_SYSTEM; handle: CUInt; index: CInt; nestedhandle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNumPlugins             (system: PFMOD_SYSTEM; plugintype: TFMOD_PLUGINTYPE; numplugins: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetPluginHandle           (system: PFMOD_SYSTEM; plugintype: TFMOD_PLUGINTYPE; index: CInt; handle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetPluginInfo             (system: PFMOD_SYSTEM; handle: CUInt; plugintype: PFMOD_PLUGINTYPE; name: PChar; namelen: CInt; version: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetOutputByPlugin         (system: PFMOD_SYSTEM; handle: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetOutputByPlugin         (system: PFMOD_SYSTEM; handle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_CreateDSPByPlugin         (system: PFMOD_SYSTEM; handle: CUInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_System_GetDSPInfoByPlugin        (system: PFMOD_SYSTEM; handle: CUInt; description: PPFMOD_DSP_DESCRIPTION): TFMOD_RESULT; extdecl;
function FMOD_System_RegisterCodec             (system: PFMOD_SYSTEM; description: PFMOD_CODEC_DESCRIPTION; handle: PCUInt; priority: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_RegisterDSP               (system: PFMOD_SYSTEM; description: PFMOD_DSP_DESCRIPTION; handle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_RegisterOutput            (system: PFMOD_SYSTEM; description: PFMOD_OUTPUT_DESCRIPTION; handle: PCUInt): TFMOD_RESULT; extdecl;
*)

{ Init/Close. }
function FMOD_System_Init                      (system: PFMOD_SYSTEM; maxchannels: CInt; flags: TFMOD_INITFLAGS; extradriverdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_System_Close                     (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ General post-init system functions. }
function FMOD_System_Update                    (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_SetSpeakerPosition        (system: PFMOD_SYSTEM; speaker: TFMOD_SPEAKER; x: CFloat; y: CFloat; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_GetSpeakerPosition        (system: PFMOD_SYSTEM; speaker: TFMOD_SPEAKER; x: PCFloat; y: PCFloat; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_SetStreamBufferSize       (system: PFMOD_SYSTEM; filebuffersize: CUInt; filebuffersizetype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_System_GetStreamBufferSize       (system: PFMOD_SYSTEM; filebuffersize: PCUInt; filebuffersizetype: PFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DSettings             (system: PFMOD_SYSTEM; dopplerscale: CFloat; distancefactor: CFloat; rolloffscale: CFloat): TFMOD_RESULT; extdecl;
function FMOD_System_Get3DSettings             (system: PFMOD_SYSTEM; dopplerscale: PCFloat; distancefactor: PCFloat; rolloffscale: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DNumListeners         (system: PFMOD_SYSTEM; numlisteners: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_Get3DNumListeners         (system: PFMOD_SYSTEM; numlisteners: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DListenerAttributes   (system: PFMOD_SYSTEM; listener: CInt; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_System_Get3DListenerAttributes   (system: PFMOD_SYSTEM; listener: CInt; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DRolloffCallback      (system: PFMOD_SYSTEM; callback: TFMOD_3D_ROLLOFF_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_System_MixerSuspend              (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_MixerResume               (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_GetDefaultMixMatrix       (system: PFMOD_SYSTEM; sourcespeakermode: TFMOD_SPEAKERMODE; targetspeakermode: TFMOD_SPEAKERMODE; matrix: PCFloat; matrixhop: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetSpeakerModeChannels    (system: PFMOD_SYSTEM; mode: TFMOD_SPEAKERMODE; channels: PCInt): TFMOD_RESULT; extdecl;

{ System information functions. }
function FMOD_System_GetVersion                (system: PFMOD_SYSTEM; version: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetOutputHandle           (system: PFMOD_SYSTEM; handle: PPointer): TFMOD_RESULT; extdecl;
function FMOD_System_GetChannelsPlaying        (system: PFMOD_SYSTEM; channels: PCInt; realchannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetCPUUsage               (system: PFMOD_SYSTEM; dsp: PCFloat; stream: PCFloat; geometry: PCFloat; update: PCFloat; total: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_System_GetFileUsage              (system: PFMOD_SYSTEM; sampleBytesRead: PCLongLong; streamBytesRead: PCLongLong; otherBytesRead: PCLongLong): TFMOD_RESULT; extdecl;

{ Sound/DSP/Channel/FX creation and retrieval. }
function FMOD_System_CreateSound               (system: PFMOD_SYSTEM; name_or_data: PChar; mode: TFMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_System_CreateStream              (system: PFMOD_SYSTEM; name_or_data: PChar; mode: TFMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
// TODO function FMOD_System_CreateDSP                 (system: PFMOD_SYSTEM; description: PFMOD_DSP_DESCRIPTION; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
// TODO function FMOD_System_CreateDSPByType           (system: PFMOD_SYSTEM; type_: TFMOD_DSP_TYPE; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_System_CreateChannelGroup        (system: PFMOD_SYSTEM; name: PChar; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_System_CreateSoundGroup          (system: PFMOD_SYSTEM; name: PChar; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
function FMOD_System_CreateReverb3D            (system: PFMOD_SYSTEM; reverb: PPFMOD_REVERB3D): TFMOD_RESULT; extdecl;
function FMOD_System_PlaySound                 (system: PFMOD_SYSTEM; sound: PFMOD_SOUND; channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_System_PlayDSP                   (system: PFMOD_SYSTEM; dsp: PFMOD_DSP; channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_System_GetChannel                (system: PFMOD_SYSTEM; channelid: CInt; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_System_GetMasterChannelGroup     (system: PFMOD_SYSTEM; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_System_GetMasterSoundGroup       (system: PFMOD_SYSTEM; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{ Routing to ports. }
function FMOD_System_AttachChannelGroupToPort  (system: PFMOD_SYSTEM; portType: TFMOD_PORT_TYPE; portIndex: TFMOD_PORT_INDEX; channelgroup: PFMOD_CHANNELGROUP; passThru: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_DetachChannelGroupFromPort(system: PFMOD_SYSTEM; channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{ Reverb API. }
function FMOD_System_SetReverbProperties       (system: PFMOD_SYSTEM; instance: CInt; prop: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
function FMOD_System_GetReverbProperties       (system: PFMOD_SYSTEM; instance: CInt; prop: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;

{ System level DSP functionality. }
function FMOD_System_LockDSP                   (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_UnlockDSP                 (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ Recording API. }
function FMOD_System_GetRecordNumDrivers       (system: PFMOD_SYSTEM; numdrivers: PCInt; numconnected: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetRecordDriverInfo       (system: PFMOD_SYSTEM; id: CInt; name: PChar; namelen: CInt; guid: PFMOD_GUID; systemrate: PCInt; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PCInt; state: PFMOD_DRIVER_STATE): TFMOD_RESULT; extdecl;
function FMOD_System_GetRecordPosition         (system: PFMOD_SYSTEM; id: CInt; position: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_RecordStart               (system: PFMOD_SYSTEM; id: CInt; sound: PFMOD_SOUND; loop: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_RecordStop                (system: PFMOD_SYSTEM; id: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_IsRecording               (system: PFMOD_SYSTEM; id: CInt; recording: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{ Geometry API. }
function FMOD_System_CreateGeometry            (system: PFMOD_SYSTEM; maxpolygons: CInt; maxvertices: CInt; geometry: PPFMOD_GEOMETRY): TFMOD_RESULT; extdecl;
function FMOD_System_SetGeometrySettings       (system: PFMOD_SYSTEM; maxworldsize: CFloat): TFMOD_RESULT; extdecl;
function FMOD_System_GetGeometrySettings       (system: PFMOD_SYSTEM; maxworldsize: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_System_LoadGeometry              (system: PFMOD_SYSTEM; data: Pointer; datasize: CInt; geometry: PPFMOD_GEOMETRY): TFMOD_RESULT; extdecl;
function FMOD_System_GetGeometryOcclusion      (system: PFMOD_SYSTEM; listener: PFMOD_VECTOR; source: PFMOD_VECTOR; direct: PCFloat; reverb: PCFloat): TFMOD_RESULT; extdecl;

{ Network functions. }
function FMOD_System_SetNetworkProxy           (system: PFMOD_SYSTEM; proxy: PChar): TFMOD_RESULT; extdecl;
function FMOD_System_GetNetworkProxy           (system: PFMOD_SYSTEM; proxy: PChar; proxylen: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetNetworkTimeout         (system: PFMOD_SYSTEM; timeout: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNetworkTimeout         (system: PFMOD_SYSTEM; timeout: PCInt): TFMOD_RESULT; extdecl;

{ Userdata set/get. }
function FMOD_System_SetUserData               (system: PFMOD_SYSTEM; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_System_GetUserData               (system: PFMOD_SYSTEM; userdata: PPointer): TFMOD_RESULT; extdecl;

{ Sound API }

function FMOD_Sound_Release                    (sound: PFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSystemObject            (sound: PFMOD_SOUND; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     Standard sound manipulation functions.
}

function FMOD_Sound_Lock                       (sound: PFMOD_SOUND; offset: CUInt; length: CUInt; ptr1: PPointer; ptr2: PPointer; len1: PCUInt; len2: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_Unlock                     (sound: PFMOD_SOUND; ptr1: Pointer; ptr2: Pointer; len1: CUInt; len2: CUInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetDefaults                (sound: PFMOD_SOUND; frequency: CFloat; priority: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetDefaults                (sound: PFMOD_SOUND; frequency: PCFloat; priority: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_Set3DMinMaxDistance        (sound: PFMOD_SOUND; min: CFloat; max: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Get3DMinMaxDistance        (sound: PFMOD_SOUND; min: PCFloat; max: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Set3DConeSettings          (sound: PFMOD_SOUND; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Get3DConeSettings          (sound: PFMOD_SOUND; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Set3DCustomRolloff         (sound: PFMOD_SOUND; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_Get3DCustomRolloff         (sound: PFMOD_SOUND; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSubSound                (sound: PFMOD_SOUND; index: CInt; subsound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSubSoundParent          (sound: PFMOD_SOUND; parentsound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetName                    (sound: PFMOD_SOUND; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetLength                  (sound: PFMOD_SOUND; length: PCUInt; lengthtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetFormat                  (sound: PFMOD_SOUND; type_: PFMOD_SOUND_TYPE; format: PFMOD_SOUND_FORMAT; channels: PCInt; bits: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetNumSubSounds            (sound: PFMOD_SOUND; numsubsounds: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetNumTags                 (sound: PFMOD_SOUND; numtags: PCInt; numtagsupdated: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetTag                     (sound: PFMOD_SOUND; name: PChar; index: CInt; tag: PFMOD_TAG): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetOpenState               (sound: PFMOD_SOUND; openstate: PFMOD_OPENSTATE; percentbuffered: PCUInt; starving: PFMOD_BOOL; diskbusy: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Sound_ReadData                   (sound: PFMOD_SOUND; buffer: Pointer; length: CUInt; read: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SeekData                   (sound: PFMOD_SOUND; pcm: CUInt): TFMOD_RESULT; extdecl;

function FMOD_Sound_SetSoundGroup              (sound: PFMOD_SOUND; soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSoundGroup              (sound: PFMOD_SOUND; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{
     Synchronization poAPI: CInt.  These points can come from markers embedded in wav files; and can also generate channel callbacks.
}

function FMOD_Sound_GetNumSyncPoints           (sound: PFMOD_SOUND; numsyncpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSyncPoint               (sound: PFMOD_SOUND; index: CInt; point: PPFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSyncPointInfo           (sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT; name: PChar; namelen: CInt; offset: PCUInt; offsettype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Sound_AddSyncPoint               (sound: PFMOD_SOUND; offset: CUInt; offsettype: TFMOD_TIMEUNIT; name: PChar; point: PPFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;
function FMOD_Sound_DeleteSyncPoint            (sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;

{
     Functions also in Channel class but here they are the 'default' to save having to change it in Channel all the time.
}

function FMOD_Sound_SetMode                    (sound: PFMOD_SOUND; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetMode                    (sound: PFMOD_SOUND; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetLoopCount               (sound: PFMOD_SOUND; loopcount: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetLoopCount               (sound: PFMOD_SOUND; loopcount: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetLoopPoints              (sound: PFMOD_SOUND; loopstart: CUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: CUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetLoopPoints              (sound: PFMOD_SOUND; loopstart: PCUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: PCUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;

{
     For MOD/S3M/XM/IT/MID sequenced formats only.
}

function FMOD_Sound_GetMusicNumChannels        (sound: PFMOD_SOUND; numchannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetMusicChannelVolume      (sound: PFMOD_SOUND; channel: CInt; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetMusicChannelVolume      (sound: PFMOD_SOUND; channel: CInt; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetMusicSpeed              (sound: PFMOD_SOUND; speed: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetMusicSpeed              (sound: PFMOD_SOUND; speed: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Sound_SetUserData                (sound: PFMOD_SOUND; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetUserData                (sound: PFMOD_SOUND; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Channel' API
}

function FMOD_Channel_GetSystemObject          (channel: PFMOD_CHANNEL; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     General control functionality for Channels and ChannelGroups.
}

function FMOD_Channel_Stop                     (channel: PFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPaused                (channel: PFMOD_CHANNEL; paused: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPaused                (channel: PFMOD_CHANNEL; paused: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetVolume                (channel: PFMOD_CHANNEL; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetVolume                (channel: PFMOD_CHANNEL; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetVolumeRamp            (channel: PFMOD_CHANNEL; ramp: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetVolumeRamp            (channel: PFMOD_CHANNEL; ramp: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetAudibility            (channel: PFMOD_CHANNEL; audibility: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPitch                 (channel: PFMOD_CHANNEL; pitch: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPitch                 (channel: PFMOD_CHANNEL; pitch: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMute                  (channel: PFMOD_CHANNEL; mute: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetMute                  (channel: PFMOD_CHANNEL; mute: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetReverbProperties      (channel: PFMOD_CHANNEL; instance: CInt; wet: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetReverbProperties      (channel: PFMOD_CHANNEL; instance: CInt; wet: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetLowPassGain           (channel: PFMOD_CHANNEL; gain: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetLowPassGain           (channel: PFMOD_CHANNEL; gain: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMode                  (channel: PFMOD_CHANNEL; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetMode                  (channel: PFMOD_CHANNEL; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetCallback              (channel: PFMOD_CHANNEL; callback: TFMOD_CHANNELCONTROL_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_Channel_IsPlaying                (channel: PFMOD_CHANNEL; isplaying: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Note all 'set' functions alter a final matrix; this is why the only get function is getMixMatrix; to avoid other get functions returning incorrect/obsolete values.
}

function FMOD_Channel_SetPan                   (channel: PFMOD_CHANNEL; pan: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMixLevelsOutput       (channel: PFMOD_CHANNEL; frontleft: CFloat; frontright: CFloat; center: CFloat; lfe: CFloat; surroundleft: CFloat; surroundright: CFloat; backleft: CFloat; backright: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMixLevelsInput        (channel: PFMOD_CHANNEL; levels: PCFloat; numlevels: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMixMatrix             (channel: PFMOD_CHANNEL; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetMixMatrix             (channel: PFMOD_CHANNEL; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;

{
     Clock based functionality.
}

function FMOD_Channel_GetDSPClock              (channel: PFMOD_CHANNEL; dspclock: PCULongLong; parentclock: PCULongLong): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetDelay                 (channel: PFMOD_CHANNEL; dspclock_start: CULongLong; dspclock_end: CULongLong; stopchannels: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetDelay                 (channel: PFMOD_CHANNEL; dspclock_start: PCULongLong; dspclock_end: PCULongLong; stopchannels: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_AddFadePoint             (channel: PFMOD_CHANNEL; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetFadePointRamp         (channel: PFMOD_CHANNEL; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_RemoveFadePoints         (channel: PFMOD_CHANNEL; dspclock_start: CULongLong; dspclock_end: CULongLong): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetFadePoints            (channel: PFMOD_CHANNEL; numpoints: PCUInt; point_dspclock: PCULongLong; point_volume: PCFloat): TFMOD_RESULT; extdecl;

{
     DSP effects.
}

function FMOD_Channel_GetDSP                   (channel: PFMOD_CHANNEL; index: CInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_Channel_AddDSP                   (channel: PFMOD_CHANNEL; index: CInt; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_Channel_RemoveDSP                (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetNumDSPs               (channel: PFMOD_CHANNEL; numdsps: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetDSPIndex              (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetDSPIndex              (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: PCInt): TFMOD_RESULT; extdecl;

{
     3D functionality.
}

function FMOD_Channel_Set3DAttributes          (channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DAttributes          (channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DMinMaxDistance      (channel: PFMOD_CHANNEL; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DMinMaxDistance      (channel: PFMOD_CHANNEL; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DConeSettings        (channel: PFMOD_CHANNEL; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DConeSettings        (channel: PFMOD_CHANNEL; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DConeOrientation     (channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DConeOrientation     (channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DCustomRolloff       (channel: PFMOD_CHANNEL; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DCustomRolloff       (channel: PFMOD_CHANNEL; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DOcclusion           (channel: PFMOD_CHANNEL; directocclusion: CFloat; reverbocclusion: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DOcclusion           (channel: PFMOD_CHANNEL; directocclusion: PCFloat; reverbocclusion: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DSpread              (channel: PFMOD_CHANNEL; angle: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DSpread              (channel: PFMOD_CHANNEL; angle: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DLevel               (channel: PFMOD_CHANNEL; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DLevel               (channel: PFMOD_CHANNEL; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DDopplerLevel        (channel: PFMOD_CHANNEL; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DDopplerLevel        (channel: PFMOD_CHANNEL; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DDistanceFilter      (channel: PFMOD_CHANNEL; custom: TFMOD_BOOL; customLevel: CFloat; centerFreq: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DDistanceFilter      (channel: PFMOD_CHANNEL; custom: PFMOD_BOOL; customLevel: PCFloat; centerFreq: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Channel_SetUserData              (channel: PFMOD_CHANNEL; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetUserData              (channel: PFMOD_CHANNEL; userdata: PPointer): TFMOD_RESULT; extdecl;

{
     Channel specific control functionality.
}

function FMOD_Channel_SetFrequency             (channel: PFMOD_CHANNEL; frequency: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetFrequency             (channel: PFMOD_CHANNEL; frequency: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPriority              (channel: PFMOD_CHANNEL; priority: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPriority              (channel: PFMOD_CHANNEL; priority: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPosition              (channel: PFMOD_CHANNEL; position: CUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPosition              (channel: PFMOD_CHANNEL; position: PCUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetChannelGroup          (channel: PFMOD_CHANNEL; channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetChannelGroup          (channel: PFMOD_CHANNEL; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetLoopCount             (channel: PFMOD_CHANNEL; loopcount: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetLoopCount             (channel: PFMOD_CHANNEL; loopcount: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetLoopPoints            (channel: PFMOD_CHANNEL; loopstart: CUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: CUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetLoopPoints            (channel: PFMOD_CHANNEL; loopstart: PCUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: PCUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

function FMOD_Channel_IsVirtual                (channel: PFMOD_CHANNEL; isvirtual: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetCurrentSound          (channel: PFMOD_CHANNEL; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetIndex                 (channel: PFMOD_CHANNEL; index: PCInt): TFMOD_RESULT; extdecl;

{
    'ChannelGroup' API
}

function FMOD_ChannelGroup_GetSystemObject     (channelgroup: PFMOD_CHANNELGROUP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     General control functionality for Channels and ChannelGroups.
}

function FMOD_ChannelGroup_Stop                (channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetPaused           (channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetPaused           (channelgroup: PFMOD_CHANNELGROUP; paused: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetVolume           (channelgroup: PFMOD_CHANNELGROUP; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetVolume           (channelgroup: PFMOD_CHANNELGROUP; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetVolumeRamp       (channelgroup: PFMOD_CHANNELGROUP; ramp: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetVolumeRamp       (channelgroup: PFMOD_CHANNELGROUP; ramp: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetAudibility       (channelgroup: PFMOD_CHANNELGROUP; audibility: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetPitch            (channelgroup: PFMOD_CHANNELGROUP; pitch: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetPitch            (channelgroup: PFMOD_CHANNELGROUP; pitch: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMute             (channelgroup: PFMOD_CHANNELGROUP; mute: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetMute             (channelgroup: PFMOD_CHANNELGROUP; mute: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetReverbProperties (channelgroup: PFMOD_CHANNELGROUP; instance: CInt; wet: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetReverbProperties (channelgroup: PFMOD_CHANNELGROUP; instance: CInt; wet: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetLowPassGain      (channelgroup: PFMOD_CHANNELGROUP; gain: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetLowPassGain      (channelgroup: PFMOD_CHANNELGROUP; gain: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMode             (channelgroup: PFMOD_CHANNELGROUP; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetMode             (channelgroup: PFMOD_CHANNELGROUP; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetCallback         (channelgroup: PFMOD_CHANNELGROUP; callback: TFMOD_CHANNELCONTROL_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_IsPlaying           (channelgroup: PFMOD_CHANNELGROUP; isplaying: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Note all 'set' functions alter a final matrix; this is why the only get function is getMixMatrix; to avoid other get functions returning incorrect/obsolete values.
}

function FMOD_ChannelGroup_SetPan              (channelgroup: PFMOD_CHANNELGROUP; pan: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMixLevelsOutput  (channelgroup: PFMOD_CHANNELGROUP; frontleft: CFloat; frontright: CFloat; center: CFloat; lfe: CFloat; surroundleft: CFloat; surroundright: CFloat; backleft: CFloat; backright: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMixLevelsInput   (channelgroup: PFMOD_CHANNELGROUP; levels: PCFloat; numlevels: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMixMatrix        (channelgroup: PFMOD_CHANNELGROUP; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetMixMatrix        (channelgroup: PFMOD_CHANNELGROUP; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;

{
     Clock based functionality.
}

function FMOD_ChannelGroup_GetDSPClock         (channelgroup: PFMOD_CHANNELGROUP; dspclock: PCULongLong; parentclock: PCULongLong): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetDelay            (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: CULongLong; dspclock_end: CULongLong; stopchannels: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetDelay            (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: PCULongLong; dspclock_end: PCULongLong; stopchannels: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_AddFadePoint        (channelgroup: PFMOD_CHANNELGROUP; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetFadePointRamp    (channelgroup: PFMOD_CHANNELGROUP; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_RemoveFadePoints    (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: CULongLong; dspclock_end: CULongLong): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetFadePoints       (channelgroup: PFMOD_CHANNELGROUP; numpoints: PCUInt; point_dspclock: PCULongLong; point_volume: PCFloat): TFMOD_RESULT; extdecl;

{
     DSP effects.
}

function FMOD_ChannelGroup_GetDSP              (channelgroup: PFMOD_CHANNELGROUP; index: CInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_AddDSP              (channelgroup: PFMOD_CHANNELGROUP; index: CInt; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_RemoveDSP           (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetNumDSPs          (channelgroup: PFMOD_CHANNELGROUP; numdsps: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetDSPIndex         (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetDSPIndex         (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: PCInt): TFMOD_RESULT; extdecl;

{
     3D functionality.
}

function FMOD_ChannelGroup_Set3DAttributes     (channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DAttributes     (channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DMinMaxDistance (channelgroup: PFMOD_CHANNELGROUP; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DMinMaxDistance (channelgroup: PFMOD_CHANNELGROUP; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DConeSettings   (channelgroup: PFMOD_CHANNELGROUP; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DConeSettings   (channelgroup: PFMOD_CHANNELGROUP; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DConeOrientation(channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DConeOrientation(channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DCustomRolloff  (channelgroup: PFMOD_CHANNELGROUP; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DCustomRolloff  (channelgroup: PFMOD_CHANNELGROUP; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DOcclusion      (channelgroup: PFMOD_CHANNELGROUP; directocclusion: CFloat; reverbocclusion: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DOcclusion      (channelgroup: PFMOD_CHANNELGROUP; directocclusion: PCFloat; reverbocclusion: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DSpread         (channelgroup: PFMOD_CHANNELGROUP; angle: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DSpread         (channelgroup: PFMOD_CHANNELGROUP; angle: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DLevel          (channelgroup: PFMOD_CHANNELGROUP; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DLevel          (channelgroup: PFMOD_CHANNELGROUP; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DDopplerLevel   (channelgroup: PFMOD_CHANNELGROUP; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DDopplerLevel   (channelgroup: PFMOD_CHANNELGROUP; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DDistanceFilter (channelgroup: PFMOD_CHANNELGROUP; custom: TFMOD_BOOL; customLevel: CFloat; centerFreq: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DDistanceFilter (channelgroup: PFMOD_CHANNELGROUP; custom: PFMOD_BOOL; customLevel: PCFloat; centerFreq: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_ChannelGroup_SetUserData         (channelgroup: PFMOD_CHANNELGROUP; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetUserData         (channelgroup: PFMOD_CHANNELGROUP; userdata: PPointer): TFMOD_RESULT; extdecl;

function FMOD_ChannelGroup_Release             (channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{
     Nested channel groups.
}

function FMOD_ChannelGroup_AddGroup            (channelgroup: PFMOD_CHANNELGROUP; group: PFMOD_CHANNELGROUP; propagatedspclock: TFMOD_BOOL; connection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetNumGroups        (channelgroup: PFMOD_CHANNELGROUP; numgroups: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetGroup            (channelgroup: PFMOD_CHANNELGROUP; index: CInt; group: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetParentGroup      (channelgroup: PFMOD_CHANNELGROUP; group: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

function FMOD_ChannelGroup_GetName             (channelgroup: PFMOD_CHANNELGROUP; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetNumChannels      (channelgroup: PFMOD_CHANNELGROUP; numchannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetChannel          (channelgroup: PFMOD_CHANNELGROUP; index: CInt; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;

{
    'SoundGroup' API
}

function FMOD_SoundGroup_Release               (soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetSystemObject       (soundgroup: PFMOD_SOUNDGROUP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     SoundGroup control functions.
}

function FMOD_SoundGroup_SetMaxAudible         (soundgroup: PFMOD_SOUNDGROUP; maxaudible: CInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetMaxAudible         (soundgroup: PFMOD_SOUNDGROUP; maxaudible: PCInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_SetMaxAudibleBehavior (soundgroup: PFMOD_SOUNDGROUP; behavior: TFMOD_SOUNDGROUP_BEHAVIOR): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetMaxAudibleBehavior (soundgroup: PFMOD_SOUNDGROUP; behavior: PFMOD_SOUNDGROUP_BEHAVIOR): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_SetMuteFadeSpeed      (soundgroup: PFMOD_SOUNDGROUP; speed: CFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetMuteFadeSpeed      (soundgroup: PFMOD_SOUNDGROUP; speed: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_SetVolume             (soundgroup: PFMOD_SOUNDGROUP; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetVolume             (soundgroup: PFMOD_SOUNDGROUP; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_Stop                  (soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

function FMOD_SoundGroup_GetName               (soundgroup: PFMOD_SOUNDGROUP; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetNumSounds          (soundgroup: PFMOD_SOUNDGROUP; numsounds: PCInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetSound              (soundgroup: PFMOD_SOUNDGROUP; index: CInt; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetNumPlaying         (soundgroup: PFMOD_SOUNDGROUP; numplaying: PCInt): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_SoundGroup_SetUserData           (soundgroup: PFMOD_SOUNDGROUP; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetUserData           (soundgroup: PFMOD_SOUNDGROUP; userdata: PPointer): TFMOD_RESULT; extdecl;

(* TODO: translate
{
    'DSP' API
}

function FMOD_DSP_Release                      (dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetSystemObject              (dsp: PFMOD_DSP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     Connection / disconnection / input and output enumeration.
}

function FMOD_DSP_AddInput                     (dsp: PFMOD_DSP; input: PFMOD_DSP; connection: PPFMOD_DSPCONNECTION; type_: TFMOD_DSPCONNECTION_TYPE): TFMOD_RESULT; extdecl;
function FMOD_DSP_DisconnectFrom               (dsp: PFMOD_DSP; target: PFMOD_DSP; connection: PFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
function FMOD_DSP_DisconnectAll                (dsp: PFMOD_DSP; inputs: TFMOD_BOOL; outputs: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetNumInputs                 (dsp: PFMOD_DSP; numinputs: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetNumOutputs                (dsp: PFMOD_DSP; numoutputs: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetInput                     (dsp: PFMOD_DSP; index: CInt; input: PPFMOD_DSP; inputconnection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetOutput                    (dsp: PFMOD_DSP; index: CInt; output: PPFMOD_DSP; outputconnection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;

{
     DSP unit control.
}

function FMOD_DSP_SetActive                    (dsp: PFMOD_DSP; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetActive                    (dsp: PFMOD_DSP; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetBypass                    (dsp: PFMOD_DSP; bypass: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetBypass                    (dsp: PFMOD_DSP; bypass: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetWetDryMix                 (dsp: PFMOD_DSP; prewet: CFloat; postwet: CFloat; dry: CFloat): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetWetDryMix                 (dsp: PFMOD_DSP; prewet: PCFloat; postwet: PCFloat; dry: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetChannelFormat             (dsp: PFMOD_DSP; channelmask: TFMOD_CHANNELMASK; numchannels: CInt; source_speakermode: TFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetChannelFormat             (dsp: PFMOD_DSP; channelmask: PFMOD_CHANNELMASK; numchannels: PCInt; source_speakermode: PFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetOutputChannelFormat       (dsp: PFMOD_DSP; inmask: TFMOD_CHANNELMASK; inchannels: CInt; inspeakermode: TFMOD_SPEAKERMODE; outmask: PFMOD_CHANNELMASK; outchannels: PCInt; outspeakermode: PFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
function FMOD_DSP_Reset                        (dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;

{
     DSP parameter control.
}

function FMOD_DSP_SetParameterFloat            (dsp: PFMOD_DSP; index: CInt; value: CFloat): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetParameterInt              (dsp: PFMOD_DSP; index: CInt; value: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetParameterBool             (dsp: PFMOD_DSP; index: CInt; value: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetParameterData             (dsp: PFMOD_DSP; index: CInt; data: Pointer; length: CUInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterFloat            (dsp: PFMOD_DSP; index: CInt; value: PCFloat; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterInt              (dsp: PFMOD_DSP; index: CInt; value: PCInt; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterBool             (dsp: PFMOD_DSP; index: CInt; value: PFMOD_BOOL; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterData             (dsp: PFMOD_DSP; index: CInt; data: PPointer; length: PCUInt; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetNumParameters             (dsp: PFMOD_DSP; numparams: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterInfo             (dsp: PFMOD_DSP; index: CInt; desc: PPFMOD_DSP_PARAMETER_DESC): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetDataParameterIndex        (dsp: PFMOD_DSP; datatype: CInt; index: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_ShowConfigDialog             (dsp: PFMOD_DSP; hwnd: Pointer; show: TFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     DSP attributes.
}

function FMOD_DSP_GetInfo                      (dsp: PFMOD_DSP; name: PChar; version: PCUInt; channels: PCInt; configwidth: PCInt; configheight: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetType                      (dsp: PFMOD_DSP; type_: PFMOD_DSP_TYPE): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetIdle                      (dsp: PFMOD_DSP; idle: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_DSP_SetUserData                  (dsp: PFMOD_DSP; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetUserData                  (dsp: PFMOD_DSP; userdata: PPointer): TFMOD_RESULT; extdecl;

{
     Metering.
}

function FMOD_DSP_SetMeteringEnabled           (dsp: PFMOD_DSP; inputEnabled: TFMOD_BOOL; outputEnabled: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetMeteringEnabled           (dsp: PFMOD_DSP; inputEnabled: PFMOD_BOOL; outputEnabled: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetMeteringInfo              (dsp: PFMOD_DSP; inputInfo: PFMOD_DSP_METERING_INFO; outputInfo: PFMOD_DSP_METERING_INFO): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetCPUUsage                  (dsp: PFMOD_DSP; exclusive: PCUInt; inclusive: PCUInt): TFMOD_RESULT; extdecl;
*)

{
    'DSPConnection' API
}

function FMOD_DSPConnection_GetInput           (dspconnection: PFMOD_DSPCONNECTION; input: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetOutput          (dspconnection: PFMOD_DSPCONNECTION; output: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_SetMix             (dspconnection: PFMOD_DSPCONNECTION; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetMix             (dspconnection: PFMOD_DSPCONNECTION; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_SetMixMatrix       (dspconnection: PFMOD_DSPCONNECTION; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetMixMatrix       (dspconnection: PFMOD_DSPCONNECTION; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
// TODO function FMOD_DSPConnection_GetType            (dspconnection: PFMOD_DSPCONNECTION; type_: PFMOD_DSPCONNECTION_TYPE): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_DSPConnection_SetUserData        (dspconnection: PFMOD_DSPCONNECTION; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetUserData        (dspconnection: PFMOD_DSPCONNECTION; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Geometry' API
}

function FMOD_Geometry_Release                 (geometry: PFMOD_GEOMETRY): TFMOD_RESULT; extdecl;

{
     Polygon manipulation.
}

function FMOD_Geometry_AddPolygon              (geometry: PFMOD_GEOMETRY; directocclusion: CFloat; reverbocclusion: CFloat; doublesided: TFMOD_BOOL; numvertices: CInt; vertices: PFMOD_VECTOR; polygonindex: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetNumPolygons          (geometry: PFMOD_GEOMETRY; numpolygons: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetMaxPolygons          (geometry: PFMOD_GEOMETRY; maxpolygons: PCInt; maxvertices: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPolygonNumVertices   (geometry: PFMOD_GEOMETRY; index: CInt; numvertices: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetPolygonVertex        (geometry: PFMOD_GEOMETRY; index: CInt; vertexindex: CInt; vertex: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPolygonVertex        (geometry: PFMOD_GEOMETRY; index: CInt; vertexindex: CInt; vertex: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetPolygonAttributes    (geometry: PFMOD_GEOMETRY; index: CInt; directocclusion: CFloat; reverbocclusion: CFloat; doublesided: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPolygonAttributes    (geometry: PFMOD_GEOMETRY; index: CInt; directocclusion: PCFloat; reverbocclusion: PCFloat; doublesided: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Object manipulation.
}

function FMOD_Geometry_SetActive               (geometry: PFMOD_GEOMETRY; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetActive               (geometry: PFMOD_GEOMETRY; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetRotation             (geometry: PFMOD_GEOMETRY; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetRotation             (geometry: PFMOD_GEOMETRY; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetPosition             (geometry: PFMOD_GEOMETRY; position: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPosition             (geometry: PFMOD_GEOMETRY; position: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetScale                (geometry: PFMOD_GEOMETRY; scale: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetScale                (geometry: PFMOD_GEOMETRY; scale: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_Save                    (geometry: PFMOD_GEOMETRY; data: Pointer; datasize: PCInt): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Geometry_SetUserData             (geometry: PFMOD_GEOMETRY; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetUserData             (geometry: PFMOD_GEOMETRY; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Reverb3D' API
}

function FMOD_Reverb3D_Release                 (reverb3d: PFMOD_REVERB3D): TFMOD_RESULT; extdecl;

{
     Reverb manipulation.
}

function FMOD_Reverb3D_Set3DAttributes         (reverb3d: PFMOD_REVERB3D; position: PFMOD_VECTOR; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_Get3DAttributes         (reverb3d: PFMOD_REVERB3D; position: PFMOD_VECTOR; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_SetProperties           (reverb3d: PFMOD_REVERB3D; properties: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_GetProperties           (reverb3d: PFMOD_REVERB3D; properties: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_SetActive               (reverb3d: PFMOD_REVERB3D; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_GetActive               (reverb3d: PFMOD_REVERB3D; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Reverb3D_SetUserData             (reverb3d: PFMOD_REVERB3D; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_GetUserData             (reverb3d: PFMOD_REVERB3D; userdata: PPointer): TFMOD_RESULT; extdecl;

implementation

procedure InitializeFmodLibrary;
begin
end;

function FmodLibraryAvailable: Boolean;
begin
  Result := true;
end;

procedure FmodLibraryUsingBegin;
begin
end;

procedure FmodLibraryUsingEnd;
begin
end;

end.
