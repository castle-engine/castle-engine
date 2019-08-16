{ FMOD API expressed in Pascal, for Castle Game Engine.
  See https://github.com/castle-engine/castle-engine/wiki/FMOD
  about using FMOD with CGE.

  Based on FMOD 2.00.01, fmod.h and fmod_common.h header files.

  Translation to Pascal copyright by Michalis Kamburelis 2019-2019.

  ----------------------------------------------------------------------------
}
{ Link to FMOD at compile-time.
  Allows to link to dynamic libraries (.so) on Linux only.
  The library does not have to exist at compile-time. }

{$define extdecl := extdecl_callback}

var

{ FMOD global system functions (optional). }
  FMOD_Memory_Initialize           : function (poolmem: Pointer; poollen: CInt; useralloc: TFMOD_MEMORY_ALLOC_CALLBACK; userrealloc: TFMOD_MEMORY_REALLOC_CALLBACK; userfree: TFMOD_MEMORY_FREE_CALLBACK; memtypeflags: TFMOD_MEMORY_TYPE): TFMOD_RESULT; extdecl;
  FMOD_Memory_GetStats             : function (currentalloced: PCInt; maxalloced: PCInt; blocking: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Debug_Initialize            : function (flags: TFMOD_DEBUG_FLAGS; mode: TFMOD_DEBUG_MODE; callback: TFMOD_DEBUG_CALLBACK; filename: PChar): TFMOD_RESULT; extdecl;
  FMOD_File_SetDiskBusy            : function (busy: CInt): TFMOD_RESULT; extdecl;
  FMOD_File_GetDiskBusy            : function (busy: PCInt): TFMOD_RESULT; extdecl;

{ FMOD System factory functions.
  Use this to create an FMOD System Instance.
  below you will see FMOD_System_Init/Close to get started. }
  FMOD_System_Create               : function (system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;
  FMOD_System_Release              : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ 'System' API }

{ Setup functions. }
  FMOD_System_SetOutput                 : function (system: PFMOD_SYSTEM; output: TFMOD_OUTPUTTYPE): TFMOD_RESULT; extdecl;
  FMOD_System_GetOutput                 : function (system: PFMOD_SYSTEM; output: PFMOD_OUTPUTTYPE): TFMOD_RESULT; extdecl;
  FMOD_System_GetNumDrivers             : function (system: PFMOD_SYSTEM; numdrivers: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetDriverInfo             : function (system: PFMOD_SYSTEM; id: CInt; name: PChar; namelen: CInt; guid: PFMOD_GUID; systemrate: PCInt; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetDriver                 : function (system: PFMOD_SYSTEM; driver: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetDriver                 : function (system: PFMOD_SYSTEM; driver: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetSoftwareChannels       : function (system: PFMOD_SYSTEM; numsoftwarechannels: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetSoftwareChannels       : function (system: PFMOD_SYSTEM; numsoftwarechannels: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetSoftwareFormat         : function (system: PFMOD_SYSTEM; samplerate: CInt; speakermode: TFMOD_SPEAKERMODE; numrawspeakers: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetSoftwareFormat         : function (system: PFMOD_SYSTEM; samplerate: PCInt; speakermode: PFMOD_SPEAKERMODE; numrawspeakers: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetDSPBufferSize          : function (system: PFMOD_SYSTEM; bufferlength: CUInt; numbuffers: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetDSPBufferSize          : function (system: PFMOD_SYSTEM; bufferlength: PCUInt; numbuffers: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetFileSystem             : function (system: PFMOD_SYSTEM; useropen: TFMOD_FILE_OPEN_CALLBACK; userclose: TFMOD_FILE_CLOSE_CALLBACK; userread: TFMOD_FILE_READ_CALLBACK; userseek: TFMOD_FILE_SEEK_CALLBACK; userasyncread: TFMOD_FILE_ASYNCREAD_CALLBACK; userasynccancel: TFMOD_FILE_ASYNCCANCEL_CALLBACK; blockalign: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_AttachFileSystem          : function (system: PFMOD_SYSTEM; useropen: TFMOD_FILE_OPEN_CALLBACK; userclose: TFMOD_FILE_CLOSE_CALLBACK; userread: TFMOD_FILE_READ_CALLBACK; userseek: TFMOD_FILE_SEEK_CALLBACK): TFMOD_RESULT; extdecl;
  FMOD_System_SetAdvancedSettings       : function (system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): TFMOD_RESULT; extdecl;
  FMOD_System_GetAdvancedSettings       : function (system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): TFMOD_RESULT; extdecl;
  FMOD_System_SetCallback               : function (system: PFMOD_SYSTEM; callback: TFMOD_SYSTEM_CALLBACK; callbackmask: TFMOD_SYSTEM_CALLBACK_TYPE): TFMOD_RESULT; extdecl;

{ Plug-in support. }
(* TODO: translate
  FMOD_System_SetPluginPath             : function (system: PFMOD_SYSTEM; path: PChar): TFMOD_RESULT; extdecl;
  FMOD_System_LoadPlugin                : function (system: PFMOD_SYSTEM; filename: PChar; handle: PCUInt; priority: CUInt): TFMOD_RESULT; extdecl;
  FMOD_System_UnloadPlugin              : function (system: PFMOD_SYSTEM; handle: CUInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetNumNestedPlugins       : function (system: PFMOD_SYSTEM; handle: CUInt; count: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetNestedPlugin           : function (system: PFMOD_SYSTEM; handle: CUInt; index: CInt; nestedhandle: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetNumPlugins             : function (system: PFMOD_SYSTEM; plugintype: TFMOD_PLUGINTYPE; numplugins: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetPluginHandle           : function (system: PFMOD_SYSTEM; plugintype: TFMOD_PLUGINTYPE; index: CInt; handle: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetPluginInfo             : function (system: PFMOD_SYSTEM; handle: CUInt; plugintype: PFMOD_PLUGINTYPE; name: PChar; namelen: CInt; version: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetOutputByPlugin         : function (system: PFMOD_SYSTEM; handle: CUInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetOutputByPlugin         : function (system: PFMOD_SYSTEM; handle: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_CreateDSPByPlugin         : function (system: PFMOD_SYSTEM; handle: CUInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_System_GetDSPInfoByPlugin        : function (system: PFMOD_SYSTEM; handle: CUInt; description: PPFMOD_DSP_DESCRIPTION): TFMOD_RESULT; extdecl;
  FMOD_System_RegisterCodec             : function (system: PFMOD_SYSTEM; description: PFMOD_CODEC_DESCRIPTION; handle: PCUInt; priority: CUInt): TFMOD_RESULT; extdecl;
  FMOD_System_RegisterDSP               : function (system: PFMOD_SYSTEM; description: PFMOD_DSP_DESCRIPTION; handle: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_RegisterOutput            : function (system: PFMOD_SYSTEM; description: PFMOD_OUTPUT_DESCRIPTION; handle: PCUInt): TFMOD_RESULT; extdecl;
*)

{ Init/Close. }
  FMOD_System_Init                      : function (system: PFMOD_SYSTEM; maxchannels: CInt; flags: TFMOD_INITFLAGS; extradriverdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_System_Close                     : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ General post-init system functions. }
  FMOD_System_Update                    : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
  FMOD_System_SetSpeakerPosition        : function (system: PFMOD_SYSTEM; speaker: TFMOD_SPEAKER; x: CFloat; y: CFloat; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_System_GetSpeakerPosition        : function (system: PFMOD_SYSTEM; speaker: TFMOD_SPEAKER; x: PCFloat; y: PCFloat; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_System_SetStreamBufferSize       : function (system: PFMOD_SYSTEM; filebuffersize: CUInt; filebuffersizetype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_System_GetStreamBufferSize       : function (system: PFMOD_SYSTEM; filebuffersize: PCUInt; filebuffersizetype: PFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_System_Set3DSettings             : function (system: PFMOD_SYSTEM; dopplerscale: CFloat; distancefactor: CFloat; rolloffscale: CFloat): TFMOD_RESULT; extdecl;
  FMOD_System_Get3DSettings             : function (system: PFMOD_SYSTEM; dopplerscale: PCFloat; distancefactor: PCFloat; rolloffscale: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_System_Set3DNumListeners         : function (system: PFMOD_SYSTEM; numlisteners: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_Get3DNumListeners         : function (system: PFMOD_SYSTEM; numlisteners: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_Set3DListenerAttributes   : function (system: PFMOD_SYSTEM; listener: CInt; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_System_Get3DListenerAttributes   : function (system: PFMOD_SYSTEM; listener: CInt; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_System_Set3DRolloffCallback      : function (system: PFMOD_SYSTEM; callback: TFMOD_3D_ROLLOFF_CALLBACK): TFMOD_RESULT; extdecl;
  FMOD_System_MixerSuspend              : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
  FMOD_System_MixerResume               : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
  FMOD_System_GetDefaultMixMatrix       : function (system: PFMOD_SYSTEM; sourcespeakermode: TFMOD_SPEAKERMODE; targetspeakermode: TFMOD_SPEAKERMODE; matrix: PCFloat; matrixhop: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetSpeakerModeChannels    : function (system: PFMOD_SYSTEM; mode: TFMOD_SPEAKERMODE; channels: PCInt): TFMOD_RESULT; extdecl;

{ System information functions. }
  FMOD_System_GetVersion                : function (system: PFMOD_SYSTEM; version: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetOutputHandle           : function (system: PFMOD_SYSTEM; handle: PPointer): TFMOD_RESULT; extdecl;
  FMOD_System_GetChannelsPlaying        : function (system: PFMOD_SYSTEM; channels: PCInt; realchannels: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetCPUUsage               : function (system: PFMOD_SYSTEM; dsp: PCFloat; stream: PCFloat; geometry: PCFloat; update: PCFloat; total: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_System_GetFileUsage              : function (system: PFMOD_SYSTEM; sampleBytesRead: PCLongLong; streamBytesRead: PCLongLong; otherBytesRead: PCLongLong): TFMOD_RESULT; extdecl;

{ Sound/DSP/Channel/FX creation and retrieval. }
  FMOD_System_CreateSound               : function (system: PFMOD_SYSTEM; name_or_data: PChar; mode: TFMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
  FMOD_System_CreateStream              : function (system: PFMOD_SYSTEM; name_or_data: PChar; mode: TFMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
// TODO   FMOD_System_CreateDSP                 : function (system: PFMOD_SYSTEM; description: PFMOD_DSP_DESCRIPTION; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
// TODO   FMOD_System_CreateDSPByType           : function (system: PFMOD_SYSTEM; type_: TFMOD_DSP_TYPE; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_System_CreateChannelGroup        : function (system: PFMOD_SYSTEM; name: PChar; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
  FMOD_System_CreateSoundGroup          : function (system: PFMOD_SYSTEM; name: PChar; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
  FMOD_System_CreateReverb3D            : function (system: PFMOD_SYSTEM; reverb: PPFMOD_REVERB3D): TFMOD_RESULT; extdecl;
  FMOD_System_PlaySound                 : function (system: PFMOD_SYSTEM; sound: PFMOD_SOUND; channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
  FMOD_System_PlayDSP                   : function (system: PFMOD_SYSTEM; dsp: PFMOD_DSP; channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
  FMOD_System_GetChannel                : function (system: PFMOD_SYSTEM; channelid: CInt; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
  FMOD_System_GetMasterChannelGroup     : function (system: PFMOD_SYSTEM; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
  FMOD_System_GetMasterSoundGroup       : function (system: PFMOD_SYSTEM; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{ Routing to ports. }
  FMOD_System_AttachChannelGroupToPort  : function (system: PFMOD_SYSTEM; portType: TFMOD_PORT_TYPE; portIndex: TFMOD_PORT_INDEX; channelgroup: PFMOD_CHANNELGROUP; passThru: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_System_DetachChannelGroupFromPort: function (system: PFMOD_SYSTEM; channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{ Reverb API. }
  FMOD_System_SetReverbProperties       : function (system: PFMOD_SYSTEM; instance: CInt; prop: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
  FMOD_System_GetReverbProperties       : function (system: PFMOD_SYSTEM; instance: CInt; prop: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;

{ System level DSP functionality. }
  FMOD_System_LockDSP                   : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
  FMOD_System_UnlockDSP                 : function (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ Recording API. }
  FMOD_System_GetRecordNumDrivers       : function (system: PFMOD_SYSTEM; numdrivers: PCInt; numconnected: PCInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetRecordDriverInfo       : function (system: PFMOD_SYSTEM; id: CInt; name: PChar; namelen: CInt; guid: PFMOD_GUID; systemrate: PCInt; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PCInt; state: PFMOD_DRIVER_STATE): TFMOD_RESULT; extdecl;
  FMOD_System_GetRecordPosition         : function (system: PFMOD_SYSTEM; id: CInt; position: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_System_RecordStart               : function (system: PFMOD_SYSTEM; id: CInt; sound: PFMOD_SOUND; loop: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_System_RecordStop                : function (system: PFMOD_SYSTEM; id: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_IsRecording               : function (system: PFMOD_SYSTEM; id: CInt; recording: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{ Geometry API. }
  FMOD_System_CreateGeometry            : function (system: PFMOD_SYSTEM; maxpolygons: CInt; maxvertices: CInt; geometry: PPFMOD_GEOMETRY): TFMOD_RESULT; extdecl;
  FMOD_System_SetGeometrySettings       : function (system: PFMOD_SYSTEM; maxworldsize: CFloat): TFMOD_RESULT; extdecl;
  FMOD_System_GetGeometrySettings       : function (system: PFMOD_SYSTEM; maxworldsize: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_System_LoadGeometry              : function (system: PFMOD_SYSTEM; data: Pointer; datasize: CInt; geometry: PPFMOD_GEOMETRY): TFMOD_RESULT; extdecl;
  FMOD_System_GetGeometryOcclusion      : function (system: PFMOD_SYSTEM; listener: PFMOD_VECTOR; source: PFMOD_VECTOR; direct: PCFloat; reverb: PCFloat): TFMOD_RESULT; extdecl;

{ Network functions. }
  FMOD_System_SetNetworkProxy           : function (system: PFMOD_SYSTEM; proxy: PChar): TFMOD_RESULT; extdecl;
  FMOD_System_GetNetworkProxy           : function (system: PFMOD_SYSTEM; proxy: PChar; proxylen: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_SetNetworkTimeout         : function (system: PFMOD_SYSTEM; timeout: CInt): TFMOD_RESULT; extdecl;
  FMOD_System_GetNetworkTimeout         : function (system: PFMOD_SYSTEM; timeout: PCInt): TFMOD_RESULT; extdecl;

{ Userdata set/get. }
  FMOD_System_SetUserData               : function (system: PFMOD_SYSTEM; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_System_GetUserData               : function (system: PFMOD_SYSTEM; userdata: PPointer): TFMOD_RESULT; extdecl;

{ Sound API }

  FMOD_Sound_Release                    : function (sound: PFMOD_SOUND): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetSystemObject            : function (sound: PFMOD_SOUND; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     Standard sound manipulation functions.
}

  FMOD_Sound_Lock                       : function (sound: PFMOD_SOUND; offset: CUInt; length: CUInt; ptr1: PPointer; ptr2: PPointer; len1: PCUInt; len2: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_Unlock                     : function (sound: PFMOD_SOUND; ptr1: Pointer; ptr2: Pointer; len1: CUInt; len2: CUInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_SetDefaults                : function (sound: PFMOD_SOUND; frequency: CFloat; priority: CInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetDefaults                : function (sound: PFMOD_SOUND; frequency: PCFloat; priority: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_Set3DMinMaxDistance        : function (sound: PFMOD_SOUND; min: CFloat; max: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_Get3DMinMaxDistance        : function (sound: PFMOD_SOUND; min: PCFloat; max: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_Set3DConeSettings          : function (sound: PFMOD_SOUND; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_Get3DConeSettings          : function (sound: PFMOD_SOUND; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_Set3DCustomRolloff         : function (sound: PFMOD_SOUND; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_Get3DCustomRolloff         : function (sound: PFMOD_SOUND; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetSubSound                : function (sound: PFMOD_SOUND; index: CInt; subsound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetSubSoundParent          : function (sound: PFMOD_SOUND; parentsound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetName                    : function (sound: PFMOD_SOUND; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetLength                  : function (sound: PFMOD_SOUND; length: PCUInt; lengthtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetFormat                  : function (sound: PFMOD_SOUND; type_: PFMOD_SOUND_TYPE; format: PFMOD_SOUND_FORMAT; channels: PCInt; bits: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetNumSubSounds            : function (sound: PFMOD_SOUND; numsubsounds: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetNumTags                 : function (sound: PFMOD_SOUND; numtags: PCInt; numtagsupdated: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetTag                     : function (sound: PFMOD_SOUND; name: PChar; index: CInt; tag: PFMOD_TAG): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetOpenState               : function (sound: PFMOD_SOUND; openstate: PFMOD_OPENSTATE; percentbuffered: PCUInt; starving: PFMOD_BOOL; diskbusy: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Sound_ReadData                   : function (sound: PFMOD_SOUND; buffer: Pointer; length: CUInt; read: PCUInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_SeekData                   : function (sound: PFMOD_SOUND; pcm: CUInt): TFMOD_RESULT; extdecl;

  FMOD_Sound_SetSoundGroup              : function (sound: PFMOD_SOUND; soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetSoundGroup              : function (sound: PFMOD_SOUND; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{
     Synchronization poAPI: CInt.  These points can come from markers embedded in wav files; and can also generate channel callbacks.
}

  FMOD_Sound_GetNumSyncPoints           : function (sound: PFMOD_SOUND; numsyncpoints: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetSyncPoint               : function (sound: PFMOD_SOUND; index: CInt; point: PPFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetSyncPointInfo           : function (sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT; name: PChar; namelen: CInt; offset: PCUInt; offsettype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_Sound_AddSyncPoint               : function (sound: PFMOD_SOUND; offset: CUInt; offsettype: TFMOD_TIMEUNIT; name: PChar; point: PPFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;
  FMOD_Sound_DeleteSyncPoint            : function (sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;

{
     Functions also in Channel class but here they are the 'default' to save having to change it in Channel all the time.
}

  FMOD_Sound_SetMode                    : function (sound: PFMOD_SOUND; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetMode                    : function (sound: PFMOD_SOUND; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
  FMOD_Sound_SetLoopCount               : function (sound: PFMOD_SOUND; loopcount: CInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetLoopCount               : function (sound: PFMOD_SOUND; loopcount: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_SetLoopPoints              : function (sound: PFMOD_SOUND; loopstart: CUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: CUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetLoopPoints              : function (sound: PFMOD_SOUND; loopstart: PCUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: PCUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;

{
     For MOD/S3M/XM/IT/MID sequenced formats only.
}

  FMOD_Sound_GetMusicNumChannels        : function (sound: PFMOD_SOUND; numchannels: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Sound_SetMusicChannelVolume      : function (sound: PFMOD_SOUND; channel: CInt; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetMusicChannelVolume      : function (sound: PFMOD_SOUND; channel: CInt; volume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_SetMusicSpeed              : function (sound: PFMOD_SOUND; speed: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetMusicSpeed              : function (sound: PFMOD_SOUND; speed: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_Sound_SetUserData                : function (sound: PFMOD_SOUND; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_Sound_GetUserData                : function (sound: PFMOD_SOUND; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Channel' API
}

  FMOD_Channel_GetSystemObject          : function (channel: PFMOD_CHANNEL; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     General control functionality for Channels and ChannelGroups.
}

  FMOD_Channel_Stop                     : function (channel: PFMOD_CHANNEL): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetPaused                : function (channel: PFMOD_CHANNEL; paused: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetPaused                : function (channel: PFMOD_CHANNEL; paused: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetVolume                : function (channel: PFMOD_CHANNEL; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetVolume                : function (channel: PFMOD_CHANNEL; volume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetVolumeRamp            : function (channel: PFMOD_CHANNEL; ramp: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetVolumeRamp            : function (channel: PFMOD_CHANNEL; ramp: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetAudibility            : function (channel: PFMOD_CHANNEL; audibility: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetPitch                 : function (channel: PFMOD_CHANNEL; pitch: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetPitch                 : function (channel: PFMOD_CHANNEL; pitch: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetMute                  : function (channel: PFMOD_CHANNEL; mute: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetMute                  : function (channel: PFMOD_CHANNEL; mute: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetReverbProperties      : function (channel: PFMOD_CHANNEL; instance: CInt; wet: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetReverbProperties      : function (channel: PFMOD_CHANNEL; instance: CInt; wet: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetLowPassGain           : function (channel: PFMOD_CHANNEL; gain: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetLowPassGain           : function (channel: PFMOD_CHANNEL; gain: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetMode                  : function (channel: PFMOD_CHANNEL; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetMode                  : function (channel: PFMOD_CHANNEL; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetCallback              : function (channel: PFMOD_CHANNEL; callback: TFMOD_CHANNELCONTROL_CALLBACK): TFMOD_RESULT; extdecl;
  FMOD_Channel_IsPlaying                : function (channel: PFMOD_CHANNEL; isplaying: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Note all 'set' functions alter a final matrix; this is why the only get function is getMixMatrix; to avoid other get functions returning incorrect/obsolete values.
}

  FMOD_Channel_SetPan                   : function (channel: PFMOD_CHANNEL; pan: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetMixLevelsOutput       : function (channel: PFMOD_CHANNEL; frontleft: CFloat; frontright: CFloat; center: CFloat; lfe: CFloat; surroundleft: CFloat; surroundright: CFloat; backleft: CFloat; backright: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetMixLevelsInput        : function (channel: PFMOD_CHANNEL; levels: PCFloat; numlevels: CInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetMixMatrix             : function (channel: PFMOD_CHANNEL; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetMixMatrix             : function (channel: PFMOD_CHANNEL; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;

{
     Clock based functionality.
}

  FMOD_Channel_GetDSPClock              : function (channel: PFMOD_CHANNEL; dspclock: PCULongLong; parentclock: PCULongLong): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetDelay                 : function (channel: PFMOD_CHANNEL; dspclock_start: CULongLong; dspclock_end: CULongLong; stopchannels: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetDelay                 : function (channel: PFMOD_CHANNEL; dspclock_start: PCULongLong; dspclock_end: PCULongLong; stopchannels: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_AddFadePoint             : function (channel: PFMOD_CHANNEL; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetFadePointRamp         : function (channel: PFMOD_CHANNEL; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_RemoveFadePoints         : function (channel: PFMOD_CHANNEL; dspclock_start: CULongLong; dspclock_end: CULongLong): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetFadePoints            : function (channel: PFMOD_CHANNEL; numpoints: PCUInt; point_dspclock: PCULongLong; point_volume: PCFloat): TFMOD_RESULT; extdecl;

{
     DSP effects.
}

  FMOD_Channel_GetDSP                   : function (channel: PFMOD_CHANNEL; index: CInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_Channel_AddDSP                   : function (channel: PFMOD_CHANNEL; index: CInt; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_Channel_RemoveDSP                : function (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetNumDSPs               : function (channel: PFMOD_CHANNEL; numdsps: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetDSPIndex              : function (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: CInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetDSPIndex              : function (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: PCInt): TFMOD_RESULT; extdecl;

{
     3D functionality.
}

  FMOD_Channel_Set3DAttributes          : function (channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DAttributes          : function (channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DMinMaxDistance      : function (channel: PFMOD_CHANNEL; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DMinMaxDistance      : function (channel: PFMOD_CHANNEL; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DConeSettings        : function (channel: PFMOD_CHANNEL; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DConeSettings        : function (channel: PFMOD_CHANNEL; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DConeOrientation     : function (channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DConeOrientation     : function (channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DCustomRolloff       : function (channel: PFMOD_CHANNEL; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DCustomRolloff       : function (channel: PFMOD_CHANNEL; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DOcclusion           : function (channel: PFMOD_CHANNEL; directocclusion: CFloat; reverbocclusion: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DOcclusion           : function (channel: PFMOD_CHANNEL; directocclusion: PCFloat; reverbocclusion: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DSpread              : function (channel: PFMOD_CHANNEL; angle: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DSpread              : function (channel: PFMOD_CHANNEL; angle: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DLevel               : function (channel: PFMOD_CHANNEL; level: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DLevel               : function (channel: PFMOD_CHANNEL; level: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DDopplerLevel        : function (channel: PFMOD_CHANNEL; level: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DDopplerLevel        : function (channel: PFMOD_CHANNEL; level: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Set3DDistanceFilter      : function (channel: PFMOD_CHANNEL; custom: TFMOD_BOOL; customLevel: CFloat; centerFreq: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_Get3DDistanceFilter      : function (channel: PFMOD_CHANNEL; custom: PFMOD_BOOL; customLevel: PCFloat; centerFreq: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_Channel_SetUserData              : function (channel: PFMOD_CHANNEL; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetUserData              : function (channel: PFMOD_CHANNEL; userdata: PPointer): TFMOD_RESULT; extdecl;

{
     Channel specific control functionality.
}

  FMOD_Channel_SetFrequency             : function (channel: PFMOD_CHANNEL; frequency: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetFrequency             : function (channel: PFMOD_CHANNEL; frequency: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetPriority              : function (channel: PFMOD_CHANNEL; priority: CInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetPriority              : function (channel: PFMOD_CHANNEL; priority: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetPosition              : function (channel: PFMOD_CHANNEL; position: CUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetPosition              : function (channel: PFMOD_CHANNEL; position: PCUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetChannelGroup          : function (channel: PFMOD_CHANNEL; channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetChannelGroup          : function (channel: PFMOD_CHANNEL; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetLoopCount             : function (channel: PFMOD_CHANNEL; loopcount: CInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetLoopCount             : function (channel: PFMOD_CHANNEL; loopcount: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Channel_SetLoopPoints            : function (channel: PFMOD_CHANNEL; loopstart: CUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: CUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetLoopPoints            : function (channel: PFMOD_CHANNEL; loopstart: PCUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: PCUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

  FMOD_Channel_IsVirtual                : function (channel: PFMOD_CHANNEL; isvirtual: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetCurrentSound          : function (channel: PFMOD_CHANNEL; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
  FMOD_Channel_GetIndex                 : function (channel: PFMOD_CHANNEL; index: PCInt): TFMOD_RESULT; extdecl;

{
    'ChannelGroup' API
}

  FMOD_ChannelGroup_GetSystemObject     : function (channelgroup: PFMOD_CHANNELGROUP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     General control functionality for Channels and ChannelGroups.
}

  FMOD_ChannelGroup_Stop                : function (channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetPaused           : function (channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetPaused           : function (channelgroup: PFMOD_CHANNELGROUP; paused: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetVolume           : function (channelgroup: PFMOD_CHANNELGROUP; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetVolume           : function (channelgroup: PFMOD_CHANNELGROUP; volume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetVolumeRamp       : function (channelgroup: PFMOD_CHANNELGROUP; ramp: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetVolumeRamp       : function (channelgroup: PFMOD_CHANNELGROUP; ramp: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetAudibility       : function (channelgroup: PFMOD_CHANNELGROUP; audibility: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetPitch            : function (channelgroup: PFMOD_CHANNELGROUP; pitch: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetPitch            : function (channelgroup: PFMOD_CHANNELGROUP; pitch: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetMute             : function (channelgroup: PFMOD_CHANNELGROUP; mute: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetMute             : function (channelgroup: PFMOD_CHANNELGROUP; mute: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetReverbProperties : function (channelgroup: PFMOD_CHANNELGROUP; instance: CInt; wet: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetReverbProperties : function (channelgroup: PFMOD_CHANNELGROUP; instance: CInt; wet: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetLowPassGain      : function (channelgroup: PFMOD_CHANNELGROUP; gain: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetLowPassGain      : function (channelgroup: PFMOD_CHANNELGROUP; gain: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetMode             : function (channelgroup: PFMOD_CHANNELGROUP; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetMode             : function (channelgroup: PFMOD_CHANNELGROUP; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetCallback         : function (channelgroup: PFMOD_CHANNELGROUP; callback: TFMOD_CHANNELCONTROL_CALLBACK): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_IsPlaying           : function (channelgroup: PFMOD_CHANNELGROUP; isplaying: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Note all 'set' functions alter a final matrix; this is why the only get function is getMixMatrix; to avoid other get functions returning incorrect/obsolete values.
}

  FMOD_ChannelGroup_SetPan              : function (channelgroup: PFMOD_CHANNELGROUP; pan: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetMixLevelsOutput  : function (channelgroup: PFMOD_CHANNELGROUP; frontleft: CFloat; frontright: CFloat; center: CFloat; lfe: CFloat; surroundleft: CFloat; surroundright: CFloat; backleft: CFloat; backright: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetMixLevelsInput   : function (channelgroup: PFMOD_CHANNELGROUP; levels: PCFloat; numlevels: CInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetMixMatrix        : function (channelgroup: PFMOD_CHANNELGROUP; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetMixMatrix        : function (channelgroup: PFMOD_CHANNELGROUP; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;

{
     Clock based functionality.
}

  FMOD_ChannelGroup_GetDSPClock         : function (channelgroup: PFMOD_CHANNELGROUP; dspclock: PCULongLong; parentclock: PCULongLong): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetDelay            : function (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: CULongLong; dspclock_end: CULongLong; stopchannels: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetDelay            : function (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: PCULongLong; dspclock_end: PCULongLong; stopchannels: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_AddFadePoint        : function (channelgroup: PFMOD_CHANNELGROUP; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetFadePointRamp    : function (channelgroup: PFMOD_CHANNELGROUP; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_RemoveFadePoints    : function (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: CULongLong; dspclock_end: CULongLong): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetFadePoints       : function (channelgroup: PFMOD_CHANNELGROUP; numpoints: PCUInt; point_dspclock: PCULongLong; point_volume: PCFloat): TFMOD_RESULT; extdecl;

{
     DSP effects.
}

  FMOD_ChannelGroup_GetDSP              : function (channelgroup: PFMOD_CHANNELGROUP; index: CInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_AddDSP              : function (channelgroup: PFMOD_CHANNELGROUP; index: CInt; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_RemoveDSP           : function (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetNumDSPs          : function (channelgroup: PFMOD_CHANNELGROUP; numdsps: PCInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_SetDSPIndex         : function (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: CInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetDSPIndex         : function (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: PCInt): TFMOD_RESULT; extdecl;

{
     3D functionality.
}

  FMOD_ChannelGroup_Set3DAttributes     : function (channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DAttributes     : function (channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DMinMaxDistance : function (channelgroup: PFMOD_CHANNELGROUP; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DMinMaxDistance : function (channelgroup: PFMOD_CHANNELGROUP; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DConeSettings   : function (channelgroup: PFMOD_CHANNELGROUP; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DConeSettings   : function (channelgroup: PFMOD_CHANNELGROUP; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DConeOrientation: function (channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DConeOrientation: function (channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DCustomRolloff  : function (channelgroup: PFMOD_CHANNELGROUP; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DCustomRolloff  : function (channelgroup: PFMOD_CHANNELGROUP; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DOcclusion      : function (channelgroup: PFMOD_CHANNELGROUP; directocclusion: CFloat; reverbocclusion: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DOcclusion      : function (channelgroup: PFMOD_CHANNELGROUP; directocclusion: PCFloat; reverbocclusion: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DSpread         : function (channelgroup: PFMOD_CHANNELGROUP; angle: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DSpread         : function (channelgroup: PFMOD_CHANNELGROUP; angle: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DLevel          : function (channelgroup: PFMOD_CHANNELGROUP; level: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DLevel          : function (channelgroup: PFMOD_CHANNELGROUP; level: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DDopplerLevel   : function (channelgroup: PFMOD_CHANNELGROUP; level: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DDopplerLevel   : function (channelgroup: PFMOD_CHANNELGROUP; level: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Set3DDistanceFilter : function (channelgroup: PFMOD_CHANNELGROUP; custom: TFMOD_BOOL; customLevel: CFloat; centerFreq: CFloat): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_Get3DDistanceFilter : function (channelgroup: PFMOD_CHANNELGROUP; custom: PFMOD_BOOL; customLevel: PCFloat; centerFreq: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_ChannelGroup_SetUserData         : function (channelgroup: PFMOD_CHANNELGROUP; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetUserData         : function (channelgroup: PFMOD_CHANNELGROUP; userdata: PPointer): TFMOD_RESULT; extdecl;

  FMOD_ChannelGroup_Release             : function (channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{
     Nested channel groups.
}

  FMOD_ChannelGroup_AddGroup            : function (channelgroup: PFMOD_CHANNELGROUP; group: PFMOD_CHANNELGROUP; propagatedspclock: TFMOD_BOOL; connection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetNumGroups        : function (channelgroup: PFMOD_CHANNELGROUP; numgroups: PCInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetGroup            : function (channelgroup: PFMOD_CHANNELGROUP; index: CInt; group: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetParentGroup      : function (channelgroup: PFMOD_CHANNELGROUP; group: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

  FMOD_ChannelGroup_GetName             : function (channelgroup: PFMOD_CHANNELGROUP; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetNumChannels      : function (channelgroup: PFMOD_CHANNELGROUP; numchannels: PCInt): TFMOD_RESULT; extdecl;
  FMOD_ChannelGroup_GetChannel          : function (channelgroup: PFMOD_CHANNELGROUP; index: CInt; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;

{
    'SoundGroup' API
}

  FMOD_SoundGroup_Release               : function (soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetSystemObject       : function (soundgroup: PFMOD_SOUNDGROUP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     SoundGroup control functions.
}

  FMOD_SoundGroup_SetMaxAudible         : function (soundgroup: PFMOD_SOUNDGROUP; maxaudible: CInt): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetMaxAudible         : function (soundgroup: PFMOD_SOUNDGROUP; maxaudible: PCInt): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_SetMaxAudibleBehavior : function (soundgroup: PFMOD_SOUNDGROUP; behavior: TFMOD_SOUNDGROUP_BEHAVIOR): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetMaxAudibleBehavior : function (soundgroup: PFMOD_SOUNDGROUP; behavior: PFMOD_SOUNDGROUP_BEHAVIOR): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_SetMuteFadeSpeed      : function (soundgroup: PFMOD_SOUNDGROUP; speed: CFloat): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetMuteFadeSpeed      : function (soundgroup: PFMOD_SOUNDGROUP; speed: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_SetVolume             : function (soundgroup: PFMOD_SOUNDGROUP; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetVolume             : function (soundgroup: PFMOD_SOUNDGROUP; volume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_Stop                  : function (soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

  FMOD_SoundGroup_GetName               : function (soundgroup: PFMOD_SOUNDGROUP; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetNumSounds          : function (soundgroup: PFMOD_SOUNDGROUP; numsounds: PCInt): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetSound              : function (soundgroup: PFMOD_SOUNDGROUP; index: CInt; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetNumPlaying         : function (soundgroup: PFMOD_SOUNDGROUP; numplaying: PCInt): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_SoundGroup_SetUserData           : function (soundgroup: PFMOD_SOUNDGROUP; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_SoundGroup_GetUserData           : function (soundgroup: PFMOD_SOUNDGROUP; userdata: PPointer): TFMOD_RESULT; extdecl;

(* TODO: translate
{
    'DSP' API
}

  FMOD_DSP_Release                      : function (dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetSystemObject              : function (dsp: PFMOD_DSP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     Connection / disconnection / input and output enumeration.
}

  FMOD_DSP_AddInput                     : function (dsp: PFMOD_DSP; input: PFMOD_DSP; connection: PPFMOD_DSPCONNECTION; type_: TFMOD_DSPCONNECTION_TYPE): TFMOD_RESULT; extdecl;
  FMOD_DSP_DisconnectFrom               : function (dsp: PFMOD_DSP; target: PFMOD_DSP; connection: PFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
  FMOD_DSP_DisconnectAll                : function (dsp: PFMOD_DSP; inputs: TFMOD_BOOL; outputs: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetNumInputs                 : function (dsp: PFMOD_DSP; numinputs: PCInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetNumOutputs                : function (dsp: PFMOD_DSP; numoutputs: PCInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetInput                     : function (dsp: PFMOD_DSP; index: CInt; input: PPFMOD_DSP; inputconnection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetOutput                    : function (dsp: PFMOD_DSP; index: CInt; output: PPFMOD_DSP; outputconnection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;

{
     DSP unit control.
}

  FMOD_DSP_SetActive                    : function (dsp: PFMOD_DSP; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetActive                    : function (dsp: PFMOD_DSP; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_SetBypass                    : function (dsp: PFMOD_DSP; bypass: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetBypass                    : function (dsp: PFMOD_DSP; bypass: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_SetWetDryMix                 : function (dsp: PFMOD_DSP; prewet: CFloat; postwet: CFloat; dry: CFloat): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetWetDryMix                 : function (dsp: PFMOD_DSP; prewet: PCFloat; postwet: PCFloat; dry: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_DSP_SetChannelFormat             : function (dsp: PFMOD_DSP; channelmask: TFMOD_CHANNELMASK; numchannels: CInt; source_speakermode: TFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetChannelFormat             : function (dsp: PFMOD_DSP; channelmask: PFMOD_CHANNELMASK; numchannels: PCInt; source_speakermode: PFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetOutputChannelFormat       : function (dsp: PFMOD_DSP; inmask: TFMOD_CHANNELMASK; inchannels: CInt; inspeakermode: TFMOD_SPEAKERMODE; outmask: PFMOD_CHANNELMASK; outchannels: PCInt; outspeakermode: PFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
  FMOD_DSP_Reset                        : function (dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;

{
     DSP parameter control.
}

  FMOD_DSP_SetParameterFloat            : function (dsp: PFMOD_DSP; index: CInt; value: CFloat): TFMOD_RESULT; extdecl;
  FMOD_DSP_SetParameterInt              : function (dsp: PFMOD_DSP; index: CInt; value: CInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_SetParameterBool             : function (dsp: PFMOD_DSP; index: CInt; value: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_SetParameterData             : function (dsp: PFMOD_DSP; index: CInt; data: Pointer; length: CUInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetParameterFloat            : function (dsp: PFMOD_DSP; index: CInt; value: PCFloat; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetParameterInt              : function (dsp: PFMOD_DSP; index: CInt; value: PCInt; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetParameterBool             : function (dsp: PFMOD_DSP; index: CInt; value: PFMOD_BOOL; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetParameterData             : function (dsp: PFMOD_DSP; index: CInt; data: PPointer; length: PCUInt; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetNumParameters             : function (dsp: PFMOD_DSP; numparams: PCInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetParameterInfo             : function (dsp: PFMOD_DSP; index: CInt; desc: PPFMOD_DSP_PARAMETER_DESC): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetDataParameterIndex        : function (dsp: PFMOD_DSP; datatype: CInt; index: PCInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_ShowConfigDialog             : function (dsp: PFMOD_DSP; hwnd: Pointer; show: TFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     DSP attributes.
}

  FMOD_DSP_GetInfo                      : function (dsp: PFMOD_DSP; name: PChar; version: PCUInt; channels: PCInt; configwidth: PCInt; configheight: PCInt): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetType                      : function (dsp: PFMOD_DSP; type_: PFMOD_DSP_TYPE): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetIdle                      : function (dsp: PFMOD_DSP; idle: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_DSP_SetUserData                  : function (dsp: PFMOD_DSP; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetUserData                  : function (dsp: PFMOD_DSP; userdata: PPointer): TFMOD_RESULT; extdecl;

{
     Metering.
}

  FMOD_DSP_SetMeteringEnabled           : function (dsp: PFMOD_DSP; inputEnabled: TFMOD_BOOL; outputEnabled: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetMeteringEnabled           : function (dsp: PFMOD_DSP; inputEnabled: PFMOD_BOOL; outputEnabled: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetMeteringInfo              : function (dsp: PFMOD_DSP; inputInfo: PFMOD_DSP_METERING_INFO; outputInfo: PFMOD_DSP_METERING_INFO): TFMOD_RESULT; extdecl;
  FMOD_DSP_GetCPUUsage                  : function (dsp: PFMOD_DSP; exclusive: PCUInt; inclusive: PCUInt): TFMOD_RESULT; extdecl;
*)

{
    'DSPConnection' API
}

  FMOD_DSPConnection_GetInput           : function (dspconnection: PFMOD_DSPCONNECTION; input: PPFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_DSPConnection_GetOutput          : function (dspconnection: PFMOD_DSPCONNECTION; output: PPFMOD_DSP): TFMOD_RESULT; extdecl;
  FMOD_DSPConnection_SetMix             : function (dspconnection: PFMOD_DSPCONNECTION; volume: CFloat): TFMOD_RESULT; extdecl;
  FMOD_DSPConnection_GetMix             : function (dspconnection: PFMOD_DSPCONNECTION; volume: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_DSPConnection_SetMixMatrix       : function (dspconnection: PFMOD_DSPCONNECTION; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
  FMOD_DSPConnection_GetMixMatrix       : function (dspconnection: PFMOD_DSPCONNECTION; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
// TODO   FMOD_DSPConnection_GetType            : function (dspconnection: PFMOD_DSPCONNECTION; type_: PFMOD_DSPCONNECTION_TYPE): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_DSPConnection_SetUserData        : function (dspconnection: PFMOD_DSPCONNECTION; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_DSPConnection_GetUserData        : function (dspconnection: PFMOD_DSPCONNECTION; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Geometry' API
}

  FMOD_Geometry_Release                 : function (geometry: PFMOD_GEOMETRY): TFMOD_RESULT; extdecl;

{
     Polygon manipulation.
}

  FMOD_Geometry_AddPolygon              : function (geometry: PFMOD_GEOMETRY; directocclusion: CFloat; reverbocclusion: CFloat; doublesided: TFMOD_BOOL; numvertices: CInt; vertices: PFMOD_VECTOR; polygonindex: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetNumPolygons          : function (geometry: PFMOD_GEOMETRY; numpolygons: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetMaxPolygons          : function (geometry: PFMOD_GEOMETRY; maxpolygons: PCInt; maxvertices: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetPolygonNumVertices   : function (geometry: PFMOD_GEOMETRY; index: CInt; numvertices: PCInt): TFMOD_RESULT; extdecl;
  FMOD_Geometry_SetPolygonVertex        : function (geometry: PFMOD_GEOMETRY; index: CInt; vertexindex: CInt; vertex: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetPolygonVertex        : function (geometry: PFMOD_GEOMETRY; index: CInt; vertexindex: CInt; vertex: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_SetPolygonAttributes    : function (geometry: PFMOD_GEOMETRY; index: CInt; directocclusion: CFloat; reverbocclusion: CFloat; doublesided: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetPolygonAttributes    : function (geometry: PFMOD_GEOMETRY; index: CInt; directocclusion: PCFloat; reverbocclusion: PCFloat; doublesided: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Object manipulation.
}

  FMOD_Geometry_SetActive               : function (geometry: PFMOD_GEOMETRY; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetActive               : function (geometry: PFMOD_GEOMETRY; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Geometry_SetRotation             : function (geometry: PFMOD_GEOMETRY; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetRotation             : function (geometry: PFMOD_GEOMETRY; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_SetPosition             : function (geometry: PFMOD_GEOMETRY; position: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetPosition             : function (geometry: PFMOD_GEOMETRY; position: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_SetScale                : function (geometry: PFMOD_GEOMETRY; scale: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetScale                : function (geometry: PFMOD_GEOMETRY; scale: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
  FMOD_Geometry_Save                    : function (geometry: PFMOD_GEOMETRY; data: Pointer; datasize: PCInt): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_Geometry_SetUserData             : function (geometry: PFMOD_GEOMETRY; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_Geometry_GetUserData             : function (geometry: PFMOD_GEOMETRY; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Reverb3D' API
}

  FMOD_Reverb3D_Release                 : function (reverb3d: PFMOD_REVERB3D): TFMOD_RESULT; extdecl;

{
     Reverb manipulation.
}

  FMOD_Reverb3D_Set3DAttributes         : function (reverb3d: PFMOD_REVERB3D; position: PFMOD_VECTOR; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
  FMOD_Reverb3D_Get3DAttributes         : function (reverb3d: PFMOD_REVERB3D; position: PFMOD_VECTOR; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
  FMOD_Reverb3D_SetProperties           : function (reverb3d: PFMOD_REVERB3D; properties: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
  FMOD_Reverb3D_GetProperties           : function (reverb3d: PFMOD_REVERB3D; properties: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
  FMOD_Reverb3D_SetActive               : function (reverb3d: PFMOD_REVERB3D; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
  FMOD_Reverb3D_GetActive               : function (reverb3d: PFMOD_REVERB3D; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

  FMOD_Reverb3D_SetUserData             : function (reverb3d: PFMOD_REVERB3D; userdata: Pointer): TFMOD_RESULT; extdecl;
  FMOD_Reverb3D_GetUserData             : function (reverb3d: PFMOD_REVERB3D; userdata: PPointer): TFMOD_RESULT; extdecl;

implementation

uses SysUtils, CastleDynLib;

var
  FmodLibrary: TDynLib;

procedure FinalizeFmodLibrary;
begin
  FreeAndNil(FmodLibrary);
end;

procedure InitializeFmodLibrary;
begin
  FinalizeFmodLibrary;

  FmodLibrary := TDynLib.Load('libfmod.so.11', false);
  if FmodLibrary = nil then
    FmodLibrary := TDynLib.Load('libfmod.so', false);

  if FmodLibrary <> nil then
  begin
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Memory_Initialize) := FmodLibrary.Symbol('FMOD_Memory_Initialize');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Memory_GetStats) := FmodLibrary.Symbol('FMOD_Memory_GetStats');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Debug_Initialize) := FmodLibrary.Symbol('FMOD_Debug_Initialize');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_File_SetDiskBusy) := FmodLibrary.Symbol('FMOD_File_SetDiskBusy');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_File_GetDiskBusy) := FmodLibrary.Symbol('FMOD_File_GetDiskBusy');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Create) := FmodLibrary.Symbol('FMOD_System_Create');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Release) := FmodLibrary.Symbol('FMOD_System_Release');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetOutput) := FmodLibrary.Symbol('FMOD_System_SetOutput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetOutput) := FmodLibrary.Symbol('FMOD_System_GetOutput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetNumDrivers) := FmodLibrary.Symbol('FMOD_System_GetNumDrivers');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetDriverInfo) := FmodLibrary.Symbol('FMOD_System_GetDriverInfo');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetDriver) := FmodLibrary.Symbol('FMOD_System_SetDriver');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetDriver) := FmodLibrary.Symbol('FMOD_System_GetDriver');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetSoftwareChannels) := FmodLibrary.Symbol('FMOD_System_SetSoftwareChannels');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetSoftwareChannels) := FmodLibrary.Symbol('FMOD_System_GetSoftwareChannels');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetSoftwareFormat) := FmodLibrary.Symbol('FMOD_System_SetSoftwareFormat');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetSoftwareFormat) := FmodLibrary.Symbol('FMOD_System_GetSoftwareFormat');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetDSPBufferSize) := FmodLibrary.Symbol('FMOD_System_SetDSPBufferSize');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetDSPBufferSize) := FmodLibrary.Symbol('FMOD_System_GetDSPBufferSize');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetFileSystem) := FmodLibrary.Symbol('FMOD_System_SetFileSystem');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_AttachFileSystem) := FmodLibrary.Symbol('FMOD_System_AttachFileSystem');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetAdvancedSettings) := FmodLibrary.Symbol('FMOD_System_SetAdvancedSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetAdvancedSettings) := FmodLibrary.Symbol('FMOD_System_GetAdvancedSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetCallback) := FmodLibrary.Symbol('FMOD_System_SetCallback');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Init) := FmodLibrary.Symbol('FMOD_System_Init');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Close) := FmodLibrary.Symbol('FMOD_System_Close');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Update) := FmodLibrary.Symbol('FMOD_System_Update');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetSpeakerPosition) := FmodLibrary.Symbol('FMOD_System_SetSpeakerPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetSpeakerPosition) := FmodLibrary.Symbol('FMOD_System_GetSpeakerPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetStreamBufferSize) := FmodLibrary.Symbol('FMOD_System_SetStreamBufferSize');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetStreamBufferSize) := FmodLibrary.Symbol('FMOD_System_GetStreamBufferSize');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Set3DSettings) := FmodLibrary.Symbol('FMOD_System_Set3DSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Get3DSettings) := FmodLibrary.Symbol('FMOD_System_Get3DSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Set3DNumListeners) := FmodLibrary.Symbol('FMOD_System_Set3DNumListeners');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Get3DNumListeners) := FmodLibrary.Symbol('FMOD_System_Get3DNumListeners');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Set3DListenerAttributes) := FmodLibrary.Symbol('FMOD_System_Set3DListenerAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Get3DListenerAttributes) := FmodLibrary.Symbol('FMOD_System_Get3DListenerAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_Set3DRolloffCallback) := FmodLibrary.Symbol('FMOD_System_Set3DRolloffCallback');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_MixerSuspend) := FmodLibrary.Symbol('FMOD_System_MixerSuspend');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_MixerResume) := FmodLibrary.Symbol('FMOD_System_MixerResume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetDefaultMixMatrix) := FmodLibrary.Symbol('FMOD_System_GetDefaultMixMatrix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetSpeakerModeChannels) := FmodLibrary.Symbol('FMOD_System_GetSpeakerModeChannels');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetVersion) := FmodLibrary.Symbol('FMOD_System_GetVersion');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetOutputHandle) := FmodLibrary.Symbol('FMOD_System_GetOutputHandle');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetChannelsPlaying) := FmodLibrary.Symbol('FMOD_System_GetChannelsPlaying');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetCPUUsage) := FmodLibrary.Symbol('FMOD_System_GetCPUUsage');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetFileUsage) := FmodLibrary.Symbol('FMOD_System_GetFileUsage');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateSound) := FmodLibrary.Symbol('FMOD_System_CreateSound');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateStream) := FmodLibrary.Symbol('FMOD_System_CreateStream');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateDSP) := FmodLibrary.Symbol('FMOD_System_CreateDSP');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateDSPByType) := FmodLibrary.Symbol('FMOD_System_CreateDSPByType');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateChannelGroup) := FmodLibrary.Symbol('FMOD_System_CreateChannelGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateSoundGroup) := FmodLibrary.Symbol('FMOD_System_CreateSoundGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateReverb3D) := FmodLibrary.Symbol('FMOD_System_CreateReverb3D');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_PlaySound) := FmodLibrary.Symbol('FMOD_System_PlaySound');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_PlayDSP) := FmodLibrary.Symbol('FMOD_System_PlayDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetChannel) := FmodLibrary.Symbol('FMOD_System_GetChannel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetMasterChannelGroup) := FmodLibrary.Symbol('FMOD_System_GetMasterChannelGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetMasterSoundGroup) := FmodLibrary.Symbol('FMOD_System_GetMasterSoundGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_AttachChannelGroupToPort) := FmodLibrary.Symbol('FMOD_System_AttachChannelGroupToPort');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_DetachChannelGroupFromPort) := FmodLibrary.Symbol('FMOD_System_DetachChannelGroupFromPort');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetReverbProperties) := FmodLibrary.Symbol('FMOD_System_SetReverbProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetReverbProperties) := FmodLibrary.Symbol('FMOD_System_GetReverbProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_LockDSP) := FmodLibrary.Symbol('FMOD_System_LockDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_UnlockDSP) := FmodLibrary.Symbol('FMOD_System_UnlockDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetRecordNumDrivers) := FmodLibrary.Symbol('FMOD_System_GetRecordNumDrivers');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetRecordDriverInfo) := FmodLibrary.Symbol('FMOD_System_GetRecordDriverInfo');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetRecordPosition) := FmodLibrary.Symbol('FMOD_System_GetRecordPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_RecordStart) := FmodLibrary.Symbol('FMOD_System_RecordStart');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_RecordStop) := FmodLibrary.Symbol('FMOD_System_RecordStop');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_IsRecording) := FmodLibrary.Symbol('FMOD_System_IsRecording');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_CreateGeometry) := FmodLibrary.Symbol('FMOD_System_CreateGeometry');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetGeometrySettings) := FmodLibrary.Symbol('FMOD_System_SetGeometrySettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetGeometrySettings) := FmodLibrary.Symbol('FMOD_System_GetGeometrySettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_LoadGeometry) := FmodLibrary.Symbol('FMOD_System_LoadGeometry');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetGeometryOcclusion) := FmodLibrary.Symbol('FMOD_System_GetGeometryOcclusion');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetNetworkProxy) := FmodLibrary.Symbol('FMOD_System_SetNetworkProxy');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetNetworkProxy) := FmodLibrary.Symbol('FMOD_System_GetNetworkProxy');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetNetworkTimeout) := FmodLibrary.Symbol('FMOD_System_SetNetworkTimeout');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetNetworkTimeout) := FmodLibrary.Symbol('FMOD_System_GetNetworkTimeout');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_SetUserData) := FmodLibrary.Symbol('FMOD_System_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_System_GetUserData) := FmodLibrary.Symbol('FMOD_System_GetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Release) := FmodLibrary.Symbol('FMOD_Sound_Release');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetSystemObject) := FmodLibrary.Symbol('FMOD_Sound_GetSystemObject');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Lock) := FmodLibrary.Symbol('FMOD_Sound_Lock');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Unlock) := FmodLibrary.Symbol('FMOD_Sound_Unlock');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetDefaults) := FmodLibrary.Symbol('FMOD_Sound_SetDefaults');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetDefaults) := FmodLibrary.Symbol('FMOD_Sound_GetDefaults');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Set3DMinMaxDistance) := FmodLibrary.Symbol('FMOD_Sound_Set3DMinMaxDistance');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Get3DMinMaxDistance) := FmodLibrary.Symbol('FMOD_Sound_Get3DMinMaxDistance');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Set3DConeSettings) := FmodLibrary.Symbol('FMOD_Sound_Set3DConeSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Get3DConeSettings) := FmodLibrary.Symbol('FMOD_Sound_Get3DConeSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Set3DCustomRolloff) := FmodLibrary.Symbol('FMOD_Sound_Set3DCustomRolloff');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_Get3DCustomRolloff) := FmodLibrary.Symbol('FMOD_Sound_Get3DCustomRolloff');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetSubSound) := FmodLibrary.Symbol('FMOD_Sound_GetSubSound');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetSubSoundParent) := FmodLibrary.Symbol('FMOD_Sound_GetSubSoundParent');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetName) := FmodLibrary.Symbol('FMOD_Sound_GetName');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetLength) := FmodLibrary.Symbol('FMOD_Sound_GetLength');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetFormat) := FmodLibrary.Symbol('FMOD_Sound_GetFormat');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetNumSubSounds) := FmodLibrary.Symbol('FMOD_Sound_GetNumSubSounds');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetNumTags) := FmodLibrary.Symbol('FMOD_Sound_GetNumTags');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetTag) := FmodLibrary.Symbol('FMOD_Sound_GetTag');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetOpenState) := FmodLibrary.Symbol('FMOD_Sound_GetOpenState');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_ReadData) := FmodLibrary.Symbol('FMOD_Sound_ReadData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SeekData) := FmodLibrary.Symbol('FMOD_Sound_SeekData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetSoundGroup) := FmodLibrary.Symbol('FMOD_Sound_SetSoundGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetSoundGroup) := FmodLibrary.Symbol('FMOD_Sound_GetSoundGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetNumSyncPoints) := FmodLibrary.Symbol('FMOD_Sound_GetNumSyncPoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetSyncPoint) := FmodLibrary.Symbol('FMOD_Sound_GetSyncPoint');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetSyncPointInfo) := FmodLibrary.Symbol('FMOD_Sound_GetSyncPointInfo');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_AddSyncPoint) := FmodLibrary.Symbol('FMOD_Sound_AddSyncPoint');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_DeleteSyncPoint) := FmodLibrary.Symbol('FMOD_Sound_DeleteSyncPoint');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetMode) := FmodLibrary.Symbol('FMOD_Sound_SetMode');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetMode) := FmodLibrary.Symbol('FMOD_Sound_GetMode');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetLoopCount) := FmodLibrary.Symbol('FMOD_Sound_SetLoopCount');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetLoopCount) := FmodLibrary.Symbol('FMOD_Sound_GetLoopCount');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetLoopPoints) := FmodLibrary.Symbol('FMOD_Sound_SetLoopPoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetLoopPoints) := FmodLibrary.Symbol('FMOD_Sound_GetLoopPoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetMusicNumChannels) := FmodLibrary.Symbol('FMOD_Sound_GetMusicNumChannels');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetMusicChannelVolume) := FmodLibrary.Symbol('FMOD_Sound_SetMusicChannelVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetMusicChannelVolume) := FmodLibrary.Symbol('FMOD_Sound_GetMusicChannelVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetMusicSpeed) := FmodLibrary.Symbol('FMOD_Sound_SetMusicSpeed');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetMusicSpeed) := FmodLibrary.Symbol('FMOD_Sound_GetMusicSpeed');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_SetUserData) := FmodLibrary.Symbol('FMOD_Sound_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Sound_GetUserData) := FmodLibrary.Symbol('FMOD_Sound_GetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetSystemObject) := FmodLibrary.Symbol('FMOD_Channel_GetSystemObject');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Stop) := FmodLibrary.Symbol('FMOD_Channel_Stop');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetPaused) := FmodLibrary.Symbol('FMOD_Channel_SetPaused');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetPaused) := FmodLibrary.Symbol('FMOD_Channel_GetPaused');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetVolume) := FmodLibrary.Symbol('FMOD_Channel_SetVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetVolume) := FmodLibrary.Symbol('FMOD_Channel_GetVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetVolumeRamp) := FmodLibrary.Symbol('FMOD_Channel_SetVolumeRamp');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetVolumeRamp) := FmodLibrary.Symbol('FMOD_Channel_GetVolumeRamp');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetAudibility) := FmodLibrary.Symbol('FMOD_Channel_GetAudibility');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetPitch) := FmodLibrary.Symbol('FMOD_Channel_SetPitch');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetPitch) := FmodLibrary.Symbol('FMOD_Channel_GetPitch');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetMute) := FmodLibrary.Symbol('FMOD_Channel_SetMute');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetMute) := FmodLibrary.Symbol('FMOD_Channel_GetMute');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetReverbProperties) := FmodLibrary.Symbol('FMOD_Channel_SetReverbProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetReverbProperties) := FmodLibrary.Symbol('FMOD_Channel_GetReverbProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetLowPassGain) := FmodLibrary.Symbol('FMOD_Channel_SetLowPassGain');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetLowPassGain) := FmodLibrary.Symbol('FMOD_Channel_GetLowPassGain');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetMode) := FmodLibrary.Symbol('FMOD_Channel_SetMode');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetMode) := FmodLibrary.Symbol('FMOD_Channel_GetMode');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetCallback) := FmodLibrary.Symbol('FMOD_Channel_SetCallback');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_IsPlaying) := FmodLibrary.Symbol('FMOD_Channel_IsPlaying');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetPan) := FmodLibrary.Symbol('FMOD_Channel_SetPan');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetMixLevelsOutput) := FmodLibrary.Symbol('FMOD_Channel_SetMixLevelsOutput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetMixLevelsInput) := FmodLibrary.Symbol('FMOD_Channel_SetMixLevelsInput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetMixMatrix) := FmodLibrary.Symbol('FMOD_Channel_SetMixMatrix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetMixMatrix) := FmodLibrary.Symbol('FMOD_Channel_GetMixMatrix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetDSPClock) := FmodLibrary.Symbol('FMOD_Channel_GetDSPClock');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetDelay) := FmodLibrary.Symbol('FMOD_Channel_SetDelay');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetDelay) := FmodLibrary.Symbol('FMOD_Channel_GetDelay');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_AddFadePoint) := FmodLibrary.Symbol('FMOD_Channel_AddFadePoint');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetFadePointRamp) := FmodLibrary.Symbol('FMOD_Channel_SetFadePointRamp');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_RemoveFadePoints) := FmodLibrary.Symbol('FMOD_Channel_RemoveFadePoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetFadePoints) := FmodLibrary.Symbol('FMOD_Channel_GetFadePoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetDSP) := FmodLibrary.Symbol('FMOD_Channel_GetDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_AddDSP) := FmodLibrary.Symbol('FMOD_Channel_AddDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_RemoveDSP) := FmodLibrary.Symbol('FMOD_Channel_RemoveDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetNumDSPs) := FmodLibrary.Symbol('FMOD_Channel_GetNumDSPs');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetDSPIndex) := FmodLibrary.Symbol('FMOD_Channel_SetDSPIndex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetDSPIndex) := FmodLibrary.Symbol('FMOD_Channel_GetDSPIndex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DAttributes) := FmodLibrary.Symbol('FMOD_Channel_Set3DAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DAttributes) := FmodLibrary.Symbol('FMOD_Channel_Get3DAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DMinMaxDistance) := FmodLibrary.Symbol('FMOD_Channel_Set3DMinMaxDistance');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DMinMaxDistance) := FmodLibrary.Symbol('FMOD_Channel_Get3DMinMaxDistance');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DConeSettings) := FmodLibrary.Symbol('FMOD_Channel_Set3DConeSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DConeSettings) := FmodLibrary.Symbol('FMOD_Channel_Get3DConeSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DConeOrientation) := FmodLibrary.Symbol('FMOD_Channel_Set3DConeOrientation');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DConeOrientation) := FmodLibrary.Symbol('FMOD_Channel_Get3DConeOrientation');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DCustomRolloff) := FmodLibrary.Symbol('FMOD_Channel_Set3DCustomRolloff');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DCustomRolloff) := FmodLibrary.Symbol('FMOD_Channel_Get3DCustomRolloff');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DOcclusion) := FmodLibrary.Symbol('FMOD_Channel_Set3DOcclusion');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DOcclusion) := FmodLibrary.Symbol('FMOD_Channel_Get3DOcclusion');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DSpread) := FmodLibrary.Symbol('FMOD_Channel_Set3DSpread');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DSpread) := FmodLibrary.Symbol('FMOD_Channel_Get3DSpread');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DLevel) := FmodLibrary.Symbol('FMOD_Channel_Set3DLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DLevel) := FmodLibrary.Symbol('FMOD_Channel_Get3DLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DDopplerLevel) := FmodLibrary.Symbol('FMOD_Channel_Set3DDopplerLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DDopplerLevel) := FmodLibrary.Symbol('FMOD_Channel_Get3DDopplerLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Set3DDistanceFilter) := FmodLibrary.Symbol('FMOD_Channel_Set3DDistanceFilter');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_Get3DDistanceFilter) := FmodLibrary.Symbol('FMOD_Channel_Get3DDistanceFilter');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetUserData) := FmodLibrary.Symbol('FMOD_Channel_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetUserData) := FmodLibrary.Symbol('FMOD_Channel_GetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetFrequency) := FmodLibrary.Symbol('FMOD_Channel_SetFrequency');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetFrequency) := FmodLibrary.Symbol('FMOD_Channel_GetFrequency');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetPriority) := FmodLibrary.Symbol('FMOD_Channel_SetPriority');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetPriority) := FmodLibrary.Symbol('FMOD_Channel_GetPriority');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetPosition) := FmodLibrary.Symbol('FMOD_Channel_SetPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetPosition) := FmodLibrary.Symbol('FMOD_Channel_GetPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetChannelGroup) := FmodLibrary.Symbol('FMOD_Channel_SetChannelGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetChannelGroup) := FmodLibrary.Symbol('FMOD_Channel_GetChannelGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetLoopCount) := FmodLibrary.Symbol('FMOD_Channel_SetLoopCount');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetLoopCount) := FmodLibrary.Symbol('FMOD_Channel_GetLoopCount');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_SetLoopPoints) := FmodLibrary.Symbol('FMOD_Channel_SetLoopPoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetLoopPoints) := FmodLibrary.Symbol('FMOD_Channel_GetLoopPoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_IsVirtual) := FmodLibrary.Symbol('FMOD_Channel_IsVirtual');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetCurrentSound) := FmodLibrary.Symbol('FMOD_Channel_GetCurrentSound');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Channel_GetIndex) := FmodLibrary.Symbol('FMOD_Channel_GetIndex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetSystemObject) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetSystemObject');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Stop) := FmodLibrary.Symbol('FMOD_ChannelGroup_Stop');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetPaused) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetPaused');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetPaused) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetPaused');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetVolume) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetVolume) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetVolumeRamp) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetVolumeRamp');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetVolumeRamp) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetVolumeRamp');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetAudibility) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetAudibility');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetPitch) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetPitch');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetPitch) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetPitch');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetMute) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetMute');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetMute) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetMute');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetReverbProperties) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetReverbProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetReverbProperties) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetReverbProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetLowPassGain) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetLowPassGain');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetLowPassGain) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetLowPassGain');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetMode) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetMode');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetMode) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetMode');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetCallback) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetCallback');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_IsPlaying) := FmodLibrary.Symbol('FMOD_ChannelGroup_IsPlaying');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetPan) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetPan');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetMixLevelsOutput) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetMixLevelsOutput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetMixLevelsInput) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetMixLevelsInput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetMixMatrix) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetMixMatrix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetMixMatrix) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetMixMatrix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetDSPClock) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetDSPClock');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetDelay) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetDelay');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetDelay) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetDelay');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_AddFadePoint) := FmodLibrary.Symbol('FMOD_ChannelGroup_AddFadePoint');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetFadePointRamp) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetFadePointRamp');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_RemoveFadePoints) := FmodLibrary.Symbol('FMOD_ChannelGroup_RemoveFadePoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetFadePoints) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetFadePoints');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetDSP) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_AddDSP) := FmodLibrary.Symbol('FMOD_ChannelGroup_AddDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_RemoveDSP) := FmodLibrary.Symbol('FMOD_ChannelGroup_RemoveDSP');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetNumDSPs) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetNumDSPs');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetDSPIndex) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetDSPIndex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetDSPIndex) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetDSPIndex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DAttributes) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DAttributes) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DMinMaxDistance) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DMinMaxDistance');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DMinMaxDistance) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DMinMaxDistance');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DConeSettings) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DConeSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DConeSettings) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DConeSettings');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DConeOrientation) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DConeOrientation');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DConeOrientation) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DConeOrientation');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DCustomRolloff) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DCustomRolloff');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DCustomRolloff) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DCustomRolloff');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DOcclusion) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DOcclusion');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DOcclusion) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DOcclusion');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DSpread) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DSpread');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DSpread) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DSpread');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DLevel) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DLevel) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DDopplerLevel) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DDopplerLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DDopplerLevel) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DDopplerLevel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Set3DDistanceFilter) := FmodLibrary.Symbol('FMOD_ChannelGroup_Set3DDistanceFilter');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Get3DDistanceFilter) := FmodLibrary.Symbol('FMOD_ChannelGroup_Get3DDistanceFilter');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_SetUserData) := FmodLibrary.Symbol('FMOD_ChannelGroup_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetUserData) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_Release) := FmodLibrary.Symbol('FMOD_ChannelGroup_Release');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_AddGroup) := FmodLibrary.Symbol('FMOD_ChannelGroup_AddGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetNumGroups) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetNumGroups');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetGroup) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetParentGroup) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetParentGroup');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetName) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetName');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetNumChannels) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetNumChannels');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_ChannelGroup_GetChannel) := FmodLibrary.Symbol('FMOD_ChannelGroup_GetChannel');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_Release) := FmodLibrary.Symbol('FMOD_SoundGroup_Release');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetSystemObject) := FmodLibrary.Symbol('FMOD_SoundGroup_GetSystemObject');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_SetMaxAudible) := FmodLibrary.Symbol('FMOD_SoundGroup_SetMaxAudible');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetMaxAudible) := FmodLibrary.Symbol('FMOD_SoundGroup_GetMaxAudible');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_SetMaxAudibleBehavior) := FmodLibrary.Symbol('FMOD_SoundGroup_SetMaxAudibleBehavior');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetMaxAudibleBehavior) := FmodLibrary.Symbol('FMOD_SoundGroup_GetMaxAudibleBehavior');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_SetMuteFadeSpeed) := FmodLibrary.Symbol('FMOD_SoundGroup_SetMuteFadeSpeed');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetMuteFadeSpeed) := FmodLibrary.Symbol('FMOD_SoundGroup_GetMuteFadeSpeed');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_SetVolume) := FmodLibrary.Symbol('FMOD_SoundGroup_SetVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetVolume) := FmodLibrary.Symbol('FMOD_SoundGroup_GetVolume');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_Stop) := FmodLibrary.Symbol('FMOD_SoundGroup_Stop');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetName) := FmodLibrary.Symbol('FMOD_SoundGroup_GetName');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetNumSounds) := FmodLibrary.Symbol('FMOD_SoundGroup_GetNumSounds');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetSound) := FmodLibrary.Symbol('FMOD_SoundGroup_GetSound');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetNumPlaying) := FmodLibrary.Symbol('FMOD_SoundGroup_GetNumPlaying');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_SetUserData) := FmodLibrary.Symbol('FMOD_SoundGroup_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_SoundGroup_GetUserData) := FmodLibrary.Symbol('FMOD_SoundGroup_GetUserData');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_Release) := FmodLibrary.Symbol('FMOD_DSP_Release');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetSystemObject) := FmodLibrary.Symbol('FMOD_DSP_GetSystemObject');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_AddInput) := FmodLibrary.Symbol('FMOD_DSP_AddInput');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_DisconnectFrom) := FmodLibrary.Symbol('FMOD_DSP_DisconnectFrom');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_DisconnectAll) := FmodLibrary.Symbol('FMOD_DSP_DisconnectAll');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetNumInputs) := FmodLibrary.Symbol('FMOD_DSP_GetNumInputs');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetNumOutputs) := FmodLibrary.Symbol('FMOD_DSP_GetNumOutputs');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetInput) := FmodLibrary.Symbol('FMOD_DSP_GetInput');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetOutput) := FmodLibrary.Symbol('FMOD_DSP_GetOutput');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetActive) := FmodLibrary.Symbol('FMOD_DSP_SetActive');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetActive) := FmodLibrary.Symbol('FMOD_DSP_GetActive');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetBypass) := FmodLibrary.Symbol('FMOD_DSP_SetBypass');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetBypass) := FmodLibrary.Symbol('FMOD_DSP_GetBypass');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetWetDryMix) := FmodLibrary.Symbol('FMOD_DSP_SetWetDryMix');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetWetDryMix) := FmodLibrary.Symbol('FMOD_DSP_GetWetDryMix');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetChannelFormat) := FmodLibrary.Symbol('FMOD_DSP_SetChannelFormat');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetChannelFormat) := FmodLibrary.Symbol('FMOD_DSP_GetChannelFormat');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetOutputChannelFormat) := FmodLibrary.Symbol('FMOD_DSP_GetOutputChannelFormat');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_Reset) := FmodLibrary.Symbol('FMOD_DSP_Reset');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetParameterFloat) := FmodLibrary.Symbol('FMOD_DSP_SetParameterFloat');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetParameterInt) := FmodLibrary.Symbol('FMOD_DSP_SetParameterInt');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetParameterBool) := FmodLibrary.Symbol('FMOD_DSP_SetParameterBool');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetParameterData) := FmodLibrary.Symbol('FMOD_DSP_SetParameterData');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetParameterFloat) := FmodLibrary.Symbol('FMOD_DSP_GetParameterFloat');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetParameterInt) := FmodLibrary.Symbol('FMOD_DSP_GetParameterInt');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetParameterBool) := FmodLibrary.Symbol('FMOD_DSP_GetParameterBool');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetParameterData) := FmodLibrary.Symbol('FMOD_DSP_GetParameterData');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetNumParameters) := FmodLibrary.Symbol('FMOD_DSP_GetNumParameters');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetParameterInfo) := FmodLibrary.Symbol('FMOD_DSP_GetParameterInfo');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetDataParameterIndex) := FmodLibrary.Symbol('FMOD_DSP_GetDataParameterIndex');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_ShowConfigDialog) := FmodLibrary.Symbol('FMOD_DSP_ShowConfigDialog');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetInfo) := FmodLibrary.Symbol('FMOD_DSP_GetInfo');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetType) := FmodLibrary.Symbol('FMOD_DSP_GetType');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetIdle) := FmodLibrary.Symbol('FMOD_DSP_GetIdle');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetUserData) := FmodLibrary.Symbol('FMOD_DSP_SetUserData');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetUserData) := FmodLibrary.Symbol('FMOD_DSP_GetUserData');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_SetMeteringEnabled) := FmodLibrary.Symbol('FMOD_DSP_SetMeteringEnabled');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetMeteringEnabled) := FmodLibrary.Symbol('FMOD_DSP_GetMeteringEnabled');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetMeteringInfo) := FmodLibrary.Symbol('FMOD_DSP_GetMeteringInfo');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSP_GetCPUUsage) := FmodLibrary.Symbol('FMOD_DSP_GetCPUUsage');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_GetInput) := FmodLibrary.Symbol('FMOD_DSPConnection_GetInput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_GetOutput) := FmodLibrary.Symbol('FMOD_DSPConnection_GetOutput');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_SetMix) := FmodLibrary.Symbol('FMOD_DSPConnection_SetMix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_GetMix) := FmodLibrary.Symbol('FMOD_DSPConnection_GetMix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_SetMixMatrix) := FmodLibrary.Symbol('FMOD_DSPConnection_SetMixMatrix');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_GetMixMatrix) := FmodLibrary.Symbol('FMOD_DSPConnection_GetMixMatrix');
  //   Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_GetType) := FmodLibrary.Symbol('FMOD_DSPConnection_GetType');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_SetUserData) := FmodLibrary.Symbol('FMOD_DSPConnection_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_DSPConnection_GetUserData) := FmodLibrary.Symbol('FMOD_DSPConnection_GetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_Release) := FmodLibrary.Symbol('FMOD_Geometry_Release');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_AddPolygon) := FmodLibrary.Symbol('FMOD_Geometry_AddPolygon');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetNumPolygons) := FmodLibrary.Symbol('FMOD_Geometry_GetNumPolygons');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetMaxPolygons) := FmodLibrary.Symbol('FMOD_Geometry_GetMaxPolygons');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetPolygonNumVertices) := FmodLibrary.Symbol('FMOD_Geometry_GetPolygonNumVertices');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetPolygonVertex) := FmodLibrary.Symbol('FMOD_Geometry_SetPolygonVertex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetPolygonVertex) := FmodLibrary.Symbol('FMOD_Geometry_GetPolygonVertex');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetPolygonAttributes) := FmodLibrary.Symbol('FMOD_Geometry_SetPolygonAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetPolygonAttributes) := FmodLibrary.Symbol('FMOD_Geometry_GetPolygonAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetActive) := FmodLibrary.Symbol('FMOD_Geometry_SetActive');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetActive) := FmodLibrary.Symbol('FMOD_Geometry_GetActive');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetRotation) := FmodLibrary.Symbol('FMOD_Geometry_SetRotation');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetRotation) := FmodLibrary.Symbol('FMOD_Geometry_GetRotation');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetPosition) := FmodLibrary.Symbol('FMOD_Geometry_SetPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetPosition) := FmodLibrary.Symbol('FMOD_Geometry_GetPosition');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetScale) := FmodLibrary.Symbol('FMOD_Geometry_SetScale');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetScale) := FmodLibrary.Symbol('FMOD_Geometry_GetScale');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_Save) := FmodLibrary.Symbol('FMOD_Geometry_Save');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_SetUserData) := FmodLibrary.Symbol('FMOD_Geometry_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Geometry_GetUserData) := FmodLibrary.Symbol('FMOD_Geometry_GetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_Release) := FmodLibrary.Symbol('FMOD_Reverb3D_Release');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_Set3DAttributes) := FmodLibrary.Symbol('FMOD_Reverb3D_Set3DAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_Get3DAttributes) := FmodLibrary.Symbol('FMOD_Reverb3D_Get3DAttributes');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_SetProperties) := FmodLibrary.Symbol('FMOD_Reverb3D_SetProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_GetProperties) := FmodLibrary.Symbol('FMOD_Reverb3D_GetProperties');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_SetActive) := FmodLibrary.Symbol('FMOD_Reverb3D_SetActive');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_GetActive) := FmodLibrary.Symbol('FMOD_Reverb3D_GetActive');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_SetUserData) := FmodLibrary.Symbol('FMOD_Reverb3D_SetUserData');
    Pointer({$ifndef CASTLE_OBJFPC} @ {$endif} FMOD_Reverb3D_GetUserData) := FmodLibrary.Symbol('FMOD_Reverb3D_GetUserData');
  end;
end;

function FmodLibraryAvailable: Boolean;
begin
  Result := FmodLibrary <> nil;
end;

var
  UnitFinalizationDone: Boolean;
  FmodUsing: Cardinal;

procedure FmodLibraryUsingBegin;
begin
  Assert(FmodLibraryAvailable, 'Check FmodLibraryAvailable before using FMOD');
  Inc(FmodUsing);
end;

procedure FmodLibraryUsingEnd;
begin
  Assert(FmodLibraryAvailable, 'Check FmodLibraryAvailable before using FMOD');
  Assert(FmodUsing > 0, 'FmodLibraryUsingEnd called more times than FmodLibraryUsingBegin');
  Dec(FmodUsing);
  if (FmodUsing = 0) and UnitFinalizationDone then
    FinalizeFmodLibrary;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  InitializeFmodLibrary;
  {$endif}
finalization
  UnitFinalizationDone := true;
  if FmodUsing = 0 then
    FinalizeFmodLibrary;
end.
