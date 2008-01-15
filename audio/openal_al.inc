{ translation of AL/al.h }

{**
 * OpenAL cross platform audio library
 * Copyright (C) 1999-2000 by authors.
 * This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the
 *  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *  Boston, MA  02111-1307, USA.
 * Or go to http://www.gnu.org/copyleft/lgpl.html
 *}

{$I OpenAL_altypes.inc}

var

  {*
   * OpenAL Maintenance Functions
   * State Management and Query.
   * Error Handling.
   * Extension Support.
   }

  { Renderer State management. } { }
  alEnable : procedure( capability:TALenum ); al_call

  alDisable : procedure( capability:TALenum ); al_call

  alIsEnabled : function( capability:TALenum ):TALboolean; al_call

  {$ifndef OLD_OPENAL}
  { Application preferences for driver performance choices. }
  alHint : procedure( target:TALenum ; mode:TALenum ); al_call
  {$endif}

  { State retrieval. }
  alGetBooleanv : procedure( param:TALenum; data:PALboolean ); al_call

  { State retrieval. }
  alGetIntegerv : procedure( param:TALenum; data:PALint  ); al_call

  { State retrieval. }
  alGetFloatv : procedure( param:TALenum; data:PALfloat  ); al_call

  { State retrieval. }
  alGetDoublev : procedure( param:TALenum; data:PALdouble  ); al_call

  { State retrieval. }
  alGetString : function( param:TALenum ):PChar; al_call

  { State retrieval through return value ( for compatibility ). }
  alGetBoolean : function( param:TALenum ):TALboolean; al_call
  alGetInteger : function( param:TALenum ):TALint; al_call
  alGetFloat : function( param:TALenum ):TALfloat; al_call
  alGetDouble : function( param:TALenum ):TALdouble; al_call

  {
    Error support.
    Obtain the most recent error generated in the AL state machine.
  }
  alGetError : function():TALenum; al_call

  {
    Extension support.
    Obtain the address of a function (usually an extension)
    with the name fname. All addresses are context-independent.
  }
  alIsExtensionPresent : function( fname:PChar ):TALboolean; al_call

  {
    Extension support.
    Obtain the address of a function (usually an extension)
    with the name fname. All addresses are context-independent.
  }
  alGetProcAddress : function( fname:PChar ):Pointer; al_call

  {
    Extension support.
    Obtain the integer value of an enumeration (usually an extension) with the name ename.
  }
  alGetEnumValue : function( ename:PChar ):TALenum; al_call

  {*
   * LISTENER
   * Listener is the sample position for a given context.
   * The multi-channel (usually stereo) output stream generated
   *  by the mixer is parametrized by this Listener object:
   *  its position and velocity relative to Sources, within
   *  occluder and reflector geometry.
   }

  {
    Listener Gain:  default 1.0f.
  }
  alListenerf : procedure( pname:TALenum; param:TALfloat ); al_call

  alListeneri : procedure( pname:TALenum; param:TALint ); al_call

  {*
   *
   * Listener Position:        ALfloat[3]
   * Listener Velocity:        ALfloat[3]
   } { }
  alListener3f : procedure( pname:TALenum; f1, f2, f3:TALfloat ); al_call

  {*
   *
   * Listener Position:        ALfloat[3]
   * Listener Velocity:        ALfloat[3]
   * Listener Orientation:     ALfloat[6]  (forward and up vector).
   } { }
  alListenerfv : procedure( pname:TALenum; param:PALfloat ); al_call

  {
    Retrieve listener information.
  }
  alGetListeneri : procedure( pname:TALenum; value:PALint ); al_call
  alGetListenerf : procedure( pname:TALenum; value:PALfloat ); al_call

  {$ifndef OLD_OPENAL}
  alGetListeneriv : procedure( pname:TALenum; value:PALint ); al_call
  {$endif}
  alGetListenerfv : procedure( pname:TALenum; values:PALfloat ); al_call

  alGetListener3f : procedure( pname:TALenum; f1, f2, f3:PALfloat ); al_call

  {*
   * SOURCE
   * Source objects are by default localized. Sources
   *  take the PCM data provided in the specified Buffer,
   *  apply Source-specific modifications, and then
   *  submit them to be mixed according to spatial
   *  arrangement etc.
   }

  { Create Source objects. }
  alGenSources : procedure( n:TALsizei; sources:PALuint ); al_call

  { Delete Source objects. }
  alDeleteSources : procedure( n:TALsizei; sources:PALuint ); al_call

  { Verify a handle is a valid Source. }
  alIsSource : function( sid:TALuint ):TALboolean; al_call

  { Set an integer parameter for a Source object. }
  alSourcei : procedure( sid:TALuint; param:TALenum; value:TALint ); al_call
  alSourcef : procedure( sid:TALuint; param:TALenum; value:TALfloat ); al_call
  alSource3f : procedure( sid:TALuint; param:TALenum; f1, f2, f3:TALfloat ); al_call
  alSourcefv : procedure( sid:TALuint; param:TALenum; values:PALfloat ); al_call

  { Get an integer parameter for a Source object. }
  alGetSourcei : procedure( sid:TALuint;  pname:TALenum; value:PALint  ); al_call
  {$ifndef OLD_OPENAL}
  alGetSourceiv : procedure( sid:TALuint;  pname:TALenum; values:PALint  ); al_call
  {$endif}
  alGetSourcef : procedure( sid:TALuint; pname:TALenum; value:PALfloat ); al_call
  alGetSourcefv : procedure( sid:TALuint; pname:TALenum; values:PALfloat ); al_call

  { Deprecated, included for Win compatibility. @deprecated }
  alGetSource3f : procedure( sid:TALuint; pname:TALenum; value1, value2, value3:PALfloat); al_call

  alSourcePlayv : procedure( ns:TALsizei; ids:PALuint ); al_call
  alSourceStopv : procedure( ns:TALsizei; ids:PALuint ); al_call
  alSourceRewindv : procedure( ns:TALsizei; ids:PALuint ); al_call
  alSourcePausev : procedure( ns:TALsizei; ids:PALuint ); al_call

  { Activate a source, start replay. }
  alSourcePlay : procedure( sid:TALuint ); al_call

  {
    Pause a source,
    temporarily remove it from the mixer list.
  }
  alSourcePause : procedure( sid:TALuint ); al_call

  {
    Rewind a source,
    set the source to play at the beginning.
  }
  alSourceRewind : procedure( sid:TALuint ); al_call

  {
    Stop a source,
    temporarily remove it from the mixer list,
    and reset its internal state to pre-Play.
    To remove a Source completely, it has to be
    deleted following Stop, or before Play.
  }
  alSourceStop : procedure( sid:TALuint ); al_call

  {*
   * BUFFER
   * Buffer objects are storage space for sample data.
   * Buffers are referred to by Sources. There can be more than
   *  one Source using the same Buffer data. If Buffers have
   *  to be duplicated on a per-Source basis, the driver has to
   *  take care of allocation, copying, and deallocation as well
   *  as propagating buffer data changes.
   }

  { Buffer object generation. }
  alGenBuffers : procedure( n:TALsizei; buffers:PALuint ); al_call

  alDeleteBuffers : procedure( n:TALsizei; buffers:PALuint ); al_call
  alIsBuffer : function( buffer:TALuint ):TALboolean; al_call

  {
    Specify the data to be filled into a buffer.
  }
  alBufferData : procedure( buffer:TALuint; format:TALenum; data:PALvoid; size, freq:TALsizei ); al_call

  alGetBufferi : procedure( buffer:TALuint; param:TALenum; value:PALint    ); al_call
  alGetBufferf : procedure( buffer:TALuint; param:TALenum; value:PALfloat ); al_call
  {$ifndef OLD_OPENAL}
  alGetBufferiv : procedure( buffer:TALuint; param:TALenum;  v:PALint); al_call
  alGetBufferfv : procedure( buffer:TALuint; param:TALenum;  v:PALfloat); al_call
  {$endif}

  {*
   * Frequency Domain Filters are band filters.
   *  Attenuation in Media (distance based)
   *  Reflection Material
   *  Occlusion Material (separating surface)
   *
   * Temporal Domain Filters:
   *  Early Reflections
   *  Late Reverb
   *
   }

  {*
   * EXTENSION: IASIG Level 2 Environment.
   * Environment object generation.
   * This is an EXTension that describes the Environment/Reverb
   *  properties according to IASIG Level 2 specifications.
   }

  {
    Allocate n environment ids and store them in the array environs.
    Returns the number of environments actually allocated.
  }
  alGenEnvironmentIASIG : function( n:TALsizei;  environs:PALuint ):TALsizei; al_call
  alDeleteEnvironmentIASIG : procedure( n:TALsizei;  environs:PALuint ); al_call
  alIsEnvironmentIASIG : function( environ:TALuint ):TALboolean; al_call
  alEnvironmentiIASIG : procedure( eid:TALuint; param:TALenum; value:TALint ); al_call
  { commented out by Kambi: is it not an error in OpenAL headers ?
    value:TALuint ? Should not it be value:TALfloat.
    As soon as I will know how to use this procedure I will test this.
  alEnvironmentfIASIG : procedure( eid:TALuint; param:TALenum; value:TALuint ); al_call
  }

  {*
   * Queue stuff
   } { }
  alSourceQueueBuffers : procedure( sid:TALuint; numEntries:TALsizei; bids:PALuint ); al_call
  alSourceUnqueueBuffers : procedure( sid:TALuint; numEntries:TALsizei; bids:PALuint ); al_call
  {$ifndef OLD_OPENAL}
  alQueuei : procedure( sid:TALuint; param:TALenum; value:TALint ); al_call
  {$endif}

  {*
   * Knobs and dials
   } { }
  alDopplerFactor : procedure( value:TALfloat ); al_call
  alDopplerVelocity : procedure( value:TALfloat ); al_call
  alDistanceModel : procedure( distanceModel:TALenum ); al_call
