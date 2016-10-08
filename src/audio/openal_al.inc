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
 *  Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 * Or go to http://www.gnu.org/copyleft/lgpl.html
 *}

{$I openal_altypes.inc}

var

  {*
   * OpenAL Maintenance Functions
   * State Management and Query.
   * Error Handling.
   * Extension Support.
   }

  { Renderer State management. } { }
  alEnable : procedure( capability:TALenum ); extdecl;

  alDisable : procedure( capability:TALenum ); extdecl;

  alIsEnabled : function( capability:TALenum ):TALboolean; extdecl;

  { State retrieval. }
  alGetBooleanv : procedure( param:TALenum; data:PALboolean ); extdecl;

  { State retrieval. }
  alGetIntegerv : procedure( param:TALenum; data:PALint  ); extdecl;

  { State retrieval. }
  alGetFloatv : procedure( param:TALenum; data:PALfloat  ); extdecl;

  { State retrieval. }
  alGetDoublev : procedure( param:TALenum; data:PALdouble  ); extdecl;

  { State retrieval. }
  alGetString : function( param:TALenum ):PChar; extdecl;

  { State retrieval through return value ( for compatibility ). }
  alGetBoolean : function( param:TALenum ):TALboolean; extdecl;
  alGetInteger : function( param:TALenum ):TALint; extdecl;
  alGetFloat : function( param:TALenum ):TALfloat; extdecl;
  alGetDouble : function( param:TALenum ):TALdouble; extdecl;

  {
    Error support.
    Obtain the most recent error generated in the AL state machine.
  }
  alGetError : function():TALenum; extdecl;

  {
    Extension support.
    Obtain the address of a function (usually an extension)
    with the name fname. All addresses are context-independent.
  }
  alIsExtensionPresent : function( fname:PChar ):TALboolean; extdecl;

  {
    Extension support.
    Obtain the address of a function (usually an extension)
    with the name fname. All addresses are context-independent.
  }
  alGetProcAddress : function( fname:PChar ):Pointer; extdecl;

  {
    Extension support.
    Obtain the integer value of an enumeration (usually an extension) with the name ename.
  }
  alGetEnumValue : function( ename:PChar ):TALenum; extdecl;

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
  alListenerf : procedure( pname:TALenum; param:TALfloat ); extdecl;

  alListeneri : procedure( pname:TALenum; param:TALint ); extdecl;

  {*
   *
   * Listener Position:        ALfloat[3]
   * Listener Velocity:        ALfloat[3]
   } { }
  alListener3f : procedure( pname:TALenum; f1, f2, f3:TALfloat ); extdecl;

  {*
   *
   * Listener Position:        ALfloat[3]
   * Listener Velocity:        ALfloat[3]
   * Listener Orientation:     ALfloat[6]  (forward and up vector).
   } { }
  alListenerfv : procedure( pname:TALenum; param:PALfloat ); extdecl;

  {
    Retrieve listener information.
  }
  alGetListeneri : procedure( pname:TALenum; value:PALint ); extdecl;
  alGetListenerf : procedure( pname:TALenum; value:PALfloat ); extdecl;

  {$ifdef OPENAL_DEPRECATED}
  alGetListeneriv : procedure( pname:TALenum; value:PALint ); extdecl;
  {$endif OPENAL_DEPRECATED}
  alGetListenerfv : procedure( pname:TALenum; values:PALfloat ); extdecl;

  alGetListener3f : procedure( pname:TALenum; f1, f2, f3:PALfloat ); extdecl;

  {*
   * SOURCE
   * Source objects are by default localized. Sources
   *  take the PCM data provided in the specified Buffer,
   *  apply Source-specific modifications, and then
   *  submit them to be mixed according to spatial
   *  arrangement etc.
   }

  { Create Source objects. }
  alGenSources : procedure( n:TALsizei; sources:PALuint ); extdecl;

  { Delete Source objects. }
  alDeleteSources : procedure( n:TALsizei; sources:PALuint ); extdecl;

  { Verify a handle is a valid Source. }
  alIsSource : function( sid:TALuint ):TALboolean; extdecl;

  { Set an integer parameter for a Source object. }
  alSourcei : procedure( sid:TALuint; param:TALenum; value:TALint ); extdecl;
  { Note: possibly not present in older OpenAL < 1.1, particularly from Loki }
  alSource3i : procedure( sid:TALuint; param:TALenum; f1, f2, f3:TALint ); extdecl;
  alSourcef : procedure( sid:TALuint; param:TALenum; value:TALfloat ); extdecl;
  alSource3f : procedure( sid:TALuint; param:TALenum; f1, f2, f3:TALfloat ); extdecl;
  alSourcefv : procedure( sid:TALuint; param:TALenum; values:PALfloat ); extdecl;

  { Get an integer parameter for a Source object. }
  alGetSourcei : procedure( sid:TALuint;  pname:TALenum; value:PALint  ); extdecl;
  {$ifdef OPENAL_DEPRECATED}
  alGetSourceiv : procedure( sid:TALuint;  pname:TALenum; values:PALint  ); extdecl;
  {$endif OPENAL_DEPRECATED}
  alGetSourcef : procedure( sid:TALuint; pname:TALenum; value:PALfloat ); extdecl;
  alGetSourcefv : procedure( sid:TALuint; pname:TALenum; values:PALfloat ); extdecl;

  alSourcePlayv : procedure( ns:TALsizei; ids:PALuint ); extdecl;
  alSourceStopv : procedure( ns:TALsizei; ids:PALuint ); extdecl;
  alSourceRewindv : procedure( ns:TALsizei; ids:PALuint ); extdecl;
  alSourcePausev : procedure( ns:TALsizei; ids:PALuint ); extdecl;

  { Activate a source, start replay. }
  alSourcePlay : procedure( sid:TALuint ); extdecl;

  {
    Pause a source,
    temporarily remove it from the mixer list.
  }
  alSourcePause : procedure( sid:TALuint ); extdecl;

  {
    Rewind a source,
    set the source to play at the beginning.
  }
  alSourceRewind : procedure( sid:TALuint ); extdecl;

  {
    Stop a source,
    temporarily remove it from the mixer list,
    and reset its internal state to pre-Play.
    To remove a Source completely, it has to be
    deleted following Stop, or before Play.
  }
  alSourceStop : procedure( sid:TALuint ); extdecl;

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
  alGenBuffers : procedure( n:TALsizei; buffers:PALuint ); extdecl;

  alDeleteBuffers : procedure( n:TALsizei; buffers:PALuint ); extdecl;
  alIsBuffer : function( buffer:TALuint ):TALboolean; extdecl;

  {
    Specify the data to be filled into a buffer.
  }
  alBufferData : procedure( buffer:TALuint; format:TALenum; data:PALvoid; size, freq:TALsizei ); extdecl;

  alGetBufferi : procedure( buffer:TALuint; param:TALenum; value:PALint    ); extdecl;
  alGetBufferf : procedure( buffer:TALuint; param:TALenum; value:PALfloat ); extdecl;
  {$ifdef OPENAL_DEPRECATED}
  alGetBufferiv : procedure( buffer:TALuint; param:TALenum;  v:PALint); extdecl;
  alGetBufferfv : procedure( buffer:TALuint; param:TALenum;  v:PALfloat); extdecl;
  {$endif OPENAL_DEPRECATED}

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
  alGenEnvironmentIASIG : function( n:TALsizei;  environs:PALuint ):TALsizei; extdecl;
  alDeleteEnvironmentIASIG : procedure( n:TALsizei;  environs:PALuint ); extdecl;
  alIsEnvironmentIASIG : function( environ:TALuint ):TALboolean; extdecl;
  alEnvironmentiIASIG : procedure( eid:TALuint; param:TALenum; value:TALint ); extdecl;
  { commented out by Kambi: is it not an error in OpenAL headers ?
    value:TALuint ? Should not it be value:TALfloat.
    As soon as I will know how to use this procedure I will test this.
  alEnvironmentfIASIG : procedure( eid:TALuint; param:TALenum; value:TALuint ); extdecl;
  }

  {*
   * Queue stuff
   } { }
  alSourceQueueBuffers : procedure( sid:TALuint; numEntries:TALsizei; bids:PALuint ); extdecl;
  alSourceUnqueueBuffers : procedure( sid:TALuint; numEntries:TALsizei; bids:PALuint ); extdecl;

  {*
   * Knobs and dials
   } { }
  alDopplerFactor : procedure( value:TALfloat ); extdecl;
  alDopplerVelocity : procedure( value:TALfloat ); extdecl;
  alDistanceModel : procedure( distanceModel:TALenum ); extdecl;
