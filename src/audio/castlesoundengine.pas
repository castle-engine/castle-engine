{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cross-platform, spatial sound playback engine (@link(SoundEngine) singleton). }
unit CastleSoundEngine;

{$I castleconf.inc}

{$ifdef CASTLE_NINTENDO_SWITCH}
  // Nintendo Switch has different default backend
{$else}
  { Full-featured backend using OpenAL. }
  {$define CASTLE_SOUND_BACKEND_DEFAULT_OPENAL}
{$endif}

{$ifdef CASTLE_STRICT_CLI}
  {$error When CASTLE_STRICT_CLI is defined, you cannot link to this unit.}
{$endif}

{$ifndef FPC}
  { Ignore the TSoundEngine.InternalBackend exposing private ancestor field.
    This makes this property not useful from C++ Builder
    ( https://docwiki.embarcadero.com/RADStudio/Alexandria/en/W1045_Property_declaration_references_ancestor_private_%27%25s.%25s%27_(Delphi) )
    but it is an internal property anyway, no code should use it. }
  {$warn PRIVATE_PROPACCESSOR off}
{$endif}

interface

uses SysUtils, Classes, Math, Generics.Collections, DOM,
  CastleVectors, CastleTimeUtils, CastleClassUtils, CastleStringUtils,
  CastleSoundBase, CastleInternalSoundFile, CastleInternalAbstractSoundBackend,
  CastleXMLConfig;

{$define read_interface}
type
{$I castlesoundengine_initial_types.inc}
{$I castlesoundengine_internalsoundbuffer.inc}
{$I castlesoundengine_internalsoundsource.inc}
{$I castlesoundengine_sound.inc}
{$I castlesoundengine_playingsound.inc}
{$I castlesoundengine_allocator.inc}
{$I castlesoundengine_playsoundparameters.inc}
{$I castlesoundengine_engine.inc}
{$I castlesoundengine_repoengine.inc}
{$I castlesoundengine_loopingchannel.inc}
{$I castlesoundengine_miscellaneous.inc}
{$undef read_interface}

implementation

uses XMLRead, StrUtils, Generics.Defaults,
  CastleUtils, CastleLog, CastleInternalVorbisFile, CastleInternalDataURI,
  CastleParameters, CastleXmlUtils, CastleFilesUtils, CastleConfig,
  CastleUriUtils, CastleDownload, CastleMessaging, CastleApplicationProperties
  {$ifdef CASTLE_SOUND_BACKEND_DEFAULT_OPENAL}, CastleOpenALSoundBackend{$endif}
  , CastleComponentSerialize;

{$define read_implementation}
{$I castlesoundengine_miscellaneous.inc} // must be first, as defines some internal globals
{$I castlesoundengine_internalsoundbuffer.inc}
{$I castlesoundengine_internalsoundsource.inc}
{$I castlesoundengine_sound.inc}
{$I castlesoundengine_playingsound.inc}
{$I castlesoundengine_allocator.inc}
{$I castlesoundengine_playsoundparameters.inc}
{$I castlesoundengine_engine.inc}
{$I castlesoundengine_repoengine.inc}
{$I castlesoundengine_loopingchannel.inc}
{$undef read_implementation}

initialization
  RegisterSerializableComponent(TCastleSound, 'Sound');
finalization
  FreeAndNil(FSoundEngine);
end.
