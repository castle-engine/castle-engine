{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine backend using FMOD.
  See https://github.com/castle-engine/castle-engine/wiki/FMOD
  about using FMOD with CGE.
}
unit CastleFMODSoundBackend;

interface

{ Use this to set sound engine backend to FMOD.
  You can call this at any point of your application.
  If you call it before any sound loading/playing,
  then the previous sound backend wil not even be initialized. }
procedure UseFMODSoundBackend;

implementation

uses CastleSoundEngine, CastleInternalFMODBackend;

procedure UseFMODSoundBackend;
begin
  SoundEngine.InternalBackend := TFMODSoundEngineBackend.Create;
end;

end.
