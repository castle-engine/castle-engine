This is an internal test, directly using FMOD API in `CastleInternalFMOD` unit.

It is only useful for CGE developers that develop `CastleFMODSoundBackend`,
and want to test whether our FMOD API usage is correct,
using a simple application (without any CGE dependencies,
and without the `CastleSoundEngine` layer that hides differences between various
sound backends).

For normal CGE users:

You shall not use FMOD like this in your own applications.
Instead follow https://castle-engine.io/fmod#activating-fmod-in-cge
and call `UseFMODSoundBackend` to make our sound API (everything in `CastleSoundEngine`)
use FMOD underneath.
