{ List (use) most non-internal, non-deprecated units to precompile them all for cache.
  This is bit similar to fpmake.pp contents.

  The compilation output of this project is used for "castle-engine cache",
  starting point to compile other CGE projects fast. }

unit CastleCacheUnit;

interface

uses
  // src/transform
  castlebehaviors,
  castleboxes,
  castlecameras,
  castlefrustum,
  castleinternalnurbs,
  castlequaternions,
  castlesectors,
  castletransform,
  castletriangles,
  castletriangulate,

  // src/audio
  castlesoundengine,
  castlesoundbase,

  // src/audio/fmod
  castlefmodsoundbackend,

  // src/audio/openal
  castleopenalsoundbackend,

  // src/base
  castleapplicationproperties,
  castleclassutils,
  castlecolors,
  castledynlib,
  castleinternalgzio,
  castleinternalzlib,
  castleinternalzstream,
  castlelog,
  castlemessaging,
  castleparameters,
  castleprojection,
  castlerectangles,
  castlerenderoptions,
  castleinternalrttiutils,
  castlestreamutils,
  castlestringutils,
  castlesystemlanguage,
  castletimeutils,
  castleunicode,
  castleutils,
  castlevectors,
  castlevectorsinternalsingle,
  castlevectorsinternaldouble,

  // src/base_rendering
  castlegles,
  castleglversion,
  castleglimages,
  castleglutils,
  castleglshaders,
  castlerendercontext,

  // src/services
  castleads,
  castleanalytics,
  castlefacebook,
  castlegameservice,
  castlehelpshift,
  castleinapppurchases,
  castlephotoservice,
  castleopendocument,
  castletenjin,

{$ifdef ANDROID}
  // src/base/android
  castleandroidinternalassetmanager,
  castleandroidinternalconfiguration,
  castleandroidinternalinput,
  castleandroidinternalkeycodes,
  castleandroidinternallog,
  castleandroidinternallooper,
  castleandroidinternalnativeactivity,
  castleandroidinternalnativewindow,
  castleandroidinternalrect,
  castleandroidnativeappglue,
{$endif}

  // src/castlescript
  castlecurves,
  castlescript,
  castlescriptarrays,
  castlescriptcorefunctions,
  castlescriptimages,
  castlescriptlexer,
  castlescriptparser,
  castlescriptvectors,
  castlescriptxml,

  // src/fonts
  castlefonts,
  castleinternalfreetype,
  castleinternalfreetypeh,
  castleinternalrichtext,
  castletexturefont_defaultui,
  castletexturefont_default3d_sans,
  castletexturefontdata,

  // src/images
  castleimages,
  castletextureimages,
  castlevideos,

  // src/files
  castlecomponentserialize,
  castleconfig,
  castledownload,
  castlefilefilters,
  castlefilesutils,
  castlefindfiles,
  castlelocalizationgettext,
  castlerecentfiles,
  castleuriutils,
  castlexmlconfig,
  castlexmlutils,

  // src/ui
  castlecontrols,
  castledialogviews,
  castleflasheffect,
  castleinputs,
  castlejoysticks,
  castlekeysmouse,
  castlenotifications,
  castletiledmap,
  castleuicontrols,

  // src/window
  castlemessages,
  castlewindowrecentfiles,
  castlewindow,

  // src/scene
  castledebugtransform,
  castleinternalmaterialproperties,
  castleraytracer,
  castlescene,
  castleterrain,
  castlethirdpersonnavigation,
  castleviewport;

implementation
end.
