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
  castletexturefont_dejavusans_10,
  castletexturefont_dejavusansmono_18,
  castletexturefont_dejavusansmonobold_15,
  castletexturefont_djvmono_20,
  castletexturefont_djvmonob_20,
  castletexturefont_djvmonobo_20,
  castletexturefont_djvmonoo_20,
  castletexturefont_djvsans_20,
  castletexturefont_djvsansb_20,
  castletexturefont_djvsansbo_20,
  castletexturefont_djvsanso_20,
  castletexturefont_djvserif_20,
  castletexturefont_djvserifb_20,
  castletexturefont_djvserifbi_20,
  castletexturefont_djvserifi_20,
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
