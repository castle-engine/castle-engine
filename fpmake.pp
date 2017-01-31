{ Compile Castle Game Engine by fpmake.
  This can be used to compile (and install if you want)
  all engine units. Simple instructions:

  export FPCDIR=..../lib/fpc/2.6.2/  # not needed if FPC installed in standard location
  fpc fpmake.pp
  ./fpmake compile  # use -v to get more info
  ./fpmake install  # use -v to get more info
  # to cross-compile e.g. to Android use like this:
  # ./fpmake --os=android --cpu=arm ...
}

program fpmake;

{ Only FPC >= 2.7.1 has the "Android" as a possible OS. }
{$ifndef VER2_6} {$define ANDROID_POSSIBLE} {$endif}

uses
  { It seems that FPC 3.1.1 requires thread support for FpMkUnit. }
  {$ifdef VER3_1} CThreads, {$endif}
  SysUtils, fpmkunit;

var
  P: TPackage;
  IOS: boolean; //< compiling for iOS target
  Xlib: boolean; //< OS has working Xlib packages
begin
  with Installer do
  begin
    P := AddPackage('castle-game-engine');

    { Should work on AllUnixOSes, actually.
      But let's limit the list only to the OSes actually tested. }
    P.OSes := [Darwin, Linux, Freebsd, Win32, Win64, IPhoneSim
      {$ifdef ANDROID_POSSIBLE} , Android {$endif}];

    P.Options.Text := '@castle-fpc.cfg';

    { Some variables derived from Defaults.OS/CPU. }
    IOS := (Defaults.OS = IPhoneSim) or
      ((Defaults.OS = Darwin) and (Defaults.CPU = Arm));
    Xlib := Defaults.OS in (AllUnixOSes
      {$ifdef ANDROID_POSSIBLE} - [Android] {$endif});

    { Add dependencies on FPC packages.
      These aren't really needed, as your default fpc.cfg should
      point to them anyway. They are needed only when compiling with --nofpccfg.
      Anyway, maybe this is a good place to document my dependencies
      on FPC packages --- so let's do this. }
    if {$ifdef ANDROID_POSSIBLE} (Defaults.OS <> Android) and {$endif} not IOS then
      P.Dependencies.Add('opengl');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-image');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('hash'); { CRC unit used by CastleGzioInternal }
    {$ifdef ANDROID_POSSIBLE} if Defaults.OS <> Android then {$endif}
      P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('pasjpeg');
    P.Dependencies.Add('paszlib'); { used by FpReadTiff, we don't use paszlib in our engine }
    P.Dependencies.Add('regexpr');
    if Xlib then
    begin
      P.Dependencies.Add('x11');
      P.Dependencies.Add('gtk2');
      P.Dependencies.Add('cairo');
    end else
    if Defaults.OS in AllWindowsOSes then
    begin
      P.Dependencies.Add('winunits-base');
      P.Dependencies.Add('fcl-registry');
    end;

    { Some general variables, visible only (as far as I can see) when
      using "./fpmake manifest". }
    P.Author := 'Michalis Kamburelis';
    P.License := 'GNU GPL >= 2 (or LGPL >= 2, if compiled with CASTLE_ENGINE_LGPL)';
    {$ifdef VER2_2_2}
    P.ExternalURL
    {$else}
    P.HomepageURL
    {$endif} := 'http://castle-engine.sourceforge.net/';
    P.Email := 'michalis.kambi' + '@gmail.com'; { at least protect sources from spammers }
    P.Version := {$I src/base/castleversion.inc};

    P.SourcePath.Add('src' + PathDelim + '3d');
    P.Targets.AddUnit('castle3d.pas');
    P.Targets.AddUnit('castleboxes.pas');
    P.Targets.AddUnit('castlecameras.pas');
    P.Targets.AddUnit('castleconvexhull.pas');
    P.Targets.AddUnit('castlefrustum.pas');
    P.Targets.AddUnit('castleinternaloctree.pas');
    P.Targets.AddUnit('castlerandom.pas');
    P.Targets.AddUnit('castletriangles.pas');
    P.Targets.AddUnit('castletriangulate.pas');
    P.Targets.AddUnit('castlecubemaps.pas');
    P.Targets.AddUnit('castlegeometryarrays.pas');
    P.Targets.AddUnit('castlenurbs.pas');
    P.Targets.AddUnit('castlequaternions.pas');
    P.Targets.AddUnit('castlerays.pas');
    P.Targets.AddUnit('castlesectors.pas');
    P.Targets.AddUnit('castleshaders.pas');
    P.Targets.AddUnit('castlespacefillingcurves.pas');
    P.Targets.AddUnit('castlespheresampling.pas');
    P.Targets.AddUnit('castlesphericalharmonics.pas');

    P.SourcePath.Add('src' + PathDelim + '3d' + PathDelim + 'opengl');
    P.Targets.AddUnit('castleglboxes.pas');
    P.Targets.AddUnit('castleglshadowvolumes.pas');

    P.SourcePath.Add('src' + PathDelim + 'audio');
    P.Targets.AddUnit('castlealutils.pas');
    P.Targets.AddUnit('castleogg.pas');
    P.Targets.AddUnit('castleopenal.pas');
    P.Targets.AddUnit('castlesoundallocator.pas');
    P.Targets.AddUnit('castlesoundengine.pas');
    P.Targets.AddUnit('castleefx.pas');
    P.Targets.AddUnit('castlesoundfile.pas');
    P.Targets.AddUnit('castlevorbiscodec.pas');
    P.Targets.AddUnit('castlevorbisdecoder.pas');
    P.Targets.AddUnit('castlevorbisfile.pas');

    P.SourcePath.Add('src' + PathDelim + 'base');
    P.Targets.AddUnit('castleapplicationproperties.pas');
    P.Targets.AddUnit('castleclassutils.pas');
    P.Targets.AddUnit('castlecolors.pas');
    P.Targets.AddUnit('castleconfig.pas');
    P.Targets.AddUnit('castledynlib.pas');
    P.Targets.AddUnit('castlefilesutils.pas');
    P.Targets.AddUnit('castlegziointernal.pas');
    P.Targets.AddUnit('castleinterfaces.pas');
    P.Targets.AddUnit('castlelog.pas');
    P.Targets.AddUnit('castlemessaging.pas');
    P.Targets.AddUnit('castleparameters.pas');
    P.Targets.AddUnit('castleprogress.pas');
    P.Targets.AddUnit('castlerectangles.pas');
    P.Targets.AddUnit('castlestringutils.pas');
    P.Targets.AddUnit('castlestreamutils.pas');
    P.Targets.AddUnit('castletimeutils.pas');
    P.Targets.AddUnit('castleunicode.pas');
    P.Targets.AddUnit('castleutils.pas');
    P.Targets.AddUnit('castlevectors.pas');
    P.Targets.AddUnit('castlewarnings.pas');
    P.Targets.AddUnit('castlexmlconfig.pas');
    P.Targets.AddUnit('castlexmlutils.pas');
    P.Targets.AddUnit('castlezlib.pas');
    P.Targets.AddUnit('castlezstream.pas');
    P.Targets.AddUnit('castlefindfiles.pas');
    P.Targets.AddUnit('castlefilefilters.pas');
    P.Targets.AddUnit('castlegenericlists.pas');
    P.Targets.AddUnit('castleprogressconsole.pas');
    P.Targets.AddUnit('castlerecentfiles.pas');

    P.SourcePath.Add('src' + PathDelim + 'base' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlegles20.pas');
    P.Targets.AddUnit('castleglversion.pas');

    P.SourcePath.Add('src' + PathDelim + 'services');
    P.Targets.AddUnit('castleads.pas');
    P.Targets.AddUnit('castleanalytics.pas');
    P.Targets.AddUnit('castlegiftiz.pas');
    P.Targets.AddUnit('castlegoogleplaygames.pas');
    P.Targets.AddUnit('castlehelpshift.pas');
    P.Targets.AddUnit('castleinapppurchases.pas');
    P.Targets.AddUnit('castleopendocument.pas');

    {$ifdef ANDROID_POSSIBLE}
    if Defaults.OS = Android then
    begin
      P.SourcePath.Add('src' + PathDelim + 'base' + PathDelim + 'android');
      P.Targets.AddUnit('castleandroidinternalassetmanager.pas');
      P.Targets.AddUnit('castleandroidinternalconfiguration.pas');
      P.Targets.AddUnit('castleandroidinternalinput.pas');
      P.Targets.AddUnit('castleandroidinternalkeycodes.pas');
      P.Targets.AddUnit('castleandroidinternallog.pas');
      P.Targets.AddUnit('castleandroidinternallog.pas');
      P.Targets.AddUnit('castleandroidinternallooper.pas');
      P.Targets.AddUnit('castleandroidinternalnativeactivity.pas');
      P.Targets.AddUnit('castleandroidinternalnativewindow.pas');
      P.Targets.AddUnit('castleandroidinternalrect.pas');
      P.Targets.AddUnit('castleandroidnativeappglue.pas');
    end;
    {$endif}

    if IOS then
    begin
      P.SourcePath.Add('src' + PathDelim + 'base' + PathDelim + 'ios');
    end;

    P.SourcePath.Add('src' + PathDelim + 'castlescript');
    P.Targets.AddUnit('castlenoise.pas');
    P.Targets.AddUnit('castlescript.pas');
    P.Targets.AddUnit('castlescriptarrays.pas');
    P.Targets.AddUnit('castlescriptcorefunctions.pas');
    P.Targets.AddUnit('castlescriptimages.pas');
    P.Targets.AddUnit('castlescriptlexer.pas');
    P.Targets.AddUnit('castlescriptparser.pas');
    P.Targets.AddUnit('castlescriptvectors.pas');
    P.Targets.AddUnit('castlescriptxml.pas');

    P.SourcePath.Add('src' + PathDelim + 'castlescript' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlecurves.pas');

    P.SourcePath.Add('src' + PathDelim + 'fonts');
    P.Targets.AddUnit('castlefreetypeh.pas');
    P.Targets.AddUnit('castlefreetype.pas');
    P.Targets.AddUnit('castleftfont.pas');
    P.Targets.AddUnit('castletexturefont_dejavusans_10.pas');
    P.Targets.AddUnit('castletexturefont_dejavusansmono_18.pas');
    P.Targets.AddUnit('castletexturefont_dejavusansmonobold_15.pas');
    P.Targets.AddUnit('castletexturefont_djvmono_20.pas');
    P.Targets.AddUnit('castletexturefont_djvmonob_20.pas');
    P.Targets.AddUnit('castletexturefont_djvmonobo_20.pas');
    P.Targets.AddUnit('castletexturefont_djvmonoo_20.pas');
    P.Targets.AddUnit('castletexturefont_djvsans_20.pas');
    P.Targets.AddUnit('castletexturefont_djvsansb_20.pas');
    P.Targets.AddUnit('castletexturefont_djvsansbo_20.pas');
    P.Targets.AddUnit('castletexturefont_djvsanso_20.pas');
    P.Targets.AddUnit('castletexturefont_djvserif_20.pas');
    P.Targets.AddUnit('castletexturefont_djvserifb_20.pas');
    P.Targets.AddUnit('castletexturefont_djvserifbi_20.pas');
    P.Targets.AddUnit('castletexturefont_djvserifi_20.pas');
    P.Targets.AddUnit('castletexturefontdata.pas');
    P.Targets.AddUnit('castlefont2pascal.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'fonts' + PathDelim + 'windows');
      P.Targets.AddUnit('castlewindowsfonts.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'fonts' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlefonts.pas');
    P.Targets.AddUnit('castlefontfamily.pas');

    P.SourcePath.Add('src' + PathDelim + 'game');
    P.Targets.AddUnit('castle2dscenemanager.pas');
    P.Targets.AddUnit('castlecreatures.pas');
    P.Targets.AddUnit('castlegamenotifications.pas');
    P.Targets.AddUnit('castleitems.pas');
    P.Targets.AddUnit('castlelevels.pas');
    P.Targets.AddUnit('castleplayer.pas');
    P.Targets.AddUnit('castleresources.pas');
    P.Targets.AddUnit('castlescenemanager.pas');

    P.SourcePath.Add('src' + PathDelim + 'images');
    P.Targets.AddUnit('castlefpwritepng.pas');
    P.Targets.AddUnit('castleimages.pas');
    P.Targets.AddUnit('castlepng.pas');
    P.Targets.AddUnit('castlecompositeimage.pas');
    P.Targets.AddUnit('castletextureimages.pas');
    P.Targets.AddUnit('castlevideos.pas');

    P.SourcePath.Add('src' + PathDelim + 'images' + PathDelim + 'opengl');
    P.Targets.AddUnit('castleglimages.pas');
    P.Targets.AddUnit('castleglutils.pas');
    P.Targets.AddUnit('castleglshaders.pas');
    P.Targets.AddUnit('castlescreeneffects.pas');

    P.SourcePath.Add('src' + PathDelim + 'net');
    P.Targets.AddUnit('castleuriutils.pas');
    P.Targets.AddUnit('castledatauri.pas');
    P.Targets.AddUnit('castledownload.pas');

    P.SourcePath.Add('src' + PathDelim + 'ui');
    P.Targets.AddUnit('castleinputs.pas');
    P.Targets.AddUnit('castlekeysmouse.pas');
    P.Targets.AddUnit('castleinternalpk3dconnexion.pas');
    P.Targets.AddUnit('castleuicontrols.pas');
    P.Targets.AddUnit('castlejoysticks.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'windows');
      P.Targets.AddUnit('castleinternaltdxinput_tlb.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlecontrols.pas');
    P.Targets.AddUnit('castleflasheffect.pas');
    P.Targets.AddUnit('castleinspectorcontrol.pas');
    P.Targets.AddUnit('castlenotifications.pas');
    P.Targets.AddUnit('castleonscreenmenu.pas');
    P.Targets.AddUnit('castletiledmap.pas');

    P.SourcePath.Add('src' + PathDelim + 'window');
    P.Targets.AddUnit('castlemessages.pas');
    p.targets.addunit('castlewindowrecentfiles.pas');
    P.Targets.AddUnit('castlesoundmenu.pas');
    p.targets.addunit('castlewindow.pas');
    p.targets.addunit('castlewindowtouch.pas');
    P.Targets.AddUnit('castlewindowprogress.pas');
    p.targets.addunit('castlewindowmodes.pas');
    if Xlib then
    begin
      P.SourcePath.Add('src' + PathDelim + 'window' + PathDelim + 'unix');
      P.Targets.AddUnit('castleglx.pas');
      P.Targets.AddUnit('castlexf86vmode.pas');
      P.Targets.AddUnit('castlexlib.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'x3d');
    P.Targets.AddUnit('castlearraysgenerator.pas');
    P.Targets.AddUnit('castleinternalnodeinterpolator.pas');
    P.Targets.AddUnit('castleinternalnormals.pas');
    P.Targets.AddUnit('castleinternalshapeoctree.pas');
    P.Targets.AddUnit('castleinternaltriangleoctree.pas');
    P.Targets.AddUnit('castlematerialproperties.pas');
    P.Targets.AddUnit('castleraytracer.pas');
    P.Targets.AddUnit('castlerenderingcamera.pas');
    P.Targets.AddUnit('castlescenecore.pas');
    P.Targets.AddUnit('castleshapeinternalshadowvolumes.pas');
    P.Targets.AddUnit('castleshapes.pas');
    P.Targets.AddUnit('castleterrain.pas');
    P.Targets.AddUnit('x3dcamerautils.pas');
    P.Targets.AddUnit('x3dfields.pas');
    P.Targets.AddUnit('x3dlexer.pas');
    P.Targets.AddUnit('x3dload.pas');
    P.Targets.AddUnit('x3dloadinternalcollada.pas');
    P.Targets.AddUnit('x3dloadinternalgeo.pas');
    P.Targets.AddUnit('x3dloadinternalmd3.pas');
    P.Targets.AddUnit('x3dloadinternalobj.pas');
    P.Targets.AddUnit('x3dloadinternal3ds.pas');
    P.Targets.AddUnit('x3dloadinternalspine.pas');
    P.Targets.AddUnit('x3dloadinternalutils.pas');
    P.Targets.AddUnit('x3dnodes.pas');
    P.Targets.AddUnit('x3dshadowmaps.pas');
    P.Targets.AddUnit('x3dtime.pas');
    P.Targets.AddUnit('x3dtriangles.pas');
    P.Targets.AddUnit('x3dcastlescript.pas');

    P.SourcePath.Add('src' + PathDelim + 'x3d' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlebackground.pas');
    P.Targets.AddUnit('castleglcubemaps.pas');
    P.Targets.AddUnit('castlescene.pas');
    P.Targets.AddUnit('castleshapeinternalrendershadowvolumes.pas');
    P.Targets.AddUnit('castlerendererinternalshader.pas');
    P.Targets.AddUnit('castlerendererinternallights.pas');
    P.Targets.AddUnit('castlerendererinternaltextureenv.pas');
    P.Targets.AddUnit('castlerenderer.pas');
    P.Targets.AddUnit('castleprecalculatedanimation.pas');

    Run;
  end;
end.
