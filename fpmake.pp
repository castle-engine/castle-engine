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

uses SysUtils, fpmkunit;

var
  P: TPackage;
  IOS: boolean; //< compiling for iOS target
  Xlib: boolean; //< OS has working Xlib packages
begin
  with Installer do
  begin
    P := AddPackage('castle-game-engine');

    { Should work on at least
        P.OSes := AllUnixOSes + [win32, win64];
      But below we limit the list only to the OSes actually tested. }
    P.OSes := [Darwin, Linux, Freebsd, Win32, IPhoneSim
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
    P.Version := '4.1.0';

    P.SourcePath.Add('src' + PathDelim + '3d');
    P.Targets.AddUnit('castle3d.pas');
    P.Targets.AddUnit('castleboxes.pas');
    P.Targets.AddUnit('castleconvexhull.pas');
    P.Targets.AddUnit('castlefrustum.pas');
    P.Targets.AddUnit('castlenoise.pas');
    P.Targets.AddUnit('castleoctree.pas');
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
    P.Targets.AddUnit('castleclassutils.pas');
    P.Targets.AddUnit('castlecolors.pas');
    P.Targets.AddUnit('castleconfig.pas');
    P.Targets.AddUnit('castledynlib.pas');
    P.Targets.AddUnit('castlefilesutils.pas');
    P.Targets.AddUnit('castlegziointernal.pas');
    P.Targets.AddUnit('castleinterfaces.pas');
    P.Targets.AddUnit('castlelog.pas');
    P.Targets.AddUnit('castleopendocument.pas');
    P.Targets.AddUnit('castleparameters.pas');
    P.Targets.AddUnit('castleprogress.pas');
    P.Targets.AddUnit('castlerectangles.pas');
    P.Targets.AddUnit('castlestringutils.pas');
    P.Targets.AddUnit('castletimeutils.pas');
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

    {$ifdef ANDROID_POSSIBLE}
    if Defaults.OS = Android then
    begin
      P.SourcePath.Add('src' + PathDelim + 'base' + PathDelim + 'android');
      P.Targets.AddUnit('castleandroidlog.pas');
      P.Targets.AddUnit('castleandroidassetmanager.pas');
      P.Targets.AddUnit('castleandroidconfiguration.pas');
      P.Targets.AddUnit('castleandroidinput.pas');
      P.Targets.AddUnit('castleandroidkeycodes.pas');
      P.Targets.AddUnit('castleandroidlog.pas');
      P.Targets.AddUnit('castleandroidlooper.pas');
      P.Targets.AddUnit('castleandroidnativeactivity.pas');
      P.Targets.AddUnit('castleandroidnativeappglue.pas');
      P.Targets.AddUnit('castleandroidnativewindow.pas');
      P.Targets.AddUnit('castleandroidrect.pas');
    end;
    {$endif}

    P.SourcePath.Add('src' + PathDelim + 'castlescript');
    P.Targets.AddUnit('castlescript.pas');
    P.Targets.AddUnit('castlescriptarrays.pas');
    P.Targets.AddUnit('castlescriptcorefunctions.pas');
    P.Targets.AddUnit('castlescriptimages.pas');
    P.Targets.AddUnit('castlescriptlexer.pas');
    P.Targets.AddUnit('castlescriptparser.pas');
    P.Targets.AddUnit('castlescriptvectors.pas');
    P.Targets.AddUnit('x3dcastlescript.pas');

    P.SourcePath.Add('src' + PathDelim + 'fonts');
    P.Targets.AddUnit('castletexturefont_dejavusans_10.pas');
    P.Targets.AddUnit('castletexturefont_dejavusans_20.pas');
    P.Targets.AddUnit('castletexturefont_dejavusansmono_18.pas');
    P.Targets.AddUnit('castletexturefont_dejavusansmonobold_15.pas');
    P.Targets.AddUnit('castletexturefontdata.pas');
    P.Targets.AddUnit('castlefont2pascal.pas');
    P.Targets.AddUnit('castleoutlinefontdata.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsans_bold_italic.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsans_bold.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsans_italic.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsans.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsansmono_bold_italic.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsansmono_bold.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsansmono_italic.pas');
    P.Targets.AddUnit('castleoutlinefont_bvsansmono.pas');
    P.Targets.AddUnit('castleoutlinefont_bvserif_bold_italic.pas');
    P.Targets.AddUnit('castleoutlinefont_bvserif_bold.pas');
    P.Targets.AddUnit('castleoutlinefont_bvserif_italic.pas');
    P.Targets.AddUnit('castleoutlinefont_bvserif.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'fonts' + PathDelim + 'windows');
      P.Targets.AddUnit('castlewindowsfonts.pas');
      P.Targets.AddUnit('castlewinfontconvert.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'game');
    P.Targets.AddUnit('castlecreatures.pas');
    P.Targets.AddUnit('castlegamenotifications.pas');
    P.Targets.AddUnit('castleitems.pas');
    P.Targets.AddUnit('castlelevels.pas');
    P.Targets.AddUnit('castleplayer.pas');
    P.Targets.AddUnit('castleresources.pas');
    P.Targets.AddUnit('castlematerialproperties.pas');

    P.SourcePath.Add('src' + PathDelim + 'images');
    P.Targets.AddUnit('castleimages.pas');
    P.Targets.AddUnit('castlepng.pas');
    P.Targets.AddUnit('castledds.pas');
    P.Targets.AddUnit('castletextureimages.pas');
    P.Targets.AddUnit('castlevideos.pas');

    P.SourcePath.Add('src' + PathDelim + 'net');
    P.Targets.AddUnit('castleuriutils.pas');
    P.Targets.AddUnit('castledatauri.pas');
    P.Targets.AddUnit('castledownload.pas');

    P.SourcePath.Add('src' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlefonts.pas');
    P.Targets.AddUnit('castlegles20.pas');
    P.Targets.AddUnit('castlegloutlinefonts.pas');
    P.Targets.AddUnit('castleglutils.pas');
    P.Targets.AddUnit('castlecurves.pas');
    P.Targets.AddUnit('castleglcubemaps.pas');
    P.Targets.AddUnit('castleglimages.pas');
    P.Targets.AddUnit('castleglshaders.pas');
    P.Targets.AddUnit('castleglshadowvolumes.pas');
    P.Targets.AddUnit('castleglversion.pas');
    P.Targets.AddUnit('castlescreeneffects.pas');

    P.SourcePath.Add('src' + PathDelim + 'ui');
    P.Targets.AddUnit('castlecameras.pas');
    P.Targets.AddUnit('castleinputs.pas');
    P.Targets.AddUnit('castlekeysmouse.pas');
    P.Targets.AddUnit('pk3dconnexion.pas');
    P.Targets.AddUnit('castleuicontrols.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'windows');
      P.Targets.AddUnit('tdxinput_tlb.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlecontrols.pas');
    P.Targets.AddUnit('castlenotifications.pas');
    P.Targets.AddUnit('castleonscreenmenu.pas');

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
    P.Targets.AddUnit('castlenormals.pas');
    P.Targets.AddUnit('castlerenderingcamera.pas');
    P.Targets.AddUnit('castlescenecore.pas');
    P.Targets.AddUnit('castleshapes.pas');
    P.Targets.AddUnit('castleprecalculatedanimationcore.pas');
    P.Targets.AddUnit('castleraytracer.pas');
    P.Targets.AddUnit('castleshapeoctree.pas');
    P.Targets.AddUnit('castletriangleoctree.pas');
    P.Targets.AddUnit('x3dcamerautils.pas');
    P.Targets.AddUnit('x3dfields.pas');
    P.Targets.AddUnit('x3dlexer.pas');
    P.Targets.AddUnit('x3dload.pas');
    P.Targets.AddUnit('x3dloadinternalcollada.pas');
    P.Targets.AddUnit('x3dloadinternalgeo.pas');
    P.Targets.AddUnit('x3dloadinternalmd3.pas');
    P.Targets.AddUnit('x3dloadinternalobj.pas');
    P.Targets.AddUnit('x3dloadinternal3ds.pas');
    P.Targets.AddUnit('x3dloadinternalutils.pas');
    P.Targets.AddUnit('x3dnodes.pas');
    P.Targets.AddUnit('x3dnodesdetailoptions.pas');
    P.Targets.AddUnit('x3dshadowmaps.pas');
    P.Targets.AddUnit('x3dtime.pas');
    P.Targets.AddUnit('x3dtriangles.pas');

    P.SourcePath.Add('src' + PathDelim + 'x3d' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlebackground.pas');
    P.Targets.AddUnit('castlescene.pas');
    P.Targets.AddUnit('castlescenemanager.pas');
    P.Targets.AddUnit('castlerendererinternalshader.pas');
    P.Targets.AddUnit('castlerendererinternallights.pas');
    P.Targets.AddUnit('castlerendererinternaltextureenv.pas');
    P.Targets.AddUnit('castlerenderer.pas');
    P.Targets.AddUnit('castleprecalculatedanimation.pas');

    Run;
  end;
end.
