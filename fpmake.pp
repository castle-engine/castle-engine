{ Compile Castle Game Engine by fpmake.
  This can be used to compile (and install if you want)
  all engine units. Simple instructions:

  export FPCDIR=..../lib/fpc/2.6.0/  # not needed if FPC installed in stands location
  fpc fpmake.pp
  ./fpmake compile  # use -v to get more info
  ./fpmake install  # use -v to get more info
}

program fpmake;

uses SysUtils, fpmkunit;

var
  P: TPackage;
begin
  with Installer do
  begin
    P := AddPackage('castle-game-engine');

    { Actually, may work on at least
        P.OSes := AllUnixOSes + [win32, win64];
      OSes below are actually tested. }
    P.OSes := [darwin, linux, freebsd, win32];

    P.Options {$ifndef VER2_2} .Text {$endif} := '@castle-fpc.cfg';

    { Add dependencies on FPC packages.
      These aren't really needed, as your default fpc.cfg should
      point to them anyway. They are needed only when compiling with --nofpccfg.
      Anyway, maybe this is a good place to document my dependencies
      on FPC packages --- so let's do this. }
    P.Dependencies.Add('opengl');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-image');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('pasjpeg');
    if Defaults.OS in AllUnixOSes then
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
    P.Version := '3.0.0';

    P.SourcePath.Add('src' + PathDelim + '3d');
    P.Targets.AddUnit('base3d.pas');
    P.Targets.AddUnit('boxes3d.pas');
    P.Targets.AddUnit('convexhullunit.pas');
    P.Targets.AddUnit('cubemap.pas');
    P.Targets.AddUnit('faceindex.pas');
    P.Targets.AddUnit('frustum.pas');
    P.Targets.AddUnit('geometryarrays.pas');
    P.Targets.AddUnit('castleoctree.pas');
    P.Targets.AddUnit('nurbs.pas');
    P.Targets.AddUnit('quaternions.pas');
    P.Targets.AddUnit('rayswindow.pas');
    P.Targets.AddUnit('sectorswaypoints.pas');
    P.Targets.AddUnit('spacefillingcurves.pas');
    P.Targets.AddUnit('spheresampling.pas');
    P.Targets.AddUnit('sphericalharmonics.pas');
    P.Targets.AddUnit('triangulator.pas');

    P.SourcePath.Add('src' + PathDelim + 'x3d');
    P.Targets.AddUnit('normalscalculator.pas');
    P.Targets.AddUnit('x3dload.pas');
    P.Targets.AddUnit('x3dloadinternalcollada.pas');
    P.Targets.AddUnit('x3dloadinternalgeo.pas');
    P.Targets.AddUnit('x3dloadinternalmd3.pas');
    P.Targets.AddUnit('x3dloadinternalobj.pas');
    P.Targets.AddUnit('x3dloadinternal3ds.pas');
    P.Targets.AddUnit('x3dloadinternalutils.pas');
    P.Targets.AddUnit('precalculatedanimationcore.pas');
    P.Targets.AddUnit('x3dcamerautils.pas');
    P.Targets.AddUnit('x3dfields.pas');
    P.Targets.AddUnit('arraysgenerator.pas');
    P.Targets.AddUnit('x3dlexer.pas');
    P.Targets.AddUnit('x3dnodes.pas');
    P.Targets.AddUnit('x3dnodesdetailoptions.pas');
    P.Targets.AddUnit('raytracer.pas');
    P.Targets.AddUnit('castlescenecore.pas');
    P.Targets.AddUnit('shape.pas');
    P.Targets.AddUnit('shapeoctree.pas');
    P.Targets.AddUnit('x3dtime.pas');
    P.Targets.AddUnit('triangle.pas');
    P.Targets.AddUnit('triangleoctree.pas');
    P.Targets.AddUnit('renderingcameraunit.pas');

    P.SourcePath.Add('src' + PathDelim + 'x3d' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlescenemanager.pas');
    P.Targets.AddUnit('precalculatedanimation.pas');
    P.Targets.AddUnit('background.pas');
    P.Targets.AddUnit('castlescene.pas');
    P.Targets.AddUnit('glrenderershader.pas');
    P.Targets.AddUnit('glrendererlights.pas');
    P.Targets.AddUnit('glrenderertextureenv.pas');
    P.Targets.AddUnit('glrenderer.pas');

    P.SourcePath.Add('src' + PathDelim + 'audio');
    P.Targets.AddUnit('alsoundallocator.pas');
    P.Targets.AddUnit('alsoundengine.pas');
    P.Targets.AddUnit('alutils.pas');
    P.Targets.AddUnit('castleogg.pas');
    P.Targets.AddUnit('castleopenal.pas');
    P.Targets.AddUnit('soundfile.pas');
    P.Targets.AddUnit('vorbiscodec.pas');
    P.Targets.AddUnit('vorbisdecoder.pas');
    P.Targets.AddUnit('vorbisfile.pas');

    P.SourcePath.Add('src' + PathDelim + 'base');
    P.Targets.AddUnit('castleconfig.pas');
    P.Targets.AddUnit('enumeratefiles.pas');
    P.Targets.AddUnit('filefilters.pas');
    P.Targets.AddUnit('castleclassutils.pas');
    P.Targets.AddUnit('castlecolors.pas');
    P.Targets.AddUnit('castledynlib.pas');
    P.Targets.AddUnit('castlefilesutils.pas');
    P.Targets.AddUnit('castleinterfaces.pas');
    P.Targets.AddUnit('castlelog.pas');
    P.Targets.AddUnit('castlestringutils.pas');
    P.Targets.AddUnit('castletimeutils.pas');
    P.Targets.AddUnit('castleutils.pas');
    P.Targets.AddUnit('castlewarnings.pas');
    P.Targets.AddUnit('castlexmlconfig.pas');
    P.Targets.AddUnit('castlexmlutils.pas');
    P.Targets.AddUnit('castlezlib.pas');
    P.Targets.AddUnit('castlezstream.pas');
    P.Targets.AddUnit('castleparameters.pas');
    P.Targets.AddUnit('progressconsole.pas');
    P.Targets.AddUnit('progressunit.pas');
    P.Targets.AddUnit('recentfiles.pas');
    P.Targets.AddUnit('vectormath.pas');

    P.SourcePath.Add('src' + PathDelim + 'fonts');
    P.Targets.AddUnit('bfnt_bitstreamverasans_m10_unit.pas');
    P.Targets.AddUnit('bfnt_bitstreamverasans_unit.pas');
    P.Targets.AddUnit('bfnt_bitstreamverasansmono_bold_m15_unit.pas');
    P.Targets.AddUnit('bfnt_bitstreamverasansmono_m18_unit.pas');
    P.Targets.AddUnit('bmpfontstypes.pas');
    P.Targets.AddUnit('font2pascalunit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasans_bold_italic_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasans_bold_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasans_italic_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasans_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasansmono_bold_italic_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasansmono_bold_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasansmono_italic_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamverasansmono_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamveraserif_bold_italic_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamveraserif_bold_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamveraserif_italic_unit.pas');
    P.Targets.AddUnit('ttf_bitstreamveraserif_unit.pas');
    P.Targets.AddUnit('ttfontstypes.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'fonts' + PathDelim + 'windows');
      P.Targets.AddUnit('windowsfonts.pas');
      P.Targets.AddUnit('winfontconvert.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'window');
    P.Targets.AddUnit('castlesoundmenu.pas');
    p.targets.addunit('castlewindow.pas');
    p.targets.addunit('castlerecentfiles.pas');
    P.Targets.AddUnit('castlemessages.pas');
    p.targets.addunit('windowmodes.pas');
    P.Targets.AddUnit('castleprogress.pas');
    if Defaults.OS in AllUnixOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'window' + PathDelim + 'unix');
      P.Targets.AddUnit('castleglx.pas');
      P.Targets.AddUnit('castlexf86vmode.pas');
      P.Targets.AddUnit('xlibutils.pas');
    end;
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'window' + PathDelim + 'windows');
      P.Targets.AddUnit('openglwindowsfonts.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'images');
    P.Targets.AddUnit('images.pas');
    P.Targets.AddUnit('imagescache.pas');
    P.Targets.AddUnit('castlepng.pas');
    P.Targets.AddUnit('videos.pas');
    P.Targets.AddUnit('videoscache.pas');

    P.SourcePath.Add('src' + PathDelim + 'castlescript');
    P.Targets.AddUnit('castlescript.pas');
    P.Targets.AddUnit('castlescriptarrays.pas');
    P.Targets.AddUnit('castlescriptcorefunctions.pas');
    P.Targets.AddUnit('castlescriptimages.pas');
    P.Targets.AddUnit('castlescriptlexer.pas');
    P.Targets.AddUnit('castlescriptparser.pas');
    P.Targets.AddUnit('castlescriptvectors.pas');
    P.Targets.AddUnit('x3dcastlescript.pas');

    P.SourcePath.Add('src' + PathDelim + 'net');
    P.Targets.AddUnit('castleurlutils.pas');

    P.SourcePath.Add('src' + PathDelim + 'opengl');
    P.Targets.AddUnit('beziercurve.pas');
    P.Targets.AddUnit('curve.pas');
    P.Targets.AddUnit('glantialiasing.pas');
    P.Targets.AddUnit('glcubemap.pas');
    P.Targets.AddUnit('glimages.pas');
    P.Targets.AddUnit('glshaders.pas');
    P.Targets.AddUnit('glversionunit.pas');
    P.Targets.AddUnit('castleglut.pas');
    P.Targets.AddUnit('castleglutils.pas');
    P.Targets.AddUnit('normalizationcubemap.pas');
    P.Targets.AddUnit('openglbmpfonts.pas');
    P.Targets.AddUnit('openglfonts.pas');
    P.Targets.AddUnit('openglttfonts.pas');
    P.Targets.AddUnit('glshadowvolumerenderer.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'opengl' + PathDelim + 'windows');
      P.Targets.AddUnit('openglwindowsfonts.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'ui');
    P.Targets.AddUnit('rectangles.pas');
    P.Targets.AddUnit('keysmouse.pas');
    P.Targets.AddUnit('uicontrols.pas');
    P.Targets.AddUnit('cameras.pas');
    P.Targets.AddUnit('pk3dconnexion.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'windows');
      P.Targets.AddUnit('tdxinput_tlb.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlecontrols.pas');
    P.Targets.AddUnit('onscreenmenu.pas');
    P.Targets.AddUnit('onscreenmenuimages.pas');
    P.Targets.AddUnit('castlenotifications.pas');

    P.SourcePath.Add('src' + PathDelim + 'game');
    P.Targets.AddUnit('castlecreatures.pas');
    P.Targets.AddUnit('castlegamecache.pas');
    P.Targets.AddUnit('castlegamenotifications.pas');
    P.Targets.AddUnit('castlegamevideooptions.pas');
    P.Targets.AddUnit('castleresources.pas');

    Run;
  end;
end.
