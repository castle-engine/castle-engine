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
    P.Targets.AddUnit('castle3d.pas');
    P.Targets.AddUnit('castleboxes.pas');
    P.Targets.AddUnit('castleconvexhull.pas');
    P.Targets.AddUnit('castlefrustum.pas');
    P.Targets.AddUnit('castlenoise.pas');
    P.Targets.AddUnit('castleoctree.pas');
    P.Targets.AddUnit('castletriangles.pas');
    P.Targets.AddUnit('castletriangulate.pas');
    P.Targets.AddUnit('cubemap.pas');
    P.Targets.AddUnit('geometryarrays.pas');
    P.Targets.AddUnit('nurbs.pas');
    P.Targets.AddUnit('quaternions.pas');
    P.Targets.AddUnit('rayswindow.pas');
    P.Targets.AddUnit('sectorswaypoints.pas');
    P.Targets.AddUnit('shaders.pas');
    P.Targets.AddUnit('spacefillingcurves.pas');
    P.Targets.AddUnit('spheresampling.pas');
    P.Targets.AddUnit('sphericalharmonics.pas');

    P.SourcePath.Add('src' + PathDelim + 'audio');
    P.Targets.AddUnit('alutils.pas');
    P.Targets.AddUnit('castleogg.pas');
    P.Targets.AddUnit('castleopenal.pas');
    P.Targets.AddUnit('castlesoundallocator.pas');
    P.Targets.AddUnit('castlesoundengine.pas');
    P.Targets.AddUnit('efx.pas');
    P.Targets.AddUnit('soundfile.pas');
    P.Targets.AddUnit('vorbiscodec.pas');
    P.Targets.AddUnit('vorbisdecoder.pas');
    P.Targets.AddUnit('vorbisfile.pas');

    P.SourcePath.Add('src' + PathDelim + 'base');
    P.Targets.AddUnit('castleclassutils.pas');
    P.Targets.AddUnit('castlecolors.pas');
    P.Targets.AddUnit('castleconfig.pas');
    P.Targets.AddUnit('castledynlib.pas');
    P.Targets.AddUnit('castlefilesutils.pas');
    P.Targets.AddUnit('castleinterfaces.pas');
    P.Targets.AddUnit('castlelog.pas');
    P.Targets.AddUnit('castleopendocument.pas');
    P.Targets.AddUnit('castleparameters.pas');
    P.Targets.AddUnit('castleprogress.pas');
    P.Targets.AddUnit('castlestringutils.pas');
    P.Targets.AddUnit('castletimeutils.pas');
    P.Targets.AddUnit('castleutils.pas');
    P.Targets.AddUnit('castlevectors.pas');
    P.Targets.AddUnit('castlewarnings.pas');
    P.Targets.AddUnit('castlexmlconfig.pas');
    P.Targets.AddUnit('castlexmlutils.pas');
    P.Targets.AddUnit('castlezlib.pas');
    P.Targets.AddUnit('castlezstream.pas');
    P.Targets.AddUnit('enumeratefiles.pas');
    P.Targets.AddUnit('filefilters.pas');
    P.Targets.AddUnit('genericstructlist.pas');
    P.Targets.AddUnit('progressconsole.pas');
    P.Targets.AddUnit('recentfiles.pas');

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
    P.Targets.AddUnit('castlebitmapfont_bvsans_m10.pas');
    P.Targets.AddUnit('castlebitmapfont_bvsans.pas');
    P.Targets.AddUnit('castlebitmapfont_bvsansmono_bold_m15.pas');
    P.Targets.AddUnit('castlebitmapfont_bvsansmono_m18.pas');
    P.Targets.AddUnit('castlebitmapfonts.pas');
    P.Targets.AddUnit('castlefont2pascal.pas');
    P.Targets.AddUnit('castleoutlinefonts.pas');
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
      P.Targets.AddUnit('windowsfonts.pas');
      P.Targets.AddUnit('winfontconvert.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'game');
    P.Targets.AddUnit('castlecreatures.pas');
    P.Targets.AddUnit('castlegamenotifications.pas');
    P.Targets.AddUnit('castleitems.pas');
    P.Targets.AddUnit('castlelevels.pas');
    P.Targets.AddUnit('castleplayer.pas');
    P.Targets.AddUnit('castleresources.pas');
    P.Targets.AddUnit('castletextureproperties.pas');

    P.SourcePath.Add('src' + PathDelim + 'images');
    P.Targets.AddUnit('castleimages.pas');
    P.Targets.AddUnit('castlepng.pas');
    P.Targets.AddUnit('dds.pas');
    P.Targets.AddUnit('textureimages.pas');
    P.Targets.AddUnit('videos.pas');

    P.SourcePath.Add('src' + PathDelim + 'net');
    P.Targets.AddUnit('castleurlutils.pas');
    P.Targets.AddUnit('datauri.pas');

    P.SourcePath.Add('src' + PathDelim + 'opengl');
    P.Targets.AddUnit('castleglbitmapfonts.pas');
    P.Targets.AddUnit('castlegloutlinefonts.pas');
    P.Targets.AddUnit('castleglutils.pas');
    P.Targets.AddUnit('curve.pas');
    P.Targets.AddUnit('glcubemap.pas');
    P.Targets.AddUnit('glimages.pas');
    P.Targets.AddUnit('glshaders.pas');
    P.Targets.AddUnit('glshadowvolumerenderer.pas');
    P.Targets.AddUnit('glversionunit.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'opengl' + PathDelim + 'windows');
      P.Targets.AddUnit('openglwindowsfonts.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'ui');
    P.Targets.AddUnit('cameras.pas');
    P.Targets.AddUnit('castleinputs.pas');
    P.Targets.AddUnit('castlekeysmouse.pas');
    P.Targets.AddUnit('pk3dconnexion.pas');
    P.Targets.AddUnit('uicontrols.pas');
    if Defaults.OS in AllWindowsOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'windows');
      P.Targets.AddUnit('tdxinput_tlb.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'ui' + PathDelim + 'opengl');
    P.Targets.AddUnit('castlecontrols.pas');
    P.Targets.AddUnit('castlenotifications.pas');
    P.Targets.AddUnit('onscreenmenu.pas');
    P.Targets.AddUnit('onscreenmenuimages.pas');

    P.SourcePath.Add('src' + PathDelim + 'window');
    P.Targets.AddUnit('castlemessages.pas');
    p.targets.addunit('castlerecentfiles.pas');
    P.Targets.AddUnit('castlesoundmenu.pas');
    p.targets.addunit('castlewindow.pas');
    P.Targets.AddUnit('castlewindowprogress.pas');
    p.targets.addunit('windowmodes.pas');
    if Defaults.OS in AllUnixOSes then
    begin
      P.SourcePath.Add('src' + PathDelim + 'window' + PathDelim + 'unix');
      P.Targets.AddUnit('castleglx.pas');
      P.Targets.AddUnit('castlexf86vmode.pas');
      P.Targets.AddUnit('xlibutils.pas');
    end;

    P.SourcePath.Add('src' + PathDelim + 'x3d');
    P.Targets.AddUnit('arraysgenerator.pas');
    P.Targets.AddUnit('castlenormals.pas');
    P.Targets.AddUnit('castlerenderingcamera.pas');
    P.Targets.AddUnit('castlescenecore.pas');
    P.Targets.AddUnit('castleshapes.pas');
    P.Targets.AddUnit('precalculatedanimationcore.pas');
    P.Targets.AddUnit('raytracer.pas');
    P.Targets.AddUnit('shapeoctree.pas');
    P.Targets.AddUnit('triangleoctree.pas');
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
    P.Targets.AddUnit('background.pas');
    P.Targets.AddUnit('castlescene.pas');
    P.Targets.AddUnit('castlescenemanager.pas');
    P.Targets.AddUnit('glrenderershader.pas');
    P.Targets.AddUnit('glrendererlights.pas');
    P.Targets.AddUnit('glrenderertextureenv.pas');
    P.Targets.AddUnit('glrenderer.pas');
    P.Targets.AddUnit('precalculatedanimation.pas');

    Run;
  end;
end.
