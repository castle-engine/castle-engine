{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('pasjpeg');
    P.ShortName:='pjp';
    P.Description := 'Pascal port of JPEG (.JPG) image format handling library from IJG.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.OSes:=P.OSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,wasi];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('jctrans.pas');
    with T.Dependencies do
      begin
      AddUnit('jmorecfg');
      AddUnit('jinclude');
      AddUnit('jdeferr');
      AddUnit('jerror');
      AddUnit('jutils');
      AddUnit('jpeglib');
      AddUnit('jcapimin');
      AddUnit('jcparam');
      AddUnit('jcomapi');
      AddUnit('jcmaster');
      AddUnit('jchuff');
      AddUnit('jcphuff');
      AddUnit('jcmarker');
      end;
    T:=P.Targets.AddUnit('buildpasjpeg.pp');
      T.Install:=False;
      with T.Dependencies do
        begin
          AddInclude('jconfig.inc');
          AddUnit('jcapimin');
          AddUnit('jcapistd');
          AddUnit('jccoefct');
          AddUnit('jccolor');
          AddUnit('jcdctmgr');
          AddUnit('jchuff');
          AddUnit('jcinit');
          AddUnit('jcmainct');
          AddUnit('jcmarker');
          AddUnit('jcmaster');
          AddUnit('jcomapi');
          AddUnit('jcparam');
          AddUnit('jcphuff');
          AddUnit('jcprepct');
          AddUnit('jcsample');
          AddUnit('jdapimin');
          AddUnit('jdapistd');
          AddUnit('jdatadst');
          AddUnit('jdatasrc');
          AddUnit('jdcoefct');
          AddUnit('jdcolor');
          AddUnit('jdct');
          AddUnit('jddctmgr');
          AddUnit('jdeferr');
          AddUnit('jdhuff');
          AddUnit('jdinput');
          AddUnit('jdmainct');
          AddUnit('jdmarker');
          AddUnit('jdmaster');
          AddUnit('jdmerge');
          AddUnit('jdphuff');
          AddUnit('jdpostct');
          AddUnit('jdsample');
          AddUnit('jerror');
          AddUnit('jfdctflt');
          AddUnit('jfdctfst');
          AddUnit('jfdctint');
          AddUnit('jidctflt');
          AddUnit('jidctfst');
          AddUnit('jidctint');
          AddUnit('jidctred');
          AddUnit('jinclude');
          AddUnit('jmemmgr');
          AddUnit('jmemnobs');
          AddUnit('jmorecfg');
          AddUnit('jpeglib');
          AddUnit('jquant1');
          AddUnit('jquant2');
          AddUnit('jutils');
        end;

    T:=P.Targets.AddImplicitUnit('jcapimin.pas');
    T:=P.Targets.AddImplicitUnit('jcapistd.pas');
    T:=P.Targets.AddImplicitUnit('jccoefct.pas');
    T:=P.Targets.AddImplicitUnit('jccolor.pas');
    T:=P.Targets.AddImplicitUnit('jcdctmgr.pas');
    T:=P.Targets.AddImplicitUnit('jchuff.pas');
    T:=P.Targets.AddImplicitUnit('jcinit.pas');
    T:=P.Targets.AddImplicitUnit('jcmainct.pas');
    T:=P.Targets.AddImplicitUnit('jcmarker.pas');
    T:=P.Targets.AddImplicitUnit('jcmaster.pas');
    T:=P.Targets.AddImplicitUnit('jcomapi.pas');
    T:=P.Targets.AddImplicitUnit('jcparam.pas');
    T:=P.Targets.AddImplicitUnit('jcphuff.pas');
    T:=P.Targets.AddImplicitUnit('jcprepct.pas');
    T:=P.Targets.AddImplicitUnit('jcsample.pas');
    T:=P.Targets.AddImplicitUnit('jdapimin.pas');
    T:=P.Targets.AddImplicitUnit('jdapistd.pas');
    T:=P.Targets.AddImplicitUnit('jdatadst.pas');
    T:=P.Targets.AddImplicitUnit('jdatasrc.pas');
    T:=P.Targets.AddImplicitUnit('jdcoefct.pas');
    T:=P.Targets.AddImplicitUnit('jdcolor.pas');
    T:=P.Targets.AddImplicitUnit('jdct.pas');
    T:=P.Targets.AddImplicitUnit('jddctmgr.pas');
    T:=P.Targets.AddImplicitUnit('jdeferr.pas');
    T:=P.Targets.AddImplicitUnit('jdhuff.pas');
    T:=P.Targets.AddImplicitUnit('jdinput.pas');
    T:=P.Targets.AddImplicitUnit('jdmainct.pas');
    T:=P.Targets.AddImplicitUnit('jdmarker.pas');
    T:=P.Targets.AddImplicitUnit('jdmaster.pas');
    T:=P.Targets.AddImplicitUnit('jdmerge.pas');
    T:=P.Targets.AddImplicitUnit('jdphuff.pas');
    T:=P.Targets.AddImplicitUnit('jdpostct.pas');
    T:=P.Targets.AddImplicitUnit('jdsample.pas');
    T:=P.Targets.AddImplicitUnit('jerror.pas');
    T:=P.Targets.AddImplicitUnit('jfdctflt.pas');
    T:=P.Targets.AddImplicitUnit('jfdctfst.pas');
    T:=P.Targets.AddImplicitUnit('jfdctint.pas');
    T:=P.Targets.AddImplicitUnit('jidctflt.pas');
    T:=P.Targets.AddImplicitUnit('jidctfst.pas');
    T:=P.Targets.AddImplicitUnit('jidctint.pas');
    T:=P.Targets.AddImplicitUnit('jidctred.pas');
    T:=P.Targets.AddImplicitUnit('jinclude.pas');
    T:=P.Targets.AddImplicitUnit('jmemmgr.pas');
    T:=P.Targets.AddImplicitUnit('jmemnobs.pas');
    T:=P.Targets.AddImplicitUnit('jmorecfg.pas');
    T:=P.Targets.AddImplicitUnit('jpeglib.pas');
    T:=P.Targets.AddImplicitUnit('jquant1.pas');
    T:=P.Targets.AddImplicitUnit('jquant2.pas');
    T:=P.Targets.AddImplicitUnit('jutils.pas');

    P.Sources.AddSrc('readme.txt');

    P.ExamplePath.Add('examples');

    P.Targets.AddExampleProgram('cjpeg.pas');
    P.Targets.AddExampleProgram('djpeg.pas');
    P.Targets.AddExampleProgram('rdswitch.pas');
    P.Targets.AddExampleProgram('testproj.dpr');
    P.Targets.AddExampleProgram('cdjpeg.pas');
    P.Targets.AddExampleProgram('test1.pas');
    P.Targets.AddExampleProgram('example.pas');
    P.Targets.AddExampleProgram('wrjpgcom.pas');
    P.Targets.AddExampleProgram('rdjpgcom.pas');
    P.Targets.AddExampleProgram('rdtarga.pas');
    P.Targets.AddExampleProgram('demo.pas');
    P.Targets.AddExampleProgram('wrppm.pas');
    P.Targets.AddExampleProgram('transupp.pas');
    P.Targets.AddExampleProgram('rdbmp.pas');
    P.Targets.AddExampleProgram('rdppm.pas');
    P.Targets.AddExampleProgram('rdcolmap.pas');
    P.Targets.AddExampleProgram('wrtarga.pas');
    P.Targets.AddExampleProgram('wrbmp.pas');
    P.Targets.AddExampleProgram('jpegtran.pas');
    P.Targets.AddExampleProgram('cderror.pas');
    P.Targets.AddExampleProgram('test.pas');
    P.Targets.AddExampleProgram('fcache.pas');
    // 'djpeg.res
    // 'testproj.res
    // 'script0.ijg
    // 'test1.dfm
    // 'jpegtran.res
    // 'demo.res
    // 'qtable1.ijg
    // 'script1.ijg
    // 'cjpeg.res
    // 'jpegtran.drc
    // 'script2.ijg

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
