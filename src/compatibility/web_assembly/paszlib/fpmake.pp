{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
  D : TDependency;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('paszlib');
    P.ShortName:='pzl';
    P.Description := 'Pascal port of the ZLIB library - support for deflate compression method using for GZIP, PNG, ZIP, etc.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.OSes := P.OSes - [embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,wasi];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    D:=P.Dependencies.Add('hash');
      D.Version:='3.3.1';

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    T:=P.Targets.AddUnit('paszlib.pas');
      T.Dependencies.AddUnit('adler');
      T.Dependencies.AddUnit('gzio');
      T.Dependencies.AddUnit('infblock');
      T.Dependencies.AddUnit('infcodes');
      T.Dependencies.AddUnit('inffast');
      T.Dependencies.AddUnit('inftrees');
      T.Dependencies.AddUnit('infutil');
      T.Dependencies.AddUnit('trees');
      T.Dependencies.AddUnit('zbase');
      T.Dependencies.AddUnit('zcompres');
      T.Dependencies.AddUnit('zdeflate');
      T.Dependencies.AddUnit('zinflate');
      T.Dependencies.AddUnit('zuncompr');
    T:=P.Targets.AddUnit('zip.pas');
      T.Dependencies.AddUnit('paszlib');
      T.Dependencies.AddUnit('ziputils');
    T:=P.Targets.AddUnit('unzip.pas');
      T.Dependencies.AddUnit('paszlib');
      T.Dependencies.AddUnit('ziputils');
    T:=P.Targets.AddUnit('zipper.pp');
      T.Dependencies.AddUnit('paszlib');
    T.ResourceStrings := True;
    T:=P.Targets.AddImplicitUnit('adler.pas');
      T.Dependencies.AddInclude('zconf.inc');
    T:=P.Targets.AddImplicitUnit('gzio.pas');
    T:=P.Targets.AddImplicitUnit('infblock.pas');
    T:=P.Targets.AddImplicitUnit('infcodes.pas');
    T:=P.Targets.AddImplicitUnit('inffast.pas');
    T:=P.Targets.AddImplicitUnit('inftrees.pas');
    T:=P.Targets.AddImplicitUnit('infutil.pas');
    T:=P.Targets.AddImplicitUnit('trees.pas');
    T:=P.Targets.AddImplicitUnit('zbase.pas');
    T.ResourceStrings := True;
    T:=P.Targets.AddImplicitUnit('zcompres.pas');
    T:=P.Targets.AddImplicitUnit('zdeflate.pas');
    T:=P.Targets.AddImplicitUnit('zinflate.pas');
    T:=P.Targets.AddImplicitUnit('zuncompr.pas');
    T:=P.Targets.AddImplicitUnit('ziputils.pas');
    T:=P.Targets.AddImplicitUnit('zstream.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddExampleProgram('examples/example.pas');
    T:=P.Targets.AddExampleProgram('examples/minigzip.pas');
    T:=P.Targets.AddExampleProgram('examples/miniunz.pas');
    T:=P.Targets.AddExampleProgram('examples/minizip.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

