{$ifndef ALLPACKAGES}
program fpmake;

{$mode objfpc}{$h+}

uses fpmkunit;
{$endif}

Procedure add_rtl_generics(ADirectory : string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('rtl-generics');
    P.ShortName:='rtlg';
    P.Author := 'Maciej Izak';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Generic collection library.';
    P.NeedLibC:= false;
    P.OSes := AllOSes-[embedded];
    P.Directory:=ADirectory;
    P.Version:='3.1.1';
    P.Dependencies.Add('rtl-objpas');
    P.SourcePath.Add('src');
    P.IncludePath.Add('src/inc');
    T:=P.Targets.AddUnit('generics.collections.pas');
    with T.Dependencies do
      begin
        AddUnit('generics.memoryexpanders');
        AddUnit('generics.defaults');
        AddUnit('generics.helpers');
        AddUnit('generics.strings');
      end;
    T:=P.Targets.AddUnit('generics.defaults.pas');
    with T.Dependencies do
      begin
        AddUnit('generics.hashes');
        AddUnit('generics.strings');
        AddUnit('generics.helpers');
      end;
    T:=P.Targets.AddUnit('generics.hashes.pas');
    T:=P.Targets.AddUnit('generics.helpers.pas');
    T:=P.Targets.AddUnit('generics.memoryexpanders.pas');
    T:=P.Targets.AddUnit('generics.strings.pas');
    // Examples
    P.ExamplePath.Add('examples/tarraydouble');
    T:=P.Targets.AddExampleProgram('tarrayprojectdouble.lpr');
    P.ExamplePath.Add('examples/tarraysingle');
    T:=P.Targets.AddExampleProgram('tarrayprojectsingle.lpr');
    P.ExamplePath.Add('examples/tcomparer');
    T:=P.Targets.AddExampleProgram('tcomparerproject.lpr');
    P.ExamplePath.Add('examples/thashmap');
    T:=P.Targets.AddExampleProgram('thashmapproject.lpr');
    P.ExamplePath.Add('examples/thashmapcaseinsensitive');
    T:=P.Targets.AddExampleProgram('thashmapcaseinsensitive.lpr');
    P.ExamplePath.Add('examples/thashmapextendedequalitycomparer');
    T:=P.Targets.AddExampleProgram('thashmapextendedequalitycomparer.lpr');
    P.ExamplePath.Add('examples/tobjectlist');
    T:=P.Targets.AddExampleProgram('tobjectlistproject.lpr');
    P.ExamplePath.Add('examples/tqueue');
    T:=P.Targets.AddExampleProgram('tqueueproject.lpr');
    P.ExamplePath.Add('examples/tstack');
    T:=P.Targets.AddExampleProgram('tstackproject.lpr');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_rtl_generics('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
