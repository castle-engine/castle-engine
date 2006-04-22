program test_al_sound_allocator;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainf, kambi_units;

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

