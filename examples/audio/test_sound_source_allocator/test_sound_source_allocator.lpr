program test_sound_source_allocator;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainf, castle_components;

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

