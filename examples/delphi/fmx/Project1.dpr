program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {TestCgeControl};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestCgeControl, TestCgeControl);
  Application.Run;
end.
