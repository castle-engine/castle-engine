program CastleFmx;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {TestCgeControl};

{$R *.res}

{.$I request_dedicated_gpu.inc}

begin
  Application.Initialize;
  Application.CreateForm(TTestCgeControl, TestCgeControl);
  Application.Run;
end.
