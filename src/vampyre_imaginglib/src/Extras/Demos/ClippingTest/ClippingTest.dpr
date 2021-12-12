program ClippingTest;

uses
  Forms,
  ClipForm in 'ClipForm.pas' {MainForm},
  ResultsForm in 'ResultsForm.pas' {ResultForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TResultForm, ResultForm);
  Application.Run;
end.
