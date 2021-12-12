program HqDemo;

uses
  Forms,
  FormMain in 'FormMain.pas' {MainForm},
  FormView in 'FormView.pas' {ViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.
