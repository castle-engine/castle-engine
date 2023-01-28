unit ConsoleF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleLog;

type

  { TConsole }

  TConsole = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    WasWarnings: boolean;
    procedure WarningToConsole(const Category, S: string);
  end;

var
  Console: TConsole;

implementation

uses MainF,
  CastleApplicationProperties;

{$R *.lfm}

{ TConsole }

procedure TConsole.WarningToConsole(const Category, S: string);
begin
  Memo1.Lines.Append(Category + ': ' + S);
  WasWarnings := true;
end;

procedure TConsole.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TConsole.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Main.MenuShowConsole.Checked := false;
end;

procedure TConsole.FormCreate(Sender: TObject);
begin
  ApplicationProperties.OnWarning.Add(@WarningToConsole);
end;

procedure TConsole.FormDestroy(Sender: TObject);
begin
  ApplicationProperties.OnWarning.Remove(@WarningToConsole);
end;

end.

