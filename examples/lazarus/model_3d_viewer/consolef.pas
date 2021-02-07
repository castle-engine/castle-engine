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
  private
  public
    WasWarnings: Boolean;
    procedure WarningToConsole(const Category, S: String);
  end;

var
  Console: TConsole;

implementation

uses MainF;

{$R *.lfm}

{ TConsole }

procedure TConsole.WarningToConsole(const Category, S: String);
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

end.

