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
    WasWarnings: boolean;
    procedure WarningToConsole(Sender: TObject; const Category, S: string);
  end;

var
  Console: TConsole;

implementation

uses MainF;

{ TConsole }

procedure TConsole.WarningToConsole(Sender: TObject; const Category, S: string);
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

initialization
  {$I consolef.lrs}

end.

