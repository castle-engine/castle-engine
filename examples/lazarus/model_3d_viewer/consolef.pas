unit ConsoleF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleWarnings;

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
  end;

var
  Console: TConsole;

procedure OnWarningVrmlConsole(
  const WarningType: TWarningType; const Category, S: string);

implementation

uses MainF;

procedure OnWarningVrmlConsole(
  const WarningType: TWarningType; const Category, S: string);
begin
  Console.Memo1.Lines.Append(Category + ': ' + S);
  Console.WasWarnings := true;
end;

{ TConsole }

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

