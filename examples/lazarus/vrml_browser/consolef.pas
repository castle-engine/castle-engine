unit ConsoleF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleWarnings;

type

  { TVrmlConsole }

  TVrmlConsole = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    WasWarnings: boolean;
  end;

var
  VrmlConsole: TVrmlConsole;

procedure OnWarningVrmlConsole(
  const WarningType: TWarningType; const Category, S: string);

implementation

uses MainF;

procedure OnWarningVrmlConsole(
  const WarningType: TWarningType; const Category, S: string);
begin
  VrmlConsole.Memo1.Lines.Append(Category + ': ' + S);
  VrmlConsole.WasWarnings := true;
end;

{ TVrmlConsole }

procedure TVrmlConsole.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TVrmlConsole.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Main.MenuShowVrmlConsole.Checked := false;
end;

initialization
  {$I vrmlconsolef.lrs}

end.

