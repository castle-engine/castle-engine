unit OpenGLInformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TOpenGLInformation }

  TOpenGLInformation = class(TForm)
    ButtonClose: TButton;
    MemoInfo: TMemo;
  private
    { private declarations }
  public
    class procedure Execute;
  end; 

implementation

uses CastleGLUtils;

{ TOpenGLInformation }

class procedure TOpenGLInformation.Execute;
var
  F: TOpenGLInformation;
begin
  F := TOpenGLInformation.Create(nil);
  try
    F.MemoInfo.Lines.Text := GLInformationString;
    F.ShowModal;
  finally FreeAndNil(F) end;
end;

initialization
  {$I openglinformation.lrs}

end.

