{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  CastleControl, CastleDialogs, CastleViewport, CastleScene;

type
  TMain = class(TForm)
    CastleControl1: TCastleControl;
    CastleControl2: TCastleControl;
    LabelFps1: TLabel;
    LabelFps2: TLabel;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  end;

var
  Main: TMain;

implementation

uses X3DLoad, CastleLCLUtils, CastleSceneCore;

{$R *.lfm}

procedure TMain.TimerTimer(Sender: TObject);
begin
  LabelFps1.Caption := 'FPS: ' + CastleControl1.Fps.ToString;
  LabelFps2.Caption := 'FPS: ' + CastleControl2.Fps.ToString;
end;

end.
