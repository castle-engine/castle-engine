program DelphiWindow;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, CastleLog, CastleWindow, CastleGLShaders;

var
  Window: TCastleWindowBase;

  procedure Render(Sender: TCastleContainer);
  begin
    // ... e.g. DrawRectangle or TDrawableImage.Draw calls inside
  end;

  begin
    InitializeLog;
    //LogShaders := true;
    WritelnLog('Simplest CastleWindow in Delphi');
    Window := TCastleWindowBase.Create(Application);
    Window.Caption := 'Simplest CastleWindow in Delphi';
    Window.OpenAndRun;
  end.
