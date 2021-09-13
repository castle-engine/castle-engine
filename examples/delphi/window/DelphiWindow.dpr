program DelphiWindow;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, CastleLog, CastleWindow;

var
  Window: TCastleWindowBase;

  procedure Render(Sender: TCastleContainer);
  begin
    // ... e.g. DrawRectangle or TDrawableImage.Draw calls inside
  end;

  begin
    InitializeLog;
    WritelnLog('Simplest CastleWindow on Delphi');
    Window := TCastleWindowBase.Create(Application);
    //Window1.OnResize := Resize;
    Window.Caption := 'Simplest CastleWindow on Delphi';
    Window.OpenAndRun;
  end.end.
