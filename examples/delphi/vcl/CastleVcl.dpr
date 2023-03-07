program CastleVcl;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

{ Forces using a dedicated (faster) GPU on laptops with multiple GPUs.
  See https://castle-engine.io/dedicated_gpu }
{$if (not defined(CASTLE_NO_FORCE_DEDICATED_GPU)) and (defined(cpu386) or defined(cpux64) or defined(cpuamd64)) and (defined(MSWINDOWS) or defined(Linux))}
    {$ifdef fpc}
     {$asmmode intel}
    {$endif}

    procedure NvOptimusEnablement; {$ifdef fpc}assembler; nostackframe;{$endif}
    asm
    {$ifdef cpu64}
    {$ifndef fpc}
     .NOFRAME
    {$endif}
    {$endif}
     dd 1
    end;

    procedure AmdPowerXpressRequestHighPerformance; {$ifdef fpc}assembler; nostackframe;{$endif}
    asm
    {$ifdef cpu64}
    {$ifndef fpc}
     .NOFRAME
    {$endif}
    {$endif}
     dd 1
    end;

    exports
      NvOptimusEnablement,
      AmdPowerXpressRequestHighPerformance;
{$ifend}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
