program CastleFmxPlayAnimation;

uses
  System.StartUpCopy,
  FMX.Forms, Fmx.CastleControl,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

{ Forces using a dedicated (faster) GPU on laptops with multiple GPUs.
  See https://castle-engine.io/dedicated_gpu }
{$if (not defined(CASTLE_NO_FORCE_DEDICATED_GPU)) and
     (defined(CPU386) or defined(CPUX64) or defined(CPUAMD64)) and
     (defined(MSWINDOWS) or (defined(LINUX) and defined(FPC)))
}
    {$ifdef FPC}
      {$asmmode intel}
    {$endif}

    procedure NvOptimusEnablement; {$ifdef FPC}assembler; nostackframe;{$endif}
    asm
      {$ifdef CPU64}
      {$ifndef FPC}
      .NOFRAME
      {$endif}
      {$endif}
      dd 1
    end;

    procedure AmdPowerXpressRequestHighPerformance; {$ifdef FPC}assembler; nostackframe;{$endif}
    asm
      {$ifdef CPU64}
      {$ifndef FPC}
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
  Application.CreateForm(TForm1, Form1);
  TCastleControl.ApplicationRun;
end.
