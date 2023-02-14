{ Include this in your program file to force using a dedicated GPU
  for OpenGL with Delphi and FreePascal/Lazarus
  on Multi-GPU systems such as Notebooks on Windows.

  See Castle Game Engine src/common_includes/request_dedicated_gpu.inc }

// uncomment and recomment this line, for compile-time-switching between integrated GPU and dedicated GPU
{$define ForceDedicatedGPUUsage}

{$if defined(ForceDedicatedGPUUsage) and (defined(cpu386) or defined(cpux64) or defined(cpuamd64)) and (defined(MSWINDOWS) or defined(Linux))}

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
