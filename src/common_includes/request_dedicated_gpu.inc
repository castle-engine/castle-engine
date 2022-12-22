{ Based on BeRo,
  https://gist.github.com/BeRo1985/939241b471f776ab844657d28d59368c

  Force usage of dedicated GPU for OpenGL with Delphi and FreePascal/Lazarus
  on Multi-GPU systems such as Notebooks on Windows.
  Insert that into your main source file, which is for example
  the .dpr (Delphi) or .lpr (Lazarus) file.

  TODO:
  - Is it possible on non-Windows?
  - How to nicely allow CGE applications to utilize this?
    For now, our simple recommendation is: copy-paste this into your program.
}

// uncomment and recomment this line, for compile-time-switching between integrated GPU and dedicated GPU
{$define ForceDedicatedGPUUsage}

{$if defined(ForceDedicatedGPUUsage) and (defined(cpu386) or defined(cpux64) or defined(cpuamd64))}
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
