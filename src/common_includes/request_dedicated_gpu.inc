{ Include this in your program file to force using a dedicated GPU
  for OpenGL with Delphi and FreePascal/Lazarus
  on Multi-GPU systems such as Notebooks on Windows.

  Based on BeRo:
  https://gist.github.com/BeRo1985/939241b471f776ab844657d28d59368c

  NVidia resources:
  - https://docs.nvidia.com/gameworks/content/technologies/desktop/optimus.htm
  - https://developer.download.nvidia.com/devzone/devcenter/gamegraphics/files/OptimusRenderingPolicies.pdf

  Platforms:

  - It is confirmed to work on Windows, with both FPC and Delphi applications.

  - Linux: Unsure.

    TODO: It compiles on Linux OK, but does it have any effect on e.g.
    https://wiki.archlinux.org/title/NVIDIA_Optimus on Linux?
    (Michalis) On all my Linux laptop systems with NVidia installed,
    I have a somewhat non-standard Optimus setup,
    and it seems that this flag is ignored (but maybe it's the fault of my setup).
    Reports are welcome.

  - macOS, FreeBSD: It didn't compile, so we disabled it
    (that is, including this file does nothing on these systems).

    macOS: FPC 3.2.2 fails at "exports"

    .../common_includes/request_dedicated_gpu.inc(42,1) Warning: (3186) Use of unsupported feature!
    .../common_includes/request_dedicated_gpu.inc(42,1) Fatal: (2003) Syntax error, "BEGIN" expected but "EXPORTS" found

    Similar for FreeBSD ( https://discord.com/channels/389676745957310465/389676745957310467/1057727043506286665 ).

  TODO:
  How to nicely allow CGE applications to utilize this?
  For now, our simple recommendation is: copy-paste this into your program.
}

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
