{ Pascal defines that configure Kraft compilation. }

{ -----------------------------------------------------------------------
  Hide Kraft compilation messages we don't plan to fix }

{$ifdef FPC}
  { CGE: Avoid FPC note: "nested procedures" not yet supported inside inline procedure/function
    TODO: submit to Kraft. }
  {$notes off}
{$else}
  {$hints off}
  {$warn SYMBOL_PLATFORM off}
{$endif}

{ Tweak caninline ----------------------------------------------------------- }

{ Workaround FPC 3.3.1 errors:

  - Reproduced with revision 47824 compilation error, on Darwin (not recorded CPU).
  - Reproduced also with FPC 3.3.1 revision 48998 with Android/Arm and Android/Aarrch64.

  The errors mention invalid assembler syntax.
  E.g. Android/Aarrch64 output:

    kraft.s: Assembler messages:
    kraft.s:86936: Error: operand mismatch -- `fadd d0,s0,s1'
    kraft.s:86936: Info:    did you mean this?
    kraft.s:86936: Info:    	fadd s0,s0,s1
    kraft.s:86936: Info:    other valid variant(s):
    kraft.s:86936: Info:    	fadd d0,d0,d1
    kraft.s:86938: Error: operand mismatch -- `fadd d0,d0,s1'
    kraft.s:86938: Info:    did you mean this?
    kraft.s:86938: Info:    	fadd d0,d0,d1
    kraft.s:86938: Info:    other valid variant(s):
    kraft.s:86938: Info:    	fadd s0,s0,s1
    kraft.pas(33101) Error: Error while assembling exitcode 1
}
{$if defined(FPC) and defined(VER3_3) and (defined(DARWIN) or defined(CPUARM) or defined(CPUAARCH64))}
  {$undef caninline}
{$ifend}

{ Tweak NonSIMD ----------------------------------------------------------- }

{ CGE: Define NonSIMD. Without this symbol, Kraft uses some i386-only assembler,
  that causes crashes (access violation at TRigidBody.SynchronizeFromKraft
  when doing "FLinearVelocity := VectorFromKraft(FKraftBody.LinearVelocity)").
  Testcase:

    castle-engine --os=win32 --cpu=i386 compile --mode=debug
    wine ./*.exe

  on all physics examples it seems,

    examples/physics/physics_2d_game_sopwith
    examples/physics/physics_3d_game
    examples/platformer

  With at least FPC 3.2.0 (but did not check other FPC versions).
  As this is an i386-specific optimization only (and our focus is on 64-bit platforms
  as these are, and will be, majority) so disabling it is not a problem in practice
  anyway. }
{$define NonSIMD}

{ C++ Builder specifics ----------------------------------------------------------- }

{ CGE: Make C++ Builder generated header of castletransform.hpp useful.

  Otherwise, since Kraft is used in CastleTransform interface in Pascal,
  and kraft.hpp is used in castletransform.hpp in C++,
  and in effect we get "ambiguous symbol" warnings when trying to use Vector3
  in CGE examples.

  When using CGE, Kraft should not be accessed directly.
  We use Kraft in CastleTransform interface to make it easy to define
  CGE classes based on Kraft, though all their Kraft usage is private/internal. }
{$ifndef FPC}
  {$NODEFINE Vector2}
  {$NODEFINE Vector3}
  {$NODEFINE Quaternion}
{$endif}
