{ FPC 3.3.1 produces many false warnings. }
{$ifdef FPC}
  {$ifndef VER3_0}
    {$ifndef VER3_2}
      { 2 warnings below are common because of case analysis.
        We tried to adjust to them
        ( https://castle-engine.io/coding_conventions#case_analysis )
        but in some cases there's no good adjustment of code.
        Let's disable them for now. }
      {$warn 6060 off} // case statement does not handle all possible cases
      {$warn 6018 off} // unreachable code
      { This happens because unfortunately FPC 3.3.1 doesn't understand
        that writing e.g. "WriteBuffer(Tmp, SizeOf(Tmp));" means we use Tmp value. }
      {$warn 5027 off} // Local variable is assigned but never used
    {$endif}
  {$endif}
{$endif}
