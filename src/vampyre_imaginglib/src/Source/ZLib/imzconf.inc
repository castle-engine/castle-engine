{ -------------------------------------------------------------------- }

{$DEFINE MAX_MATCH_IS_258}

{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{$UNDEF MAXSEG_64K}
{$DEFINE UNALIGNED_OK}    { requires SizeOf(ush) = 2 ! }
{$UNDEF DYNAMIC_CRC_TABLE}
{$UNDEF FASTEST}
{$DEFINE Use32}
{$DEFINE patch112}        { apply patch from the zlib home page }

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$UNDEF DEBUG} // for Delphi 2007 in DEBUG mode

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{ -------------------------------------------------------------------- }

{ Castle Game Engine configuration }
{$ifndef FPC}
  { Hide some harmless hints/warnings, to have clean compiler output. }
  {$hints off}
  {$warn IMPLICIT_STRING_CAST off}
  {$warn IMPLICIT_STRING_CAST_LOSS off}
  {$warn COMBINING_SIGNED_UNSIGNED off}
  {$if CompilerVersion >= 32} {$warn COMBINING_SIGNED_UNSIGNED64 off} {$endif} // only since Delphi 10.2
{$endif}
