{ -------------------------------------------------------------------- }

{$DEFINE MAX_MATCH_IS_258}

{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{- $DEFINE MAXSEG_64K}
{$IFDEF VER70}
  {$DEFINE TP}
  {$DEFINE MAXSEG_64K}
{$ENDIF}
{$IFNDEF WIN32}
  {$DEFINE UNALIGNED_OK}  { requires SizeOf(ush) = 2 ! }
{$ENDIF}

{$UNDEF DYNAMIC_CRC_TABLE}
{$UNDEF FASTEST}
{$define patch112}        { apply patch from the zlib home page }
{ -------------------------------------------------------------------- }
{$IFDEF WIN32}
  {$DEFINE Delphi32}
  {- $DEFINE Delphi5}  { keep compiler quiet }
{$ENDIF}

{$IFDEF DPMI}
  {$DEFINE MSDOS}
{$ENDIF}

{$IFDEF FPC}
 {$DEFINE Use32}
 {$DEFINE pointer_arith}
 {$UNDEF DPMI}
 {$UNDEF MSDOS}
 {$UNDEF UNALIGNED_OK}  { requires SizeOf(ush) = 2 ! }
 {$UNDEF MAXSEG_64K}
 {$UNDEF Delphi32}
{$ENDIF}

{- $DEFINE ZLIB_DEBUG}

{$IFDEF ZLIB_DEBUG}
  {$ASSERTIONS ON}
{$ENDIF}
