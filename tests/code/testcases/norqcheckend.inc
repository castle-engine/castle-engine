{$ifndef WITHIN_RANGE_CHECKS_DISABLED}
  {$error Included norqcheckend.inc without including norqcheckbegin.inc earlier.}
{$endif}

{$ifdef WAS_DEFINED_R} {$R+} {$endif}
{$ifdef WAS_DEFINED_Q} {$Q+} {$endif}

// cleanup symbols
{$undef WITHIN_RANGE_CHECKS_DISABLED}
{$undef WAS_DEFINED_R}
{$undef WAS_DEFINED_Q}
