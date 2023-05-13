{ Use this around code where range and overflow checks should be disabled
  (regardless of debug/release mode). Always close this by
  including norqcheckend.inc afterwards. }

{$ifopt R+} {$define WAS_DEFINED_R} {$endif}
{$ifopt Q+} {$define WAS_DEFINED_Q} {$endif}
{$define WITHIN_RANGE_CHECKS_DISABLED}

{$R-}
{$Q-}
