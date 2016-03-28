{ Use this around code where range and overflow checks should be disabled
  (regardless of debug/release mode). Always close this by
  including norqcheckend.inc afterwards. }
{$push}
{$R-}
{$Q-}
