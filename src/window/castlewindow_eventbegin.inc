{$ifdef CASTLE_WINDOW_EVENTS_LOG}

if Log then
  WritelnLog('TCastleWindowBase event', eventname+' begin'
    { + ' ' + IntToStr(GetTickCount) }
    {$ifdef BONUS_LOG_STRING} + ' ' + BONUS_LOG_STRING {$endif});

try
{$endif}
