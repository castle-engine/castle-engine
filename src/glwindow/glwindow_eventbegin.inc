{$ifdef GLWINDOW_EVENTS_LOG}

if Log then
  WritelnLog('TCastleWindowBase event', eventname+' begin'
    {$ifdef BONUS_LOG_STRING} + ' ' + BONUS_LOG_STRING {$endif});

try
{$endif}
