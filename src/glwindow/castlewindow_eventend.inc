{$ifdef CASTLE_WINDOW_EVENTS_LOG}

if Log then
  WritelnLog('TCastleWindowBase event', eventname+' end'
    {$ifdef BONUS_LOG_STRING} + ' ' + BONUS_LOG_STRING {$endif});
except
  on E:Exception do
  begin
    if Log then
      WritelnLog('TCastleWindowBase event', eventname+' raised exception '+E.ClassName+
       ' with message "'+E.Message+'"');
    raise;
  end;
end;
{$endif}
