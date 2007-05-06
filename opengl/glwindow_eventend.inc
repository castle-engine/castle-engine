{$ifdef GLWINDOW_LOGFILE}
 LogWrite('TGLWindow event '+eventname+' end'
   {$ifdef BONUS_LOG_STRING} + ' ' + BONUS_LOG_STRING {$endif});
except
 on E:Exception do
 begin
  LogWrite('TGLWindow event '+eventname+' raised exception '+E.ClassName+
   ' with message "'+E.Message+'"');
  raise;
 end;
end;
{$endif}
