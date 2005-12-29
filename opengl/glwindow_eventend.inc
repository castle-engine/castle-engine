{$ifdef GLWINDOW_LOGFILE}
 LogWrite('TGLWindow event '+eventname+' end');
except
 on E:Exception do
 begin
  LogWrite('TGLWindow event '+eventname+' raised exception '+E.ClassName+
   ' with message "'+E.Message+'"');
  raise;
 end;
end;
{$endif}
