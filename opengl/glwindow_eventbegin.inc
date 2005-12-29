{$ifdef GLWINDOW_LOGFILE}

{$ifdef BONUS_LOG_STRING}
 LogWrite('TGLWindow event '+eventname+' begin '+BONUS_LOG_STRING);
{$else}
 LogWrite('TGLWindow event '+eventname+' begin');
{$endif}

try
{$endif}
