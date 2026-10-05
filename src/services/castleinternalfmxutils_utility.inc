{ Include platform-specific TFmxOpenGLUtility implementations. }
{$if defined(MSWINDOWS)}
  {$I castleinternalfmxutils_windows.inc}
{$elseif defined(LINUX)}
  {$I castleinternalfmxutils_linux.inc}
{$else}
  {$I castleinternalfmxutils_other_os.inc}
{$endif}
