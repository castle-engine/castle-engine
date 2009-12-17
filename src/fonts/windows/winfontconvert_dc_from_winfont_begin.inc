 dc:=GetDC(0);
 Check(dc<>0, 'GetDC(0) failed');
 try
  PreviousObject:=SelectObject(dc, WinFont);
  try
