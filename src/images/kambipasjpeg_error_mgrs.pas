{ @exclude (Not to be used from outside, this is internal for KambiPasJpeg.) }
unit KambiPasJpeg_error_mgrs;

{$I kambiconf.inc}

interface

uses
  {jpeg units} jerror, jpeglib,
  SysUtils;

type
  { }
  EJPEG = class(Exception);

  pascal_error_mgr = record
    pub: jpeg_error_mgr;
  end;
  pascal_error_ptr = ^pascal_error_mgr;

function jpeg_pascal_error (var err : pascal_error_mgr) : jpeg_error_mgr_ptr;

implementation

{ Kambi+, so that we can compile our format_message }
uses jmorecfg, jdeferr;

{ ------------------------------------------------------------------------
  Kambi JPEG error handler. Based on Nomssi JPEG error handler.
  Nomssi did his handler because :
   "NOTE: we have replaced jpeg_std_error because it stores a static
          message table (JDEFERR.PAS) in the jpeg_message_table field."

  ale ja nie widze z tym problemu skoro ta statyczna tablica jest
  przez nas traktowana jako stala. Tym niemniej rzeczywiscie trzeba zrobic
  wlasnego error handlera zeby rzucac exception w error_exit.

  Ponadto Nomsi chcial miec wlasnego handlera zeby w output_message
  pod Windowsem robic MessageBox w aplikacjach GUI (stderr moze wowczas nie
  istniec) - tak robil Nomssi ale ja tego nie robie, nie chce miec zadnego
  output message.

 ------------------------------------------------------------------------ }

procedure error_exit (cinfo : j_common_ptr);
var
  buffer : string;
begin
 cinfo^.err^.format_message(cinfo, buffer);
 raise EJPEG.Create(buffer);
end;

procedure output_message (cinfo : j_common_ptr);
var
 buffer : string;
begin
 cinfo^.err^.format_message (cinfo, buffer);

 { to nie jest dobre takie wypisywanie czegos na stdout/err w dowolnej chwili
   lub przerywanie programu zeby wyswietlic MessageBoxa. Pomijam juz nawet
   fakt ze pod Windowsem obydwie metody moga zawiesc (aplikacje GUI
   nie maja stdout a co do MessageBoxa to cos nienajlepiej sie wyswietla
   gdy mamy otwarte okno OpenGL'a fullscreen).
   Tym samym wiec rzeczy ponizej sa zakomentarzowane - nigdy nie wyswietlaj
   warningow czy czegos takiego. Moduly jpeg'a moga tylko zaladowac obrazek
   lub rzucic wyjatek. }

{ TODO: jak tylko bedzie potrzeba i okazja, zrobi sie jednak jakis mechanizm
  wypuszczania tych warningow na zewnatrz, np. zmienna OnJpegWarning }

(*
 { show message using WinAPI or Writeln }
 {$ifdef MSWINDOWS} if not IsConsole then MessageBox(0,PChar(buffer),'jpeg message',MB_OK) else
 {$else}            Writeln(ErrOutput, buffer);
 {$endif}
*)
end;

{ Kambi*: copied from jerror.pas to workaround a bug in pasjpeg (original
  PasJPEG from Nomssi pages and in FPC pasjpeg package) that defined
  NO_FORMAT symbol for FPC. This makes incorrect (unformatted, with
  things like '%d' left !) error messages.

  ALL below is exactly copied from jerror.format_message (it's just that
  NO_FORMAT is undefined here). }
procedure format_message (cinfo : j_common_ptr; var buffer : string);
var
  err : jpeg_error_mgr_ptr;
  msg_code : J_MESSAGE_CODE;
  msgtext : string;
  isstring : boolean;
begin
  err := cinfo^.err;
  msg_code := J_MESSAGE_CODE(err^.msg_code);
  msgtext := '';

  { Look up message string in proper table }
  if (msg_code > JMSG_NOMESSAGE)
    and (msg_code <= J_MESSAGE_CODE(err^.last_jpeg_message)) then
  begin
    msgtext := err^.jpeg_message_table^[msg_code];
  end
  else
  if (err^.addon_message_table <> NIL) and
     (msg_code >= err^.first_addon_message) and
     (msg_code <= err^.last_addon_message) then
  begin
    msgtext := err^.addon_message_table^[J_MESSAGE_CODE
           (ord(msg_code) - ord(err^.first_addon_message))];
  end;

  { Defend against bogus message number }
  if (msgtext = '') then
  begin
    err^.msg_parm.i[0] := int(msg_code);
    msgtext := err^.jpeg_message_table^[JMSG_NOMESSAGE];
  end;

  { Check for string parameter, as indicated by %s in the message text }
  isstring := Pos('%s', msgtext) > 0;

  { Format the message into the passed buffer }
  if (isstring) then
    buffer := Concat(msgtext, err^.msg_parm.s)
  else
  begin
 {$IFDEF VER70}
    FormatStr(buffer, msgtext, err^.msg_parm.i);
 {$ELSE}
   {$IFDEF NO_FORMAT}
   buffer := msgtext;
   {$ELSE}
   buffer := Format(msgtext, [
        err^.msg_parm.i[0], err^.msg_parm.i[1],
        err^.msg_parm.i[2], err^.msg_parm.i[3],
        err^.msg_parm.i[4], err^.msg_parm.i[5],
        err^.msg_parm.i[6], err^.msg_parm.i[7] ]);
   {$ENDIF}
 {$ENDIF}
  end;
end;

function jpeg_pascal_error (var err : pascal_error_mgr) : jpeg_error_mgr_ptr;
begin
 jpeg_std_error(err.pub);
 err.pub.error_exit := {$ifdef FPC_OBJFPC} @ {$endif} error_exit;
 err.pub.output_message := {$ifdef FPC_OBJFPC} @ {$endif} output_message;
 { Kambi+, register ours format_message }
 err.pub.format_message := {$ifdef FPC_OBJFPC} @ {$endif} format_message;
 result:=@err;
end;

end.
