Unit Jerror;

{ This file contains simple error-reporting and trace-message routines.
  These are suitable for Unix-like systems and others where writing to
  stderr is the right thing to do.  Many applications will want to replace
  some or all of these routines.

  These routines are used by both the compression and decompression code. }

{ Source: jerror.c;  Copyright (C) 1991-1996, Thomas G. Lane. }
{ note: format_message still contains a hack }
{$i jconfig.inc}
interface

uses
  jmorecfg,
  jdeferr,
  jpeglib;
{
  jversion;
}

const
  EXIT_FAILURE  = 1;   { define halt() codes if not provided }

{GLOBAL}
function jpeg_std_error (var err : jpeg_error_mgr) : jpeg_error_mgr_ptr;



procedure ERREXIT(cinfo : j_common_ptr; code : J_MESSAGE_CODE);

procedure ERREXIT1(cinfo : j_common_ptr; code : J_MESSAGE_CODE; p1 : uInt);

procedure ERREXIT2(cinfo : j_common_ptr; code : J_MESSAGE_CODE; p1 : int; p2 : int);

procedure ERREXIT3(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);

procedure ERREXIT4(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);

procedure ERREXITS(cinfo : j_common_ptr;code : J_MESSAGE_CODE;
                   str : string);
{ Nonfatal errors (we can keep going, but the data is probably corrupt) }

procedure WARNMS(cinfo : j_common_ptr; code : J_MESSAGE_CODE);

procedure WARNMS1(cinfo : j_common_ptr;code : J_MESSAGE_CODE; p1 : int);

procedure WARNMS2(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int);

{ Informational/debugging messages }
procedure TRACEMS(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE);

procedure TRACEMS1(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; p1 : long);

procedure TRACEMS2(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int;
                   p2 : int);

procedure TRACEMS3(cinfo : j_common_ptr;
                   lvl : int;
                   code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);

procedure TRACEMS4(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);

procedure TRACEMS5(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int; p5 : int);

procedure TRACEMS8(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int; p3 : int; p4 : int;
                  p5 : int; p6 : int; p7 : int; p8 : int);

procedure TRACEMSS(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; str : string);

implementation


{ How to format a message string, in format_message() ? }

{$IFDEF OS2}
  {$DEFINE NO_FORMAT}
{$ENDIF}
{$IFDEF FPC}
  {.$DEFINE NO_FORMAT}
{$ENDIF}

uses
{$IFNDEF NO_FORMAT}
  {$IFDEF VER70}
    drivers, { Turbo Vision unit with FormatStr }
  {$ELSE}
    sysutils,  { Delphi Unit with Format() }
  {$ENDIF}
{$ENDIF}
  jcomapi;

{ Error exit handler: must not return to caller.

  Applications may override this if they want to get control back after
  an error.  Typically one would longjmp somewhere instead of exiting.
  The setjmp buffer can be made a private field within an expanded error
  handler object.  Note that the info needed to generate an error message
  is stored in the error object, so you can generate the message now or
  later, at your convenience.
  You should make sure that the JPEG object is cleaned up (with jpeg_abort
  or jpeg_destroy) at some point. }


{METHODDEF}
procedure error_exit (cinfo : j_common_ptr); far;
begin
  { Always display the message }
  cinfo^.err^.output_message(cinfo);

  { Let the memory manager delete any temp files before we die }
  jpeg_destroy(cinfo);

  halt(EXIT_FAILURE);
end;


{ Actual output of an error or trace message.
  Applications may override this method to send JPEG messages somewhere
  other than stderr. }

{ Macros to simplify using the error and trace message stuff }
{ The first parameter is either type of cinfo pointer }

{ Fatal errors (print message and exit) }
procedure ERREXIT(cinfo : j_common_ptr; code : J_MESSAGE_CODE);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.error_exit(cinfo);
end;

procedure ERREXIT1(cinfo : j_common_ptr; code : J_MESSAGE_CODE; p1 : uInt);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXIT2(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXIT3(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.msg_parm.i[2] := p3;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXIT4(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.msg_parm.i[2] := p3;
  cinfo^.err^.msg_parm.i[3] := p4;
  cinfo^.err^.error_exit (cinfo);
end;

procedure ERREXITS(cinfo : j_common_ptr;code : J_MESSAGE_CODE;
                   str : string);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.s := str;  { string[JMSG_STR_PARM_MAX] }
  cinfo^.err^.error_exit (cinfo);
end;

{ Nonfatal errors (we can keep going, but the data is probably corrupt) }

procedure WARNMS(cinfo : j_common_ptr; code : J_MESSAGE_CODE);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message(cinfo, -1);
end;

procedure WARNMS1(cinfo : j_common_ptr;code : J_MESSAGE_CODE; p1 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.emit_message (cinfo, -1);
end;

procedure WARNMS2(cinfo : j_common_ptr; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.emit_message (cinfo, -1);
end;

{ Informational/debugging messages }
procedure TRACEMS(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message(cinfo, lvl);
end;

procedure TRACEMS1(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; p1 : long);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS2(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int;
                   p2 : int);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.i[0] := p1;
  cinfo^.err^.msg_parm.i[1] := p2;
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS3(cinfo : j_common_ptr;
                   lvl : int;
                   code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int);
var
  _mp : int8array;
begin
  _mp[0] := p1; _mp[1] := p2; _mp[2] := p3;
  cinfo^.err^.msg_parm.i := _mp;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;


procedure TRACEMS4(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int);
var
  _mp : int8array;
begin
  _mp[0] := p1; _mp[1] := p2; _mp[2] := p3; _mp[3] := p4;
  cinfo^.err^.msg_parm.i := _mp;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS5(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                   p1 : int; p2 : int; p3 : int; p4 : int; p5 : int);
var
  _mp : ^int8array;
begin
  _mp := @cinfo^.err^.msg_parm.i;
  _mp^[0] := p1; _mp^[1] := p2; _mp^[2] := p3;
  _mp^[3] := p4; _mp^[5] := p5;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMS8(cinfo : j_common_ptr; lvl : int; code : J_MESSAGE_CODE;
                  p1 : int; p2 : int; p3 : int; p4 : int;
                  p5 : int; p6 : int; p7 : int; p8 : int);
var
  _mp : int8array;
begin
  _mp[0] := p1; _mp[1] := p2; _mp[2] := p3; _mp[3] := p4;
  _mp[4] := p5; _mp[5] := p6; _mp[6] := p7; _mp[7] := p8;
  cinfo^.err^.msg_parm.i := _mp;
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.emit_message (cinfo, lvl);
end;

procedure TRACEMSS(cinfo : j_common_ptr; lvl : int;
                   code : J_MESSAGE_CODE; str : string);
begin
  cinfo^.err^.msg_code := ord(code);
  cinfo^.err^.msg_parm.s := str; { string JMSG_STR_PARM_MAX }
  cinfo^.err^.emit_message (cinfo, lvl);
end;

{METHODDEF}
procedure output_message (cinfo : j_common_ptr); far;
var
  buffer : string; {[JMSG_LENGTH_MAX];}
begin
  { Create the message }
  cinfo^.err^.format_message (cinfo, buffer);

  { Send it to stderr, adding a newline }
  WriteLn(output, buffer);
end;



{ Decide whether to emit a trace or warning message.
  msg_level is one of:
    -1: recoverable corrupt-data warning, may want to abort.
     0: important advisory messages (always display to user).
     1: first level of tracing detail.
     2,3,...: successively more detailed tracing messages.
  An application might override this method if it wanted to abort on warnings
  or change the policy about which messages to display. }


{METHODDEF}
procedure emit_message (cinfo : j_common_ptr; msg_level : int); far;
var
  err : jpeg_error_mgr_ptr;
begin
  err := cinfo^.err;
  if (msg_level < 0) then
  begin
    { It's a warning message.  Since corrupt files may generate many warnings,
      the policy implemented here is to show only the first warning,
      unless trace_level >= 3. }

    if (err^.num_warnings = 0) or (err^.trace_level >= 3) then
      err^.output_message(cinfo);
    { Always count warnings in num_warnings. }
    Inc( err^.num_warnings );
  end
  else
  begin
    { It's a trace message.  Show it if trace_level >= msg_level. }
    if (err^.trace_level >= msg_level) then
      err^.output_message (cinfo);
  end;
end;


{ Format a message string for the most recent JPEG error or message.
  The message is stored into buffer, which should be at least JMSG_LENGTH_MAX
  characters.  Note that no '\n' character is added to the string.
  Few applications should need to override this method. }


{METHODDEF}
procedure format_message (cinfo : j_common_ptr; var buffer : string); far;
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



{ Reset error state variables at start of a new image.
  This is called during compression startup to reset trace/error
  processing to default state, without losing any application-specific
  method pointers.  An application might possibly want to override
  this method if it has additional error processing state. }


{METHODDEF}
procedure reset_error_mgr (cinfo : j_common_ptr); far;
begin
  cinfo^.err^.num_warnings := 0;
  { trace_level is not reset since it is an application-supplied parameter }
  cinfo^.err^.msg_code := 0;      { may be useful as a flag for "no error" }
end;


{ Fill in the standard error-handling methods in a jpeg_error_mgr object.
  Typical call is:
        cinfo : jpeg_compress_struct;
        err : jpeg_error_mgr;

        cinfo.err := jpeg_std_error(@err);
  after which the application may override some of the methods. }


{GLOBAL}
function jpeg_std_error (var err : jpeg_error_mgr) : jpeg_error_mgr_ptr;
begin
  err.error_exit := error_exit;
  err.emit_message := emit_message;
  err.output_message := output_message;
  err.format_message := format_message;
  err.reset_error_mgr := reset_error_mgr;

  err.trace_level := 0;         { default := no tracing }
  err.num_warnings := 0;        { no warnings emitted yet }
  err.msg_code := 0;            { may be useful as a flag for "no error" }

  { Initialize message table pointers }
  err.jpeg_message_table := @jpeg_std_message_table;
  err.last_jpeg_message := pred(JMSG_LASTMSGCODE);

  err.addon_message_table := NIL;
  err.first_addon_message := JMSG_NOMESSAGE;  { for safety }
  err.last_addon_message := JMSG_NOMESSAGE;

  jpeg_std_error := @err;
end;


end.
