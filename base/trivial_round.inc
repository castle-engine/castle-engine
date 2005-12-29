{ Trivial Round in pure Pascal, adapted from FPC rtl/inc/genmath.inc.

  Included from OpenGLBmpFonts.TextWidth:
    Without including this trivial_round we sometimes get
    Invalid Floating Point Operation raised:
      when we enter OpenGLBmpFonts.TextWidth from
      - szklane_lasy from glwinmessages.DrawMessg:DrawString from
          taMiddle: glRasterPos2f((md.MaxLineWidth - md.font.TextWidth(text))/2, 0);
        from "Koniec" lub "New game" menu item.
        or from "Can't init sound" initial message.
      - glWinEvents from TimeMessages.Draw2D

    Invalid Floating Point Operation is not raised immediately at the call
    to Round inside OpenGLBmpFonts.TextWidth but is raised a little later.

  Of course I don't know why. FPC error ? Win32 OpenGL raises floating point
  errors, I know. I'm masking them.
  Do they get reported when Round sets it's control word value ?
  TODO - to be resolved soon on some fpc- list !

  Included from glViewImage.DrawGL.

  OK, now it's included in KambiUtils_miscella.inc (in FPC + WIN32 condition)

  Hmm, probably it's fixed in FPC 1.9.6, seeing this entry in FPC 1.9.6 sources:

    Revision 1.19  2004/07/09 23:06:11  peter
    * add fclex for fpu exceptions to round/trunc
}

function Round(d: Extended): longint;
 var
  fr: Extended;
Begin
   fr := Frac(d);
   if fr > 0.5 then
      Round:=Trunc(d)+1
   else
   if fr < 0.5 then
      Round:=Trunc(d)
   else { fr = 0.5 }
      { check sign to decide ... }
      { as in Turbo Pascal...    }
      if d >= 0.0 then
        Round := Trunc(d)+1
      else
        Round := Trunc(d);
end;
