{ Add EXT_packed_depth_stencil, missing from FPC <= 2.4.2,
  submitted as http://bugs.freepascal.org/view.php?id=18054 }

{$ifdef VER2_2}   {$define ADD_EXT_packed_depth_stencil} {$endif}
{$ifdef VER2_4_0} {$define ADD_EXT_packed_depth_stencil} {$endif}
{$ifdef VER2_4_2} {$define ADD_EXT_packed_depth_stencil} {$endif}

{$ifdef ADD_EXT_packed_depth_stencil}

{$ifdef read_interface}

//***** GL_EXT_packed_depth_stencil *****//
{ }
const
  GL_DEPTH_STENCIL_EXT = $84F9;
  GL_UNSIGNED_INT_24_8_EXT = $84FA;
  GL_DEPTH24_STENCIL8_EXT = $88F0;
  GL_TEXTURE_STENCIL_SIZE_EXT = $88F1;

function Load_GL_EXT_packed_depth_stencil: Boolean;

{$endif read_interface}

{$ifdef read_implementation}
function Load_GL_EXT_packed_depth_stencil: Boolean;
var
  extstring: String;
begin

  Result := FALSE;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_packed_depth_stencil', extstring) then
  begin
    Result := TRUE;
  end;

end;

{$endif read_implementation}

{$endif ADD_EXT_packed_depth_stencil}
