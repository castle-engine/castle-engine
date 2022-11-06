{%MainUnit ../castlewindow.pas}

{ WinAPI SendInput and related structures definition,
  missing from stable FPC,
  copied from FPC bugreport https://bugs.freepascal.org/view.php?id=35064
}

const
  INPUT_MOUSE = 0;
  INPUT_KEYBOARD = 1;
  INPUT_HARDWARE = 2;

{-------------------------------------}
{ mouse input structure definition }

type
  MOUSEINPUT = record
    dx : long;
    dy : long;
    MouseData : DWORD;
    Flags : DWORD;
    Time : DWORD;
    ExtraInfo : pointer;
  end;
  PMOUSEINPUT = ^MOUSEINPUT;

{-------------------------------------}
{ keyboard input structure definition }

type
  KEYBDINPUT = record
    Vk : word;
    Scan : word;
    Flags : DWORD;
    Time : DWORD;
    ExtraInfo : pointer;
  end;
  PKEYBDINPUT = ^KEYBDINPUT;

{-------------------------------------}
{ hardware input structure definition }

type
  HARDWAREINPUT = record
    Msg : DWORD;
    wParamLo : word;
    wParamHi : word;
  end;
  PHARDWAREINPUT = ^HARDWAREINPUT;

{-------------------------------------}
{ input structure }

type
  TINPUT = record
    InputType : DWORD;

    case integer of
      1 : (mi : MOUSEINPUT);
      2 : (ki : KEYBDINPUT);
      3 : (hi : HARDWAREINPUT);
  end;
  PINPUT = ^TINPUT;

{------------------------------------------------------------------------------}
{ function definition }

function SendInput(InputsCount : UINT; InputsPtr : PINPUT; Size : integer): UINT; stdcall; external user32;
