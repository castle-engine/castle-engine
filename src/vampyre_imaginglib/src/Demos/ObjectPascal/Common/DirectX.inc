{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is DirectX.inc.                                            }
{                                                                              }
{******************************************************************************}
{$IFNDEF __TMT__}
{$IFNDEF FPC}

  // *** Borland compilers support ***
  {$INCLUDE Jedi.inc}

  {$DEFINE BORLAND}
  {$DEFINE TYPE_IDENTITY}
  {$DEFINE SUPPORTS_EXCEPTIONS}
  {$IFDEF COMPILER6_UP}
    {$DEFINE SUPPORTS_EXPL_ENUMS} // Enumerated types with explicitly assigned ordinality
    {$IFNDEF BCB6_UP}
      // C++Builder6 hack: Delphi 6-7 compilers have bugged .HPP generation for
      // enums in some cases (and D6 compiler is included with BCB6)
      {$DEFINE SUPPORTS_EXPL_ENUMS_except_BCB6}
    {$ENDIF}
  {$ENDIF}

  // Additional settings
  {$BOOLEVAL OFF}
  {$MINENUMSIZE 4}
  {$ALIGN ON}

  {$IFDEF COMPILER7_UP}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}

  {$IFDEF COMPILER9_UP}
    {$IFDEF DEBUG}
      {$INLINE OFF}
    {$ELSE}
      {$INLINE ON}
    {$ENDIF}
  {$ENDIF}
{$ELSE}

  // *** FreePascal compiler support ***
  {$INCLUDE Jedi.inc}

  {$APPTYPE GUI}

  // Additional settings
  {$H+} // Long Strings
  {$BOOLEVAL OFF}
  {$MINENUMSIZE 4}
  {$ALIGN ON}
  {$PACKRECORDS 8}
  {$INLINE ON}

  {$UNDEF TYPE_IDENTITY}
  {$DEFINE SUPPORTS_EXCEPTIONS}
  {$DEFINE SUPPORTS_INLINE}
  {$DEFINE COMPILER5_UP} // Specially for DirectDraw.pas

{$ENDIF}
{$ELSE}
  // *** TMT Pascal compiler support ***
  {.$I TMT.inc}

  // TMT compiler support
  {$IFNDEF __TMT__}
    TMT pascal compiler required here
  {$ENDIF}

  // ADD IFOPT for TMT

  {$DEFINE TMT}

  {$IFDEF __WIN32__}
    {$DEFINE WIN32}
  {$ENDIF}

  {$IFDEF __VER5__}
    {$DEFINE TMT5}
    {$DEFINE TMT5_UP}
    {$DEFINE TMT4_UP}
    {$DEFINE TMT3_UP}
  {$ENDIF}

  {$IFDEF __VER4__}
    {$DEFINE TMT4}
    {$DEFINE TMT4_UP}
    {$DEFINE TMT3_UP}
  {$ENDIF}

  {$IFDEF __VER3__}
    {$DEFINE TMT3}
    {$DEFINE TMT3_UP}
  {$ENDIF}

  {$IFDEF TMT4_UP}
    {$DEFINE SUPPORTS_INTERFACE}
  {$ENDIF}

  // Additional settings
  {$A+}  // Word alignment data
  {$OA+} // Objects and structures align
  {$Z4}  // Set minimum size of enumerated type to 4
{$ENDIF}


// By default use most recent DirectX sub-version

{$IFNDEF DX81}
  {$IFNDEF DX80}
    {$DEFINE DX81}
  {$ENDIF}
{$ENDIF}


{$IFNDEF DX92}
  {$IFNDEF DX91}
    {$IFNDEF DX90}
      {$DEFINE DX92}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

