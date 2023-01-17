{$IFDEF FPC}
  {$DEFINE VER150}   // Lazarus --> at least Delphi 7 }
{$ENDIF}

    {$ifdef VER180}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER170}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
    {$endif}

    {$ifdef VER160}        
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
    {$endif}

    {$ifdef VER150}        
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
    {$endif}

    {$ifdef VER140}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
    {$endif}

    {$ifdef VER130}
     {$define DELPHI_5_UP}
    {$endif}

    {.$DEFINE mbXP_Lib}
