{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PCX image file format header.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit pcxcomn;

{$mode objfpc}{$H+}

interface

type

  TRGB = packed record
    Red, Green, Blue: byte;
  end;

  TPCXHeader = record
    FileID:   byte;                      // signature $0A fichiers PCX, $CD fichiers SCR
    Version:  byte;                     // 0: version 2.5
    // 2: 2.8 avec palette
    // 3: 2.8 sans palette
    // 5: version 3
    Encoding: byte;                    // 0: non compresser
    // 1: encodage RLE
    BitsPerPixel: byte;                // nombre de bits par pixel de l'image: 1, 4, 8, 24
    XMin,                              // abscisse de l'angle supérieur gauche
    YMin,                              // ordonnée de l'angle supérieur gauche
    XMax,                              // abscisse de l'angle inférieur droit
    YMax,                              // ordonnée de l'angle inférieur droit
    HRes,                              // résolution horizontale en dpi
    VRes:     word;                        // résolution verticale en dpi
    ColorMap: array[0..15] of TRGB;    // Palette
    Reserved,                          // Réservé
    ColorPlanes: byte;                 // Nombre de plans de couleur
    BytesPerLine,                      // Nombre de bits par ligne
    PaletteType: word;                 // Type de palette
    //      1: couleur ou N/B
    //      2: dégradé de gris
    Fill:     array[0..57] of byte;        // Remplissage
  end;

implementation

end.
