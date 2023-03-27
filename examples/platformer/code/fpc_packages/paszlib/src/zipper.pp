{
    $Id: header,v 1.3 2013/05/26 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2014 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit Zipper;

Interface

Uses
  {$IFDEF UNIX}
   BaseUnix,
  {$ENDIF}
   SysUtils,Classes,zstream;


Const
  { Signatures }
  END_OF_CENTRAL_DIR_SIGNATURE               = $06054B50;
  ZIP64_END_OF_CENTRAL_DIR_SIGNATURE         = $06064B50;
  ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE = $07064B50;
  LOCAL_FILE_HEADER_SIGNATURE                = $04034B50;
  CENTRAL_FILE_HEADER_SIGNATURE              = $02014B50;
  ZIP64_HEADER_ID                            = $0001;
  // infozip unicode path
  INFOZIP_UNICODE_PATH_ID                    = $7075;
  EFS_LANGUAGE_ENCODING_FLAG                 = $800;

const
  OS_FAT  = 0; //MS-DOS and OS/2 (FAT/VFAT/FAT32)
  OS_UNIX = 3;
  OS_OS2  = 6; //OS/2 HPFS
  OS_NTFS = 10;
  OS_VFAT = 14;
  OS_OSX  = 19;

  UNIX_MASK = $F000;
  UNIX_FIFO = $1000;
  UNIX_CHAR = $2000;
  UNIX_DIR  = $4000;
  UNIX_BLK  = $6000;
  UNIX_FILE = $8000;
  UNIX_LINK = $A000;
  UNIX_SOCK = $C000;


  UNIX_RUSR = $0100;
  UNIX_WUSR = $0080;
  UNIX_XUSR = $0040;

  UNIX_RGRP = $0020;
  UNIX_WGRP = $0010;
  UNIX_XGRP = $0008;

  UNIX_ROTH = $0004;
  UNIX_WOTH = $0002;
  UNIX_XOTH = $0001;

  UNIX_DEFAULT = UNIX_RUSR or UNIX_WUSR or UNIX_XUSR or UNIX_RGRP or UNIX_ROTH;

Type
   Local_File_Header_Type = Packed Record //1 per zipped file
     Signature              :  LongInt; //4 bytes
     Extract_Version_Reqd   :  Word; //if zip64: >= 45
     Bit_Flag               :  Word; //"General purpose bit flag in PKZip appnote
     Compress_Method        :  Word;
     Last_Mod_Time          :  Word;
     Last_Mod_Date          :  Word;
     Crc32                  :  LongWord;
     Compressed_Size        :  LongWord;
     Uncompressed_Size      :  LongWord;
     Filename_Length        :  Word;
     Extra_Field_Length     :  Word; //refers to Extensible data field size
   end;

   Extensible_Data_Field_Header_Type = Packed Record
     // Beginning of extra field
     // after local file header
     // after central directory header
     Header_ID              :  Word;
     //e.g. $0001 (ZIP64_HEADER_ID) Zip64 extended information extra field
     //     $0009 OS/2: extended attributes
     //     $000a NTFS: (Win32 really)
     //     $000d UNIX: uid, gid etc
     Data_Size              :  Word; //size of following field data
     //... field data should follow...
   end;

   Zip64_Extended_Info_Field_Type = Packed Record //goes after Extensible_Data_Field_Header_Type
     // overrides Local and Central Directory data
     // stored in extra field
     Original_Size          :  QWord; //Uncompressed file
     Compressed_Size        :  QWord; //Compressed data
     Relative_Hdr_Offset    :  QWord; //Offset that leads to local header record
     Disk_Start_Number      :  LongWord; //on which disk this file starts
   end;

  { Define the Central Directory record types }

  Central_File_Header_Type = Packed Record
    Signature            :  LongInt; //4 bytes
    MadeBy_Version       :  Word; //if zip64: lower byte >= 45
    Extract_Version_Reqd :  Word; //if zip64: >=45
    Bit_Flag             :  Word; //General purpose bit flag in PKZip appnote
    Compress_Method      :  Word;
    Last_Mod_Time        :  Word;
    Last_Mod_Date        :  Word;
    Crc32                :  LongWord;
    Compressed_Size      :  LongWord;
    Uncompressed_Size    :  LongWord;
    Filename_Length      :  Word;
    Extra_Field_Length   :  Word;
    File_Comment_Length  :  Word;
    Starting_Disk_Num    :  Word;
    Internal_Attributes  :  Word;
    External_Attributes  :  LongWord;
    Local_Header_Offset  :  LongWord; // if zip64: 0xFFFFFFFF
  End;

  End_of_Central_Dir_Type =  Packed Record //End of central directory record
    //1 per zip file, near end, before comment
    Signature               :  LongInt; //4 bytes
    Disk_Number             :  Word;
    Central_Dir_Start_Disk  :  Word;
    Entries_This_Disk       :  Word;
    Total_Entries           :  Word;
    Central_Dir_Size        :  LongWord;
    Start_Disk_Offset       :  LongWord;
    ZipFile_Comment_Length  :  Word;
  end;

  Zip64_End_of_Central_Dir_type = Packed Record
    Signature                 : LongInt;
    Record_Size               : QWord;
    Version_Made_By           : Word; //lower byte >= 45
    Extract_Version_Reqd      : Word; //version >= 45
    Disk_Number               : LongWord;
    Central_Dir_Start_Disk    : LongWord;
    Entries_This_Disk         : QWord;
    Total_Entries             : QWord;
    Central_Dir_Size          : QWord;
    Start_Disk_Offset         : QWord;
  end;

  Zip64_End_of_Central_Dir_Locator_type = Packed Record //comes after Zip64_End_of_Central_Dir_type
    Signature                           : LongInt;
    Zip64_EOCD_Start_Disk               : LongWord; //Starting disk for Zip64 End of Central Directory record
    Central_Dir_Zip64_EOCD_Offset       : QWord; //offset of Zip64 End of Central Directory record
    Total_Disks                         : LongWord; //total number of disks (contained in zip)
  end;

Const
  Crc_32_Tab : Array[0..255] of LongWord = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );

Type

  TProgressEvent = Procedure(Sender : TObject; Const Pct : Double) of object;
  TProgressEventEx = Procedure(Sender : TObject; Const ATotPos, ATotSize: Int64) of object;
  TOnEndOfFileEvent = Procedure(Sender : TObject; Const Ratio : Double) of object;
  TOnStartFileEvent = Procedure(Sender : TObject; Const AFileName : String) of object;

Type

  { TCompressor }
  TCompressor = Class(TObject)
  private
    FTerminated: Boolean;
  Protected
    FInFile     : TStream;        { I/O file variables                         }
    FOutFile    : TStream;
    FCrc32Val   : LongWord;       { CRC calculation variable                   }
    FBufferSize : LongWord;
    FOnPercent  : Integer;
    FOnProgress : TProgressEvent;
    Procedure UpdC32(Octet: Byte);
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord); virtual;
    Procedure Compress; Virtual; Abstract;
    Class Function ZipID : Word; virtual; Abstract;
    Class Function ZipVersionReqd: Word; virtual; Abstract;
    Function ZipBitFlag: Word; virtual; Abstract;
    Procedure Terminate;
    Property BufferSize : LongWord read FBufferSize;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property Crc32Val : LongWord Read FCrc32Val Write FCrc32Val;
    Property Terminated : Boolean Read FTerminated;
  end;

  { TDeCompressor }
  TDeCompressor = Class(TObject)
  Protected
    FInFile     : TStream;        { I/O file variables                         }
    FOutFile    : TStream;
    FCrc32Val   : LongWord;       { CRC calculation variable                   }
    FBufferSize : LongWord;
    FOnPercent  : Integer;
    FOnProgress : TProgressEvent;
    FOnProgressEx: TProgressEventEx;
    FTotPos      : Int64;
    FTotSize     : Int64;
    FTerminated : Boolean;
    Procedure UpdC32(Octet: Byte);
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord); virtual;
    Procedure DeCompress; Virtual; Abstract;
    Procedure Terminate;
    Class Function ZipID : Word; virtual; Abstract;
    Property BufferSize : LongWord read FBufferSize;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property OnProgressEx : TProgressEventEx Read FOnProgressEx Write FOnProgressEx;
    Property Crc32Val : LongWord Read FCrc32Val Write FCrc32Val;
    Property Terminated : Boolean Read FTerminated;
  end;

  { TShrinker }

Const
   TABLESIZE   =   8191;
   FIRSTENTRY  =    257;

Type
  CodeRec =  Packed Record
    Child   : Smallint;
    Sibling : Smallint;
    Suffix  : Byte;
  end;
  CodeArray   =  Array[0..TABLESIZE] of CodeRec;
  TablePtr    =  ^CodeArray;

  FreeListPtr    =  ^FreeListArray;
  FreeListArray  =  Array[FIRSTENTRY..TABLESIZE] of Word;

  BufPtr      =  PByte;

  TShrinker = Class(TCompressor)
  Private
    FBufSize    : LongWord;
    MaxInBufIdx :  LongWord;      { Count of valid chars in input buffer       }
    InputEof    :  Boolean;       { End of file indicator                      }
    CodeTable   :  TablePtr;      { Points to code table for LZW compression   }
    FreeList    :  FreeListPtr;   { Table of free code table entries           }
    NextFree    :  Word;          { Index into free list table                 }

    ClearList   :  Array[0..1023] of Byte;  { Bit mapped structure used in     }
                                            {    during adaptive resets        }
    CodeSize    :  Byte;     { Size of codes (in bits) currently being written }
    MaxCode     :  Word;   { Largest code that can be written in CodeSize bits }
    InBufIdx,                     { Points to next char in buffer to be read   }
    OutBufIdx   :  LongWord;      { Points to next free space in output buffer }
    InBuf,                        { I/O buffers                                }
    OutBuf      :  BufPtr;
    FirstCh     :  Boolean;  { Flag indicating the START of a shrink operation }
    TableFull   :  Boolean;  { Flag indicating a full symbol table             }
    SaveByte    :  Byte;     { Output code buffer                              }
    BitsUsed    :  Byte;     { Index into output code buffer                   }
    BytesIn     :  LongWord;  { Count of input file bytes processed             }
    BytesOut    :  LongWord;  { Count of output bytes                           }
    FOnBytes    :  LongWord;
    Procedure FillInputBuffer;
    Procedure WriteOutputBuffer;
    Procedure FlushOutput;
    Procedure PutChar(B : Byte);
    procedure PutCode(Code : Smallint);
    Procedure InitializeCodeTable;
    Procedure Prune(Parent : Word);
    Procedure Clear_Table;
    Procedure Table_Add(Prefix : Word; Suffix : Byte);
    function  Table_Lookup(TargetPrefix : Smallint;
                           TargetSuffix : Byte;
                           Out FoundAt  : Smallint) : Boolean;
    Procedure Shrink(Suffix : Smallint);
    Procedure ProcessLine(Const Source : String);
    Procedure DoOnProgress(Const Pct : Double); Virtual;
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord); override;
    Destructor Destroy; override;
    Procedure Compress; override;
    Class Function ZipID : Word; override;
    Class Function ZipVersionReqd : Word; override;
    Function ZipBitFlag : Word; override;
  end;

  { TDeflater }

  TDeflater = Class(TCompressor)
  private
    FCompressionLevel: TCompressionlevel;
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord);override;
    Procedure Compress; override;
    Class Function ZipID : Word; override;
    Class Function ZipVersionReqd : Word; override;
    Function ZipBitFlag : Word; override;
    Property CompressionLevel : TCompressionlevel Read FCompressionLevel Write FCompressionLevel;
  end;

  { TInflater }

  TInflater = Class(TDeCompressor)
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord);override;
    Procedure DeCompress; override;
    Class Function ZipID : Word; override;
  end;

  { TZipFileEntry }

  TZipFileEntry = Class(TCollectionItem)
  private
    FArchiveFileName: String; //Name of the file as it appears in the zip file list
    FUTF8FileName : UTF8String;
    FUTF8DiskFileName : UTF8String;
    FAttributes: LongWord;
    FDateTime: TDateTime;
    FDiskFileName: String; {Name of the file on disk (i.e. uncompressed. Can be empty if based on a stream.);
    uses local OS/filesystem directory separators}
    FHeaderPos: int64;
    FNeedsZip64: Boolean; //flags whether filesize is big enough so we need a zip64 entry
    FOS: Byte;
    FSize: Int64;
    FStream: TStream;
    FCompressionLevel: TCompressionlevel;
    function GetArchiveFileName: String;
    function GetUTF8ArchiveFileName: UTF8String;
    function GetUTF8DiskFileName: UTF8String;
    procedure SetArchiveFileName(Const AValue: String);
    procedure SetDiskFileName(Const AValue: String);
    procedure SetUTF8ArchiveFileName(AValue: UTF8String);
    procedure SetUTF8DiskFileName(AValue: UTF8String);
  Protected
    // For multi-disk support, a disk number property could be added here.
    Property HdrPos : int64 Read FHeaderPos Write FheaderPos;
    Property NeedsZip64 : boolean Read FNeedsZip64 Write FNeedsZip64;
  Public
    constructor Create(ACollection: TCollection); override;
    function IsDirectory: Boolean;
    function IsLink: Boolean;
    Procedure Assign(Source : TPersistent); override;
    Property Stream : TStream Read FStream Write FStream;
  Published
    Property ArchiveFileName : String Read GetArchiveFileName Write SetArchiveFileName;
    Property UTF8ArchiveFileName : UTF8String Read GetUTF8ArchiveFileName Write SetUTF8ArchiveFileName;
    Property DiskFileName : String Read FDiskFileName Write SetDiskFileName;
    Property UTF8DiskFileName : UTF8String Read GetUTF8DiskFileName Write SetUTF8DiskFileName;
    Property Size : Int64 Read FSize Write FSize;
    Property DateTime : TDateTime Read FDateTime Write FDateTime;
    property OS: Byte read FOS write FOS;
    property Attributes: LongWord read FAttributes write FAttributes;
    Property CompressionLevel: TCompressionlevel read FCompressionLevel write FCompressionLevel;
  end;

  { TZipFileEntries }

  TZipFileEntries = Class(TCollection)
  private
    function GetZ(AIndex : Integer): TZipFileEntry;
    procedure SetZ(AIndex : Integer; const AValue: TZipFileEntry);
  Public
    Function AddFileEntry(Const ADiskFileName : String): TZipFileEntry;
    Function AddFileEntry(Const ADiskFileName, AArchiveFileName : String): TZipFileEntry;
    Function AddFileEntry(Const AStream : TSTream; Const AArchiveFileName : String): TZipFileEntry;
    Procedure AddFileEntries(Const List : TStrings);
    Property Entries[AIndex : Integer] : TZipFileEntry Read GetZ Write SetZ; default;
  end;

  { TZipper }

  TZipper = Class(TObject)
  Private
    FEntries        : TZipFileEntries;
    FTerminated: Boolean;
    FZipping        : Boolean;
    FBufSize        : LongWord;
    FFileName       : RawByteString;         { Name of resulting Zip file                 }
    FFileComment    : String;
    FFiles          : TStrings;
    FInMemSize      : Int64;
    FZipFileNeedsZip64 : Boolean; //flags whether at least one file is big enough to require a zip64 record
    FOutStream      : TStream;
    FInFile         : TStream;     { I/O file variables                         }
    LocalHdr        : Local_File_Header_Type;
    LocalZip64ExtHdr: Extensible_Data_Field_Header_Type; //Extra field header fixed to zip64 (i.e. .ID=1)
    LocalZip64Fld   : Zip64_Extended_Info_Field_Type; //header is in LocalZip64ExtHdr
    CentralHdr      : Central_File_Header_Type;
    EndHdr          : End_of_Central_Dir_Type;
    FOnPercent      : LongInt;
    FOnProgress     : TProgressEvent;
    FOnEndOfFile    : TOnEndOfFileEvent;
    FOnStartFile    : TOnStartFileEvent;
    FCurrentCompressor : TCompressor;
    FUseLanguageEncoding: Boolean;
    function CheckEntries: Integer;
    procedure SetEntries(const AValue: TZipFileEntries);
  Protected
    Procedure CloseInput(Item : TZipFileEntry);
    Procedure StartZipFile(Item : TZipFileEntry);
    Function  UpdateZipHeader(Item : TZipFileEntry; FZip : TStream; ACRC : LongWord;AMethod : Word; AZipVersionReqd : Word; AZipBitFlag : Word) : Boolean;
    Procedure BuildZipDirectory; //Builds central directory based on local headers
    Procedure DoEndOfFile;
    Procedure ZipOneFile(Item : TZipFileEntry); virtual;
    Function  OpenInput(Item : TZipFileEntry) : Boolean;
    Procedure GetFileInfo;
    Procedure SetBufSize(Value : LongWord);
    Procedure SetFileName(Value : RawByteString);
    Function CreateCompressor(Item : TZipFileEntry; AinFile,AZipStream : TStream) : TCompressor; virtual;
    Property NeedsZip64 : boolean Read FZipFileNeedsZip64 Write FZipFileNeedsZip64;
  Public
    Constructor Create;
    Destructor Destroy;override;
    Procedure ZipAllFiles; virtual;
    // Saves zip to file and changes FileName
    Procedure SaveToFile(const AFileName: RawByteString);
    // Saves zip to stream
    Procedure SaveToStream(AStream: TStream);
    // Zips specified files into a zip with name AFileName
    Procedure ZipFile(const aFileToBeZipped : RawByteString);
    Procedure ZipFile(const AZipFileName,aFileToBeZipped : RawByteString);
    Procedure ZipFiles(const AZipFileName : RawByteString; FileList : TStrings);
    Procedure ZipFiles(const AZipFileName : RawByteString; const FileList : Array of RawbyteString);
    Procedure ZipFiles(const aFileList : Array of RawbyteString);
    Procedure ZipFiles(FileList : TStrings);
    // Zips specified entries into a zip with name AFileName
    Procedure ZipFiles(const AZipFileName : RawByteString; Entries : TZipFileEntries);
    Procedure ZipFiles(Entries : TZipFileEntries);
    // Easy access method
    // Zip single file
    Class Procedure Zip(const AZipFileName : RawByteString; const aFileToBeZipped: RawByteString);
    // Zip multiple file
    Class Procedure Zip(const AZipFileName : RawByteString; aFileList : Array of RawByteString);
    Class Procedure Zip(const AZipFileName : RawByteString; aFileList : TStrings);
    Procedure Clear;
    Procedure Terminate;
  Public
    Property BufferSize : LongWord Read FBufSize Write SetBufSize;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property OnStartFile : TOnStartFileEvent Read FOnStartFile Write FOnStartFile;
    Property OnEndFile : TOnEndOfFileEvent Read FOnEndOfFile Write FOnEndOfFile;
    Property FileName : RawByteString Read FFileName Write SetFileName;
    Property FileComment: String Read FFileComment Write FFileComment;
    // Deprecated. Use Entries.AddFileEntry(FileName) or Entries.AddFileEntries(List) instead.
    Property Files : TStrings Read FFiles; deprecated;
    Property InMemSize : Int64 Read FInMemSize Write FInMemSize;
    Property Entries : TZipFileEntries Read FEntries Write SetEntries;
    Property Terminated : Boolean Read FTerminated;
    // EFS/language encoding using UTF-8
    Property UseLanguageEncoding : Boolean Read FUseLanguageEncoding Write FUseLanguageEncoding;
  end;

  { TFullZipFileEntry }

  TFullZipFileEntry = Class(TZipFileEntry)
  private
    FBitFlags: Word;
    FCompressedSize: QWord;
    FCompressMethod: Word;
    FCRC32: LongWord;
  Public
    Property BitFlags : Word Read FBitFlags;
    Property CompressMethod : Word Read FCompressMethod;
    Property CompressedSize : QWord Read FCompressedSize;
    property CRC32: LongWord read FCRC32 write FCRC32;
  end;

  TOnCustomStreamEvent = Procedure(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry) of object;
  TCustomInputStreamEvent = Procedure(Sender: TObject; var AStream: TStream) of object;

  { TFullZipFileEntries }

  TFullZipFileEntries = Class(TZipFileEntries)
  private
    function GetFZ(AIndex : Integer): TFullZipFileEntry;
    procedure SetFZ(AIndex : Integer; const AValue: TFullZipFileEntry);
  Public
    Property FullEntries[AIndex : Integer] : TFullZipFileEntry Read GetFZ Write SetFZ; default;
  end;

  { TUnZipper }

  TUnZipper = Class(TObject)
  Private
    FOnCloseInputStream: TCustomInputStreamEvent;
    FOnCreateStream: TOnCustomStreamEvent;
    FOnDoneStream: TOnCustomStreamEvent;
    FOnOpenInputStream: TCustomInputStreamEvent;
    FUnZipping  : Boolean;
    FBufSize    : LongWord;
    FFileName   : RawByteString;         { Name of resulting Zip file                 }
    FOutputPath : RawByteString;
    FFileComment: String;
    FEntries    : TFullZipFileEntries;
    FFiles      : TStrings;
    FUseUTF8    : Boolean;
    FFlat       : Boolean;
    FZipStream  : TStream;     { I/O file variables                         }
    LocalHdr    : Local_File_Header_Type; //Local header, before compressed file data
    LocalZip64Fld   : Zip64_Extended_Info_Field_Type; //header is in LocalZip64ExtHdr
    CentralHdr  : Central_File_Header_Type;
    FTotPos     : Int64;
    FTotSize    : Int64;
    FTerminated: Boolean;
    FOnPercent  : LongInt;
    FOnProgress : TProgressEvent;
    FOnProgressEx : TProgressEventEx;
    FOnEndOfFile : TOnEndOfFileEvent;
    FOnStartFile : TOnStartFileEvent;
    FCurrentDecompressor: TDecompressor;
    function CalcTotalSize(AllFiles: Boolean): Int64;
    function IsMatch(I: TFullZipFileEntry): Boolean;
  Protected
    Procedure OpenInput;
    Procedure CloseOutput(Item : TFullZipFileEntry; var OutStream: TStream);
    Procedure CloseInput;
    Procedure FindEndHeaders(
      out AEndHdr: End_of_Central_Dir_Type;
      out AEndHdrPos: Int64;
      out AEndZip64Hdr: Zip64_End_of_Central_Dir_type;
      out AEndZip64HdrPos: Int64);
    Procedure ReadZipDirectory;
    Procedure ReadZipHeader(Item : TFullZipFileEntry; out AMethod : Word);
    Procedure DoEndOfFile;
    Procedure UnZipOneFile(Item : TFullZipFileEntry); virtual;
    Function  OpenOutput(OutFileName : RawByteString; Out OutStream: TStream; Item : TFullZipFileEntry) : Boolean;
    Procedure SetBufSize(Value : LongWord);
    Procedure SetFileName(Value : RawByteString);
    Procedure SetOutputPath(Value: RawByteString);
    Function CreateDeCompressor(Item : TZipFileEntry; AMethod : Word;AZipFile,AOutFile : TStream) : TDeCompressor; virtual;
  Public
    Constructor Create;
    Destructor Destroy;override;
    Procedure UnZipAllFiles; virtual;
    Procedure UnZipFile(const aExtractFileName: RawByteString);
    Procedure UnZipFile(const AZipFileName, aExtractFileName: RawByteString);
    Procedure UnZipFiles(const AZipFileName : RawByteString; FileList : TStrings);
    Procedure UnZipFiles(const AZipFileName : RawByteString; aFileList : Array of RawBytestring);
    Procedure UnZipFiles(aFileList : TStrings);
    Procedure UnZipAllFiles(const AZipFileName : RawByteString);
    // Easy access methods. No instance needed, uses default options.
    // Unzip all files
    Class Procedure Unzip(const AZipFileName : RawByteString);
    // Unzip a single file.
    Class Procedure Unzip(const AZipFileName : RawByteString;aExtractFileName : RawByteString);
    Class Procedure UnZip(const AZipFileName, aExtractFileName: RawByteString; aOutputFileName : string);
    // Unzip several files
    Class Procedure Unzip(const AZipFileName : RawByteString; aFileList : Array of RawByteString);
    Class Procedure Unzip(const AZipFileName : RawByteString; aFileList : TStrings);
    Class Procedure Unzip(const AZipFileName : RawByteString; aFileList : Array of RawByteString; aOutputDir : RawByteString; aFlat : Boolean = false);
    Class Procedure Unzip(const AZipFileName : RawByteString; aFileList : TStrings; aOutputDir : RawByteString; aFlat : Boolean = false);
    Procedure Clear;
    Procedure Examine;
    Procedure Terminate;
  Public
    Property BufferSize : LongWord Read FBufSize Write SetBufSize;
    Property OnOpenInputStream: TCustomInputStreamEvent read FOnOpenInputStream write FOnOpenInputStream;
    Property OnCloseInputStream: TCustomInputStreamEvent read FOnCloseInputStream write FOnCloseInputStream;
    Property OnCreateStream : TOnCustomStreamEvent Read FOnCreateStream Write FOnCreateStream;
    Property OnDoneStream : TOnCustomStreamEvent Read FOnDoneStream Write FOnDoneStream;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property OnProgressEx : TProgressEventEx Read FOnProgressEx Write FOnProgressEx;
    Property OnStartFile : TOnStartFileEvent Read FOnStartFile Write FOnStartFile;
    Property OnEndFile : TOnEndOfFileEvent Read FOnEndOfFile Write FOnEndOfFile;
    Property FileName : RawByteString Read FFileName Write SetFileName;
    Property OutputPath : RawByteString Read FOutputPath Write SetOutputPath;
    Property FileComment: String Read FFileComment;
    Property Files : TStrings Read FFiles;
    Property Entries : TFullZipFileEntries Read FEntries;
    Property UseUTF8 : Boolean Read FUseUTF8 Write FUseUTF8;
    Property Flat : Boolean Read FFlat Write FFlat; // enable flat extraction, like -j when using unzip
    Property Terminated : Boolean Read FTerminated;
  end;

  EZipError = Class(Exception);

Implementation

uses rtlconsts;

ResourceString
  SErrBufsizeChange = 'Changing buffer size is not allowed while (un)zipping.';
  SErrFileChange = 'Changing output file name is not allowed while (un)zipping.';
  SErrInvalidCRC = 'Invalid CRC checksum while unzipping %s.';
  SErrCorruptZIP = 'Corrupt ZIP file %s.';
  SErrUnsupportedCompressionFormat = 'Unsupported compression format %d';
  SErrUnsupportedMultipleDisksCD = 'A central directory split over multiple disks is unsupported.';
  SErrMaxEntries = 'Encountered %d file entries; maximum supported is %d.';
  SErrMissingFileName = 'Missing filename in entry %d.';
  SErrMissingArchiveName = 'Missing archive filename in streamed entry %d.';
  SErrFileDoesNotExist = 'File "%s" does not exist.';
  SErrPosTooLarge = 'Position/offset %d is larger than maximum supported %d.';
  SErrNoFileName = 'No archive filename for examine operation.';
  SErrNoStream = 'No stream is opened.';
  SErrEncryptionNotSupported = 'Cannot unzip item "%s" : encryption is not supported.';
  SErrPatchSetNotSupported = 'Cannot unzip item "%s" : Patch sets are not supported.';

{ ---------------------------------------------------------------------
    Auxiliary
  ---------------------------------------------------------------------}
Type
  // A local version of TFileStream which uses rawbytestring. It
  TFileStream = class(THandleStream)
  Private
    FFileName : RawBytestring;
  public
    constructor Create(const AFileName: RawBytestring; Mode: Word);
    constructor Create(const AFileName: RawBytestring; Mode: Word; Rights: Cardinal);
    destructor Destroy; override;
    property FileName : RawBytestring Read FFilename;
  end;
  constructor TFileStream.Create(const AFileName: rawbytestring; Mode: Word);

  begin
    Create(AFileName,Mode,438);
  end;


  constructor TFileStream.Create(const AFileName: rawbytestring; Mode: Word; Rights: Cardinal);
    {$ifdef Windows}
    function FixLongFilename(const Fn: RawByteString): RawByteString;
    begin
      Result := Fn;
      if (Length(Fn)>MAX_PATH) and not ((Pos('\\?\', Fn)=1) or (Pos('\\.\', Fn)=1) or (Pos('\\?\UNC\', Fn)=1)) then
        begin
          if (Pos('\\', Fn)=1) and (length(FN)>2) then
            Insert('?\UNC\',Result,3)
          else
            Result:='\\?\'+Fn;
        end;
    end;
    {$endif}

  Var
    H : Thandle;

  begin
    {$ifdef Windows}
    FFileName:=FixLongFilename(AFileName);
    {$else}
    FFileName:=AFileName;
    {$endif}
    If (Mode and fmCreate) > 0 then
      H:=FileCreate(FFileName,Mode,Rights)
    else
      H:=FileOpen(FFileName,Mode);

    If (THandle(H)=feInvalidHandle) then
      If Mode=fmcreate then
        raise EFCreateError.createfmt(SFCreateError,[AFileName])
      else
        raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
    Inherited Create(H);
  end;


  destructor TFileStream.Destroy;

  begin
    FileClose(Handle);
  end;

{$IFDEF FPC_BIG_ENDIAN}
function SwapLFH(const Values: Local_File_Header_Type): Local_File_Header_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Bit_Flag := SwapEndian(Bit_Flag);
    Result.Compress_Method := SwapEndian(Compress_Method);
    Result.Last_Mod_Time := SwapEndian(Last_Mod_Time);
    Result.Last_Mod_Date := SwapEndian(Last_Mod_Date);
    Result.Crc32 := SwapEndian(Crc32);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Uncompressed_Size := SwapEndian(Uncompressed_Size);
    Result.Filename_Length := SwapEndian(Filename_Length);
    Result.Extra_Field_Length := SwapEndian(Extra_Field_Length);
  end;
end;

function SwapEDFH(const Values: Extensible_Data_Field_Header_Type): Extensible_Data_Field_Header_Type;
begin
  with Values do
  begin
    Result.Header_ID := SwapEndian(Header_ID);
    Result.Data_Size := SwapEndian(Data_Size);
  end;
end;

function SwapZ64EIF(const Values: Zip64_Extended_Info_Field_Type): Zip64_Extended_Info_Field_Type;
begin
  with Values do
  begin
    Result.Original_Size := SwapEndian(Original_Size);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Relative_Hdr_Offset := SwapEndian(Relative_Hdr_Offset);
    Result.Disk_Start_Number := SwapEndian(Disk_Start_Number);
  end;
end;

function SwapCFH(const Values: Central_File_Header_Type): Central_File_Header_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.MadeBy_Version := SwapEndian(MadeBy_Version);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Bit_Flag := SwapEndian(Bit_Flag);
    Result.Compress_Method := SwapEndian(Compress_Method);
    Result.Last_Mod_Time := SwapEndian(Last_Mod_Time);
    Result.Last_Mod_Date := SwapEndian(Last_Mod_Date);
    Result.Crc32 := SwapEndian(Crc32);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Uncompressed_Size := SwapEndian(Uncompressed_Size);
    Result.Filename_Length := SwapEndian(Filename_Length);
    Result.Extra_Field_Length := SwapEndian(Extra_Field_Length);
    Result.File_Comment_Length := SwapEndian(File_Comment_Length);
    Result.Starting_Disk_Num := SwapEndian(Starting_Disk_Num);
    Result.Internal_Attributes := SwapEndian(Internal_Attributes);
    Result.External_Attributes := SwapEndian(External_Attributes);
    Result.Local_Header_Offset := SwapEndian(Local_Header_Offset);
  end;
end;

function SwapECD(const Values: End_of_Central_Dir_Type): End_of_Central_Dir_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Disk_Number := SwapEndian(Disk_Number);
    Result.Central_Dir_Start_Disk := SwapEndian(Central_Dir_Start_Disk);
    Result.Entries_This_Disk := SwapEndian(Entries_This_Disk);
    Result.Total_Entries := SwapEndian(Total_Entries);
    Result.Central_Dir_Size := SwapEndian(Central_Dir_Size);
    Result.Start_Disk_Offset := SwapEndian(Start_Disk_Offset);
    Result.ZipFile_Comment_Length := SwapEndian(ZipFile_Comment_Length);
  end;
end;

function SwapZ64ECD(const Values: Zip64_End_of_Central_Dir_Type): Zip64_End_of_Central_Dir_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Record_Size := SwapEndian(Record_Size);
    Result.Version_Made_By := SwapEndian(Version_Made_By);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Disk_Number := SwapEndian(Disk_Number);
    Result.Central_Dir_Start_Disk := SwapEndian(Central_Dir_Start_Disk);
    Result.Entries_This_Disk := SwapEndian(Entries_This_Disk);
    Result.Total_Entries := SwapEndian(Total_Entries);
    Result.Central_Dir_Size := SwapEndian(Central_Dir_Size);
    Result.Start_Disk_Offset := SwapEndian(Start_Disk_Offset);
  end;
end;

function SwapZ64ECDL(const Values: Zip64_End_of_Central_Dir_Locator_type): Zip64_End_of_Central_Dir_Locator_type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Zip64_EOCD_Start_Disk := SwapEndian(Zip64_EOCD_Start_Disk);
    Result.Central_Dir_Zip64_EOCD_Offset := SwapEndian(Central_Dir_Zip64_EOCD_Offset);
    Result.Total_Disks := SwapEndian(Total_Disks);
  end;
end;
{$ENDIF FPC_BIG_ENDIAN}

Procedure DateTimeToZipDateTime(DT : TDateTime; out ZD,ZT : Word);

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDate(DT,Y,M,D);
  DecodeTime(DT,H,N,S,MS);
  if Y<1980 then
  begin
    // Invalid date/time; set to earliest possible
    Y:=0;
    M:=1;
    D:=1;
    H:=0;
    N:=0;
    S:=0;
    MS:=0;
  end
  else
  begin
    Y:=Y-1980;
  end;
  ZD:=d+(32*M)+(512*Y);
  ZT:=(S div 2)+(32*N)+(2048*h);
end;

Procedure ZipDateTimeToDateTime(ZD,ZT : Word;out DT : TDateTime);

Var
  Y,M,D,H,N,S,MS : Word;
  aDate,aTime : TDateTime;

begin
  MS:=0;
  S:=(ZT and 31) shl 1;
  N:=(ZT shr 5) and 63;
  H:=ZT shr 11;
  D:=ZD and 31;
  M:=(ZD shr 5) and 15;
  Y:=((ZD shr 9) and 127)+1980;
  // Some corrections
  if M < 1 then M := 1;
  if M > 12 then M:=12;
  if D < 1 then D := 1;
  if D>MonthDays[IsLeapYear(Y)][M] then
    D:=MonthDays[IsLeapYear(Y)][M];
  // Try to encode the result, fall back on today if it fails
  if Not TryEncodeDate(Y,M,D,aDate) then
    aDate:=Date;
  if not TryEncodeTime(H,N,S,MS,aTime) then
    aTime:=Time;
  // Return result
  DT:=ComposeDateTime(aDate,ATime);
end;



function ZipUnixAttrsToFatAttrs(const Name: String; Attrs: Longint): Longint;
begin
  Result := faArchive;

  if (Pos('.', Name) = 1) and (Name <> '.') and (Name <> '..') then
    Result := Result + faHidden;
  case (Attrs and UNIX_MASK) of
    UNIX_DIR:  Result := Result + faDirectory;
    UNIX_LINK: Result := Result + faSymLink;
    UNIX_FIFO, UNIX_CHAR, UNIX_BLK, UNIX_SOCK:
               Result := Result + faSysFile;
  end;

  if (Attrs and UNIX_WUSR) = 0 then
    Result := Result + faReadOnly;
end;

function ZipFatAttrsToUnixAttrs(Attrs: Longint): Longint;
begin
  Result := UNIX_DEFAULT;
  if (faReadOnly and Attrs) > 0 then
    Result := Result and not (UNIX_WUSR);

  if (faSymLink and Attrs) > 0 then
    Result := Result or UNIX_LINK
  else
    if (faDirectory and Attrs) > 0 then
      Result := Result or UNIX_DIR
    else
      Result := Result or UNIX_FILE;
end;

function CRC32Str(const s:string):DWord;
var
  i:Integer;
begin
  Result:=$FFFFFFFF;
  if Length(S)>0 then
    for i:=1 to Length(s) do
      Result:=Crc_32_Tab[Byte(Result XOR LongInt(s[i]))] XOR ((Result SHR 8) AND $00FFFFFF);
  Result:=not Result;
end;

{ ---------------------------------------------------------------------
    TDeCompressor
  ---------------------------------------------------------------------}


Procedure TDeCompressor.UpdC32(Octet: Byte);

Begin
  FCrc32Val := Crc_32_Tab[Byte(FCrc32Val XOR LongInt(Octet))] XOR ((FCrc32Val SHR 8) AND $00FFFFFF);
end;

constructor TDeCompressor.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  FinFile:=AInFile;
  FoutFile:=AOutFile;
  FBufferSize:=ABufSize;
  CRC32Val:=$FFFFFFFF;
end;

procedure TDeCompressor.Terminate;
begin
  FTerminated:=True;
end;


{ ---------------------------------------------------------------------
    TCompressor
  ---------------------------------------------------------------------}


Procedure TCompressor.UpdC32(Octet: Byte);

Begin
  FCrc32Val := Crc_32_Tab[Byte(FCrc32Val XOR LongInt(Octet))] XOR ((FCrc32Val SHR 8) AND $00FFFFFF);
end;

constructor TCompressor.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  FinFile:=AInFile;
  FoutFile:=AOutFile;
  FBufferSize:=ABufSize;
  CRC32Val:=$FFFFFFFF;
end;

procedure TCompressor.Terminate;
begin
  FTerminated:=True;
end;


{ ---------------------------------------------------------------------
    TDeflater
  ---------------------------------------------------------------------}

constructor TDeflater.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  Inherited;
  FCompressionLevel:=clDefault;
end;


procedure TDeflater.Compress;
Var
  Buf : PByte;
  I,Count : integer;
  C : TCompressionStream;
  BytesNow : Int64;
  NextMark : Int64;
  OnBytes : Int64;
  FSize : Int64;
begin
  CRC32Val:=$FFFFFFFF;
  Buf:=GetMem(FBufferSize);
  if FOnPercent = 0 then
    FOnPercent := 1;
  OnBytes:=Round((FInFile.Size * FOnPercent) / 100);
  BytesNow:=0;
  NextMark := OnBytes;
  FSize:=FInfile.Size;
  Try
    C:=TCompressionStream.Create(FCompressionLevel,FOutFile,True);
    Try
      if assigned(FOnProgress) then
        fOnProgress(self,0);
      Repeat
        Count:=FInFile.Read(Buf^,FBufferSize);
        For I:=0 to Count-1 do
          UpdC32(Buf[i]);
        // Writebuffer will loop  
        C.WriteBuffer(Buf^,Count);
        inc(BytesNow,Count);
        if BytesNow>NextMark Then
          begin
            if (FSize>0) and assigned(FOnProgress) Then
              FOnProgress(self,100 * ( BytesNow / FSize));
            inc(NextMark,OnBytes);
          end;
      Until (Count=0) or Terminated;
    Finally
      C.Free;
    end;
  Finally
    FreeMem(Buf);
  end;
  if assigned(FOnProgress) then
    fOnProgress(self,100.0);
  Crc32Val:=NOT Crc32Val;
end;

class function TDeflater.ZipID: Word;
begin
  Result:=8;
end;

class function TDeflater.ZipVersionReqd: Word;
begin
  Result:=20;
end;

function TDeflater.ZipBitFlag: Word;
begin
  case CompressionLevel of
    clnone: Result := %110;
    clfastest: Result := %100;
    cldefault: Result := %000;
    clmax: Result := %010;
    else
      Result := 0;
  end;
end;

{ ---------------------------------------------------------------------
    TInflater
  ---------------------------------------------------------------------}

constructor TInflater.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  Inherited;
end;


procedure TInflater.DeCompress;

Var
  Buf : PByte;
  I,Count : Integer;
  C : TDeCompressionStream;
  BytesNow : Integer;
  NextMark : Integer;
  OnBytes  : Integer;
  FSize    : Integer;

begin
  CRC32Val:=$FFFFFFFF;
  if FOnPercent = 0 then
    FOnPercent := 1;
  OnBytes:=Round((FInFile.Size * FOnPercent) / 100);
  BytesNow:=0; NextMark := OnBytes;
  FSize:=FInfile.Size;

  If Assigned(FOnProgress) then
    fOnProgress(self,0);

  Buf:=GetMem(FBufferSize);
  Try
    C:=TDeCompressionStream.Create(FInFile,True);
    Try
      Repeat
        Count:=C.Read(Buf^,FBufferSize);
        For I:=0 to Count-1 do
          UpdC32(Buf[i]);
        FOutFile.WriteBuffer(Buf^,Count);
        inc(BytesNow,Count);
        if BytesNow>NextMark Then
           begin
             if (FSize>0) and assigned(FOnProgress) Then
               FOnProgress(self,100 * ( BytesNow / FSize));
             if assigned(FOnProgressEx) Then
               FOnProgressEx(Self, FTotPos + BytesNow, FTotSize);
             inc(NextMark,OnBytes);
           end;
      Until (Count=0) or Terminated;
      FTotPos := FTotPos + FOutFile.Size;
    Finally
      C.Free;
    end;
  Finally
    FreeMem(Buf);
  end;
 if assigned(FOnProgress) then
   fOnProgress(self,100.0);
 if assigned(FOnProgressEx) then
   FOnProgressEx(Self, FTotPos, FTotSize);
  Crc32Val:=NOT Crc32Val;
end;

class function TInflater.ZipID: Word;
begin
  Result:=8;
end;


{ ---------------------------------------------------------------------
    TShrinker
  ---------------------------------------------------------------------}

Const
   DefaultInMemSize = 256*1024; { Files larger than 256k are processed on disk   }
   DefaultBufSize =  16384;     { Use 16K file buffers                             }
   MINBITS     =      9;        { Starting code size of 9 bits                     }
   MAXBITS     =     13;        { Maximum code size of 13 bits                     }
   SPECIAL     =    256;        { Special function code                            }
   INCSIZE     =      1;        { Code indicating a jump in code size              }
   CLEARCODE   =      2;        { Code indicating code table has been cleared      }
   STDATTR     =    faAnyFile;  { Standard file attribute for DOS Find First/Next  }

constructor TShrinker.Create(AInFile, AOutFile : TStream; ABufSize : LongWord);
begin
  Inherited;
  FBufSize:=ABufSize;
  InBuf:=GetMem(FBUFSIZE);
  OutBuf:=GetMem(FBUFSIZE);
  CodeTable:=GetMem(SizeOf(CodeTable^));
  FreeList:=GetMem(SizeOf(FreeList^));
end;

destructor TShrinker.Destroy;
begin
  FreeMem(CodeTable);
  FreeMem(FreeList);
  FreeMem(InBuf);
  FreeMem(OutBuf);
  inherited Destroy;
end;

Procedure TShrinker.Compress;

Var
   OneString : String;
   Remaining : Word;

begin
  BytesIn := 1;
  BytesOut := 1;
  InitializeCodeTable;
  FillInputBuffer;
  FirstCh:= TRUE;
  Crc32Val:=$FFFFFFFF;
  FOnBytes:=Round((FInFile.Size * FOnPercent) / 100);
  While Not InputEof do
    begin
    Remaining:=Succ(MaxInBufIdx - InBufIdx);
    If Remaining>255 then
      Remaining:=255;
    If Remaining=0 then
      FillInputBuffer
    else
      begin
      SetLength(OneString,Remaining);
      Move(InBuf[InBufIdx], OneString[1], Remaining);
      Inc(InBufIdx, Remaining);
      ProcessLine(OneString);
      end;
    end;
   Crc32Val := Not Crc32Val;
   ProcessLine('');
end;

class function TShrinker.ZipID: Word;
begin
  Result:=1;
end;

class function TShrinker.ZipVersionReqd: Word;
begin
  Result:=10;
end;

function TShrinker.ZipBitFlag: Word;
begin
  Result:=0;
end;


Procedure TShrinker.DoOnProgress(Const Pct: Double);

begin
  If Assigned(FOnProgress) then
    FOnProgress(Self,Pct);
end;


Procedure TShrinker.FillInputBuffer;

Begin
   MaxInbufIDx:=FInfile.Read(InBuf[0], FBufSize);
   If MaxInbufIDx=0 then
      InputEof := TRUE
   else
      InputEOF := FALSE;
   InBufIdx := 0;
end;


Procedure TShrinker.WriteOutputBuffer;
Begin
  FOutFile.WriteBuffer(OutBuf[0], OutBufIdx);
  OutBufIdx := 0;
end;


Procedure TShrinker.PutChar(B : Byte);

Begin
  OutBuf[OutBufIdx] := B;
  Inc(OutBufIdx);
  If OutBufIdx>=FBufSize then
    WriteOutputBuffer;
  Inc(BytesOut);
end;

Procedure TShrinker.FlushOutput;
Begin
  If OutBufIdx>0 then
    WriteOutputBuffer;
End;


procedure TShrinker.PutCode(Code : Smallint);

var
  ACode : LongInt;
  XSize : Smallint;

begin
  if (Code=-1) then
    begin
    if BitsUsed>0 then
      PutChar(SaveByte);
    end
  else
    begin
    ACode := Longint(Code);
    XSize := CodeSize+BitsUsed;
    ACode := (ACode shl BitsUsed) or SaveByte;
    while (XSize div 8) > 0 do
      begin
      PutChar(Lo(ACode));
      ACode := ACode shr 8;
      Dec(XSize,8);
      end;
    BitsUsed := XSize;
    SaveByte := Lo(ACode);
    end;
end;


Procedure TShrinker.InitializeCodeTable;

Var
   I  :  Word;
Begin
   For I := 0 to TableSize do
     begin
     With CodeTable^[I] do
       begin
       Child := -1;
       Sibling := -1;
       If (I<=255) then
         Suffix := I;
       end;
     If (I>=257) then
       FreeList^[I] := I;
     end;
   NextFree  := FIRSTENTRY;
   TableFull := FALSE;
end;


Procedure TShrinker.Prune(Parent : Word);

Var
   CurrChild   : Smallint;
   NextSibling : Smallint;
Begin
  CurrChild := CodeTable^[Parent].Child;
  { Find first Child that has descendants .. clear any that don't }
  While (CurrChild <> -1) and (CodeTable^[CurrChild].Child = -1) do
    begin
    CodeTable^[Parent].Child := CodeTable^[CurrChild].Sibling;
    CodeTable^[CurrChild].Sibling := -1;
     { Turn on ClearList bit to indicate a cleared entry }
    ClearList[CurrChild DIV 8] := (ClearList[CurrChild DIV 8] OR (1 SHL (CurrChild MOD 8)));
    CurrChild := CodeTable^[Parent].Child;
    end;
  If CurrChild <> -1 then
    begin   { If there are any children left ...}
    Prune(CurrChild);
    NextSibling := CodeTable^[CurrChild].Sibling;
    While NextSibling <> -1 do
      begin
      If CodeTable^[NextSibling].Child = -1 then
        begin
        CodeTable^[CurrChild].Sibling := CodeTable^[NextSibling].Sibling;
        CodeTable^[NextSibling].Sibling := -1;
        { Turn on ClearList bit to indicate a cleared entry }
        ClearList[NextSibling DIV 8] := (ClearList[NextSibling DIV 8] OR (1 SHL (NextSibling MOD 8)));
        NextSibling := CodeTable^[CurrChild].Sibling;
        end
      else
        begin
        CurrChild := NextSibling;
        Prune(CurrChild);
        NextSibling := CodeTable^[CurrChild].Sibling;
        end;
      end;
    end;
end;


Procedure TShrinker.Clear_Table;
Var
   Node : Word;
Begin
   FillChar(ClearList, SizeOf(ClearList), $00);
   For Node := 0 to 255 do
     Prune(Node);
   NextFree := Succ(TABLESIZE);
   For Node := TABLESIZE downto FIRSTENTRY do
     begin
     If (ClearList[Node DIV 8] AND (1 SHL (Node MOD 8))) <> 0 then
       begin
       Dec(NextFree);
       FreeList^[NextFree] := Node;
       end;
     end;
   If NextFree <= TABLESIZE then
     TableFull := FALSE;
end;


Procedure TShrinker.Table_Add(Prefix : Word; Suffix : Byte);
Var
   FreeNode : Word;
Begin
  If NextFree <= TABLESIZE then
    begin
    FreeNode := FreeList^[NextFree];
    Inc(NextFree);
    CodeTable^[FreeNode].Child := -1;
    CodeTable^[FreeNode].Sibling := -1;
    CodeTable^[FreeNode].Suffix := Suffix;
    If CodeTable^[Prefix].Child  = -1 then
      CodeTable^[Prefix].Child := FreeNode
    else
      begin
      Prefix := CodeTable^[Prefix].Child;
      While CodeTable^[Prefix].Sibling <> -1 do
        Prefix := CodeTable^[Prefix].Sibling;
      CodeTable^[Prefix].Sibling := FreeNode;
      end;
    end;
  if NextFree > TABLESIZE then
    TableFull := TRUE;
end;

function TShrinker.Table_Lookup(    TargetPrefix : Smallint;
                          TargetSuffix : Byte;
                      Out FoundAt      : Smallint   ) : Boolean;

var TempPrefix : Smallint;

begin
  TempPrefix := TargetPrefix;
  Table_lookup := False;
  if CodeTable^[TempPrefix].Child <> -1 then
    begin
    TempPrefix := CodeTable^[TempPrefix].Child;
    repeat
      if CodeTable^[TempPrefix].Suffix = TargetSuffix then
        begin
        Table_lookup := True;
        break;
        end;
      if CodeTable^[TempPrefix].Sibling = -1 then
        break;
      TempPrefix := CodeTable^[TempPrefix].Sibling;
    until False;
  end;
  if Table_Lookup then
    FoundAt := TempPrefix
  else
    FoundAt := -1;
end;

Procedure TShrinker.Shrink(Suffix : Smallint);

Const
  LastCode : Smallint = 0;

Var
  WhereFound : Smallint;

Begin
  If FirstCh then
    begin
    SaveByte := $00;
    BitsUsed := 0;
    CodeSize := MINBITS;
    MaxCode  := (1 SHL CodeSize) - 1;
    LastCode := Suffix;
    FirstCh  := FALSE;
    end
  else
    begin
    If Suffix <> -1 then
      begin
      If TableFull then
        begin
        Putcode(LastCode);
        PutCode(SPECIAL);
        Putcode(CLEARCODE);
        Clear_Table;
        Table_Add(LastCode, Suffix);
        LastCode := Suffix;
        end
      else
        begin
        If Table_Lookup(LastCode, Suffix, WhereFound) then
          begin
          LastCode  := WhereFound;
          end
        else
          begin
          PutCode(LastCode);
          Table_Add(LastCode, Suffix);
          LastCode := Suffix;
          If (FreeList^[NextFree] > MaxCode) and (CodeSize < MaxBits) then
            begin
            PutCode(SPECIAL);
            PutCode(INCSIZE);
            Inc(CodeSize);
            MaxCode := (1 SHL CodeSize) -1;
            end;
          end;
        end;
      end
    else
      begin
      PutCode(LastCode);
      PutCode(-1);
      FlushOutput;
      end;
    end;
end;

Procedure TShrinker.ProcessLine(Const Source : String);

Var
  I : Word;

Begin
  If Source = '' then
    Shrink(-1)
  else
    For I := 1 to Length(Source) do
      begin
      Inc(BytesIn);
      If (Pred(BytesIn) MOD FOnBytes) = 0 then
        DoOnProgress(100 * ( BytesIn / FInFile.Size));
      UpdC32(Ord(Source[I]));
      Shrink(Ord(Source[I]));
      end;
end;

{ ---------------------------------------------------------------------
    TZipper
  ---------------------------------------------------------------------}


Procedure TZipper.GetFileInfo;

Var
  F    : TZipFileEntry;
  Info : TSearchRec;
  I    : integer; //zip spec allows QWord but FEntries.Count does not support it
{$IFDEF UNIX}
  UnixInfo: Stat;
{$ENDIF}
Begin
  For I := 0 to FEntries.Count-1 do
    begin
    F:=FEntries[i];
    If F.Stream=Nil then
      begin
      If (F.DiskFileName='') then
        Raise EZipError.CreateFmt(SErrMissingFileName,[I]);
      If FindFirst(F.DiskFileName, STDATTR, Info)=0 then
        try
          if Info.Attr and faDirectory <> 0 then //in Linux directory Size <> 0
            F.Size := 0
          else
            F.Size:=Info.Size;
          F.DateTime:=FileDateToDateTime(Info.Time);
        {$IFDEF UNIX}
          if fplstat(F.DiskFileName, @UnixInfo) = 0 then
            F.Attributes := UnixInfo.st_mode;
        {$ELSE}
          F.Attributes := Info.Attr;
        {$ENDIF}
        finally
          FindClose(Info);
        end
      else
        Raise EZipError.CreateFmt(SErrFileDoesNotExist,[F.DiskFileName]);
      end
    else
    begin
      If (F.ArchiveFileName='') then
        Raise EZipError.CreateFmt(SErrMissingArchiveName,[I]);
      F.Size:=F.Stream.Size;
      if (F.Attributes = 0) then
      begin
      {$IFDEF UNIX}
        F.Attributes := UNIX_FILE or UNIX_DEFAULT;
      {$ELSE}
        F.Attributes := faArchive;
      {$ENDIF}
      end;	
    end;
  end;
end;


procedure TZipper.SetEntries(const AValue: TZipFileEntries);
begin
  if FEntries=AValue then exit;
  FEntries.Assign(AValue);
end;

Function TZipper.OpenInput(Item : TZipFileEntry) : Boolean;

Begin
  If (Item.Stream<>nil) then
    FInFile:=Item.Stream
  else
    if Item.IsDirectory then
      FInFile := TStringStream.Create('')
    else
      FInFile:=TFileStream.Create(Item.DiskFileName,fmOpenRead);
  Result:=True;
  If Assigned(FOnStartFile) then
    FOnStartFile(Self,Item.ArchiveFileName);
End;


Procedure TZipper.CloseInput(Item : TZipFileEntry);

Begin
  If (FInFile<>Item.Stream) then
    FreeAndNil(FInFile)
  else
    FinFile:=Nil;
  DoEndOfFile;
end;


Procedure TZipper.StartZipFile(Item : TZipFileEntry);

Begin
  FillChar(LocalHdr,SizeOf(LocalHdr),0);
  FillChar(LocalZip64Fld,SizeOf(LocalZip64Fld),0);
  With LocalHdr do
    begin
    Signature := LOCAL_FILE_HEADER_SIGNATURE;
    Extract_Version_Reqd := 20; //default value, v2.0
    Bit_Flag := 0;
    Compress_Method := 1;
    DateTimeToZipDateTime(Item.DateTime,Last_Mod_Date,Last_Mod_Time);
    Crc32 := 0;
    Compressed_Size := 0;
    LocalZip64Fld.Compressed_Size := 0;
    if Item.Size >= $FFFFFFFF then
      begin
      Uncompressed_Size := $FFFFFFFF;
      LocalZip64Fld.Original_Size := Item.Size;
      end
    else
      begin
      Uncompressed_Size := Item.Size;
      LocalZip64Fld.Original_Size := 0;
      end;
    FileName_Length := 0;
    if (LocalZip64Fld.Original_Size>0) or
      (LocalZip64Fld.Compressed_Size>0) or
      (LocalZip64Fld.Disk_Start_Number>0) or
      (LocalZip64Fld.Relative_Hdr_Offset>0) then
      Extra_Field_Length := SizeOf(LocalZip64ExtHdr) + SizeOf(LocalZip64Fld)
    else
      Extra_Field_Length := 0;
  end;
End;


function TZipper.UpdateZipHeader(Item: TZipFileEntry; FZip: TStream;
  ACRC: LongWord; AMethod: Word; AZipVersionReqd: Word; AZipBitFlag: Word
  ): Boolean;
  // Update header for a single zip file (local header)
var
  IsZip64           : boolean; //Must the local header be in zip64 format?
  // Separate from zip64 status of entire zip file.
  ZFileName         : RawByteString;
Begin
  ZFileName := Item.ArchiveFileName;
  IsZip64 := false;
  With LocalHdr do
    begin
    if FUseLanguageEncoding then begin
      SetCodePage(ZFileName, CP_UTF8, True);
      Bit_Flag := Bit_Flag or EFS_LANGUAGE_ENCODING_FLAG;
    end;
    FileName_Length := Length(ZFileName);
    Crc32 := ACRC;
    if LocalZip64Fld.Original_Size > 0 then
      Result := Not (FZip.Size >= LocalZip64Fld.Original_Size)
    else
      Result := Not (Compressed_Size >= Uncompressed_Size);
    if Item.CompressionLevel=clNone
      then Result:=false; //user wishes override or invalid compression
    If Not Result then
      begin
      Compress_Method := 0; // No use for compression: change storage type & compression size...
      if LocalZip64Fld.Original_Size>0 then
        begin
        IsZip64 := true;
        Compressed_Size := $FFFFFFFF;
        LocalZip64Fld.Compressed_Size := LocalZip64Fld.Original_Size;
        end
      else
        begin
        Compressed_Size := Uncompressed_Size;
        LocalZip64Fld.Compressed_Size := 0;
        end;
      end
    else { Using compression }
      begin
      Compress_method := AMethod;
      Bit_Flag := Bit_Flag or AZipBitFlag;
      if FZip.Size >= $FFFFFFFF then
      begin
        IsZip64 := true;
        Compressed_Size := $FFFFFFFF;
        LocalZip64Fld.Compressed_Size := FZip.Size;
      end
      else
      begin
        Compressed_Size := FZip.Size;
        LocalZip64Fld.Compressed_Size := 0;
        if LocalZip64Fld.Original_Size > 0 then
          IsZip64 := true;
      end;
      if AZipVersionReqd > Extract_Version_Reqd then
        Extract_Version_Reqd := AZipVersionReqd;
      end;
    if (IsZip64) and (Extract_Version_Reqd<45) then
      Extract_Version_Reqd := 45;
    end;
  if IsZip64 then
    LocalHdr.Extra_Field_Length:=SizeOf(LocalZip64ExtHdr)+SizeOf(LocalZip64Fld);
  FOutStream.WriteBuffer({$IFDEF ENDIAN_BIG}SwapLFH{$ENDIF}(LocalHdr),SizeOf(LocalHdr));
  // Append extensible field header+zip64 extensible field if needed:
  FOutStream.WriteBuffer(ZFileName[1],Length(ZFileName));
  if IsZip64 then
  begin
    FOutStream.WriteBuffer({$IFDEF ENDIAN_BIG}SwapEDFH{$ENDIF}(LocalZip64ExtHdr),SizeOf(LocalZip64ExtHdr));
    FOutStream.WriteBuffer({$IFDEF ENDIAN_BIG}SwapZ64EIF{$ENDIF}(LocalZip64Fld),SizeOf(LocalZip64Fld));
  end;
End;


Procedure TZipper.BuildZipDirectory;
// Write out all central file headers using info from local headers
Var
  SavePos   : Int64;
  HdrPos    : Int64; //offset from disk where file begins to local header
  CenDirPos : Int64;
  ACount    : QWord; //entry counter
  ZFileName : string; //archive filename
  IsZip64   : boolean; //local header=zip64 format?
  MinReqdVersion: word; //minimum needed to extract
  ExtInfoHeader : Extensible_Data_Field_Header_Type;
  Zip64ECD  : Zip64_End_of_Central_Dir_type;
  Zip64ECDL : Zip64_End_of_Central_Dir_Locator_type;
Begin
  ACount := 0;
  MinReqdVersion:=0;
  CenDirPos := FOutStream.Position;
  FOutStream.Seek(0,soBeginning);             { Rewind output file }
  HdrPos := FOutStream.Position;
  FOutStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
  LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
  Repeat
    SetLength(ZFileName,LocalHdr.FileName_Length);
    FOutStream.ReadBuffer(ZFileName[1], LocalHdr.FileName_Length);
    IsZip64:=(LocalHdr.Compressed_Size=$FFFFFFFF) or (LocalHdr.Uncompressed_Size=$FFFFFFFF) or (HdrPos>=$FFFFFFFF);
    FillChar(LocalZip64Fld,SizeOf(LocalZip64Fld),0); // easier to check compressed length
    if LocalHdr.Extra_Field_Length>0 then
      begin
      SavePos := FOutStream.Position;
      if (IsZip64 and (LocalHdr.Extra_Field_Length>=SizeOf(LocalZip64ExtHdr)+SizeOf(LocalZip64Fld))) then
        while FOutStream.Position<SavePos+LocalHdr.Extra_Field_Length do
          begin
          FOutStream.ReadBuffer(ExtInfoHeader, SizeOf(ExtInfoHeader));
        {$IFDEF FPC_BIG_ENDIAN}
          ExtInfoHeader := SwapEDFH(ExtInfoHeader);
        {$ENDIF}
          if ExtInfoHeader.Header_ID=ZIP64_HEADER_ID then
            begin
            FOutStream.ReadBuffer(LocalZip64Fld, SizeOf(LocalZip64Fld));
          {$IFDEF FPC_BIG_ENDIAN}
            LocalZip64Fld := SwapZ64EIF(LocalZip64Fld);
          {$ENDIF}
            end
          else
            begin
            // Read past non-zip64 extra field
            FOutStream.Seek(ExtInfoHeader.Data_Size,soFromCurrent);
            end;
          end;
      // Move past extra fields
      //FOutStream.Seek(SavePos+LocalHdr.Extra_Field_Length,soFromBeginning);
      end;
    SavePos := FOutStream.Position;
    FillChar(CentralHdr,SizeOf(CentralHdr),0);
    With CentralHdr do
      begin
      Signature := CENTRAL_FILE_HEADER_SIGNATURE;
      MadeBy_Version := LocalHdr.Extract_Version_Reqd;
      if (IsZip64) and (MadeBy_Version<45) then
        MadeBy_Version := 45;
    {$IFDEF UNIX}
      {$IFDEF DARWIN} //OSX
      MadeBy_Version := MadeBy_Version or (OS_OSX shl 8);
      {$ELSE}
      MadeBy_Version := MadeBy_Version or (OS_UNIX shl 8);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF OS2}
      MadeBy_Version := MadeBy_Version or (OS_OS2 shl 8);
    {$ENDIF}
      {$warning TODO: find a way to recognize VFAT and NTFS}
      // Copy over extract_version_reqd..extra_field_length
      Move(LocalHdr.Extract_Version_Reqd, Extract_Version_Reqd, 26);
      if (IsZip64) and (Extract_Version_Reqd<45) then
        Extract_Version_Reqd := 45;
      // Keep track of the minimum version required to extract
      // zip file as a whole
      if Extract_Version_Reqd>MinReqdVersion then
        MinReqdVersion:=Extract_Version_Reqd;
      Last_Mod_Time:=localHdr.Last_Mod_Time;
      Last_Mod_Date:=localHdr.Last_Mod_Date;
      File_Comment_Length := 0;
      Starting_Disk_Num := 0;
      Internal_Attributes := 0;
    {$IFDEF UNIX}
      External_Attributes := Entries[ACount].Attributes shl 16;
    {$ELSE}
      External_Attributes := Entries[ACount].Attributes;
    {$ENDIF}
      if HdrPos>=$FFFFFFFF then
      begin
        FZipFileNeedsZip64:=true;
        IsZip64:=true;
        Local_Header_offset := $FFFFFFFF;
        // LocalZip64Fld will be written out as central dir extra field later
        LocalZip64Fld.Relative_Hdr_Offset := HdrPos;
      end
      else
        Local_Header_Offset := HdrPos;
      end;

      if IsZip64 then
      begin
          CentralHdr.Extra_Field_Length:=SizeOf(LocalZip64ExtHdr)+SizeOf(LocalZip64Fld);
      end else CentralHdr.Extra_Field_Length :=0;

    FOutStream.Seek(0,soEnd);
    FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapCFH{$ENDIF}(CentralHdr),SizeOf(CentralHdr));
    FOutStream.WriteBuffer(ZFileName[1],Length(ZFileName));

    if IsZip64 then
      begin
      FOutStream.Seek(0,soEnd);
      FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapEDFH{$ENDIF}(LocalZip64ExtHdr),SizeOf(LocalZip64ExtHdr));
      FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapZ64EIF{$ENDIF}(LocalZip64Fld),SizeOf(LocalZip64Fld));
      end;

    Inc(ACount);
    // Move past compressed file data to next header:
    if LocalZip64Fld.Compressed_Size > 0 then
      FOutStream.Seek(SavePos + LocalZip64Fld.Compressed_Size,soBeginning)
    else
      FOutStream.Seek(SavePos + LocalHdr.Compressed_Size,soBeginning);
    HdrPos:=FOutStream.Position;
    FOutStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
  {$IFDEF FPC_BIG_ENDIAN}
    LocalHdr := SwapLFH(LocalHdr);
  {$ENDIF}
  Until LocalHdr.Signature = CENTRAL_FILE_HEADER_SIGNATURE ;

  FOutStream.Seek(0,soEnd);
  FillChar(EndHdr,SizeOf(EndHdr),0);

  // Write end of central directory record
  // We'll use the zip64 variants to store counts etc
  // and copy to the old record variables if possible
  // This seems to match expected behaviour of unzippers like
  // unrar that only look at the zip64 record
  FillChar(Zip64ECD, SizeOf(Zip64ECD), 0);
  Zip64ECD.Signature:=ZIP64_END_OF_CENTRAL_DIR_SIGNATURE;
  FillChar(Zip64ECDL, SizeOf(Zip64ECDL), 0);
  Zip64ECDL.Signature:=ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE;
  Zip64ECDL.Total_Disks:=1; //default and no support for multi disks yet anyway
  With EndHdr do
    begin
    Signature := END_OF_CENTRAL_DIR_SIGNATURE;
    Disk_Number := 0;
    Central_Dir_Start_Disk := 0;

    Zip64ECD.Entries_This_Disk:=ACount;
    Zip64ECD.Total_Entries:=Acount;
    if ACount>$FFFF then
      begin
      FZipFileNeedsZip64 := true;
      Entries_This_Disk := $FFFF;
      Total_Entries := $FFFF;
      end
    else
      begin
      Entries_This_Disk := Zip64ECD.Entries_This_Disk;
      Total_Entries := Zip64ECD.Total_Entries;
      end;

    Zip64ECD.Central_Dir_Size := FOutStream.Size-CenDirPos;
    if (Zip64ECD.Central_Dir_Size)>$FFFFFFFF then
      begin
      FZipFileNeedsZip64 := true;
      Central_Dir_Size := $FFFFFFFF;
      end
    else
      begin
      Central_Dir_Size := Zip64ECD.Central_Dir_Size;
      end;

    Zip64ECD.Start_Disk_Offset := CenDirPos;
    if Zip64ECD.Start_Disk_Offset>$FFFFFFFF then
      begin
      FZipFileNeedsZip64 := true;
      Start_Disk_Offset := $FFFFFFFF;
      end
    else
      begin
      Start_Disk_Offset := Zip64ECD.Start_Disk_Offset;
      end;

    ZipFile_Comment_Length := Length(FFileComment);

    if FZipFileNeedsZip64 then
    begin
      //Write zip64 end of central directory record if needed
      if MinReqdVersion<45 then
        MinReqdVersion := 45;
      Zip64ECD.Extract_Version_Reqd := MinReqdVersion;
      Zip64ECD.Version_Made_By := MinReqdVersion;
      Zip64ECD.Record_Size := SizeOf(Zip64ECD)-12; //Assumes no variable length field following
      Zip64ECDL.Central_Dir_Zip64_EOCD_Offset := FOutStream.Position;
      Zip64ECDL.Zip64_EOCD_Start_Disk := 0;
      FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapZ64ECD{$ENDIF}(Zip64ECD), SizeOf(Zip64ECD));

      //Write zip64 end of central directory locator if needed
      FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapZ64ECDL{$ENDIF}(Zip64ECDL), SizeOf(Zip64ECDL));
    end;

    FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapECD{$ENDIF}(EndHdr), SizeOf(EndHdr));
    if Length(FFileComment) > 0 then
      FOutStream.WriteBuffer(FFileComment[1],Length(FFileComment));
    end;
end;

Function TZipper.CreateCompressor(Item : TZipFileEntry; AInFile,AZipStream : TStream) : TCompressor;

begin
  Result:=TDeflater.Create(AinFile,AZipStream,FBufSize);
  (Result as TDeflater).CompressionLevel:=Item.CompressionLevel;
  FCurrentCompressor:=Result;
end;

Procedure TZipper.ZipOneFile(Item : TZipFileEntry);

Var
  CRC : LongWord;
  ZMethod : Word;
  ZVersionReqd : Word;
  ZBitFlag : Word;
  ZipStream : TStream;
  TmpFileName, Start : String;
  I : Integer;

Begin
  OpenInput(Item);
  Try
    StartZipFile(Item);
    If (FInfile.Size<=FInMemSize) then
      ZipStream:=TMemoryStream.Create
    else
      begin
      Start := ChangeFileExt(FFileName, '');
      I := 0;
      repeat
        TmpFileName := Format('%s%.5d.tmp', [Start, I]);
        Inc(I);
      until not FileExists(TmpFileName);
      ZipStream:=TFileStream.Create(TmpFileName,fmCreate);
      end;
    Try
      With CreateCompressor(Item, FinFile,ZipStream) do
        Try
          OnProgress:=Self.OnProgress;
          OnPercent:=Self.OnPercent;
          Compress;
          CRC:=Crc32Val;
          ZMethod:=ZipID;
          ZVersionReqd:=ZipVersionReqd;
          ZBitFlag:=ZipBitFlag;
        Finally
          FCurrentCompressor:=Nil;
          Free;
        end;
      If UpdateZipHeader(Item,ZipStream,CRC,ZMethod,ZVersionReqd,ZBitFlag) then
        // Compressed file smaller than original file.
        FOutStream.CopyFrom(ZipStream,0)
      else
        begin
        // Original file smaller than compressed file.
        FInfile.Seek(0,soBeginning);
        FOutStream.CopyFrom(FInFile,0);
        end;
    finally
      ZipStream.Free;
      If (TmpFileName<>'') then
        DeleteFile(TmpFileName);
    end;
  Finally
    CloseInput(Item);
  end;
end;

// Just like SaveToFile, but uses the FileName property
Procedure TZipper.ZipAllFiles;
begin
  SaveToFile(FileName);
end;

procedure TZipper.SaveToFile(const AFileName: RawByteString);
var
  lStream: TFileStream;
begin
  FFileName:=AFileName;
  lStream:=TFileStream.Create(FFileName,fmCreate);
  try
    SaveToStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

procedure TZipper.SaveToStream(AStream: TStream);
Var
  I : integer; //could be qword but limited by FEntries.Count
begin
  FTerminated:=False;
  FOutStream := AStream;
  If CheckEntries=0 then
    Exit;
  FZipping:=True;
  Try
    GetFileInfo; //get info on file entries in zip
    I:=0;
    While (I<FEntries.Count) and not Terminated do
      begin
      ZipOneFile(FEntries[i]);
      Inc(I);
      end;
    if (FEntries.Count>0) and not Terminated then
      BuildZipDirectory;
  finally
    FZipping:=False;
    // Remove entries that have been added by CheckEntries from Files.
    for I:=0 to FFiles.Count-1 do
      FEntries.Delete(FEntries.Count-1);
  end;
end;

procedure TZipper.ZipFile(const aFileToBeZipped: RawByteString);
begin
  ZipFiles([aFileToBeZipped]);
end;

procedure TZipper.ZipFile(const AZipFileName, aFileToBeZipped: RawByteString);
begin
  FileName:=aZipFileName;
  ZipFile(aFileToBeZipped);
end;


Procedure TZipper.SetBufSize(Value : LongWord);

begin
  If FZipping then
    Raise EZipError.Create(SErrBufsizeChange);
  If Value>=DefaultBufSize then
    FBufSize:=Value;
end;

Procedure TZipper.SetFileName(Value : RawByteString);

begin
  If FZipping then
    Raise EZipError.Create(SErrFileChange);
  FFileName:=Value;
end;

Procedure TZipper.ZipFiles(Const AZipFileName : RawByteString; FileList : TStrings);

begin
  FFileName:=AZipFileName;
  ZipFiles(FileList);
end;

procedure TZipper.ZipFiles(const AZipFileName: RawByteString; const FileList: array of RawbyteString);

begin
  FileName:=aZipFileName;
  ZipFiles(FileList);
end;

procedure TZipper.ZipFiles(const aFileList: array of RawbyteString);
Var
  L : TStringList;
  S : RawByteString;
begin
  L:=TStringList.Create;
  try
    L.Capacity:=Length(aFileList);
    for S in aFileList do
      L.Add(S);
    ZipFiles(L);
  finally
    L.Free;
  end;
end;

procedure TZipper.ZipFiles(FileList: TStrings);
begin
  FFiles.Assign(FileList);
  ZipAllFiles;
end;

procedure TZipper.ZipFiles(const AZipFileName: RawByteString; Entries: TZipFileEntries);
begin
  FFileName:=AZipFileName;
  ZipFiles(Entries);
end;

procedure TZipper.ZipFiles(Entries: TZipFileEntries);
begin
  FEntries.Assign(Entries);
  ZipAllFiles;
end;

class procedure TZipper.Zip(const AZipFileName: RawByteString; const aFileToBeZipped: RawByteString);
begin
  With Self.Create do
    try
      ZipFile(aZipFileName,aFileToBeZipped);
    finally
      Free;
    end;
end;

class procedure TZipper.Zip(const AZipFileName: RawByteString; aFileList: array of RawByteString);
begin
  With Self.Create do
    try
      ZipFiles(aZipFileName,aFileList);
    finally
      Free;
    end;
end;

class procedure TZipper.Zip(const AZipFileName: RawByteString; aFileList: TStrings);
begin
  With Self.Create do
    try
      ZipFiles(aZipFileName,aFileList);
    finally
      Free;
    end;
end;

Procedure TZipper.DoEndOfFile;

Var
  ComprPct : Double;

begin
  if (FZipFileNeedsZip64) and (LocalZip64Fld.Original_Size>0) then
    ComprPct := (100.0 * (LocalZip64Fld.Original_size - LocalZip64Fld.Compressed_Size)) / LocalZip64Fld.Original_Size
  else if (LocalHdr.Uncompressed_Size>0) then
    ComprPct := (100.0 * (LocalHdr.Uncompressed_Size - LocalHdr.Compressed_Size)) / LocalHdr.Uncompressed_Size
  else
    ComprPct := 0;
  If Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self,ComprPct);
end;

Constructor TZipper.Create;

begin
  FBufSize:=DefaultBufSize;
  FInMemSize:=DefaultInMemSize;
  FFiles:=TStringList.Create;
  FEntries:=TZipFileEntries.Create(TZipFileEntry);
  FOnPercent:=1;
  FZipFileNeedsZip64:=false;
  LocalZip64ExtHdr.Header_ID:=ZIP64_HEADER_ID;
  LocalZip64ExtHdr.Data_Size:=SizeOf(Zip64_Extended_Info_Field_Type);
end;

Function TZipper.CheckEntries : Integer;

Var
  I : integer; //Could be QWord but limited by FFiles.Count

begin
  for I:=0 to FFiles.Count-1 do
    FEntries.AddFileEntry(FFiles[i]);

  // Use zip64 when number of file entries
  // or individual (un)compressed sizes
  // require it.
  if FEntries.Count >= $FFFF then
    FZipFileNeedsZip64:=true;

  if not(FZipFileNeedsZip64) then
    begin
    for I:=0 to FFiles.Count-1 do
      begin
      if FEntries[i].FNeedsZip64 then
        begin
        FZipFileNeedsZip64:=true;
        break;
        end;
      end;
    end;

  Result:=FEntries.Count;
end;


Procedure TZipper.Clear;

begin
  FEntries.Clear;
  FFiles.Clear;
end;

procedure TZipper.Terminate;
begin
  FTerminated:=True;
  if Assigned(FCurrentCompressor) then
    FCurrentCompressor.Terminate;
end;

Destructor TZipper.Destroy;

begin
  Clear;
  FreeAndNil(FEntries);
  FreeAndNil(FFiles);
  Inherited;
end;

{ ---------------------------------------------------------------------
    TUnZipper
  ---------------------------------------------------------------------}

procedure TUnZipper.OpenInput;

Begin
  if Assigned(FOnOpenInputStream) then
    FOnOpenInputStream(Self, FZipStream);
  if FZipStream = nil then
    FZipStream:=TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite);
End;


function TUnZipper.OpenOutput(OutFileName: RawByteString;
  out OutStream: TStream; Item: TFullZipFileEntry): Boolean;
Var
  Path: RawByteString;
  OldDirectorySeparators: set of char;

Begin
  { the default RTL behavior is broken on Unix platforms
    for Windows compatibility: it allows both '/' and '\'
    as directory separator. We don't want that behavior
    here, since 'abc\' is a valid file name under Unix.
	
    The zip standard appnote.txt says zip files must have '/' as path
    separator, even on Windows: 4.4.17.1:
    "The path stored MUST not contain a drive or device letter, or a leading
    slash. All slashes MUST be forward slashes '/' as opposed to backwards
    slashes '\'" See also mantis issue #15836
    However, old versions of FPC on Windows (and possibly other utilities)
    created incorrect zip files with \ separator, so accept these as well as
    they're not valid in Windows file names anyway.
  }
  OldDirectorySeparators:=AllowDirectorySeparators;
  {$ifdef Windows}
  // Explicitly allow / and \ regardless of what Windows supports
  AllowDirectorySeparators:=['\','/'];
  {$else}
  // Follow the standard: only allow / regardless of actual separator on OS
  AllowDirectorySeparators:=['/'];
  {$endif}
  Path:=ExtractFilePath(OutFileName);
  OutStream:=Nil;
  If Assigned(FOnCreateStream) then
    FOnCreateStream(Self, OutStream, Item);
  // If FOnCreateStream didn't create one, we create one now.
  If (OutStream=Nil) and (not Item.IsDirectory) then
    begin
    if (Path<>'') then
      ForceDirectories(Path);
    AllowDirectorySeparators:=OldDirectorySeparators;
    OutStream:=TFileStream.Create(OutFileName,fmCreate);
	
    end;
	
  AllowDirectorySeparators:=OldDirectorySeparators;
  Result:=True;
  If Assigned(FOnStartFile) then
    FOnStartFile(Self,OutFileName);
End;


procedure TUnZipper.CloseOutput(Item: TFullZipFileEntry; var OutStream: TStream
  );

Begin
  if Assigned(FOnDoneStream) then
  begin
    FOnDoneStream(Self, OutStream, Item);
    OutStream := nil;
  end
  else
    if Assigned(OutStream) then
      FreeAndNil(OutStream);
  DoEndOfFile;
end;


procedure TUnZipper.CloseInput;

Begin
  if Assigned(FOnCloseInputStream) then
    FOnCloseInputStream(Self, FZipStream);
  FreeAndNil(FZipStream);
end;


procedure TUnZipper.ReadZipHeader(Item: TFullZipFileEntry; out AMethod: Word);
Var
  S : RawByteString;
  U : UTF8String;
  D : TDateTime;
  ExtraFieldHdr: Extensible_Data_Field_Header_Type;
  SavePos: int64; //could be qword but limited by stream
  // Infozip unicode path
  Infozip_Unicode_Path_Ver:Byte;
  Infozip_Unicode_Path_CRC32:DWord;
Begin
  FZipStream.Seek(Item.HdrPos,soBeginning);
  FZipStream.ReadBuffer(LocalHdr,SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
  LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
  FillChar(LocalZip64Fld,SizeOf(LocalZip64Fld),0); //ensure no erroneous info
  With LocalHdr do
    begin
      Item.FBitFlags:=Bit_Flag;
      SetLength(S,Filename_Length);
      FZipStream.ReadBuffer(S[1],Filename_Length);
      if Bit_Flag and EFS_LANGUAGE_ENCODING_FLAG <> 0 then
        SetCodePage(S, CP_UTF8, False);
      Item.ArchiveFileName:=S;
      Item.DiskFileName:=S;
      SavePos:=FZipStream.Position; //after filename, before extra fields
      if Extra_Field_Length>0 then
        begin
        SavePos := FZipStream.Position;
        if (LocalHdr.Extra_Field_Length>=SizeOf(ExtraFieldHdr)) then
          while FZipStream.Position<SavePos+LocalHdr.Extra_Field_Length do
            begin
            FZipStream.ReadBuffer(ExtraFieldHdr, SizeOf(ExtraFieldHdr));
          {$IFDEF FPC_BIG_ENDIAN}
            ExtraFieldHdr := SwapEDFH(ExtraFieldHdr);
          {$ENDIF}
            if ExtraFieldHdr.Header_ID=ZIP64_HEADER_ID then
              begin
              FZipStream.ReadBuffer(LocalZip64Fld, SizeOf(LocalZip64Fld));
            {$IFDEF FPC_BIG_ENDIAN}
              LocalZip64Fld := SwapZ64EIF(LocalZip64Fld);
            {$ENDIF}
              end
            // Infozip unicode path
            else if ExtraFieldHdr.Header_ID=INFOZIP_UNICODE_PATH_ID then
              begin
              FZipStream.ReadBuffer(Infozip_Unicode_Path_Ver,1);
              if Infozip_Unicode_Path_Ver=1 then
                begin
                FZipStream.ReadBuffer(Infozip_Unicode_Path_CRC32,sizeof(Infozip_Unicode_Path_CRC32));
                {$IFDEF FPC_BIG_ENDIAN}
                Infozip_Unicode_Path_CRC32:=SwapEndian(Infozip_Unicode_Path_CRC32);
                {$ENDIF}
                if CRC32Str(S)=Infozip_Unicode_Path_CRC32 then
                  begin
                  SetLength(U,ExtraFieldHdr.Data_Size-5);
                  FZipStream.ReadBuffer(U[1],Length(U));
                  Item.UTF8ArchiveFileName:=U;
                  Item.UTF8DiskFileName:=U;
                  end
                else
                  FZipStream.Seek(ExtraFieldHdr.Data_Size-5,soFromCurrent);
                end
              else
                FZipStream.Seek(ExtraFieldHdr.Data_Size-1,soFromCurrent);
              end
            else
              FZipStream.Seek(ExtraFieldHdr.Data_Size,soFromCurrent);
            end;
        // Move past extra fields
        FZipStream.Seek(SavePos+Extra_Field_Length,soFromBeginning);
        end;
      Item.Size:=Uncompressed_Size;
      ZipDateTimeToDateTime(Last_Mod_Date,Last_Mod_Time,D);
      Item.DateTime:=D;
      if Crc32 <> 0 then
        Item.CRC32 := Crc32;
      AMethod:=Compress_method;
    end;
End;

procedure TUnZipper.FindEndHeaders(
  out AEndHdr: End_of_Central_Dir_Type;
  out AEndHdrPos: Int64;
  out AEndZip64Hdr: Zip64_End_of_Central_Dir_type;
  out AEndZip64HdrPos: Int64);
// Reads backwords from the end of the zip file,
// following end of central directory, and, if present
// zip64 end of central directory locator and
// zip64 end of central directory record

// If valid regular end of directory found, AEndHdrPos>0
// If valid zip64 end of directory found, AEndZip64HdrPos>0
var
  EndZip64Locator: Zip64_End_of_Central_Dir_Locator_type;
  procedure SearchForSignature;
  // Search for end of central directory record signature
  // If failed, set AEndHdrPos to 0
  var
    I: Integer;
    Buf: PByte;
    BufSize: Integer;
    result: boolean;
  begin
    result:=false;
    // scan the last (64k + something) bytes for the END_OF_CENTRAL_DIR_SIGNATURE
    // (zip file comments are 64k max).
    BufSize := 65536 + SizeOf(AEndHdr) + 128;
    if FZipStream.Size < BufSize then
      BufSize := FZipStream.Size;

    Buf := GetMem(BufSize);
    try
      FZipStream.Seek(FZipStream.Size - BufSize, soBeginning);
      FZipStream.ReadBuffer(Buf^, BufSize);

      for I := BufSize - SizeOf(AEndHdr) downto 0 do
      begin
        if (Buf[I] or (Buf[I + 1] shl 8) or (Buf[I + 2] shl 16) or (Buf[I + 3] shl 24)) = END_OF_CENTRAL_DIR_SIGNATURE then
        begin
          Move(Buf[I], AEndHdr, SizeOf(AEndHdr));
          {$IFDEF FPC_BIG_ENDIAN}
          AEndHdr := SwapECD(AEndHdr);
          {$ENDIF}
          if (AEndHdr.Signature = END_OF_CENTRAL_DIR_SIGNATURE) and
             (I + SizeOf(AEndHdr) + AEndHdr.ZipFile_Comment_Length <= BufSize) then
          begin
            AEndHdrPos := FZipStream.Size - BufSize + I;
            FZipStream.Seek(AEndHdrPos + SizeOf(AEndHdr), soBeginning);
            SetLength(FFileComment, AEndHdr.ZipFile_Comment_Length);
            FZipStream.ReadBuffer(FFileComment[1], Length(FFileComment));
            result:=true; //found it
            break;
          end;
        end;
      end;
    finally
      FreeMem(Buf);
    end;
    if not(result) then
    begin
      AEndHdrPos := 0;
      FillChar(AEndHdr, SizeOf(AEndHdr), 0);
    end;
  end;

  procedure ZeroData;
  begin
    AEndHdrPos := 0;
    FillChar(AEndHdr, SizeOf(AEndHdr), 0);
    AEndZip64HdrPos:=0;
    FillChar(AEndZip64Hdr, SizeOf(AEndZip64Hdr), 0);
  end;

begin
  // Zip64 records may not exist, so fill out default values
  FillChar(AEndZip64Hdr,SizeOf(AEndZip64Hdr), 0);
  AEndZip64HdrPos:=0;
  // Look for end of central directory record from
  // back of file based on signature (only way due to
  // variable length zip comment etc)
  FFileComment := '';
  // Zip file requires end of central dir header so
  // is corrupt if it is smaller than that
  if FZipStream.Size < SizeOf(AEndHdr) then
  begin
    ZeroData;
    exit;
  end;

  AEndHdrPos := FZipStream.Size - SizeOf(AEndHdr);
  FZipStream.Seek(AEndHdrPos, soBeginning);
  FZipStream.ReadBuffer(AEndHdr, SizeOf(AEndHdr));
  {$IFDEF FPC_BIG_ENDIAN}
  AEndHdr := SwapECD(AEndHdr);
  {$ENDIF}
  // Search unless record is right at the end of the file:
  if (AEndHdr.Signature <> END_OF_CENTRAL_DIR_SIGNATURE) or
     (AEndHdr.ZipFile_Comment_Length <> 0) then
    SearchForSignature;
  if AEndHdrPos=0 then
  begin
    ZeroData;
    exit;
  end;

  // With a valid end of dir record, see if there's zip64
  // fields:
  FZipStream.Seek(AEndHdrPos-SizeOf(Zip64_End_of_Central_Dir_Locator_type),soBeginning);
  FZipStream.ReadBuffer(EndZip64Locator, SizeOf(EndZip64Locator));
  {$IFDEF FPC_BIG_ENDIAN}
  EndZip64Locator := SwapZ64ECDL(EndZip64Locator);
  {$ENDIF}
  if EndZip64Locator.Signature=ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE then
  begin
    //Read EndZip64Locator.Total_Disks when implementing multiple disks support
    if EndZip64Locator.Central_Dir_Zip64_EOCD_Offset>High(Int64) then
      raise EZipError.CreateFmt(SErrPosTooLarge,[EndZip64Locator.Central_Dir_Zip64_EOCD_Offset,High(Int64)]);
    AEndZip64HdrPos:=EndZip64Locator.Central_Dir_Zip64_EOCD_Offset;
    FZipStream.Seek(AEndZip64HdrPos, soBeginning);
    FZipStream.ReadBuffer(AEndZip64Hdr, SizeOf(AEndZip64Hdr));
    {$IFDEF FPC_BIG_ENDIAN}
    AEndZip64Hdr := SwapZ64ECD(AEndZip64Hdr);
    {$ENDIF}
    if AEndZip64Hdr.Signature<>ZIP64_END_OF_CENTRAL_DIR_SIGNATURE then
    begin
      //Corrupt header
      ZeroData;
      Exit;
    end;
  end
  else
  begin
    // No zip64 data, so follow the offset in the end of central directory record
    AEndZip64HdrPos:=0;
    FillChar(AEndZip64Hdr, SizeOf(AEndZip64Hdr), 0);
  end;
end;

procedure TUnZipper.ReadZipDirectory;

Var
  EndHdr      : End_of_Central_Dir_Type;
  EndZip64Hdr : Zip64_End_of_Central_Dir_type;
  i : integer; //could be Qword but limited to number of items in collection
  EndHdrPos,
  EndZip64HdrPos,
  CenDirPos,
  SavePos   : Int64; //could be QWord but limited to stream maximums
  ExtraFieldHeader : Extensible_Data_Field_Header_Type;
  EntriesThisDisk : QWord;
  Zip64Field: Zip64_Extended_Info_Field_Type;
  NewNode   : TFullZipFileEntry;
  D : TDateTime;
  S : RawByteString;
  U : UTF8String;
  // infozip unicode path
  Infozip_unicode_path_ver : byte; // always 1
  Infozip_unicode_path_crc32 : DWord;
Begin
  FindEndHeaders(EndHdr, EndHdrPos,
    EndZip64Hdr, EndZip64HdrPos);
  if EndHdrPos=0 then
    raise EZipError.CreateFmt(SErrCorruptZIP,[FileName]);
  if (EndZip64HdrPos>0) and (EndZip64Hdr.Start_Disk_Offset>0) then
    begin
    if EndZip64Hdr.Start_Disk_Offset>High(Int64) then
      raise EZipError.CreateFmt(SErrPosTooLarge,[EndZip64Hdr.Start_Disk_Offset,High(Int64)]);
    CenDirPos := EndZip64Hdr.Start_Disk_Offset;
    end
  else
    CenDirPos := EndHdr.Start_Disk_Offset;
  FZipStream.Seek(CenDirPos,soBeginning);
  FEntries.Clear;
  if (EndZip64HdrPos>0) and (EndZip64Hdr.Entries_This_Disk>0) then
  begin
    EntriesThisDisk := EndZip64Hdr.Entries_This_Disk;
    if EntriesThisDisk<>EndZip64Hdr.Total_Entries then
      raise EZipError.Create(SErrUnsupportedMultipleDisksCD);
  end
  else
  begin
    EntriesThisDisk :=EndHdr.Entries_This_Disk;
    if EntriesThisDisk<>EndHdr.Total_Entries then
      raise EZipError.Create(SErrUnsupportedMultipleDisksCD);
  end;

  // Entries are added to a collection. The max number of items
  // in a collection limits the entries we can process.
  if EntriesThisDisk>MaxInt then
    raise EZipError.CreateFmt(SErrMaxEntries,[EntriesThisDisk,MaxInt]);

  // Using while instead of for loop so qword can be used on 32 bit as well.
  for i:=0 to EntriesThisDisk-1 do
    begin
    FZipStream.ReadBuffer(CentralHdr, SizeOf(CentralHdr));
{$IFDEF FPC_BIG_ENDIAN}
    CentralHdr := SwapCFH(CentralHdr);
{$ENDIF}
    With CentralHdr do
      begin
      if Signature<>CENTRAL_FILE_HEADER_SIGNATURE then
        raise EZipError.CreateFmt(SErrCorruptZIP,[FileName]);
      NewNode:=FEntries.Add as TFullZipFileEntry;
      // Header position will be corrected later with zip64 version, if needed..
      NewNode.HdrPos := Local_Header_Offset;
      NewNode.FBitFlags:=Bit_Flag;
      SetLength(S,Filename_Length);
      FZipStream.ReadBuffer(S[1],Filename_Length);
      if Bit_Flag and EFS_LANGUAGE_ENCODING_FLAG <> 0 then
        SetCodePage(S, CP_UTF8, False);
      SavePos:=FZipStream.Position; //After fixed part of central directory...
      // and the filename; before any extra field(s)
      NewNode.ArchiveFileName:=S;
      // Size/compressed size will be adjusted by zip64 entries if needed...
      NewNode.Size:=Uncompressed_Size;
      NewNode.FCompressedSize:=Compressed_Size;
      NewNode.CRC32:=CRC32;
      NewNode.OS := MadeBy_Version shr 8;
      if NewNode.OS = OS_UNIX then
        NewNode.Attributes := External_Attributes shr 16
      else
        NewNode.Attributes := External_Attributes;
      ZipDateTimeToDateTime(Last_Mod_Date,Last_Mod_Time,D);
      NewNode.DateTime:=D;

      // Go through any extra fields and extract any zip64 info
      if Extra_Field_Length>0 then
        begin
        while (FZipStream.Position<SavePos+Extra_Field_Length) do
          begin
          FZipStream.ReadBuffer(ExtraFieldHeader, SizeOf(ExtraFieldHeader));
        {$IFDEF FPC_BIG_ENDIAN}
          ExtraFieldHeader := SwapEDFH(ExtraFieldHeader);
        {$ENDIF}
          if ExtraFieldHeader.Header_ID = ZIP64_HEADER_ID then
            begin
            FZipStream.ReadBuffer(Zip64Field, SizeOf(Zip64Field));
          {$IFDEF FPC_BIG_ENDIAN}
            Zip64Field := SwapZ64EIF(Zip64Field);
          {$ENDIF}
            if Zip64Field.Compressed_Size > 0 then
              NewNode.FCompressedSize := Zip64Field.Compressed_Size;
            if Zip64Field.Original_Size>0 then
              NewNode.Size := Zip64Field.Original_Size;
            if Zip64Field.Relative_Hdr_Offset<>0 then
              begin
              if Zip64Field.Relative_Hdr_Offset>High(Int64) then
                raise EZipError.CreateFmt(SErrPosTooLarge,[Zip64Field.Relative_Hdr_Offset,High(Int64)]);
              NewNode.HdrPos := Zip64Field.Relative_Hdr_Offset;
              end;
            end
            // infozip unicode path extra field
          else if ExtraFieldHeader.Header_ID = INFOZIP_UNICODE_PATH_ID then
            begin
            FZipStream.ReadBuffer(Infozip_unicode_path_ver,1);
            if Infozip_unicode_path_ver=1 then
              begin
              FZipStream.ReadBuffer(Infozip_unicode_path_crc32,sizeof(Infozip_unicode_path_crc32));
              {$IFDEF FPC_BIG_ENDIAN}
              Infozip_unicode_path_crc32:=SwapEndian(Infozip_unicode_path_crc32);
              {$ENDIF}
              if CRC32Str(S)=Infozip_unicode_path_crc32 then
                begin
                SetLength(U,ExtraFieldHeader.Data_Size-5);
				FZipStream.ReadBuffer(U[1],Length(U));
                NewNode.UTF8ArchiveFileName:=U;
                end
              else
                FZipStream.Seek(ExtraFieldHeader.Data_Size-5,soFromCurrent);
              end
            else
              FZipStream.Seek(ExtraFieldHeader.Data_Size-1,soFromCurrent);
            end
          else
            begin
              // Read past non-Zip64 extra field
              FZipStream.Seek(ExtraFieldHeader.Data_Size,soFromCurrent);
            end;
          end;
        end;
      // Move past extra fields and file comment to next header
      if File_Comment_Length > 0 then
          FZipStream.Seek(File_Comment_Length,soFromCurrent);
      // this doesn't work properly when zip file size is over 4Gb, so commented off
      //FZipStream.Seek(SavePos+Extra_Field_Length+File_Comment_Length,soFromBeginning);
      end;
    end;
end;

function TUnZipper.CreateDeCompressor(Item: TZipFileEntry; AMethod: Word;
  AZipFile, AOutFile: TStream): TDeCompressor;
begin
  case AMethod of
    8 :
      Result:=TInflater.Create(AZipFile,AOutFile,FBufSize);
  else
    raise EZipError.CreateFmt(SErrUnsupportedCompressionFormat,[AMethod]);
  end;
  FCurrentDecompressor:=Result;
end;

procedure TUnZipper.UnZipOneFile(Item: TFullZipFileEntry);

Var
  ZMethod : Word;
{$ifdef unix}
  LinkTargetStream: TStringStream;
{$endif}
  OutputFileName: RawByteString;
  FOutStream: TStream;
  IsLink: Boolean;
  IsCustomStream: Boolean;
  U : UnicodeString;

  Procedure SetAttributes;
  Var
    Attrs : Longint;
  begin
    // set attributes
    FileSetDate(OutputFileName, DateTimeToFileDate(Item.DateTime));
    if (Item.Attributes <> 0) then
      begin
      Attrs := 0;
      {$IFDEF UNIX}
      if (Item.OS in [OS_UNIX,OS_OSX]) then Attrs := Item.Attributes;
      if (Item.OS in [OS_FAT,OS_NTFS,OS_OS2,OS_VFAT]) then
        Attrs := ZipFatAttrsToUnixAttrs(Item.Attributes);
      {$ELSE}
      if (Item.OS in [OS_FAT,OS_NTFS,OS_OS2,OS_VFAT]) then Attrs := Item.Attributes;
      if (Item.OS in [OS_UNIX,OS_OSX]) then
        Attrs := ZipUnixAttrsToFatAttrs(ExtractFileName(Item.ArchiveFileName), Item.Attributes);
      {$ENDIF}
      if Attrs <> 0 then
        begin
        {$IFDEF UNIX}
        FpChmod(OutputFileName, Attrs);
        {$ELSE}
        FileSetAttr(OutputFileName, Attrs);
        {$ENDIF}
        end;
      end;
  end;

  procedure DoUnzip(const Dest: TStream);

  begin
    if ZMethod=0 then
      begin
      if (LocalHdr.Compressed_Size<>0) then
        begin
        if LocalZip64Fld.Compressed_Size>0 then
          Dest.CopyFrom(FZipStream,LocalZip64Fld.Compressed_Size)
        else
          Dest.CopyFrom(FZipStream,LocalHdr.Compressed_Size);
        {$warning TODO: Implement CRC Check}
        end;
      end
    else
      With CreateDecompressor(Item, ZMethod, FZipStream, Dest) do
        Try
          FTotPos := Self.FTotPos;
          FTotSize := Self.FTotSize;
          OnProgress:=Self.OnProgress;
          OnProgressEx := Self.OnProgressEx;
          OnPercent:=Self.OnPercent;
          OnProgress:=Self.OnProgress;
          OnPercent:=Self.OnPercent;
          DeCompress;
          Self.FTotPos := FTotPos;
          if Item.CRC32 <> Crc32Val then
            raise EZipError.CreateFmt(SErrInvalidCRC,[Item.ArchiveFileName]);
        Finally
          FCurrentDecompressor:=Nil;
          Free;
        end;
  end;

  Procedure GetOutputFileName;

  Var
    I : Integer;

  begin
    if Not UseUTF8 then
      OutputFileName:=StringReplace(Item.DiskFileName,'/',DirectorySeparator,[rfReplaceAll])
    else
      begin
      // Sets codepage.
      OutputFileName:=Item.UTF8DiskFileName;
      U:=UTF8Decode(OutputFileName);
      // Do not use stringreplace, it will mess up the codepage.
      if '/'<>DirectorySeparator then
        For I:=1 to Length(U) do
          if U[i]='/' then
            U[i]:=DirectorySeparator;
      OutputFileName:=UTF8Encode(U);
      end;
    if (Not IsCustomStream) then
      begin
      if Flat then
        OutputFileName:=ExtractFileName(OutputFileName);
      if (FOutputPath<>'') then
        begin
        // Do not use IncludeTrailingPathdelimiter
        OutputFileName:=FOutputPath+OutputFileName;
        end;
      end;
  end;

Begin
  ReadZipHeader(Item, ZMethod);
  if (Item.BitFlags and 1)<>0 then
    Raise EZipError.CreateFmt(SErrEncryptionNotSupported,[Item.ArchiveFileName]);
  if (Item.BitFlags and (1 shl 5))<>0 then
    Raise EZipError.CreateFmt(SErrPatchSetNotSupported,[Item.ArchiveFileName]);
  // Normalize output filename to conventions of target platform.
  // Zip file always has / path separators
  IsCustomStream := Assigned(FOnCreateStream);
  GetOutputFileName;
  IsLink := Item.IsLink;
{$IFNDEF UNIX}
  if IsLink and Not IsCustomStream then
    begin
    {$warning TODO: Implement symbolic link creation for non-unix, e.g.
    Windows NTFS}
    IsLink := False;
    end;
{$ENDIF}
  if IsCustomStream then
    begin
    try
      OpenOutput(OutputFileName, FOutStream, Item);
      if (IsLink = False) and (Item.IsDirectory = False) then
        DoUnzip(FOutStream);
    Finally
      CloseOutput(Item, FOutStream);
    end;
    end
  else
    begin
    if IsLink then
      begin
      {$IFDEF UNIX}
        LinkTargetStream := TStringStream.Create('');
        try
          DoUnzip(LinkTargetStream);
          fpSymlink(PChar(LinkTargetStream.DataString), PChar(OutputFileName));
        finally
          LinkTargetStream.Free;
        end;
      {$ENDIF}
      end
    else if Item.IsDirectory then
      begin
        if (NOT Flat) then ForceDirectories(OutputFileName);
      end
    else
      begin
      try
        OpenOutput(OutputFileName, FOutStream, Item);
        DoUnzip(FOutStream);
      Finally
        CloseOutput(Item, FOutStream);
      end;
      end;
    SetAttributes;
    end;
end;

Function TUnZipper.IsMatch(I : TFullZipFileEntry) : Boolean;

begin
  if UseUTF8 then
    Result:=(FFiles.IndexOf(I.UTF8ArchiveFileName)<>-1)
  else
    Result:=(FFiles.IndexOf(I.ArchiveFileName)<>-1)
end;

Function TUnZipper.CalcTotalSize(AllFiles : Boolean) : Int64;

Var
  I : Integer;
  Item : TFullZipFileEntry;

begin
  Result:=0;
  for i:=0 to FEntries.Count-1 do
    begin
    Item := FEntries[i];
    if AllFiles or IsMatch(Item) then
      Result := Result + TZipFileEntry(Item).Size;
    end;
end;

procedure TUnZipper.UnZipAllFiles;


Var
  Item : TFullZipFileEntry;
  I : integer; //Really QWord but limited to FEntries.Count
  AllFiles : Boolean;

Begin
  FTerminated:=False;
  FUnZipping:=True;
  Try
    AllFiles:=(FFiles.Count=0);
    OpenInput;
    Try
      ReadZipDirectory;
      FTotPos := 0;
      FTotSize := CalcTotalSize(AllFiles);
      i:=0;
      While (I<FEntries.Count) and not Terminated do
        begin
        Item:=FEntries[i];
        if AllFiles or IsMatch(Item) then
          UnZipOneFile(Item);
        inc(I);
        end;
      if Assigned(FOnProgressEx) and not Terminated then
        FOnProgressEx(Self, FTotPos, FTotSize);
    Finally
      CloseInput;
    end;
  finally
    FUnZipping:=False;
  end;
end;


procedure TUnZipper.SetBufSize(Value: LongWord);

begin
  If FUnZipping then
    Raise EZipError.Create(SErrBufsizeChange);
  If Value>=DefaultBufSize then
    FBufSize:=Value;
end;

procedure TUnZipper.SetFileName(Value: RawByteString);

begin
  If FUnZipping then
    Raise EZipError.Create(SErrFileChange);
  FFileName:=Value;
end;

procedure TUnZipper.SetOutputPath(Value: RawByteString);

Var
  DS : RawByteString;

begin
  If FUnZipping then
    Raise EZipError.Create(SErrFileChange);
  FOutputPath:=Value;
  If (FOutputPath<>'') and (FoutputPath[Length(FoutputPath)]<>DirectorySeparator) then
    begin
    // Preserve codepage of outputpath
    DS:=DirectorySeparator;
    SetCodePage(DS,StringCodePage(FoutputPath),False);
    FOutputPath:=FoutputPath+DS;
    end;
end;

procedure TUnZipper.UnZipFile(const aExtractFileName: RawByteString);
begin
  UnzipFile(FFileName, aExtractFileName);
end;

procedure TUnZipper.UnZipFile(const AZipFileName, aExtractFileName: RawByteString);
var
  L: TStrings;
begin
  FFileName := AZipFileName;
  L := TStringList.Create;
  try
    L.Add(aExtractFileName);
    UnzipFiles(L);
  finally
    L.Free;
  end;
end;

procedure TUnZipper.UnZipFiles(const AZipFileName: RawByteString; FileList: TStrings);

begin
  FFileName:=AZipFileName;
  UnZipFiles(FileList);
end;

procedure TUnZipper.UnZipFiles(const AZipFileName: RawByteString; aFileList: array of RawBytestring);

Var
  L : TStringList;
  S : RawByteString;

begin
  L:=TStringList.Create;
  try
    L.Capacity:=Length(aFileList);
    for S in aFileList do
      L.Add(S);
    UnZipFiles(aZipFileName,L);
  finally
    L.Free;
  end;
end;

procedure TUnZipper.UnZipFiles(aFileList: TStrings);
begin
  FFiles.Assign(aFileList);
  UnZipAllFiles;
end;

procedure TUnZipper.UnZipAllFiles(const AZipFileName: RawByteString);

begin
  FFileName:=AZipFileName;
  UnZipAllFiles;
end;

class procedure TUnZipper.Unzip(const AZipFileName: RawByteString);

begin
  With Self.Create do
    try
      FileName:=aZipFileName;
      UnZipAllFiles;
    finally
      Free;
    end;
end;

class procedure TUnZipper.Unzip(const AZipFileName: RawByteString; aExtractFileName: RawByteString);

begin
  With Self.Create do
    try
      UnZipFile(aZipFileName,aExtractFileName);
    finally
      Free;
    end;
end;

Type

  { TCustomExtractor }

  TCustomExtractor = Class(TObject)
  Private
    FStream : TStream;
    FunZipper  : TUnzipper;
    procedure DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  Public
    Constructor Create(aUnZipper : TUnzipper);
    Destructor Destroy; override;
    Procedure UnZip(const AZipFileName, aExtractFileName: RawByteString; aOutputFileName: string);
  end;

{ TCustomExtractor }

procedure TCustomExtractor.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  aStream:=FStream;
  FStream:=Nil;
end;

constructor TCustomExtractor.Create(aUnZipper: TUnzipper);
begin
  FStream:=Nil;
  FUnzipper:=aUnzipper;
end;

destructor TCustomExtractor.Destroy;
begin
  FreeAndNil(FUnZipper);
  FreeAndNil(FStream);
  Inherited;
end;

procedure TCustomExtractor.UnZip(const AZipFileName, aExtractFileName: RawByteString; aOutputFileName: string);
begin
  FStream:=TFileStream.Create(aOutputFileName,fmCreate);
  FUnZipper.OnCreateStream:=@DoCreateStream;
  FUnzipper.UnzipFile(aZipFileName,aExtractFileName);
end;

class procedure TUnZipper.UnZip(const AZipFileName, aExtractFileName: RawByteString; aOutputFileName: string);



begin
  With TCustomExtractor.Create(Self.Create) do
    try
      Unzip(aZipFileName,aExtractFileName,aOutputFileName);
    Finally
      Free;
    end;
end;

class procedure TUnZipper.Unzip(const AZipFileName: RawByteString; aFileList: array of RawByteString);
begin
  With Self.Create do
    try
      UnZipFiles(aZipFileName,aFileList);
    finally
      Free;
    end;
end;

class procedure TUnZipper.Unzip(const AZipFileName: RawByteString; aFileList: TStrings);
begin
  With Self.Create do
    try
      UnZipFiles(aZipFileName,aFileList);
    finally
      Free;
    end;
end;

class procedure TUnZipper.Unzip(const AZipFileName: RawByteString; aFileList: array of RawByteString; aOutputDir: RawByteString;
  aFlat: Boolean);
begin
  With Self.Create do
    try
      Flat:=aFlat;
      OutputPath:=aOutputDir;
      UnZipFiles(aZipFileName,aFileList);
    finally
      Free;
    end;
end;

class procedure TUnZipper.Unzip(const AZipFileName: RawByteString; aFileList: TStrings; aOutputDir: RawByteString; aFlat: Boolean);
begin
  With Self.Create do
    try
      Flat:=aFlat;
      OutputPath:=aOutputDir;
      UnZipFiles(aZipFileName,aFileList);
    finally
      Free;
    end;
end;

procedure TUnZipper.DoEndOfFile;

Var
  ComprPct : Double;
  Uncompressed: QWord;
  Compressed: QWord;
begin
  If LocalZip64Fld.Original_Size > 0 then
    Uncompressed := LocalZip64Fld.Original_Size
  else
    Uncompressed := LocalHdr.Uncompressed_Size;

  If LocalZip64Fld.Compressed_Size > 0 then
    Compressed := LocalZip64Fld.Compressed_Size
  else
    Compressed := LocalHdr.Compressed_Size;

  If (Compressed>0) and (Uncompressed>0) then
    if (Compressed>Uncompressed) then
      ComprPct := (-100.0 * (Compressed - Uncompressed)) / Uncompressed
    else
      ComprPct := (100.0 * (Uncompressed - Compressed)) / Uncompressed
  else
    ComprPct := 0;
  If Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self,ComprPct);
end;

constructor TUnZipper.Create;

begin
  FBufSize:=DefaultBufSize;
  FFiles:=TStringList.Create;
  TStringlist(FFiles).Sorted:=True;
  FEntries:=TFullZipFileEntries.Create(TFullZipFileEntry);
  FOnPercent:=1;
end;

procedure TUnZipper.Clear;

begin
  FFiles.Clear;
  FEntries.Clear;
end;

procedure TUnZipper.Examine;
begin
  if (FOnOpenInputStream = nil) and (FFileName='') then
    Raise EZipError.Create(SErrNoFileName);
  OpenInput;
  If (FZipStream=nil) then
    Raise EZipError.Create(SErrNoStream);
  Try
    ReadZipDirectory;
  Finally
    CloseInput;
  end;
end;

procedure TUnZipper.Terminate;
begin
  FTerminated:=True;
  if Assigned(FCurrentDecompressor) then
    FCurrentDecompressor.Terminate;
end;

destructor TUnZipper.Destroy;

begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FEntries);
  Inherited;
end;

{ TZipFileEntry }

function TZipFileEntry.GetArchiveFileName: String;
begin
  Result:=FArchiveFileName;
  If (Result='') then
    Result:=FDiskFileName;
end;

function TZipFileEntry.GetUTF8ArchiveFileName: UTF8String;
begin
  Result:=FUTF8FileName;
  If Result='' then
    Result:=ArchiveFileName;
end;

function TZipFileEntry.GetUTF8DiskFileName: UTF8String;
begin
  Result:=FUTF8DiskFileName;
  If Result='' then
    Result:=DiskFileName;
end;

constructor TZipFileEntry.Create(ACollection: TCollection);

begin
{$IFDEF UNIX}
  FOS := OS_UNIX;
{$ELSE}
  FOS := OS_FAT;
{$ENDIF}
  FCompressionLevel:=cldefault;
  FDateTime:=now;
  FNeedsZip64:=false;
  FAttributes:=0;

  inherited create(ACollection);
end;

function TZipFileEntry.IsDirectory: Boolean;
begin
  Result := (DiskFileName <> '') and (DiskFileName[Length(DiskFileName)] = DirectorySeparator);
  if Attributes <> 0 then
  begin
    case OS of
      OS_FAT: Result := (faDirectory and Attributes) > 0;
      OS_UNIX: Result := (Attributes and UNIX_MASK) = UNIX_DIR;
    end;
  end;
end;

function TZipFileEntry.IsLink: Boolean;
begin
  Result := False;
  if Attributes <> 0 then
  begin
    case OS of
      OS_FAT: Result := (faSymLink and Attributes) > 0;
      OS_UNIX: Result := (Attributes and UNIX_MASK) = UNIX_LINK;
    end;
  end;
end;

procedure TZipFileEntry.SetArchiveFileName(const AValue: String);

begin
  if FArchiveFileName=AValue then Exit;
  // Zip standard: filenames inside the zip archive have / path separator
  if DirectorySeparator='/' then
    FArchiveFileName:=AValue
  else
    FArchiveFileName:=StringReplace(AValue, DirectorySeparator, '/', [rfReplaceAll]);
end;

procedure TZipFileEntry.SetDiskFileName(const AValue: String);
begin
  if FDiskFileName=AValue then Exit;
  // Zip file uses / as directory separator on all platforms
  // so convert to separator used on current OS
  if DirectorySeparator='/' then
    FDiskFileName:=AValue
  else
    FDiskFileName:=StringReplace(AValue,'/',DirectorySeparator,[rfReplaceAll]);
end;

procedure TZipFileEntry.SetUTF8ArchiveFileName(AValue: UTF8String);
begin
  FUTF8FileName:=AValue;
  If ArchiveFileName='' then
    if DefaultSystemCodePage<>CP_UTF8 then
      ArchiveFileName:=Utf8ToAnsi(AValue)
    else
      ArchiveFileName:=AValue;
end;

procedure TZipFileEntry.SetUTF8DiskFileName(AValue: UTF8String);
begin
  FUTF8DiskFileName:=AValue;
  If DiskFileName='' then
    if DefaultRTLFileSystemCodePage<>CP_UTF8 then
      DiskFileName:=Utf8ToAnsi(AValue)
    else
      DiskFileName:=AValue;
end;


procedure TZipFileEntry.Assign(Source: TPersistent);

Var
  Z : TZipFileEntry;

begin
  if Source is TZipFileEntry then
    begin
    Z:=Source as TZipFileEntry;
    FArchiveFileName:=Z.FArchiveFileName;
    FDiskFileName:=Z.FDiskFileName;
    FSize:=Z.FSize;
    FDateTime:=Z.FDateTime;
    FStream:=Z.FStream;
    FOS:=Z.OS;
    FAttributes:=Z.Attributes;
    end
  else
    inherited Assign(Source);
end;

{ TZipFileEntries }

function TZipFileEntries.GetZ(AIndex : Integer): TZipFileEntry;
begin
  Result:=TZipFileEntry(Items[AIndex]);
end;

procedure TZipFileEntries.SetZ(AIndex : Integer; const AValue: TZipFileEntry);
begin
  Items[AIndex]:=AValue;
end;

function TZipFileEntries.AddFileEntry(const ADiskFileName: String): TZipFileEntry;
begin
  Result:=Add as TZipFileEntry;
  Result.DiskFileName:=ADiskFileName;
end;

function TZipFileEntries.AddFileEntry(const ADiskFileName,
  AArchiveFileName: String): TZipFileEntry;
begin
  Result:=AddFileEntry(ADiskFileName);
  Result.ArchiveFileName:=AArchiveFileName;
end;

function TZipFileEntries.AddFileEntry(const AStream: TSTream;
  const AArchiveFileName: String): TZipFileEntry;
begin
  Result:=Add as TZipFileEntry;
  Result.Stream:=AStream;
  Result.ArchiveFileName:=AArchiveFileName;
end;

Procedure TZipFileEntries.AddFileEntries(Const List : TStrings);

Var
  I : integer;

begin
  For I:=0 to List.Count-1 do
    AddFileEntry(List[i]);
end;

{ TFullZipFileEntries }

function TFullZipFileEntries.GetFZ(AIndex : Integer): TFullZipFileEntry;
begin
  Result:=TFullZipFileEntry(Items[AIndex]);
end;

procedure TFullZipFileEntries.SetFZ(AIndex : Integer;
  const AValue: TFullZipFileEntry);
begin
  Items[AIndex]:=AValue;
end;

End.
