program MiniZip;

{ minizip demo package by Gilles Vollant

  Usage : minizip [-o] file.zip [files_to_add]

  a file.zip file is created, all files listed in [files_to_add] are added
  to the new .zip file.
  -o an existing .zip file with be overwritten without warning

  Pascal tranlastion
  Copyright (C) 2000 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

uses
  SysUtils, zlib, ctypes, ziputils, zip;

const
  WRITEBUFFERSIZE = Z_BUFSIZE;
  MAXFILENAME     = Z_MAXFILENAMEINZIP;

  function filetime(f: PChar;               { name of file to get info on }
    var tmzip: tm_zip): cuLong;             { return value: access, modific. and creation times }
  var
    dtrec: TDateTime;    { For Pack/UnpackTime}
    stime: TSystemTime;
  begin
    if not FileExists(f) then exit;
    
    dtrec := FileDateToDateTime(FileAge(f));
    DateTimeToSystemTime(dtrec, stime);
    tmzip.tm_sec  := stime.second;
    tmzip.tm_min  := stime.minute;
    tmzip.tm_hour := stime.hour;
    tmzip.tm_mday := stime.day;
    tmzip.tm_mon  := stime.month;
    tmzip.tm_year := stime.year;
    filetime      := 0;
  end;

  procedure do_banner;
  begin
    WriteLn('MiniZip 0.15, demo package written by Gilles Vollant');
    WriteLn('Pascal port by Jacques Nomssi Nzali');
    WriteLn('more info at http://www.tu-chemnitz.de/~nomssi/paszlib.html');
    WriteLn;
  end;

  procedure do_help;
  begin
    WriteLn('Usage : minizip [-o] file.zip [files_to_add]');
    WriteLn;
  end;

  function main: longint;
  var
    answer: string[128];
    argstr: string;
    buf:    pointer;
    c:      char;
    dot_found: longint;
    err:    longint;
    errclose: longint;
    filenameinzip: PChar;
    filename_try: array[0..MAXFILENAME - 1] of char;
    fin:    FILEptr;
    i:      longint;
    len:    longint;
    opt_compress_level: longint;
    opt_overwrite: longint;
    p:      PChar;
    rep:    char;
    size_buf: longint;
    size_read: longint;
    zf:     zipFile;
    zi:     zip_fileinfo;
    zipfilenamearg: longint;
    zipok:  longint;
  begin
    opt_overwrite := 0;
    opt_compress_level := Z_DEFAULT_COMPRESSION;
    zipfilenamearg := 0;
    err  := 0;
    main := 0;

    do_banner;
    if (ParamCount = 0) then
    begin
      do_help;
      main := 0;
      exit;
    end
    else
      for i := 1 to ParamCount - 1 + 1 do
      begin
        argstr := ParamStr(i) + #0;
        if (argstr[1] = '-') then
        begin
          p := @argstr[1 + 1];       {const char *p=argv[i]+1;}

          while (p^ <> #0) do
          begin
            c := p^;
            Inc(p);
            if (c = 'o') or (c = 'O') then
              opt_overwrite := 1;
            if (c >= '0') and (c <= '9') then
              opt_compress_level := byte(c) - byte('0');
          end;
        end
        else
        if (zipfilenamearg = 0) then
          zipfilenamearg := i;
      end;

    size_buf := WRITEBUFFERSIZE;
    buf      := AllocMem(size_buf);
    if (buf = nil) then
    begin
      WriteLn('Error allocating memory');
      main := ZIP_INTERNALERROR;
      exit;
    end;

    if (zipfilenamearg = 0) then
      zipok := 0
    else
    begin
      dot_found := 0;

      zipok  := 1;
      argstr := ParamStr(zipfilenamearg) + #0;
      strcopy(filename_try, PChar(@argstr[1]));
      len := strlen(filename_try);
      for i := 0 to len - 1 do
        if (filename_try[i] = '.') then
          dot_found := 1;

      if (dot_found = 0) then
        strcat(filename_try, '.zip');

      if (opt_overwrite = 0) then
        if FileExists(filename_try) then
        begin
          repeat
            WriteLn('The file ', filename_try,
              ' exist. Overwrite ? [y]es, [n]o : ');
            ReadLn(answer);
            rep := answer[1];
            if (rep >= 'a') and (rep <= 'z') then
              Dec(rep, $20);
          until (rep = 'Y') or (rep = 'N');
          if (rep = 'N') then
            zipok := 0;
        end;
    end;

    if (zipok = 1) then
    begin
      zf := zipOpen(filename_try, 0);
      if (zf = nil) then
      begin
        WriteLn('error opening ', filename_try);
        err := ZIP_ERRNO;
      end
      else
        WriteLn('creating ', filename_try);

      i := zipfilenamearg + 1;
      while (i <= ParamCount) and (err = ZIP_OK) do
      begin
        argstr := ParamStr(i) + #0;
        if (argstr[1] <> '-') and (argstr[1] <> '/') then
        begin
          filenameinzip := PChar(@argstr[1]);

          zi.tmz_date.tm_sec := 0;
          zi.tmz_date.tm_min := 0;
          zi.tmz_date.tm_hour := 0;
          zi.tmz_date.tm_mday := 0;
          zi.tmz_date.tm_min := 0;
          zi.tmz_date.tm_year := 0;
          zi.dosDate     := 0;
          zi.internal_fa := 0;
          zi.external_fa := 0;
          filetime(filenameinzip, zi.tmz_date);

          if (opt_compress_level <> 0) then
            err := zipOpenNewFileInZip(zf, filenameinzip, @zi,
              nil, 0, nil, 0, nil { comment}, Z_DEFLATED, opt_compress_level)
          else
            err := zipOpenNewFileInZip(zf, filenameinzip, @zi,
              nil, 0, nil, 0, nil, 0, opt_compress_level);

          if (err <> ZIP_OK) then
            WriteLn('error in opening ', filenameinzip, ' in zipfile')
          else
          begin
            fin := fopen(filenameinzip, fopenread);
            if (fin = nil) then
            begin
              err := ZIP_ERRNO;
              WriteLn('error in opening ', filenameinzip, ' for reading');
            end;

            if (err = ZIP_OK) then
              repeat
                err := ZIP_OK;
                size_read := fread(buf, 1, size_buf, fin);

                if (size_read < size_buf) then
                  if feof(fin) = 0 then
                  begin
                    WriteLn('error in reading ', filenameinzip);
                    err := ZIP_ERRNO;
                  end;

                if (size_read > 0) then
                begin
                  err := zipWriteInFileInZip(zf, buf, size_read);
                  if (err < 0) then
                    WriteLn('error in writing ', filenameinzip, ' in the zipfile');
                end;
              until (err <> ZIP_OK) or (size_read = 0);

            fclose(fin);
          end;
          if (err < 0) then
            err := ZIP_ERRNO
          else
          begin
            err := zipCloseFileInZip(zf);
            if (err <> ZIP_OK) then
              WriteLn('error in closing ', filenameinzip, ' in the zipfile');
          end;
          Inc(i);
        end; { while }
      end; { if }

      errclose := zipClose(zf, nil);
      if (errclose <> ZIP_OK) then
        WriteLn('error in closing ', filename_try);
    end;

    FreeMem(buf); {FreeMem(buf, size_buf);}
  end;

begin
  main;
  Write('Done...');
  ReadLn;
end.
