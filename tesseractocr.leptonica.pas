unit tesseractocr.leptonica;

{ The MIT License (MIT)

 TTesseractOCR4
 Copyright (c) 2018 Damian Woroch, http://rime.ddns.net/

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE. }

interface
uses
  tesseractocr.consts;

type
  l_uint8 = Byte;
  pl_uint8 = ^l_uint8;
  l_uint32 = Cardinal;
  pl_uint32 = ^l_uint32;
  l_int32 = Integer;
  l_float32 = Single;
  size_t = NativeInt;

  TPixColormap = record
    arr: Pointer;
    depth: l_int32;
    nalloc: l_int32;
    n: l_int32;
  end;
  PPixColormap = ^TPixColormap;

  TPix = record
    w: l_uint32;
    h: l_uint32;
    d: l_uint32;
    spp: l_uint32;     //*!< number of samples per pixel       */
    wpl: l_uint32;
    refcount: l_uint32;
    xres: l_int32;
    yres: l_int32;
    informat: l_int32;
    special: l_int32;   //*!< special instructions for I/O, etc */
    text: PUTF8Char;
    colormap: PPixColormap;
    data: pl_uint32;
  end;
  PPix = ^TPix;
  PPPix = ^PPix;

  TBox = record
    x: l_int32;
    y: l_int32;
    w: l_int32;
    h: l_int32;
    refcount: l_uint32;
  end;
  PBox = ^TBox;
  PPBox = ^PBox;

  TBoxa = record
    n: l_int32;
    nalloc: l_int32;
    refcount: l_uint32;
    box: PPBox;
  end;
  PBoxa = ^TBoxa;

  TPixa = record
    n: l_int32;
    nalloc: l_int32;
    refcount: l_uint32;
    pix: PPPix;
    boxa: PBoxa;
  end;
  PPixa = ^TPixa;

const
  L_INSERT = 0;
  L_COPY = 1;
  L_CLONE = 2;

type
  TfnpixRead = function(const filename: PUTF8Char): PPix; cdecl;
  TfnpixDestroy = procedure(var pix: PPix); cdecl;
  TfnboxaGetBox = function(boxa: PBoxa; index: l_int32; accessflag: l_int32): Pbox; cdecl;
  TfnboxaGetCount = function(boxa: PBoxa): l_int32; cdecl;
  TfnboxDestroy = procedure(var box: PBox); cdecl;
  TfnboxaDestroy = procedure(var boxa: PBoxa); cdecl;
  TfnpixReadMem = function(const pdata: pl_uint8; size: size_t): PPix; cdecl;
  TfnpixWriteMemPng = function(pdata: pl_uint8; out psize: size_t; pix: PPix; gamma: l_float32): l_int32; cdecl;
  TfnpixWriteMemBmp = function(pdata: pl_uint8; out psize: size_t; pix: PPix): l_int32; cdecl;
  TfnpixDeskew = function(pixs: PPix; redsearch: l_int32): PPix; cdecl;
  Tfnlept_free = procedure(ptr: Pointer); cdecl;

var
  pixRead: TfnpixRead;
  pixDestroy: TfnpixDestroy;
  boxaGetBox: TfnboxaGetBox;
  boxaGetCount: TfnboxaGetCount;
  boxDestroy: TfnboxDestroy;
  boxaDestroy: TfnboxaDestroy;
  pixReadMem: TfnpixReadMem;
  pixWriteMemPng: TfnpixWriteMemPng;
  pixWriteMemBmp: TfnpixWriteMemBmp;
  pixDeskew: TfnpixDeskew;
  lept_free: Tfnlept_free;

implementation
uses
  {$IFNDEF FPC}Winapi.Windows, System.SysUtils{$ELSE}dynlibs, SysUtils{$ENDIF};

var
  hLeptonicaLib: THandle;

procedure FreeLeptonicaLib;
begin
  if (hLeptonicaLib <> 0) then
  begin
    FreeLibrary(hLeptonicaLib);
    hLeptonicaLib := 0;
  end;
end;

function InitLeptonicaLib: Boolean;

  function GetLeptonicaProcAddress(var AProcPtr: Pointer; AProcName: AnsiString): Boolean;
  begin
    AProcPtr := GetProcAddress(hLeptonicaLib, {$IFDEF FPC}AProcName{$ELSE}PAnsiChar(AProcName){$ENDIF});
    Result := Assigned(AProcPtr);
    if not Result then
      raise Exception.Create('Error while loading Leptonica function: ' + String(AProcName));
  end;

begin
  Result := False;

  if (hLeptonicaLib = 0) then
  begin
    hLeptonicaLib := LoadLibrary({$IFDEF FPC}libleptonica{$ELSE}PChar(libleptonica){$ENDIF});
    if (hLeptonicaLib <> 0) then
    begin
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixRead{$IFDEF FPC}){$ENDIF}, 'pixRead');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixDestroy{$IFDEF FPC}){$ENDIF}, 'pixDestroy');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxaGetBox{$IFDEF FPC}){$ENDIF}, 'boxaGetBox');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxaGetCount{$IFDEF FPC}){$ENDIF}, 'boxaGetCount');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxDestroy{$IFDEF FPC}){$ENDIF}, 'boxDestroy');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxaDestroy{$IFDEF FPC}){$ENDIF}, 'boxaDestroy');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixReadMem{$IFDEF FPC}){$ENDIF}, 'pixReadMem');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixWriteMemPng{$IFDEF FPC}){$ENDIF}, 'pixWriteMemPng');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixWriteMemBmp{$IFDEF FPC}){$ENDIF}, 'pixWriteMemBmp');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixDeskew{$IFDEF FPC}){$ENDIF}, 'pixDeskew');
      GetLeptonicaProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}lept_free{$IFDEF FPC}){$ENDIF}, 'lept_free');

      Result := True;
    end;
  end;
end;

initialization
  InitLeptonicaLib;

finalization
  FreeLeptonicaLib;

end.
