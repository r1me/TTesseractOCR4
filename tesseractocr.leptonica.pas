unit tesseractocr.leptonica;

{ The MIT License (MIT)

 TTesseractOCR4
 Copyright (c) 2017 Damian Woroch, http://r1me.pl

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
    wpl: l_uint32;
    refcount: l_uint32;
    xres: l_int32;
    yres: l_int32;
    informat: l_int32;
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
  {$IFNDEF FPC}Winapi.Windows{$ELSE}dynlibs, SysUtils{$ENDIF};

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
begin
  Result := False;

  if (hLeptonicaLib = 0) then
  begin
    hLeptonicaLib := LoadLibrary({$IFDEF FPC}ExtractFilePath(ParamStr(0)) + libleptonica{$ELSE}PChar(libleptonica){$ENDIF});
    if (hLeptonicaLib <> 0) then
    begin
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixRead{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'pixRead');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixDestroy{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'pixDestroy');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxaGetBox{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'boxaGetBox');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxaGetCount{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'boxaGetCount');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxDestroy{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'boxDestroy');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}boxaDestroy{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'boxaDestroy');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixReadMem{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'pixReadMem');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixWriteMemPng{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'pixWriteMemPng');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixWriteMemBmp{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'pixWriteMemBmp');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}pixDeskew{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'pixDeskew');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}lept_free{$IFDEF FPC}){$ENDIF} := GetProcAddress(hLeptonicaLib, 'lept_free');

      Result := Assigned(pixRead) and
                Assigned(pixDestroy) and
                Assigned(boxaGetBox) and
                Assigned(boxaGetCount) and
                Assigned(boxDestroy) and
                Assigned(boxaDestroy) and
                Assigned(pixReadMem) and
                Assigned(pixWriteMemPng) and
                Assigned(pixWriteMemBmp) and
                Assigned(pixDeskew) and
                Assigned(lept_free);

      if not Result then
        FreeLeptonicaLib;
    end;
  end;
end;

initialization
  InitLeptonicaLib;

finalization
  FreeLeptonicaLib;

end.
