program lazarus_console_simple;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  tesseractocr;

begin
  Tesseract := TTesseractOCR4.Create;
  try
    if Tesseract.Initialize('tessdata' + PathDelim, 'eng') then
    begin
      Tesseract.SetImage('samples' + PathDelim + 'eng-text.png');
      WriteLn(Tesseract.RecognizeAsText);
    end;
  finally
    Tesseract.Free;
  end;
end.

