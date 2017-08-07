program lazarus_console_simple;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  tesseractocr;

begin
  Tesseract := TTesseractOCR4.Create;
  try
    if Tesseract.Initialize('tessdata\', 'eng') then
    begin
      Tesseract.SetImage('samples\eng-text.png');
      WriteLn(Tesseract.RecognizeAsText);
    end;
  finally
    Tesseract.Free;
  end;
end.

