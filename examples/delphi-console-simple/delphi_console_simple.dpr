program delphi_console_simple;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  tesseractocr in '..\..\tesseractocr.pas';

begin
  Tesseract := TTesseractOCR4.Create;
  try
    if Tesseract.Initialize('tessdata\', 'eng') then
    begin
      Tesseract.SetImage('samples\eng-text.png');
      WriteLn(Tesseract.RecognizeAsText);
      ReadLn;
    end;
  finally
    Tesseract.Free;
  end;
end.
