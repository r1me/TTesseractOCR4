program delphi_console_pdfconvert;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  tesseractocr in '..\..\tesseractocr.pas';

var
  inputFileName,
  outputFileName: String;
begin
  Tesseract := TTesseractOCR4.Create;
  try
    if Tesseract.Initialize('tessdata\', 'eng') then
    begin
      inputFileName := 'samples\multi-page.tif';
      outputFileName := 'multi-page.pdf';

      if Tesseract.CreatePDF(inputFileName, outputFileName) then
      begin
        WriteLn('PDF was saved succesfully to ' + outputFileName);
        ReadLn;
      end;
    end;
  finally
    Tesseract.Free;
  end;
end.
