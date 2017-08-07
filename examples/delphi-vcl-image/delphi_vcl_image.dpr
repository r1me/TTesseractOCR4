program delphi_vcl_image;

uses
  Vcl.Forms,
  FormTesseractOCRImage in 'FormTesseractOCRImage.pas' {TesseractOCRImageForm},
  tesseractocr.capi in '..\..\tesseractocr.capi.pas',
  tesseractocr.leptonica in '..\..\tesseractocr.leptonica.pas',
  tesseractocr in '..\..\tesseractocr.pas',
  tesseractocr.pagelayout in '..\..\tesseractocr.pagelayout.pas',
  tesseractocr.utils in '..\..\tesseractocr.utils.pas',
  tesseractocr.consts in '..\..\tesseractocr.consts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTesseractOCRImageForm, TesseractOCRImageForm);
  Application.Run;
end.
