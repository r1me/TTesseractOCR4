# TTesseractOCR4
[![Click here to lend your support to r1me and make a donation at pledgie.com !](https://pledgie.com/campaigns/34292.png)](https://pledgie.com/campaigns/34292)

TTesseractOCR4 is a Object Pascal binding for [tesseract-ocr](https://github.com/tesseract-ocr/tesseract) 4.x - an optical character recognition engine.

## Building examples
Examples were tested in Delphi 10.2 Tokyo Starter (**32-bit**) and Lazarus 1.8. 

1. Clone this repository to a local folder.
2. Obtain Tesseract 4.x binaries. There are many sources to download binaries from, or ways to build them. I recommend using latest version, build from master branch of the project.  
In Windows: there is no official installer. You can [download precompiled binaries](http://r1me.pl/tesseractocr-master.zip) ([*Microsoft Visual C++ 2017 Redistributable x86*](https://go.microsoft.com/fwlink/?LinkId=746571) must be installed on the computer) and copy all DLL files to `bin\`. If you want to build Tesseract: [compiling instructions](https://github.com/tesseract-ocr/tesseract/wiki/Compiling#windows) (the easiest way is to use Visual Studio 2015/2017 and CPPAN).
3. Download trained language data files from [tesseract-ocr/tessdata/](https://github.com/tesseract-ocr/tessdata/) to `bin\tessdata`.  
All examples in this repository require English data file ([`eng.traineddata`](https://github.com/tesseract-ocr/tessdata/blob/master/eng.traineddata)).  
Additionally `examples\delphi-console-pdfconvert` requires also  [`osd.traineddata`](https://github.com/tesseract-ocr/tessdata/blob/master/osd.traineddata) and [`pdf.ttf`](https://github.com/tesseract-ocr/tesseract/blob/master/tessdata/pdf.ttf).
4. Open and compile example project: 
   - `examples\delphi-console-simple`. Recognize text in `samples\eng-text.png` and write to console output
   ![delphi-console-simple](examples/delphi-console-simple/delphi-console-simple.png)
   
   - `examples\delphi-vcl-image`  
   ![delphi-vcl-image](examples/delphi-vcl-image/delphi-vcl-image.gif)  
   4 tabs: 
      - Image: View input image
      - Text: Recognized text coded as UTF-8
      - HOCR: Recognized text in HTML format
      - Layout: View page layout (paragraphs, text lines, words...) 
    
   - `examples\delphi-console-pdfconvert`. Convert `samples\multi-page.tif` (multiple page image file) to a PDF file
   - `examples\lazarus-console-simple`. `examples\delphi-console-simple` for Lazarus 

## License
MIT
