unit tesseractocr.pagelayout;

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
  {$IFNDEF FPC}
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.SysUtils,
  {$ELSE}
  Classes,
  Types,
  SysUtils,
  fgl,
  {$ENDIF}
  tesseractocr.capi;

type
  TBaseLine = array[0..1] of TPoint;

type
  TTesseractBlock = class;
  TTesseractParagraph = class;
  TTesseractTextLine = class;
  TTesseractWord = class;

  TTesseractSymbol = class
  private
    FOwner: TTesseractWord;
    FBoundingRect: TRect;
    FCharacter: String;
    FConfidence: Single;
    FIsSuperscript: Boolean;
    FIsSubscript: Boolean;
    FIsDropcap: Boolean;
  public
    property Owner: TTesseractWord read FOwner;
    property BoundingRect: TRect read FBoundingRect;
    property Character: String read FCharacter;
    property Confidence: Single read FConfidence;
    property IsSuperscript: Boolean read FIsSuperscript;
    property IsSubscript: Boolean read FIsSubscript;
    property IsDropcap: Boolean read FIsDropcap;

    constructor Create(AOwner: TTesseractWord; ABoundingRect: TRect; ACharacter: String;
      AConfidence: Single; AIsSuperscript, AIsSubscript, AIsDropcap: Boolean);
  end;
  TTesseractSymbols = {$IFNDEF FPC}TObjectList<TTesseractSymbol>{$ELSE}specialize TFPGObjectList<TTesseractSymbol>{$ENDIF};

  TTesseractWord = class
  private
    FOwner: TTesseractTextLine;
    FBoundingRect: TRect;
    FText: String;
    FConfidence: Single;
    FLanguage: String;
    FIsBold: Boolean;
    FIsItalic: Boolean;
    FIsUnderlined: Boolean;
    FIsMonospace: Boolean;
    FIsSerif: Boolean;
    FIsSmallCaps: Boolean;
    FPointSize: Integer;
    FFontId: Integer;
    FFontName: String;
    FInDictionary: Boolean;
    FIsNumeric: Boolean;
    FSymbols: TTesseractSymbols;
  public
    property Owner: TTesseractTextLine read FOwner;
    property BoundingRect: TRect read FBoundingRect;
    property Text: String read FText;
    property Confidence: Single read FConfidence;
    property Language: String read FLanguage;
    property IsBold: Boolean read FIsBold;
    property IsItalic: Boolean read FIsItalic;
    property IsUnderlined: Boolean read FIsUnderlined;
    property IsMonospace: Boolean read FIsMonospace;
    property IsSerif: Boolean read FIsSerif;
    property IsSmallCaps: Boolean read FIsSmallCaps;
    property PointSize: Integer read FPointSize;
    property FontId: Integer read FFontId;
    property FontName: String read FFontName;
    property InDictionary: Boolean read FInDictionary;
    property IsNumeric: Boolean read FIsNumeric;
    property Symbols: TTesseractSymbols read FSymbols write FSymbols;

    constructor Create(AOwner: TTesseractTextLine; ABoundingRect: TRect; AText: String;
      AConfidence: Single; ALanguage: String; AIsBold, AIsItalic, AIsUnderlined: Boolean;
      AIsMonospace, AIsSerif, AIsSmallCaps: Boolean; APointSize: Integer;
      AFontId: Integer; AFontName: String; AInDictionary: Boolean; AIsNumeric: Boolean);
    destructor Destroy; override;
  end;
  TTesseractWords = {$IFNDEF FPC}TObjectList<TTesseractWord>{$ELSE}specialize TFPGObjectList<TTesseractWord>{$ENDIF};

  TTesseractTextLine = class
  private
    FOwner: TTesseractParagraph;
    FBoundingRect: TRect;
    FBaseLine: TBaseLine;
    FWords: TTesseractWords;
  public
    property Owner: TTesseractParagraph read FOwner;
    property BoundingRect: TRect read FBoundingRect;
    property Words: TTesseractWords read FWords write FWords;

    constructor Create(AOwner: TTesseractParagraph; ABoundingRect: TRect; ABaseLine: TBaseLine);
    destructor Destroy; override;
  end;
  TTesseractTextLines = {$IFNDEF FPC}TObjectList<TTesseractTextLine>{$ELSE}specialize TFPGObjectList<TTesseractTextLine>{$ENDIF};

  TTesseractParagraph = class
  private
    FOwner: TTesseractBlock;
    FBoundingRect: TRect;
    FJustification: TessParagraphJustification;
    FIsListItem: Boolean;
    FIsCrown: Boolean;
    FFirstLineIndent: Integer;
    FTextLines: TTesseractTextLines;
  public
    property Owner: TTesseractBlock read FOwner;
    property BoundingRect: TRect read FBoundingRect;
    property Justification: TessParagraphJustification read FJustification;
    property IsListItem: Boolean read FIsListItem;
    property IsCrown: Boolean read FIsCrown;
    property FirstLineIndent: Integer read FFirstLineIndent;
    property TextLines: TTesseractTextLines read FTextLines write FTextLines;

    constructor Create(AOwner: TTesseractBlock; ABoundingRect: TRect;
      AJustification: TessParagraphJustification; AIsListItem, AIsCrown: Boolean;
      AFirstLineIndent: Integer);
    destructor Destroy; override;
  end;
  TTesseractParagraphs = {$IFNDEF FPC}TObjectList<TTesseractParagraph>{$ELSE}specialize TFPGObjectList<TTesseractParagraph>{$ENDIF};

  TTesseractBlock = class
  private
    FBoundingRect: TRect;
    FBlockType: TessPolyBlockType;
    FBaseLine: TBaseLine;
    FParagraphs: TTesseractParagraphs;
  public
    property BoundingRect: TRect read FBoundingRect;
    property BlockType: TessPolyBlockType read FBlockType;
    property BaseLine: TBaseLine read FBaseLine;
    property Paragraphs: TTesseractParagraphs read FParagraphs write FParagraphs;

    constructor Create(ABoundingRect: TRect; ABlockType: TessPolyBlockType; ABaseLine: TBaseLine);
    destructor Destroy; override;
  end;
  TTesseractBlocks = {$IFNDEF FPC}TObjectList<TTesseractBlock>{$ELSE}specialize TFPGObjectList<TTesseractBlock>{$ENDIF};

type
  TTesseractPageLayout = class(TObject)
  protected
    FTessBaseAPI: TessBaseAPI;
  private
    FOrientation: TessOrientation;
    FWritingDirection: TessWritingDirection;
    FTextlineOrder: TessTextlineOrder;
    FDeskewAngle: Single;
    FMeanWordConfidence: Integer;
    FDataReady: Boolean;
    FBlocks: TTesseractBlocks;
    FParagraphs: TTesseractParagraphs;
    FTextLines: TTesseractTextLines;
    FWords: TTesseractWords;
    FSymbols: TTesseractSymbols;
    procedure Clear;
  public
    property Orientation: TessOrientation read FOrientation;
    property WritingDirection: TessWritingDirection read FWritingDirection;
    property TextlineOrder: TessTextlineOrder read FTextlineOrder;
    property DeskewAngle: Single read FDeskewAngle;
    property MeanWordConfidence: Integer read FMeanWordConfidence;

    property Blocks: TTesseractBlocks read FBlocks;
    property Paragraphs: TTesseractParagraphs read FParagraphs;
    property TextLines: TTesseractTextLines read FTextLines;
    property Words: TTesseractWords read FWords;
    property Symbols: TTesseractSymbols read FSymbols;

    function AnalyseLayout: Boolean;
    property DataReady: Boolean read FDataReady;

    constructor Create(ABaseAPI: TessBaseAPI);
    destructor Destroy; override;
  end;

implementation
uses
  tesseractocr.utils;

{ TTesseractPageLayout }

constructor TTesseractPageLayout.Create(ABaseAPI: TessBaseAPI);
begin
  FTessBaseAPI := ABaseAPI;
  FBlocks := TTesseractBlocks.Create;
  FParagraphs := TTesseractParagraphs.Create(False);
  FTextLines := TTesseractTextLines.Create(False);
  FWords := TTesseractWords.Create(False);
  FSymbols := TTesseractSymbols.Create(False);
end;

destructor TTesseractPageLayout.Destroy;
begin
  FBlocks.Free;
  FParagraphs.Free;
  FTextLines.Free;
  FWords.Free;
  FSymbols.Free;
  inherited;
end;

procedure TTesseractPageLayout.Clear;
begin
  FDeskewAngle := 0;
  FMeanWordConfidence := 0;
  FDataReady := False;
  FBlocks.Clear;
  FParagraphs.Clear;
  FTextLines.Clear;
  FWords.Clear;
  FSymbols.Clear;
end;

function TTesseractPageLayout.AnalyseLayout: Boolean;

  function GetBaseLine(APageIterator: TessPageIterator; ALevel: TessPageIteratorLevel): TBaseLine;
  var
    baseX1, baseY1, baseX2, baseY2: Integer;
  begin
    if TessPageIteratorBaseline(APageIterator, ALevel, baseX1, baseY1, baseX2, baseY2) then
    begin
      Result[0].x := baseX1;
      Result[0].y := baseY1;
      Result[1].x := baseX2;
      Result[1].y := baseY2;
    end else
    begin
      Result[0].x := 0;
      Result[0].y := 0;
      Result[1].x := 0;
      Result[1].y := 0;
    end;
  end;

var
  pi, piBlock, piPara, piTextLine, piWord, piSymbol: TessPageIterator;
  ri, riBlock, riPara, riTextLine, riWord, riSymbol: TessResultIterator;
  block: TTesseractBlock;
  para: TTesseractParagraph;
  textline: TTesseractTextLine;
  word: TTesseractWord;
  symbol: TTesseractSymbol;
  confidence: Single;
  lang: String;
  isBold, isItalic, isUnderlined: LongBool;
  isMonospace, isSerif, isSmallCaps: LongBool;
  isSuperscript, isSubscript, isDropcap: Boolean;
  pointSize: Integer;
  fontId: Integer;
  fontName: String;
  inDictionary: Boolean;
  isNumeric: Boolean;
  isListItem, isCrown: LongBool;
  justification: TessParagraphJustification;
  firstLineIdent: Integer;
  boundBox: TRect;
  l, t, r, b: Integer;
begin
  Result := False;
  Clear;
  try
    ri := TessBaseAPIGetIterator(FTessBaseAPI);
    pi := TessResultIteratorGetPageIterator(ri);
    if not Assigned(pi) then Exit;
    TessPageIteratorOrientation(pi, FOrientation, FWritingDirection, FTextlineOrder, FDeskewAngle);
    FMeanWordConfidence := TessBaseAPIMeanTextConf(FTessBaseAPI);
    try
      TessPageIteratorBegin(pi);

      piBlock := TessPageIteratorCopy(pi);
      riBlock := TessResultIteratorCopy(ri);
      repeat
        if TessPageIteratorIsAtBeginningOf(piBlock, RIL_BLOCK) then
        begin
          if TessPageIteratorBoundingBox(piBlock, RIL_BLOCK, l, t, r, b) then
            boundBox := Rect(l, t, r, b)
          else
            boundBox := Rect(0, 0, 0, 0);
          block := TTesseractBlock.Create(boundBox,
            TessPageIteratorBlockType(piBlock), GetBaseLine(piBlock, RIL_BLOCK));
          FBlocks.Add(block);

          piPara := TessPageIteratorCopy(piBlock);
          riPara := TessResultIteratorCopy(riBlock);
          repeat
            if TessPageIteratorIsAtBeginningOf(piPara, RIL_PARA) then
            begin
              if TessPageIteratorBoundingBox(piPara, RIL_PARA, l, t, r, b) then
                boundBox := Rect(l, t, r, b)
              else
                boundBox := Rect(0, 0, 0, 0);
              TessPageIteratorParagraphInfo(piPara, justification, isListItem, isCrown, firstLineIdent);
              para := TTesseractParagraph.Create(block, boundBox, justification,
                isListItem, isCrown, firstLineIdent);
              block.Paragraphs.Add(para);
              FParagraphs.Add(para);

              piTextLine := TessPageIteratorCopy(piPara);
              riTextLine := TessResultIteratorCopy(riPara);
              repeat
                if TessPageIteratorIsAtBeginningOf(piTextLine, RIL_TEXTLINE) then
                begin
                  if TessPageIteratorBoundingBox(piTextLine, RIL_TEXTLINE, l, t, r, b) then
                    boundBox := Rect(l, t, r, b)
                  else
                    boundBox := Rect(0, 0, 0, 0);
                  textline := TTesseractTextLine.Create(para, boundBox, GetBaseLine(piTextLine, RIL_TEXTLINE));
                  para.TextLines.Add(textline);
                  FTextLines.Add(textline);

                  piWord := TessPageIteratorCopy(piTextLine);
                  riWord := TessResultIteratorCopy(riTextLine);
                  repeat
                    if TessPageIteratorIsAtBeginningOf(piWord, RIL_WORD) then
                    begin
                      if TessPageIteratorBoundingBox(piWord, RIL_WORD, l, t, r, b) then
                        boundBox := Rect(l, t, r, b)
                      else
                        boundBox := Rect(0, 0, 0, 0);
                      confidence := TessResultIteratorConfidence(riWord, RIL_WORD);
                      lang := PUTF8CharToString(TessResultIteratorWordRecognitionLanguage(riWord), False);
                      fontName := PUTF8CharToString(TessResultIteratorWordFontAttributes(riWord,
                        isBold, isItalic, isUnderlined, isMonospace, isSerif, isSmallCaps, pointSize, fontId), False);
                      inDictionary := TessResultIteratorWordIsFromDictionary(riWord);
                      isNumeric := TessResultIteratorWordIsNumeric(riWord);

                      word := TTesseractWord.Create(textline, boundBox,
                        PUTF8CharToString(TessResultIteratorGetUTF8Text(riWord, RIL_WORD)),
                        confidence, lang, isBold, isItalic, isUnderlined, isMonospace, isSerif,
                        isSmallCaps, pointSize, fontId, fontName, inDictionary, isNumeric);
                      textline.Words.Add(word);
                      FWords.Add(word);

                      piSymbol := TessPageIteratorCopy(piWord);
                      riSymbol := TessResultIteratorCopy(riWord);
                      repeat
                        if TessPageIteratorIsAtBeginningOf(piSymbol, RIL_SYMBOL) then
                        begin
                          if TessPageIteratorBoundingBox(piSymbol, RIL_SYMBOL, l, t, r, b) then
                            boundBox := Rect(l, t, r, b)
                          else
                            boundBox := Rect(0, 0, 0, 0);
                          confidence := TessResultIteratorConfidence(riSymbol, RIL_SYMBOL);
                          isSuperscript := TessResultIteratorSymbolIsSuperscript(riSymbol);
                          isSubscript := TessResultIteratorSymbolIsSubscript(riSymbol);
                          isDropcap := TessResultIteratorSymbolIsDropcap(riSymbol);

                          symbol := TTesseractSymbol.Create(word, boundBox,
                            PUTF8CharToString(TessResultIteratorGetUTF8Text(riSymbol, RIL_SYMBOL)), confidence,
                            isSuperscript, isSubscript, isDropcap);
                          word.Symbols.Add(symbol);
                          FSymbols.Add(symbol);
                        end;
                        if TessPageIteratorIsAtFinalElement(piSymbol, RIL_WORD, RIL_SYMBOL) then
                          Break;
                      until not (TessPageIteratorNext(piSymbol, RIL_SYMBOL) and TessResultIteratorNext(riSymbol, RIL_SYMBOL));
                      TessPageIteratorDelete(piSymbol);
                      TessResultIteratorDelete(riSymbol);
                    end;
                    if TessPageIteratorIsAtFinalElement(piWord, RIL_TEXTLINE, RIL_WORD) then
                      Break;
                  until not (TessPageIteratorNext(piWord, RIL_WORD) and TessResultIteratorNext(riWord, RIL_WORD));
                  TessPageIteratorDelete(piWord);
                  TessResultIteratorDelete(riWord);
                end;
                if TessPageIteratorIsAtFinalElement(piTextLine, RIL_PARA, RIL_TEXTLINE) then
                  Break;
              until not (TessPageIteratorNext(piTextLine, RIL_TEXTLINE) and TessResultIteratorNext(riTextLine, RIL_TEXTLINE));
              TessPageIteratorDelete(piTextLine);
              TessResultIteratorDelete(riTextLine);
            end;
            if TessPageIteratorIsAtFinalElement(piPara, RIL_BLOCK, RIL_PARA) then
              Break;
          until not (TessPageIteratorNext(piPara, RIL_PARA) and TessResultIteratorNext(riPara, RIL_PARA));
          TessPageIteratorDelete(piPara);
          TessResultIteratorDelete(riPara);
        end;
      until not (TessPageIteratorNext(piBlock, RIL_BLOCK) and TessResultIteratorNext(riBlock, RIL_BLOCK));
      TessPageIteratorDelete(piBlock);
      TessResultIteratorDelete(riBlock);
    finally
      TessResultIteratorDelete(ri);
    end;
  finally
    FDataReady := True;
  end;

  Result := True;
end;

{ TTesseractBlock }

constructor TTesseractBlock.Create(ABoundingRect: TRect;
  ABlockType: TessPolyBlockType; ABaseLine: TBaseLine);
begin
  FBoundingRect := ABoundingRect;
  FBlockType := ABlockType;
  FBaseLine := ABaseLine;
  FParagraphs := TTesseractParagraphs.Create;
end;

destructor TTesseractBlock.Destroy;
begin
  FParagraphs.Free;
  inherited;
end;

{ TTesseractParagraph }

constructor TTesseractParagraph.Create(AOwner: TTesseractBlock;
  ABoundingRect: TRect; AJustification: TessParagraphJustification;
  AIsListItem, AIsCrown: Boolean; AFirstLineIndent: Integer);
begin
  FOwner := AOwner;
  FBoundingRect := ABoundingRect;
  FJustification := AJustification;
  FIsListItem := AIsListItem;
  FIsCrown := AIsCrown;
  FFirstLineIndent := AFirstLineIndent;
  FTextLines := TTesseractTextLines.Create;
end;

destructor TTesseractParagraph.Destroy;
begin
  FTextLines.Free;
  inherited;
end;

{ TTesseractTextLine }

constructor TTesseractTextLine.Create(AOwner: TTesseractParagraph;
  ABoundingRect: TRect; ABaseLine: TBaseLine);
begin
  FOwner := AOwner;
  FBoundingRect := ABoundingRect;
  FBaseLine := ABaseLine;
  FWords := TTesseractWords.Create;
end;

destructor TTesseractTextLine.Destroy;
begin
  FWords.Free;
  inherited;
end;

{ TTesseractWord }

constructor TTesseractWord.Create(AOwner: TTesseractTextLine; ABoundingRect: TRect;
  AText: String; AConfidence: Single; ALanguage: String; AIsBold, AIsItalic, AIsUnderlined: Boolean;
  AIsMonospace, AIsSerif, AIsSmallCaps: Boolean; APointSize: Integer;
  AFontId: Integer; AFontName: String; AInDictionary: Boolean; AIsNumeric: Boolean);
begin
  FOwner := AOwner;
  FBoundingRect := ABoundingRect;
  FText := AText;
  FConfidence := AConfidence;
  FLanguage := ALanguage;
  FIsBold := AIsBold;
  FIsItalic := AIsItalic;
  FIsUnderlined := AIsUnderlined;
  FIsMonospace := AIsMonospace;
  FIsSerif := AIsSerif;
  FIsSmallCaps := AIsSmallCaps;
  FPointSize := APointSize;
  FFontId := AFontId;
  FFontName := AFontName;
  FInDictionary := AInDictionary;
  FIsNumeric := AIsNumeric;
  FSymbols := TTesseractSymbols.Create;
end;

destructor TTesseractWord.Destroy;
begin
  FSymbols.Free;
  inherited;
end;

{ TTesseractSymbol }

constructor TTesseractSymbol.Create(AOwner: TTesseractWord; ABoundingRect: TRect;
  ACharacter: String; AConfidence: Single; AIsSuperscript, AIsSubscript, AIsDropcap: Boolean);
begin
  FOwner := AOwner;
  FBoundingRect := ABoundingRect;
  FCharacter := ACharacter;
  FConfidence := AConfidence;
  FIsSuperscript := AIsSuperscript;
  FIsSubscript := AIsSubscript;
  FIsDropcap := AIsDropcap;
end;

end.
