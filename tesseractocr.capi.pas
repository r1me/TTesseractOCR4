unit tesseractocr.capi;

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
  tesseractocr.leptonica,
  tesseractocr.consts;

type
  TessResultRenderer = Pointer;
  TessTextRenderer = Pointer;
  TessHOcrRenderer = Pointer;
  TessPDFRenderer = Pointer;
  TessUnlvRenderer = Pointer;
  TessBoxTextRenderer = Pointer;
  TessBaseAPI = Pointer;
  TessPageIterator = Pointer;
  TessResultIterator = Pointer;
  TessMutableIterator = Pointer;
  TessChoiceIterator = Pointer;

type
  TessOcrEngineMode = (OEM_TESSERACT_ONLY, OEM_LSTM_ONLY, OEM_TESSERACT_LSTM_COMBINED, OEM_DEFAULT);

type
  TessPageSegMode = (PSM_OSD_ONLY, PSM_AUTO_OSD, PSM_AUTO_ONLY, PSM_AUTO, PSM_SINGLE_COLUMN, PSM_SINGLE_BLOCK_VERT_TEXT,
    PSM_SINGLE_BLOCK, PSM_SINGLE_LINE, PSM_SINGLE_WORD, PSM_CIRCLE_WORD, PSM_SINGLE_CHAR, PSM_SPARSE_TEXT,
    PSM_SPARSE_TEXT_OSD, PSM_RAW_LINE, PSM_COUNT);

type
  TessPageIteratorLevel = (RIL_BLOCK, RIL_PARA, RIL_TEXTLINE, RIL_WORD, RIL_SYMBOL);

type
  TessPolyBlockType = (PT_UNKNOWN, PT_FLOWING_TEXT, PT_HEADING_TEXT, PT_PULLOUT_TEXT,
    PT_EQUATION, PT_INLINE_EQUATION, PT_TABLE, PT_VERTICAL_TEXT, PT_CAPTION_TEXT,
    PT_FLOWING_IMAGE, PT_HEADING_IMAGE, PT_PULLOUT_IMAGE,
    PT_HORZ_LINE, PT_VERT_LINE, PT_NOISE, PT_COUNT);

type
  TessOrientation = (ORIENTATION_PAGE_UP, ORIENTATION_PAGE_RIGHT,
    ORIENTATION_PAGE_DOWN, ORIENTATION_PAGE_LEFT);

type
  TessParagraphJustification = (JUSTIFICATION_UNKNOWN, JUSTIFICATION_LEFT,
    JUSTIFICATION_CENTER, JUSTIFICATION_RIGHT);

type
  TessWritingDirection = (WRITING_DIRECTION_LEFT_TO_RIGHT,
    WRITING_DIRECTION_RIGHT_TO_LEFT, WRITING_DIRECTION_TOP_TO_BOTTOM);

type
  TessTextlineOrder = (TEXTLINE_ORDER_LEFT_TO_RIGHT, TEXTLINE_ORDER_RIGHT_TO_LEFT,
    TEXTLINE_ORDER_TOP_TO_BOTTOM);

type
  float = Single;
  BOOL = LongBool;
  size_t = NativeUInt;
  int = Integer;
  Pint = ^int;

type
  CANCEL_FUNC = function(cancel_this: Pointer; words: int): Boolean; cdecl;
  PROGRESS_FUNC = function(progress: int; left, right, top, bottom: int): Boolean; cdecl;

type
  EANYCODE_CHAR = record
    char_code: UINT16;
    left: INT16;
    right: INT16;
    top: INT16;
    bottom: INT16;
    font_index: INT16;
    confidence: UINT8;
    point_size: UINT8;
    blanks: INT8;
    formatting: UINT8;
  end;

type
  ETEXT_DESC = record
    count: INT16;
    progress: INT16;
    more_to_come: INT8;
    ocr_alive: INT8;
    err_code: INT8;
    cancel: CANCEL_FUNC;
    progress_callback: PROGRESS_FUNC;
    end_time: Integer;
    text: array[0..0] of EANYCODE_CHAR;
  end;
  PETEXT_DESC = ^ETEXT_DESC;

type
  PPUTF8Char = ^PUTF8Char;

type
  // General free functions
  TfnTessVersion = function: PUTF8Char; cdecl;
  TfnTessDeleteText = procedure(text: PUTF8Char); cdecl;
  TfnTessDeleteTextArray = procedure(arr: PPUTF8Char); cdecl;
  TfnTessDeleteIntArray = procedure(arr: Pointer); cdecl;

  // Renderer API
  TfnTessTextRendererCreate = function(const outputbase: PUTF8Char): TessResultRenderer; cdecl;
  TfnTessHOcrRendererCreate = function(const outputbase: PUTF8Char): TessResultRenderer; cdecl;
  TfnTessHOcrRendererCreate2 = function(const outputbase: PUTF8Char; font_info: BOOL): TessResultRenderer; cdecl;
  TfnTessPDFRendererCreate = function(const outputbase: PUTF8Char; const datadir: PUTF8Char;
    textonly: BOOL): TessResultRenderer; cdecl;
  TfnTessUnlvRendererCreate = function(const outputbase: PUTF8Char): TessResultRenderer; cdecl;
  TfnTessBoxTextRendererCreate = function(const outputbase: PUTF8Char): TessResultRenderer; cdecl;

  TfnTessDeleteResultRenderer = procedure(renderer: TessResultRenderer); cdecl;
  TfnTessResultRendererInsert = procedure(renderer: TessResultRenderer; next: TessResultRenderer); cdecl;
  TfnTessResultRendererNext = function(renderer: TessResultRenderer): TessResultRenderer; cdecl;
  TfnTessResultRendererBeginDocument = function(renderer: TessResultRenderer; const title: PUTF8Char): BOOL; cdecl;
  TfnTessResultRendererAddImage = function(renderer: TessResultRenderer; api: TessBaseAPI): BOOL; cdecl;
  TfnTessResultRendererEndDocument = function(renderer: TessResultRenderer): BOOL; cdecl;

  TfnTessResultRendererExtention = function(renderer: TessResultRenderer): PUTF8Char; cdecl;
  TfnTessResultRendererTitle = function(renderer: TessResultRenderer): PUTF8Char; cdecl;
  TfnTessResultRendererImageNum = function(renderer: TessResultRenderer): int; cdecl;

  // Base API
  TfnTessBaseAPICreate = function: TessBaseAPI; cdecl;
  TfnTessBaseAPIDelete = procedure(handle: TessBaseAPI); cdecl;

  TfnTessBaseAPIGetOpenCLDevice = function(handle: TessBaseAPI; device: PPointer): size_t; cdecl;

  TfnTessBaseAPISetInputName = procedure(handle: TessBaseAPI; const name: PUTF8Char); cdecl;
  TfnTessBaseAPIGetInputName = function(handle: TessBaseAPI): PUTF8Char; cdecl;

  TfnTessBaseAPISetInputImage = procedure(handle: TessBaseAPI; const pix: PPix); cdecl;
  TfnTessBaseAPIGetInputImage = function(handle: TessBaseAPI): PPix; cdecl;

  TfnTessBaseAPIGetSourceYResolution = function(handle: TessBaseAPI): int; cdecl;
  TfnTessBaseAPIGetDatapath = function(handle: TessBaseAPI): PUTF8Char; cdecl;

  TfnTessBaseAPISetOutputName = procedure(handle: TessBaseAPI; const name: PUTF8Char); cdecl;

  TfnTessBaseAPISetVariable = function(handle: TessBaseAPI; const name: PUTF8Char; const value: PUTF8Char): BOOL; cdecl;
  TfnTessBaseAPISetDebugVariable = function(handle: TessBaseAPI; const name: PUTF8Char; const value: PUTF8Char): BOOL; cdecl;

  TfnTessBaseAPIGetIntVariable = function(const handle: TessBaseAPI; const name: PUTF8Char; out value: int): BOOL; cdecl;
  TfnTessBaseAPIGetBoolVariable = function(const handle: TessBaseAPI; const name: PUTF8Char; out value: BOOL): BOOL; cdecl;
  TfnTessBaseAPIGetDoubleVariable = function(const handle: TessBaseAPI; const name: PUTF8Char; out value: double): BOOL; cdecl;
  TfnTessBaseAPIGetStringVariable = function(const handle: TessBaseAPI; const name: PUTF8Char): PUTF8Char; cdecl;

  TfnTessBaseAPIPrintVariables = procedure(const handle: TessBaseAPI; fp: Pointer); cdecl;
  TfnTessBaseAPIPrintVariablesToFile = function(const handle: TessBaseAPI; const filename: PUTF8Char): BOOL; cdecl;

  TfnTessBaseAPIInit1 = function(handle: TessBaseAPI; const datapath: PUTF8Char;
    const language: PUTF8Char; oem: TessOcrEngineMode; configs: PPUTF8Char; configs_size: int): int; cdecl;
  TfnTessBaseAPIInit2 = function(handle: TessBaseAPI; const datapath: PUTF8Char;
    const language: PUTF8Char; oem: TessOcrEngineMode): int; cdecl;
  TfnTessBaseAPIInit3 = function(handle: TessBaseAPI; const datapath: PUTF8Char; const language: PUTF8Char): int; cdecl;

  TfnTessBaseAPIInit4 = function(handle: TessBaseAPI; const datapath: PUTF8Char; const language: PUTF8Char; oem: TessOcrEngineMode;
    configs: PPUTF8Char; configs_size: int;
    vars_vec: PPUTF8Char; vars_values: PPUTF8Char; vars_vec_size: size_t;
    set_only_non_debug_params: BOOL): int; cdecl;

  TfnTessBaseAPIGetInitLanguagesAsString = function(const handle: TessBaseAPI): PUTF8Char; cdecl;
  TfnTessBaseAPIGetLoadedLanguagesAsVector = function(const handle: TessBaseAPI): PPUTF8Char; cdecl;
  TfnTessBaseAPIGetAvailableLanguagesAsVector = function(const handle: TessBaseAPI): PPUTF8Char; cdecl;

  TfnTessBaseAPIInitLangMod = function(handle: TessBaseAPI; const datapath: PUTF8Char; const language: PUTF8Char): int; cdecl;
  TfnTessBaseAPIInitForAnalysePage = procedure(handle: TessBaseAPI); cdecl;

  TfnTessBaseAPIReadConfigFile = procedure(handle: TessBaseAPI; const filename: PUTF8Char); cdecl;
  TfnTessBaseAPIReadDebugConfigFile = procedure(handle: TessBaseAPI; const filename: PUTF8Char); cdecl;

  TfnTessBaseAPISetPageSegMode = procedure(handle: TessBaseAPI; mode: TessPageSegMode); cdecl;
  TfnTessBaseAPIGetPageSegMode = function(handle: TessBaseAPI): TessPageSegMode; cdecl;

  TfnTessBaseAPIRect = function(handle: TessBaseAPI; const imagedata: PByte; bytes_per_pixel: int;
    bytes_per_line: int; left: int; top: int; width: int; height: int): PUTF8Char; cdecl;

  TfnTessBaseAPIClearAdaptiveClassifier = procedure(handle: TessBaseAPI); cdecl;

  TfnTessBaseAPISetImage = procedure(handle: TessBaseAPI; const imagedata: PByte;
    width: int; height: int; bytes_per_pixel: int; bytes_per_line: int); cdecl;
  TfnTessBaseAPISetImage2 = procedure(handle: TessBaseAPI; pix: PPix); cdecl;

  TfnTessBaseAPISetSourceResolution = procedure(handle: TessBaseAPI; ppi: int); cdecl;

  TfnTessBaseAPISetRectangle = procedure(handle: TessBaseAPI; left: int; top: int; width: int; height: int); cdecl;

  TfnTessBaseAPIGetThresholdedImage = function(handle: TessBaseAPI): PPix; cdecl;
  TfnTessBaseAPIGetRegions = function(handle: TessBaseAPI; var pixa: PPixa): PBoxa; cdecl;
  TfnTessBaseAPIGetTextlines = function(handle: TessBaseAPI; var pixa: PPixa; var blockids: Pint): PBoxa; cdecl;
  TfnTessBaseAPIGetTextlines1 = function(handle: TessBaseAPI; const raw_image: BOOL; const raw_padding: int;
    var pixa: PPixa; var blockids: Pint; var paraids: Pint): PBoxa; cdecl;
  TfnTessBaseAPIGetStrips = function(handle: TessBaseAPI; var pixa: PPixa; var blockids: Pint): PBoxa; cdecl;
  TfnTessBaseAPIGetWords = function(handle: TessBaseAPI; var pixa: PPixa): PBoxa; cdecl;
  TfnTessBaseAPIGetConnectedComponents = function(handle: TessBaseAPI; var cc: PPixa): PBoxa; cdecl;
  TfnTessBaseAPIGetComponentImages = function(handle: TessBaseAPI; const level: TessPageIteratorLevel;
    const text_only: BOOL; var pixa: PPixa; var blockids: Pint): PBoxa; cdecl;
  TfnTessBaseAPIGetComponentImages1 = function(handle: TessBaseAPI; const level: TessPageIteratorLevel;
    const text_only: BOOL; const raw_image: BOOL; const raw_padding: int;
    var pixa: PPixa; var blockids: Pint; var paraids: Pint): PBoxa; cdecl;

  TfnTessBaseAPIGetThresholdedImageScaleFactor = function(const handle: TessBaseAPI): int; cdecl;

  TfnTessBaseAPIDumpPGM = procedure(const handle: TessBaseAPI; const filename: PUTF8Char); cdecl;

  TfnTessBaseAPIAnalyseLayout = function(handle: TessBaseAPI): TessPageIterator; cdecl;

  TfnTessBaseAPIRecognize = function(handle: TessBaseAPI; var monitor: ETEXT_DESC): int; cdecl;
  TfnTessBaseAPIRecognizeForChopTest = function(handle: TessBaseAPI; var monitor: ETEXT_DESC): int; cdecl;
  TfnTessBaseAPIProcessPages = function(handle: TessBaseAPI; const filename: PUTF8Char;
    const retry_config: PUTF8Char; timeout_millisec: int; renderer: TessResultRenderer): BOOL; cdecl;
  TfnTessBaseAPIProcessPage = function(handle: TessBaseAPI; pix: PPix; page_index: int; const filename: PUTF8Char;
    const retry_config: PUTF8Char; timeout_millisec: int; renderer: TessResultRenderer): BOOL; cdecl;

  TfnTessBaseAPIGetIterator = function(handle: TessBaseAPI): TessResultIterator; cdecl;
  TfnTessBaseAPIGetMutableIterator = function(handle: TessBaseAPI): TessMutableIterator; cdecl;

  TfnTessBaseAPIGetUTF8Text = function(handle: TessBaseAPI): PUTF8Char; cdecl;
  TfnTessBaseAPIGetHOCRText = function(handle: TessBaseAPI; page_number: int): PUTF8Char; cdecl;
  TfnTessBaseAPIGetBoxText = function(handle: TessBaseAPI; page_number: int): PUTF8Char; cdecl;
  TfnTessBaseAPIGetUNLVText = function(handle: TessBaseAPI): PUTF8Char; cdecl;
  TfnTessBaseAPIMeanTextConf = function(handle: TessBaseAPI): int; cdecl;
  TfnTessBaseAPIAllWordConfidences = function(handle: TessBaseAPI): Pint; cdecl;
  TfnTessBaseAPIAdaptToWordStr = function(handle: TessBaseAPI; mode: TessPageSegMode; const wordstr: PUTF8Char): BOOL; cdecl;

  TfnTessBaseAPIClear = procedure(handle: TessBaseAPI); cdecl;
  TfnTessBaseAPIEnd = procedure(handle: TessBaseAPI); cdecl;

  TfnTessBaseAPIIsValidWord = function(handle: TessBaseAPI; const word: PUTF8Char): int; cdecl;
  TfnTessBaseAPIGetTextDirection = function(handle: TessBaseAPI; var out_offset: int; var out_slope: float): BOOL; cdecl;

  TfnTessBaseAPIGetUnichar = function(handle: TessBaseAPI; unichar_id: int): PUTF8Char; cdecl;

  TfnTessBaseAPISetMinOrientationMargin = procedure(handle: TessBaseAPI; margin: double); cdecl;

  // Page iterator
  TfnTessPageIteratorDelete = procedure(handle: TessBaseAPI); cdecl;
  TfnTessPageIteratorCopy = function(const handle: TessBaseAPI): TessPageIterator; cdecl;

  TfnTessPageIteratorBegin = procedure(handle: TessPageIterator); cdecl;
  TfnTessPageIteratorNext = function(handle: TessPageIterator; level: TessPageIteratorLevel): BOOL; cdecl;
  TfnTessPageIteratorIsAtBeginningOf = function(const handle: TessPageIterator; level: TessPageIteratorLevel): BOOL; cdecl;
  TfnTessPageIteratorIsAtFinalElement = function(const handle: TessPageIterator; level: TessPageIteratorLevel;
    element: TessPageIteratorLevel): BOOL; cdecl;

  TfnTessPageIteratorBoundingBox = function(const handle: TessPageIterator; level: TessPageIteratorLevel;
    out left: int; out top: int; out right: int; out bottom: int): BOOL; cdecl;

  TfnTessPageIteratorBlockType = function(const handle: TessPageIterator): TessPolyBlockType; cdecl;

  TfnTessPageIteratorGetBinaryImage = function(const handle: TessPageIterator; level: TessPageIteratorLevel): PPix; cdecl;
  TfnTessPageIteratorGetImage = function(const handle: TessPageIterator; level: TessPageIteratorLevel;
    padding: int; original_image: PPix; var left: int; var top: int): PPix; cdecl;

  TfnTessPageIteratorBaseline = function(const handle: TessPageIterator; level: TessPageIteratorLevel;
    out x1: int; out y1: int; out x2: int; out y2: int): BOOL; cdecl;

  TfnTessPageIteratorOrientation = procedure(handle: TessPageIterator; out orientation: TessOrientation;
    out writing_direction: TessWritingDirection; out textline_order: TessTextlineOrder;
    out deskew_angle: float); cdecl;

  TfnTessPageIteratorParagraphInfo = procedure(handle: TessPageIterator; out justification: TessParagraphJustification;
    out is_list_item: BOOL; out is_crown: BOOL; out first_line_indent: int); cdecl;

  // Result iterator
  TfnTessResultIteratorDelete = procedure(handle: TessResultIterator); cdecl;
  TfnTessResultIteratorCopy = function(const handle: TessResultIterator): TessResultIterator; cdecl;
  TfnTessResultIteratorGetPageIterator = function(const handle: TessResultIterator): TessPageIterator; cdecl;
  TfnTessResultIteratorGetPageIteratorConst = function(const handle: TessResultIterator): TessPageIterator; cdecl;
  TfnTessResultIteratorGetChoiceIterator = function(const handle: TessResultIterator): TessChoiceIterator; cdecl;

  TfnTessResultIteratorNext = function(handle: TessResultIterator; level: TessPageIteratorLevel): BOOL; cdecl;
  TfnTessResultIteratorGetUTF8Text = function(const handle: TessResultIterator; level: TessPageIteratorLevel): PUTF8Char; cdecl;
  TfnTessResultIteratorConfidence = function(const handle: TessResultIterator; level: TessPageIteratorLevel): float; cdecl;
  TfnTessResultIteratorWordRecognitionLanguage = function(const handle: TessResultIterator): PUTF8Char; cdecl;
  TfnTessResultIteratorWordFontAttributes = function(const handle: TessResultIterator;
    out is_bold: BOOL; out is_italic: BOOL;
    out is_underlined: BOOL; out is_monospace: BOOL; out is_serif: BOOL;
    out is_smallcaps: BOOL; out pointsize: int; out font_id: int): PUTF8Char; cdecl;

  TfnTessResultIteratorWordIsFromDictionary = function(const handle: TessResultIterator): BOOL; cdecl;
  TfnTessResultIteratorWordIsNumeric = function(const handle: TessResultIterator): BOOL; cdecl;
  TfnTessResultIteratorSymbolIsSuperscript = function(const handle: TessResultIterator): BOOL; cdecl;
  TfnTessResultIteratorSymbolIsSubscript = function(const handle: TessResultIterator): BOOL; cdecl;
  TfnTessResultIteratorSymbolIsDropcap = function(const handle: TessResultIterator): BOOL; cdecl;

  TfnTessChoiceIteratorDelete = procedure(handle: TessChoiceIterator); cdecl;
  TfnTessChoiceIteratorNext = function(handle: TessChoiceIterator): BOOL; cdecl;
  TfnTessChoiceIteratorGetUTF8Text = function(const handle: TessChoiceIterator): PUTF8Char; cdecl;
  TfnTessChoiceIteratorConfidence = function(const handle: TessChoiceIterator): float; cdecl;

var
  // General free functions
  TessVersion: TfnTessVersion;
  TessDeleteText: TfnTessDeleteText;
  TessDeleteTextArray: TfnTessDeleteTextArray;
  TessDeleteIntArray: TfnTessDeleteIntArray;
  // Renderer API
  TessTextRendererCreate: TfnTessTextRendererCreate;
  TessHOcrRendererCreate: TfnTessHOcrRendererCreate;
  TessHOcrRendererCreate2: TfnTessHOcrRendererCreate2;
  TessPDFRendererCreate: TfnTessPDFRendererCreate;
  TessUnlvRendererCreate: TfnTessUnlvRendererCreate;
  TessBoxTextRendererCreate: TfnTessBoxTextRendererCreate;
  TessDeleteResultRenderer: TfnTessDeleteResultRenderer;
  TessResultRendererInsert: TfnTessResultRendererInsert;
  TessResultRendererNext: TfnTessResultRendererNext;
  TessResultRendererBeginDocument: TfnTessResultRendererBeginDocument;
  TessResultRendererAddImage: TfnTessResultRendererAddImage;
  TessResultRendererEndDocument: TfnTessResultRendererEndDocument;
  TessResultRendererExtention: TfnTessResultRendererExtention;
  TessResultRendererTitle: TfnTessResultRendererTitle;
  TessResultRendererImageNum: TfnTessResultRendererImageNum;
  // Base API
  TessBaseAPICreate: TfnTessBaseAPICreate;
  TessBaseAPIDelete: TfnTessBaseAPIDelete;
  TessBaseAPIGetOpenCLDevice: TfnTessBaseAPIGetOpenCLDevice;
  TessBaseAPISetInputName: TfnTessBaseAPISetInputName;
  TessBaseAPIGetInputName: TfnTessBaseAPIGetInputName;
  TessBaseAPISetInputImage: TfnTessBaseAPISetInputImage;
  TessBaseAPIGetInputImage: TfnTessBaseAPIGetInputImage;
  TessBaseAPIGetSourceYResolution: TfnTessBaseAPIGetSourceYResolution;
  TessBaseAPIGetDatapath: TfnTessBaseAPIGetDatapath;
  TessBaseAPISetOutputName: TfnTessBaseAPISetOutputName;
  TessBaseAPISetVariable: TfnTessBaseAPISetVariable;
  TessBaseAPISetDebugVariable: TfnTessBaseAPISetDebugVariable;
  TessBaseAPIGetIntVariable: TfnTessBaseAPIGetIntVariable;
  TessBaseAPIGetBoolVariable: TfnTessBaseAPIGetBoolVariable;
  TessBaseAPIGetDoubleVariable: TfnTessBaseAPIGetDoubleVariable;
  TessBaseAPIGetStringVariable: TfnTessBaseAPIGetStringVariable;
  TessBaseAPIPrintVariables: TfnTessBaseAPIPrintVariables;
  TessBaseAPIPrintVariablesToFile: TfnTessBaseAPIPrintVariablesToFile;
  TessBaseAPIInit1: TfnTessBaseAPIInit1;
  TessBaseAPIInit2: TfnTessBaseAPIInit2;
  TessBaseAPIInit3: TfnTessBaseAPIInit3;
  TessBaseAPIInit4: TfnTessBaseAPIInit4;
  TessBaseAPIGetInitLanguagesAsString: TfnTessBaseAPIGetInitLanguagesAsString;
  TessBaseAPIGetLoadedLanguagesAsVector: TfnTessBaseAPIGetLoadedLanguagesAsVector;
  TessBaseAPIGetAvailableLanguagesAsVector: TfnTessBaseAPIGetAvailableLanguagesAsVector;
  TessBaseAPIInitLangMod: TfnTessBaseAPIInitLangMod;
  TessBaseAPIInitForAnalysePage: TfnTessBaseAPIInitForAnalysePage;
  TessBaseAPIReadConfigFile: TfnTessBaseAPIReadConfigFile;
  TessBaseAPIReadDebugConfigFile: TfnTessBaseAPIReadDebugConfigFile;
  TessBaseAPISetPageSegMode: TfnTessBaseAPISetPageSegMode;
  TessBaseAPIGetPageSegMode: TfnTessBaseAPIGetPageSegMode;
  TessBaseAPIRect: TfnTessBaseAPIRect;
  TessBaseAPIClearAdaptiveClassifier: TfnTessBaseAPIClearAdaptiveClassifier;
  TessBaseAPISetImage: TfnTessBaseAPISetImage;
  TessBaseAPISetImage2: TfnTessBaseAPISetImage2;
  TessBaseAPISetSourceResolution: TfnTessBaseAPISetSourceResolution;
  TessBaseAPISetRectangle: TfnTessBaseAPISetRectangle;
  TessBaseAPIGetThresholdedImage: TfnTessBaseAPIGetThresholdedImage;
  TessBaseAPIGetRegions: TfnTessBaseAPIGetRegions;
  TessBaseAPIGetTextlines: TfnTessBaseAPIGetTextlines;
  TessBaseAPIGetTextlines1: TfnTessBaseAPIGetTextlines1;
  TessBaseAPIGetStrips: TfnTessBaseAPIGetStrips;
  TessBaseAPIGetWords: TfnTessBaseAPIGetWords;
  TessBaseAPIGetConnectedComponents: TfnTessBaseAPIGetConnectedComponents;
  TessBaseAPIGetComponentImages: TfnTessBaseAPIGetComponentImages;
  TessBaseAPIGetComponentImages1: TfnTessBaseAPIGetComponentImages1;
  TessBaseAPIGetThresholdedImageScaleFactor: TfnTessBaseAPIGetThresholdedImageScaleFactor;
  TessBaseAPIDumpPGM: TfnTessBaseAPIDumpPGM;
  TessBaseAPIAnalyseLayout: TfnTessBaseAPIAnalyseLayout;
  TessBaseAPIRecognize: TfnTessBaseAPIRecognize;
  TessBaseAPIRecognizeForChopTest: TfnTessBaseAPIRecognizeForChopTest;
  TessBaseAPIProcessPages: TfnTessBaseAPIProcessPages;
  TessBaseAPIProcessPage: TfnTessBaseAPIProcessPage;
  TessBaseAPIGetIterator: TfnTessBaseAPIGetIterator;
  TessBaseAPIGetMutableIterator: TfnTessBaseAPIGetMutableIterator;
  TessBaseAPIGetUTF8Text: TfnTessBaseAPIGetUTF8Text;
  TessBaseAPIGetHOCRText: TfnTessBaseAPIGetHOCRText;
  TessBaseAPIGetBoxText: TfnTessBaseAPIGetBoxText;
  TessBaseAPIGetUNLVText: TfnTessBaseAPIGetUNLVText;
  TessBaseAPIMeanTextConf: TfnTessBaseAPIMeanTextConf;
  TessBaseAPIAllWordConfidences: TfnTessBaseAPIAllWordConfidences;
  TessBaseAPIAdaptToWordStr: TfnTessBaseAPIAdaptToWordStr;
  TessBaseAPIClear: TfnTessBaseAPIClear;
  TessBaseAPIEnd: TfnTessBaseAPIEnd;
  TessBaseAPIIsValidWord: TfnTessBaseAPIIsValidWord;
  TessBaseAPIGetTextDirection: TfnTessBaseAPIGetTextDirection;
  TessBaseAPIGetUnichar: TfnTessBaseAPIGetUnichar;
  TessBaseAPISetMinOrientationMargin: TfnTessBaseAPISetMinOrientationMargin;
  // Page iterator
  TessPageIteratorDelete: TfnTessPageIteratorDelete;
  TessPageIteratorCopy: TfnTessPageIteratorCopy;
  TessPageIteratorBegin: TfnTessPageIteratorBegin;
  TessPageIteratorNext: TfnTessPageIteratorNext;
  TessPageIteratorIsAtBeginningOf: TfnTessPageIteratorIsAtBeginningOf;
  TessPageIteratorIsAtFinalElement: TfnTessPageIteratorIsAtFinalElement;
  TessPageIteratorBoundingBox: TfnTessPageIteratorBoundingBox;
  TessPageIteratorBlockType: TfnTessPageIteratorBlockType;
  TessPageIteratorGetBinaryImage: TfnTessPageIteratorGetBinaryImage;
  TessPageIteratorGetImage: TfnTessPageIteratorGetBinaryImage;
  TessPageIteratorBaseline: TfnTessPageIteratorBaseline;
  TessPageIteratorOrientation: TfnTessPageIteratorOrientation;
  TessPageIteratorParagraphInfo: TfnTessPageIteratorParagraphInfo;
  // Result iterator
  TessResultIteratorDelete: TfnTessResultIteratorDelete;
  TessResultIteratorCopy: TfnTessResultIteratorCopy;
  TessResultIteratorGetPageIterator: TfnTessResultIteratorGetPageIterator;
  TessResultIteratorGetPageIteratorConst: TfnTessResultIteratorGetPageIteratorConst;
  TessResultIteratorGetChoiceIterator: TfnTessResultIteratorGetChoiceIterator;
  TessResultIteratorNext: TfnTessResultIteratorNext;
  TessResultIteratorGetUTF8Text: TfnTessResultIteratorGetUTF8Text;
  TessResultIteratorConfidence: TfnTessResultIteratorConfidence;
  TessResultIteratorWordRecognitionLanguage: TfnTessResultIteratorWordRecognitionLanguage;
  TessResultIteratorWordFontAttributes: TfnTessResultIteratorWordFontAttributes;
  TessResultIteratorWordIsFromDictionary: TfnTessResultIteratorWordIsFromDictionary;
  TessResultIteratorWordIsNumeric: TfnTessResultIteratorWordIsNumeric;
  TessResultIteratorSymbolIsSuperscript: TfnTessResultIteratorSymbolIsSuperscript;
  TessResultIteratorSymbolIsSubscript: TfnTessResultIteratorSymbolIsSubscript;
  TessResultIteratorSymbolIsDropcap: TfnTessResultIteratorSymbolIsDropcap;
  TessChoiceIteratorDelete: TfnTessChoiceIteratorDelete;
  TessChoiceIteratorNext: TfnTessChoiceIteratorNext;
  TessChoiceIteratorGetUTF8Text: TfnTessChoiceIteratorGetUTF8Text;
  TessChoiceIteratorConfidence: TfnTessChoiceIteratorConfidence;

var
  hTesseractLib: THandle;

function TeVersion: PUTF8Char; cdecl; external libtesseract name 'TessVersion';

implementation
uses
  {$IFNDEF FPC}Winapi.Windows{$ELSE}dynlibs, SysUtils{$ENDIF};

procedure FreeTesseractLib;
begin
  if (hTesseractLib <> 0) then
  begin
    FreeLibrary(hTesseractLib);
    hTesseractLib := 0;
  end;
end;

function InitTesseractLib: Boolean;
begin
  Result := False;

  if (hTesseractLib = 0) then
  begin
    hTesseractLib := LoadLibrary({$IFDEF FPC}ExtractFilePath(ParamStr(0)) + libtesseract{$ELSE}PChar(libtesseract){$ENDIF});
    if (hTesseractLib <> 0) then
    begin
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessVersion{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessVersion');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteText{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessDeleteText');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteTextArray{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessDeleteTextArray');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteIntArray{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessDeleteIntArray');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessTextRendererCreate{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessTextRendererCreate');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessHOcrRendererCreate{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessHOcrRendererCreate');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessHOcrRendererCreate2{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessHOcrRendererCreate2');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPDFRendererCreate{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPDFRendererCreate');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessUnlvRendererCreate{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessUnlvRendererCreate');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBoxTextRendererCreate{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBoxTextRendererCreate');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteResultRenderer{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessDeleteResultRenderer');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererInsert{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererInsert');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererNext{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererNext');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererBeginDocument{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererBeginDocument');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererAddImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererAddImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererEndDocument{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererEndDocument');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererExtention{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererExtention');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererTitle{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererTitle');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererImageNum{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultRendererImageNum');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPICreate{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPICreate');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIDelete{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIDelete');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetOpenCLDevice{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetOpenCLDevice');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetInputName{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetInputName');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetInputName{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetInputName');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetInputImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetInputImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetInputImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetInputImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetSourceYResolution{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetSourceYResolution');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetDatapath{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetDatapath');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetOutputName{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetOutputName');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetVariable{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetVariable');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetDebugVariable{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetDebugVariable');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetIntVariable{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetIntVariable');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetBoolVariable{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetBoolVariable');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetDoubleVariable{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetDoubleVariable');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetStringVariable{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetStringVariable');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIPrintVariables{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIPrintVariables');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIPrintVariablesToFile{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIPrintVariablesToFile');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit1{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIInit1');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit2{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIInit2');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit3{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIInit3');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit4{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIInit4');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetInitLanguagesAsString{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetInitLanguagesAsString');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetLoadedLanguagesAsVector{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetLoadedLanguagesAsVector');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetAvailableLanguagesAsVector{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetAvailableLanguagesAsVector');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInitLangMod{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIInitLangMod');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInitForAnalysePage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIInitForAnalysePage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIReadConfigFile{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIReadConfigFile');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIReadDebugConfigFile{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIReadDebugConfigFile');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetPageSegMode{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetPageSegMode');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetPageSegMode{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetPageSegMode');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIRect{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIRect');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIClearAdaptiveClassifier{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIClearAdaptiveClassifier');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetImage2{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetImage2');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetSourceResolution{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetSourceResolution');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetRectangle{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetRectangle');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetThresholdedImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetThresholdedImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetRegions{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetRegions');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetTextlines{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetTextlines');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetTextlines1{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetTextlines1');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetStrips{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetStrips');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetWords{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetWords');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetConnectedComponents{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetConnectedComponents');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetComponentImages{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetComponentImages');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetComponentImages1{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetComponentImages1');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetThresholdedImageScaleFactor{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetThresholdedImageScaleFactor');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIDumpPGM{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIDumpPGM');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIAnalyseLayout{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIAnalyseLayout');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIRecognize{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIRecognize');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIRecognizeForChopTest{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIRecognizeForChopTest');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIProcessPages{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIProcessPages');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIProcessPage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIProcessPage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetIterator{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetIterator');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetMutableIterator{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetMutableIterator');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetUTF8Text{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetUTF8Text');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetHOCRText{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetHOCRText');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetBoxText{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetBoxText');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetUNLVText{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetUNLVText');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIMeanTextConf{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIMeanTextConf');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIAllWordConfidences{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIAllWordConfidences');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIAdaptToWordStr{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIAdaptToWordStr');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIClear{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIClear');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIEnd{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIEnd');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIIsValidWord{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIIsValidWord');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetTextDirection{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetTextDirection');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetUnichar{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPIGetUnichar');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetMinOrientationMargin{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessBaseAPISetMinOrientationMargin');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorDelete{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorDelete');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorCopy{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorCopy');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBegin{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorBegin');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorNext{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorNext');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorIsAtBeginningOf{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorIsAtBeginningOf');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorIsAtFinalElement{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorIsAtFinalElement');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBoundingBox{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorBoundingBox');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBlockType{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorBlockType');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorGetBinaryImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorGetBinaryImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorGetImage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorGetImage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBaseline{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorBaseline');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorOrientation{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorOrientation');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorParagraphInfo{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessPageIteratorParagraphInfo');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorDelete{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorDelete');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorCopy{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorCopy');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetPageIterator{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorGetPageIterator');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetPageIteratorConst{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorGetPageIteratorConst');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetChoiceIterator{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorGetChoiceIterator');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorNext{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorNext');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetUTF8Text{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorGetUTF8Text');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorConfidence{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorConfidence');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordRecognitionLanguage{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorWordRecognitionLanguage');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordFontAttributes{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorWordFontAttributes');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordIsFromDictionary{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorWordIsFromDictionary');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordIsNumeric{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorWordIsNumeric');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorSymbolIsSuperscript{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorSymbolIsSuperscript');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorSymbolIsSubscript{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorSymbolIsSubscript');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorSymbolIsDropcap{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessResultIteratorSymbolIsDropcap');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorDelete{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessChoiceIteratorDelete');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorNext{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessChoiceIteratorNext');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorGetUTF8Text{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessChoiceIteratorGetUTF8Text');
      {$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorConfidence{$IFDEF FPC}){$ENDIF} := GetProcAddress(hTesseractLib, 'TessChoiceIteratorConfidence');

      Result := Assigned(TessVersion) and
                Assigned(TessDeleteText) and
                Assigned(TessDeleteTextArray) and
                Assigned(TessDeleteIntArray) and
                Assigned(TessTextRendererCreate) and
                Assigned(TessHOcrRendererCreate) and
                Assigned(TessHOcrRendererCreate2) and
                Assigned(TessPDFRendererCreate) and
                Assigned(TessUnlvRendererCreate) and
                Assigned(TessBoxTextRendererCreate) and
                Assigned(TessDeleteResultRenderer) and
                Assigned(TessResultRendererInsert) and
                Assigned(TessResultRendererNext) and
                Assigned(TessResultRendererBeginDocument) and
                Assigned(TessResultRendererAddImage) and
                Assigned(TessResultRendererEndDocument) and
                Assigned(TessResultRendererExtention) and
                Assigned(TessResultRendererTitle) and
                Assigned(TessResultRendererImageNum) and
                Assigned(TessBaseAPICreate) and
                Assigned(TessBaseAPIDelete) and
                Assigned(TessBaseAPIGetOpenCLDevice) and
                Assigned(TessBaseAPISetInputName) and
                Assigned(TessBaseAPIGetInputName) and
                Assigned(TessBaseAPIGetSourceYResolution) and
                Assigned(TessBaseAPIGetDatapath) and
                Assigned(TessBaseAPISetOutputName) and
                Assigned(TessBaseAPISetVariable) and
                Assigned(TessBaseAPISetDebugVariable) and
                Assigned(TessBaseAPIGetIntVariable) and
                Assigned(TessBaseAPIGetBoolVariable) and
                Assigned(TessBaseAPIGetDoubleVariable) and
                Assigned(TessBaseAPIGetStringVariable) and
                Assigned(TessBaseAPIPrintVariables) and
                Assigned(TessBaseAPIPrintVariablesToFile) and
                Assigned(TessBaseAPIInit1) and
                Assigned(TessBaseAPIInit2) and
                Assigned(TessBaseAPIInit3) and
                Assigned(TessBaseAPIInit4) and
                Assigned(TessBaseAPIGetInitLanguagesAsString) and
                Assigned(TessBaseAPIGetLoadedLanguagesAsVector) and
                Assigned(TessBaseAPIGetAvailableLanguagesAsVector) and
                Assigned(TessBaseAPIInitLangMod) and
                Assigned(TessBaseAPIInitForAnalysePage) and
                Assigned(TessBaseAPIReadConfigFile) and
                Assigned(TessBaseAPIReadDebugConfigFile) and
                Assigned(TessBaseAPISetPageSegMode) and
                Assigned(TessBaseAPIGetPageSegMode) and
                Assigned(TessBaseAPIRect) and
                Assigned(TessBaseAPIClearAdaptiveClassifier) and
                Assigned(TessBaseAPISetImage) and
                Assigned(TessBaseAPISetImage2) and
                Assigned(TessBaseAPISetSourceResolution) and
                Assigned(TessBaseAPISetRectangle) and
                Assigned(TessBaseAPIGetThresholdedImage) and
                Assigned(TessBaseAPIGetRegions) and
                Assigned(TessBaseAPIGetTextlines) and
                Assigned(TessBaseAPIGetTextlines1) and
                Assigned(TessBaseAPIGetStrips) and
                Assigned(TessBaseAPIGetWords) and
                Assigned(TessBaseAPIGetConnectedComponents) and
                Assigned(TessBaseAPIGetComponentImages) and
                Assigned(TessBaseAPIGetComponentImages1) and
                Assigned(TessBaseAPIGetThresholdedImageScaleFactor) and
                Assigned(TessBaseAPIDumpPGM) and
                Assigned(TessBaseAPIAnalyseLayout) and
                Assigned(TessBaseAPIRecognize) and
                Assigned(TessBaseAPIRecognizeForChopTest) and
                Assigned(TessBaseAPIProcessPages) and
                Assigned(TessBaseAPIProcessPage) and
                Assigned(TessBaseAPIGetIterator) and
                Assigned(TessBaseAPIGetMutableIterator) and
                Assigned(TessBaseAPIGetUTF8Text) and
                Assigned(TessBaseAPIGetHOCRText) and
                Assigned(TessBaseAPIGetBoxText) and
                Assigned(TessBaseAPIGetUNLVText) and
                Assigned(TessBaseAPIMeanTextConf) and
                Assigned(TessBaseAPIAllWordConfidences) and
                Assigned(TessBaseAPIAdaptToWordStr) and
                Assigned(TessBaseAPIClear) and
                Assigned(TessBaseAPIEnd) and
                Assigned(TessBaseAPIIsValidWord) and
                Assigned(TessBaseAPIGetTextDirection) and
                Assigned(TessBaseAPIGetUnichar) and
                Assigned(TessBaseAPISetMinOrientationMargin) and
                Assigned(TessPageIteratorDelete) and
                Assigned(TessPageIteratorCopy) and
                Assigned(TessPageIteratorBegin) and
                Assigned(TessPageIteratorNext) and
                Assigned(TessPageIteratorIsAtBeginningOf) and
                Assigned(TessPageIteratorIsAtFinalElement) and
                Assigned(TessPageIteratorBoundingBox) and
                Assigned(TessPageIteratorBlockType) and
                Assigned(TessPageIteratorGetBinaryImage) and
                Assigned(TessPageIteratorGetImage) and
                Assigned(TessPageIteratorBaseline) and
                Assigned(TessPageIteratorOrientation) and
                Assigned(TessPageIteratorParagraphInfo) and
                Assigned(TessResultIteratorDelete) and
                Assigned(TessResultIteratorCopy) and
                Assigned(TessResultIteratorGetPageIterator) and
                Assigned(TessResultIteratorGetPageIteratorConst) and
                Assigned(TessResultIteratorGetChoiceIterator) and
                Assigned(TessResultIteratorNext) and
                Assigned(TessResultIteratorGetUTF8Text) and
                Assigned(TessResultIteratorConfidence) and
                Assigned(TessResultIteratorWordRecognitionLanguage) and
                Assigned(TessResultIteratorWordFontAttributes) and
                Assigned(TessResultIteratorWordIsFromDictionary) and
                Assigned(TessResultIteratorWordIsNumeric) and
                Assigned(TessResultIteratorSymbolIsSuperscript) and
                Assigned(TessResultIteratorSymbolIsSubscript) and
                Assigned(TessResultIteratorSymbolIsDropcap) and
                Assigned(TessChoiceIteratorDelete) and
                Assigned(TessChoiceIteratorNext) and
                Assigned(TessChoiceIteratorGetUTF8Text) and
                Assigned(TessChoiceIteratorConfidence);

      if not Result then
        FreeTesseractLib;
    end;
  end;
end;

initialization
  InitTesseractLib;

finalization
  FreeTesseractLib;

end.
