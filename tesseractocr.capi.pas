unit tesseractocr.capi;

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

implementation
uses
  {$IFNDEF FPC}Winapi.Windows, System.SysUtils{$ELSE}dynlibs, SysUtils{$ENDIF};

procedure FreeTesseractLib;
begin
  if (hTesseractLib <> 0) then
  begin
    FreeLibrary(hTesseractLib);
    hTesseractLib := 0;
  end;
end;

function InitTesseractLib: Boolean;

  function GetTesseractProcAddress(var AProcPtr: Pointer; AProcName: AnsiString): Boolean;
  begin
    AProcPtr := GetProcAddress(hTesseractLib, {$IFDEF FPC}AProcName{$ELSE}PAnsiChar(AProcName){$ENDIF});
    Result := Assigned(AProcPtr);
    if not Result then
      raise Exception.Create('Error while loading Tesseract function: ' + String(AProcName));
  end;

begin
  Result := False;

  if (hTesseractLib = 0) then
  begin
    hTesseractLib := LoadLibrary({$IFDEF FPC}libtesseract{$ELSE}PChar(libtesseract){$ENDIF});
    if (hTesseractLib <> 0) then
    begin
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessVersion{$IFDEF FPC}){$ENDIF}, 'TessVersion');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteText{$IFDEF FPC}){$ENDIF}, 'TessDeleteText');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteTextArray{$IFDEF FPC}){$ENDIF}, 'TessDeleteTextArray');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteIntArray{$IFDEF FPC}){$ENDIF}, 'TessDeleteIntArray');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessTextRendererCreate{$IFDEF FPC}){$ENDIF}, 'TessTextRendererCreate');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessHOcrRendererCreate{$IFDEF FPC}){$ENDIF}, 'TessHOcrRendererCreate');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessHOcrRendererCreate2{$IFDEF FPC}){$ENDIF}, 'TessHOcrRendererCreate2');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPDFRendererCreate{$IFDEF FPC}){$ENDIF}, 'TessPDFRendererCreate');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessUnlvRendererCreate{$IFDEF FPC}){$ENDIF}, 'TessUnlvRendererCreate');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBoxTextRendererCreate{$IFDEF FPC}){$ENDIF}, 'TessBoxTextRendererCreate');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessDeleteResultRenderer{$IFDEF FPC}){$ENDIF}, 'TessDeleteResultRenderer');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererInsert{$IFDEF FPC}){$ENDIF}, 'TessResultRendererInsert');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererNext{$IFDEF FPC}){$ENDIF}, 'TessResultRendererNext');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererBeginDocument{$IFDEF FPC}){$ENDIF}, 'TessResultRendererBeginDocument');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererAddImage{$IFDEF FPC}){$ENDIF}, 'TessResultRendererAddImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererEndDocument{$IFDEF FPC}){$ENDIF}, 'TessResultRendererEndDocument');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererExtention{$IFDEF FPC}){$ENDIF}, 'TessResultRendererExtention');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererTitle{$IFDEF FPC}){$ENDIF}, 'TessResultRendererTitle');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultRendererImageNum{$IFDEF FPC}){$ENDIF}, 'TessResultRendererImageNum');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPICreate{$IFDEF FPC}){$ENDIF}, 'TessBaseAPICreate');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIDelete{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIDelete');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetOpenCLDevice{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetOpenCLDevice');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetInputName{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetInputName');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetInputName{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetInputName');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetInputImage{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetInputImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetInputImage{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetInputImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetSourceYResolution{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetSourceYResolution');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetDatapath{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetDatapath');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetOutputName{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetOutputName');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetVariable{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetVariable');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetDebugVariable{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetDebugVariable');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetIntVariable{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetIntVariable');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetBoolVariable{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetBoolVariable');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetDoubleVariable{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetDoubleVariable');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetStringVariable{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetStringVariable');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIPrintVariables{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIPrintVariables');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIPrintVariablesToFile{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIPrintVariablesToFile');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit1{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIInit1');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit2{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIInit2');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit3{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIInit3');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInit4{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIInit4');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetInitLanguagesAsString{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetInitLanguagesAsString');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetLoadedLanguagesAsVector{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetLoadedLanguagesAsVector');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetAvailableLanguagesAsVector{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetAvailableLanguagesAsVector');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInitLangMod{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIInitLangMod');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIInitForAnalysePage{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIInitForAnalysePage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIReadConfigFile{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIReadConfigFile');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIReadDebugConfigFile{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIReadDebugConfigFile');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetPageSegMode{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetPageSegMode');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetPageSegMode{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetPageSegMode');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIRect{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIRect');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIClearAdaptiveClassifier{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIClearAdaptiveClassifier');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetImage{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetImage2{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetImage2');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetSourceResolution{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetSourceResolution');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetRectangle{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetRectangle');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetThresholdedImage{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetThresholdedImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetRegions{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetRegions');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetTextlines{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetTextlines');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetTextlines1{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetTextlines1');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetStrips{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetStrips');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetWords{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetWords');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetConnectedComponents{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetConnectedComponents');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetComponentImages{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetComponentImages');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetComponentImages1{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetComponentImages1');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetThresholdedImageScaleFactor{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetThresholdedImageScaleFactor');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIAnalyseLayout{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIAnalyseLayout');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIRecognize{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIRecognize');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIRecognizeForChopTest{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIRecognizeForChopTest');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIProcessPages{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIProcessPages');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIProcessPage{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIProcessPage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetIterator{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetIterator');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetMutableIterator{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetMutableIterator');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetUTF8Text{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetUTF8Text');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetHOCRText{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetHOCRText');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetBoxText{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetBoxText');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetUNLVText{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetUNLVText');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIMeanTextConf{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIMeanTextConf');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIAllWordConfidences{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIAllWordConfidences');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIAdaptToWordStr{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIAdaptToWordStr');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIClear{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIClear');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIEnd{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIEnd');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIIsValidWord{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIIsValidWord');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetTextDirection{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetTextDirection');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPIGetUnichar{$IFDEF FPC}){$ENDIF}, 'TessBaseAPIGetUnichar');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessBaseAPISetMinOrientationMargin{$IFDEF FPC}){$ENDIF}, 'TessBaseAPISetMinOrientationMargin');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorDelete{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorDelete');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorCopy{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorCopy');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBegin{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorBegin');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorNext{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorNext');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorIsAtBeginningOf{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorIsAtBeginningOf');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorIsAtFinalElement{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorIsAtFinalElement');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBoundingBox{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorBoundingBox');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBlockType{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorBlockType');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorGetBinaryImage{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorGetBinaryImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorGetImage{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorGetImage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorBaseline{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorBaseline');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorOrientation{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorOrientation');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessPageIteratorParagraphInfo{$IFDEF FPC}){$ENDIF}, 'TessPageIteratorParagraphInfo');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorDelete{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorDelete');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorCopy{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorCopy');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetPageIterator{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorGetPageIterator');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetPageIteratorConst{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorGetPageIteratorConst');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetChoiceIterator{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorGetChoiceIterator');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorNext{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorNext');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorGetUTF8Text{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorGetUTF8Text');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorConfidence{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorConfidence');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordRecognitionLanguage{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorWordRecognitionLanguage');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordFontAttributes{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorWordFontAttributes');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordIsFromDictionary{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorWordIsFromDictionary');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorWordIsNumeric{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorWordIsNumeric');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorSymbolIsSuperscript{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorSymbolIsSuperscript');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorSymbolIsSubscript{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorSymbolIsSubscript');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessResultIteratorSymbolIsDropcap{$IFDEF FPC}){$ENDIF}, 'TessResultIteratorSymbolIsDropcap');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorDelete{$IFDEF FPC}){$ENDIF}, 'TessChoiceIteratorDelete');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorNext{$IFDEF FPC}){$ENDIF}, 'TessChoiceIteratorNext');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorGetUTF8Text{$IFDEF FPC}){$ENDIF}, 'TessChoiceIteratorGetUTF8Text');
      GetTesseractProcAddress({$IFNDEF FPC}@{$ELSE}Pointer({$ENDIF}TessChoiceIteratorConfidence{$IFDEF FPC}){$ENDIF}, 'TessChoiceIteratorConfidence');

      Result := True;
    end;
  end;
end;

initialization
  InitTesseractLib;

finalization
  FreeTesseractLib;

end.
