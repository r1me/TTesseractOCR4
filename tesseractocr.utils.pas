unit tesseractocr.utils;

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
  tesseractocr.capi,
  tesseractocr.consts;

function PageOrientationToString(ATessPageOrientation: TessOrientation): String;
function TextlineOrderToString(ATessTextlineOrder: TessTextlineOrder): String;
function WritingDirectionToString(ATessWritingDirection: TessWritingDirection): String;
function ParagraphJustificationToString(ATessParagraphJustification: TessParagraphJustification): String;
function BlockTypeToString(ATessPolyBlockType: TessPolyBlockType): String;
function PUTF8CharToString(AUTF8Char: PUTF8Char; ADeleteText: Boolean = True): String;

implementation

function PageOrientationToString(ATessPageOrientation: TessOrientation): String;
begin
  case ATessPageOrientation of
    ORIENTATION_PAGE_UP: Result := 'Up';
    ORIENTATION_PAGE_RIGHT: Result := 'Right';
    ORIENTATION_PAGE_DOWN: Result := 'Down';
    ORIENTATION_PAGE_LEFT: Result := 'Left';
	else 
	  Result := '';
  end;
end;

function TextlineOrderToString(ATessTextlineOrder: TessTextlineOrder): String;
begin
  case ATessTextlineOrder of
    TEXTLINE_ORDER_LEFT_TO_RIGHT: Result := 'LTR';
    TEXTLINE_ORDER_RIGHT_TO_LEFT: Result := 'RTL';
    TEXTLINE_ORDER_TOP_TO_BOTTOM: Result := 'TTB';
	else 
	  Result := '';
  end;
end;

function WritingDirectionToString(ATessWritingDirection: TessWritingDirection): String;
begin
  case ATessWritingDirection of
    WRITING_DIRECTION_LEFT_TO_RIGHT: Result := 'LTR';
    WRITING_DIRECTION_RIGHT_TO_LEFT: Result := 'RTL';
    WRITING_DIRECTION_TOP_TO_BOTTOM: Result := 'TTB';
	else 
	  Result := '';
  end;
end;

function ParagraphJustificationToString(ATessParagraphJustification: TessParagraphJustification): String;
begin
  case ATessParagraphJustification of
    JUSTIFICATION_UNKNOWN: Result := 'Unknown';
    JUSTIFICATION_LEFT: Result := 'Left';
    JUSTIFICATION_CENTER: Result := 'Center';
    JUSTIFICATION_RIGHT: Result := 'Right';
	else 
	  Result := '';
  end;
end;

function BlockTypeToString(ATessPolyBlockType: TessPolyBlockType): String;
begin
  case ATessPolyBlockType of
    PT_UNKNOWN: Result := 'Unknown';
    PT_FLOWING_TEXT: Result := 'Flowing text';
    PT_HEADING_TEXT: Result := 'Heading text';
    PT_PULLOUT_TEXT: Result := 'Pull-out text';
    PT_EQUATION: Result := 'Equation';
    PT_INLINE_EQUATION: Result := 'Inline equation';
    PT_TABLE: Result := 'Table';
    PT_VERTICAL_TEXT: Result := 'Vertical text';
    PT_CAPTION_TEXT: Result := 'Caption';
    PT_FLOWING_IMAGE: Result := 'Flowing image';
    PT_HEADING_IMAGE: Result := 'Heading image';
    PT_PULLOUT_IMAGE: Result := 'Pull-out image';
    PT_HORZ_LINE: Result := 'Horizontal line';
    PT_VERT_LINE: Result := 'Vertical line';
    PT_NOISE: Result := 'Noise';
    PT_COUNT: Result := 'Count';
	else 
	  Result := '';
  end;
end;

function PUTF8CharToString(AUTF8Char: PUTF8Char; ADeleteText: Boolean = True): String;
var
  utfString: UTF8String;
begin
  Result := '';

  if Assigned(AUTF8Char) then
  begin
    SetString(utfString, AUTF8Char, Length(AUTF8Char));
    if ADeleteText then
      TessDeleteText(AUTF8Char);
    Result := String(utfString);
  end;
end;

end.
