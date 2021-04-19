{ Draws ascii art text using fonts from www.figlet.org.

  Copyright (C) 2021 Domingo Galmés dgalmesp@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit figletfont;

{$mode ObjFPC}{$H+}{$M+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type

  TFigletDrawMode = (fdmFull, fdmFitted, fdmSmush,fdmSmushU);
  TFigletAlign = (faLeft, faCenter, faRight, faLeft2R);

  TFigletFont = class;

  TCharacter = record
    Character: char;
    LineStart: integer;
    Width: integer;
    LK: integer;
    RK: integer;
    procedure Init(aCharCode: integer; aFont: TFigletFont);
  end;

  TFigletFont = class
  protected
    FLines: TStringList;
    Hardblank: char;
    Height: integer;
    Baseline: integer;
    Max_Length: integer;
    Old_Layout: integer;
    Comment_Lines: integer;
    Print_Direction: integer;
    Full_Layout: integer;
    Codetag_Count: integer;
    CurrentChar: TCharacter;
    LastChar: TCharacter;
    FAlign: TFigletAlign;
    FDrawMode: TFigletDrawMode;
    FLineStartText: string;
    FLineEndText: string;
    FLineWidth: integer;
    FAdditionalSpaces: integer;
    FontSmushMode:integer;  //<default options for the font.
    SmushMode: integer;
    function FindCharCodeLineStart(aCharCode: integer): integer;
    procedure ReadHeader;
    function DrawChar(aCharCode: integer; aStartPos: integer): integer;
    function SmushChars(aRow: integer; aLIndex: integer; aRIndex: integer): char;
    function FindCharCodeWidth(LineStart: integer): integer;
    function FindKerning:integer;
  public
    Output: TStringList;
    constructor Create(aFontFileName: string); virtual;
    destructor Destroy; override;
    procedure DrawText(aText: string);
    function LoadFont(aFileName: string): boolean;
    function GetFontSource: string;
  published
    property Align: TFigletAlign read FAlign write FAlign;
    property DrawMode: TFigletDrawMode read FDrawMode write FDrawMode;
    property LineStartText: string read FLineStartText write FLineStartText;
    property LineEndText: string read FLineEndText write FLineEndText;
    property LineWidth: integer read FLineWidth write FLineWidth;
    property AdditionalSpaces: integer read FAdditionalSpaces write FAdditionalSpaces;
  end;

implementation

uses
  StreamUnzipper, Math;

const
  MAX_LINE_LENGTH = 200;
  SM_SMUSH=128;
  SM_KERN=64;
  SM_EQUAL=1;
  SM_LOWLINE=2;
  SM_HIERARCHY=4;
  SM_PAIR=8;
  SM_BIGX=16;
  SM_HARDBLANK=32;

procedure TCharacter.Init(aCharCode: integer; aFont: TFigletFont);
begin
  Character := char(aCharCode);
  LineStart := 0;
  Width := 0;
  if aCharCode <> 0 then
  begin
    LineStart := aFont.FindCharCodeLineStart(aCharCode);
    Width := aFont.FindCharCodeWidth(LineStart);
  end;
end;

//Bytes starting with '0' (0xxxxxxx) are reserved for ASCII-compatible single byte characters. With multi-byte codepoints the number of 1’s in the leading byte determines the number of bytes the codepoint occupies. Like this :

//1 byte  : 0xxxxxxx
//2 bytes : 110xxxxx 10xxxxxx
//3 bytes : 1110xxxx 10xxxxxx 10xxxxxx
//4 bytes : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
//https://nullprogram.com/blog/2017/10/06/
function Utf8decodeDGP(bytes: pansichar; out aLenght: integer): uint32;
var
  ch, ch1, ch2, ch3: byte;
begin
  ch := byte(bytes^);
  if ch <= $07F then
  begin
    Result := ch;
    aLenght := 1;
    exit;
  end
  else if (ch and $E0 = $C0) then
  begin
    Inc(bytes);
    ch1 := byte(bytes^);
    Result := (word(ch and $1F) shl 6) or word(ch1 and $3F);
    aLenght := 2;
    exit;
  end
  else if (ch and $F0 = $E0) then
  begin
    Inc(bytes);
    ch1 := byte(bytes^);
    Inc(bytes);
    ch2 := byte(bytes^);

    Result := (word(ch and $0F) shl 12) or (word(ch1 and $3F) shl 6) or (word(ch2 and $3F));
    aLenght := 3;
    exit;
  end
  else if (ch and $F8 = $F0) then
  begin
    Inc(bytes);
    ch1 := byte(bytes^);
    Inc(bytes);
    ch2 := byte(bytes^);
    Inc(bytes);
    ch3 := byte(bytes^);
    Result := (word(ch1 and $04) shl 18) or (word(ch1 and $3F) shl 12) or (word(ch2 and $3F) shl 6) or
      (word(ch3 and $3F));
    aLenght := 4;
    exit;
  end;
end;

//lee un stream en una cadena ansi.
function StreamToString(aStream: TStream): ansistring;
begin
  Result := '';
  try
    SetLength(Result, aStream.Size);
    aStream.Position := 0;
    aStream.Read(Pointer(Result)^, aStream.Size);
  except
    Result := '';  // Deallocates memory
    raise;
  end;
end;

{
//  _______ ______ _       _      _   ______          _                       //
// |__   __|  ____(_)     | |    | | |  ____|        | |                      //
//    | |  | |__   _  __ _| | ___| |_| |__ ___  _ __ | |_                     //
//    | |  |  __| | |/ _` | |/ _ \ __|  __/ _ \| '_ \| __|                    //
//    | |  | |    | | (_| | |  __/ |_| | | (_) | | | | |_                     //
//    |_|  |_|    |_|\__, |_|\___|\__|_|  \___/|_| |_|\__|                    //
//                    __/ |                                                   //
//                   |___/                                                    //
}

function TFigletFont.LoadFont(aFileName: string): boolean;
var
  wFStream: TFileStream;
  wZipper: TStreamUnZipper;
  wStream: TMemoryStream;
  wCount: integer;
  wIndex: integer;
  wSignature: array [0..10] of ansichar;

  //flf2a
  function TestSignature(aPtr: pansichar): boolean;
  const
    MAGIC = 'flf2a';
  var
    wI: integer;
  begin
    wI := 1;
    Result := False;
    while wI <= 5 do
    begin
      if aPtr^ <> MAGIC[wI] then
        Exit;
      Inc(aPtr);
      Inc(wI);
    end;
    Result := True;
  end;

begin
  Result := False;
  FLines.Clear;
  wFStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    wFStream.Read(wSignature[0], 5);
    //flf2a
    if TestSignature(wSignature) then  //ya esta descomprimido.
    begin
      wFStream.Position := 0;
      FLines.Text := StreamToString(wFStream);
      ReadHeader;
      Exit(True);
    end;
    wFStream.Position := 0;
    wZipper := TStreamUnZipper.Create(wFStream);
    try
      wZipper.Examine;
      wCount := wZipper.Entries.Count;
      //wZipper.Entries.Entries[i].ArchiveFileName);
      wIndex := 0;
      if wCount >= 1 then
      begin
        // extraemos el primero
        wStream := TMemoryStream.Create;
        try
          //wZipper.UnzipFile(wZipper.Entries[wIndex].ArchiveFileName,wStream);
          wZipper.UnzipFile(wIndex, wStream);
          wStream.Position := 0;
          wStream.Read(wSignature[0], 5);
          if TestSignature(wSignature) then
          begin
            wStream.Position := 0;
            FLines.Text := StreamToString(wStream);
            ReadHeader;
            Result := True;
          end;
        finally
          wStream.Free;
        end;
      end;
    finally
      wZipper.Free;
    end;
  finally
    wFStream.Free;
  end;
end;

function TFigletFont.GetFontSource: string;
begin
  Result := FLines.Text;
end;

// 121  0512 octal  0x1da2 hexadecimal
function ReadNumber(const aStr: string; var aPos: integer;var aReadedOk:boolean): integer;
var
  wC: char;
  wIsNegative: boolean;
  wL: integer;
  wBase: integer;
  wH: integer;
begin
  Result := 0;
  aReadedOk:=false;
  wL := length(aStr);
  if (aPos < 1) or (aPos > wL) then
    exit;
  wBase := 10;
  wC := aStr[aPos];
  //skip spaces
  while (wC = ' ') or (wC = #9) do
  begin
    Inc(aPos);
    if aPos > wL then
      Exit;
    wC := aStr[aPos];
  end;
  wIsNegative := False;
  // numeros 212 o 0xadbd   0octal
  if (aPos <= wL) and (aStr[aPos] = '-') then
  begin
    wIsNegative := True;
    Inc(aPos);
  end;
  if (aPos <= wL) and (aStr[aPos] = '0') then
  begin
    wBase := 8;
    Inc(aPos);
  end;
  if (aPos <= wL) and (wBase = 8) and ((aStr[aPos] = 'x') or (aStr[aPos] = 'X')) then
  begin
    wBase := 16;
    Inc(aPos);
  end;
  wC := aStr[aPos];
  if aPos > WL then
    Exit;
  while wC <> ' ' do
  begin
    wH := 0;
    if (wC >= '0') and (wC <= '9') then
      wH := Ord(wc) - Ord('0')
    else if (wC >= 'a') and (wC <= 'f') then
      wH := Ord(wc) - Ord('a')
    else if (wC >= 'A') and (wC <= 'F') then
      wH := Ord(wc) - Ord('A');
    Result := wBase * Result + wH;
    Inc(aPos);
    if aPos > wL then
      Break;
    wC := aStr[aPos];
  end;
  if wIsNegative then
    Result := -Result;
  aReadedOk:=true;
end;

procedure TFigletFont.ReadHeader;
var
  wText: string;
  wI: integer;
  wReadResult:boolean;
begin
  wText := Flines[0];
  HardBlank := wText[6];
  wI := 7;
  Height := ReadNumber(wText, wI,wReadResult);
  Baseline := ReadNumber(wText, wI,wReadResult);
  Max_Length := ReadNumber(wText, wI,wReadResult);
  Old_Layout := ReadNumber(wText, wI,wReadResult);
  Comment_Lines := ReadNumber(wText, wI,wReadResult);
  Print_Direction := ReadNumber(wText, wI,wReadResult);
  Full_Layout := ReadNumber(wText, wI,wReadResult);
  FontSmushMode:=Full_Layout;
  if wReadResult=false then
  begin
    if Old_Layout=0 then
      FontSmushMode := SM_KERN
    else if Old_Layout<0 then
      FontSmushMode :=0
    else
      FontSmushMode := (Old_Layout and 31) or SM_SMUSH;
  end;
  Codetag_Count := ReadNumber(wText, wI,wReadResult);
end;

constructor TFigletFont.Create(aFontFileName: string);
begin
  inherited Create;
  FAlign := faLeft;
  FDrawMode := fdmFull;
  FLines := TStringList.Create;
  Output := TStringList.Create;
  LoadFont(aFontFileName);
end;

destructor TFigletFont.Destroy;
begin
  FLines.Free;
  Output.Free;
  inherited;
end;

const
  deutsch: array [0..6] of integer = (196, 214, 220, 228, 246, 252, 223);

function TFigletFont.FindCharCodeLineStart(aCharCode: integer): integer;
const
  CHAR_NOT_EXIST_IN_FONT = '?';
var
  wI: integer;
  wJ: integer;
  wReadResult:boolean;
begin
  Result := Comment_Lines + 1 + (Ord(CHAR_NOT_EXIST_IN_FONT) - 32) * Height;
  if aCharCode >= 32 then
  begin
    if aCharCode >= 127 then
    begin
      // mirar si es caracter aleman.
      for wI := 0 to 6 do
      begin
        if aCharCode = deutsch[wI] then
          Exit(Comment_Lines + 1 + (127 + wI - 32) * Height);
      end;
      //buscar si existe.
      wI := Comment_Lines + 1 + (134 - 32) * Height;
      while wI < FLines.Count do
      begin
        wJ := 1;
        if aCharCode = ReadNumber(FLines[wI], wJ, wReadResult) then
          Exit(wI + 1);
        Inc(wI, Height + 1);
      end;
    end
    else
      Exit(Comment_Lines + 1 + (aCharCode - 32) * Height);
  end;
end;

function TFigletFont.FindCharCodeWidth(LineStart: integer): integer;
var
  wS: string;
  wEndChar: char;
  wLenWS: integer;
begin
  wS := FLines[LineStart];
  wLenWS := length(wS);
  wEndChar := wS[wLenWS];
  Dec(wLenWS);
  while (wLenWS > 0) and (wS[wLenWS] = wEndChar) do
    Dec(wLenWS);
  Result := wLenWS;
end;

function TFigletFont.SmushChars(aRow: integer; aLIndex: integer; aRIndex: integer): char;
var
  wLchar, wRchar: char;
begin
  if (LastChar.LineStart = 0) then
    Exit(FLines[CurrentChar.LineStart + aRow][aRIndex]);
  if  (aLIndex < 1) or (aLIndex > LastChar.Width) then
    wLChar:=' '
  else
    wLchar := FLines[LastChar.LineStart + aRow][aLIndex];
  if  (aRIndex < 1) or (aRIndex > CurrentChar.Width) then
    wRChar:=' '
  else
    wRchar := FLines[CurrentChar.LineStart + aRow][aRIndex];
  if wLchar = ' ' then Exit(wRChar);
  if wRchar = ' ' then Exit(wLChar);
  if FDrawMode = fdmFitted then Exit(#0);

  if (LastChar.Width < 2) or (CurrentChar.Width < 2) then Exit(#0);
  //* Disallows overlapping if the previous character */
  //* or the current character has a width of 1 or zero. */

  if (SmushMode and SM_HARDBLANK)<>0 then
  begin
    if (wLChar = Hardblank) and (wRChar = Hardblank) then Exit(wLChar);
    if (wLChar = Hardblank) or (wRChar = Hardblank) then Exit(#0);
  end;
  if (SmushMode and SM_EQUAL)<>0 then
  begin
    if wLChar = wRChar then Exit(wLChar);
  end;
  if (SmushMode and SM_LOWLINE)<>0 then
  begin
    if (wLChar = '_') and (Pos(wRChar, '|/\[]{}()<>') > 0) then Exit(wRChar);
    if (wRChar = '_') and (Pos(wLChar, '|/\[]{}()<>') > 0) then Exit(wLChar);
  end;
  if (SmushMode and SM_HIERARCHY)<>0 then
  begin
    if (wLChar = '|') and (Pos(wRChar, '/\[]{}()<>') > 0) then Exit(wRChar);
    if (wRChar = '|') and (Pos(wLChar, '/\[]{}()<>') > 0) then Exit(wLChar);
    if (Pos(wLChar, '/\') > 0) and (Pos(wRChar, '[]{}()<>') > 0) then Exit(wRChar);
    if (Pos(wRChar, '/\') > 0) and (Pos(wLChar, '[]{}()<>') > 0) then Exit(wLChar);
    if (Pos(wLChar, '[]') > 0) and (Pos(wRChar, '{}()<>') > 0) then Exit(wRChar);
    if (Pos(wRChar, '[]') > 0) and (Pos(wLChar, '{}()<>') > 0) then Exit(wLChar);
    if (Pos(wLChar, '{}') > 0) and (Pos(wRChar, '()<>') > 0) then Exit(wRChar);
    if (Pos(wRChar, '{}') > 0) and (Pos(wLChar, '()<>') > 0) then Exit(wLChar);
    if (Pos(wLChar, '()') > 0) and (Pos(wRChar, '<>`') > 0) then Exit(wRChar);
    if (Pos(wRChar, '()') > 0) and (Pos(wLChar, '<>') > 0) then Exit(wLChar);
  end;
  if (SmushMode and SM_PAIR)<>0 then
  begin
    if (wLChar = '[') and (wRChar = ']') then Exit('|');
    if (wRChar = '[') and (wLChar = ']') then Exit('|');
    if (wLChar = '{') and (wRChar = '}') then Exit('|');
    if (wRChar = '{') and (wLChar = '}') then Exit('|');
    if (wLChar = '(') and (wRChar = ')') then Exit('|');
    if (wRChar = '(') and (wLChar = ')') then Exit('|');
  end;
  if (SmushMode and SM_BIGX)<>0 then
  begin
    if (wLChar = '/') and (wRChar = '\') then Exit('|');
    if (wRChar = '/') and (wLChar = '\') then Exit('Y');
    if (wLChar = '>') and (wRChar = '<') then Exit('X');
    // Don't want the reverse of above to give 'X'.
  end;
  Result := #0;
end;

// contamos los espacios en blanco a la derecha de lastchar y a la izquierda
// de current char.
function TFigletFont.FindKerning:integer;
var
  wMinL, wMinR, wMinRL: integer;
  wF: integer;
  wLK, wRK: integer;
  wS: string;
begin
  Result:=0;
  if (CurrentChar.LineStart=0) or (FDrawMode = fdmFull) then
  begin
    CurrentChar.LK := 0;
    LastChar.RK := 0;
    Exit;
  end;
  Result:=99999;
  wMinL := -99999;
  wMinR := 99999;
  wMinRL := 99999;

  // count empty spaces
  for wF := 0 to Pred(Height) do
  begin
    wRK := 0;
    wS := FLines[CurrentChar.LineStart + wF];
    while wS[wRK + 1] = ' ' do
      Inc(wRK);
    if wRK < wMinR then
      wMinR := wRK;
    wLK := 0;
    if LastChar.LineStart > 0 then
    begin
      wS := FLines[LastChar.LineStart + wF];
      while (wLK < LastChar.Width) and (wS[LastChar.Width - wLK] = ' ') do
        Inc(wLK);

      if FDrawMode = fdmSmush then
      begin
        if SmushChars(wF, LastChar.Width - wLK-1, wRK+1) <> #0 then
          Inc(wLK);
      end;
      if FDrawMode= fdmSmushU then
      begin
        if SmushChars(wF, -1, wRK+1) <> #0 then
          Inc(wLK);
      end;

    end;
    if (wRK + wLK) <= wMinRL then
    begin
      if wLK>wMinL then
        wMinL := wLK;
      wMinRL := wLK + wRK;
    end;
    if (wRK+wLK)<Result then
      Result:=wRK+wLK;
  end;
  CurrentChar.LK := wMinR;
  if LastChar.LineStart > 0 then
    LastChar.RK := wMinL; // - 1;
end;


//returns width  num chars of character
function TFigletFont.DrawChar(aCharCode: integer; aStartPos: integer): integer;
var
  wLine: integer;
  wJ: integer;
  wC: char;
  wCI: integer;
  wS: string;
  wTemp: string;
  wCharCount: integer;
  wKerning:integer;
begin
  Result := 0;
  LastChar := CurrentChar;
  CurrentChar.Init(aCharCode, Self);
  wLine := CurrentChar.LineStart;
  if (wLine <= 0) or (wLine > FLines.Count) then
    Exit;
  wKerning:=FindKerning;
  for wJ := 0 to Pred(Height) do
  begin
    wCharCount := -wKerning;
    wS := FLines[wLine + wJ];
    wTemp := Output[wJ];
    for wCI := 1 to CurrentChar.Width do
    begin
      if (aStartPos + wCharCount) > length(wTemp) then
        break;
      if (FDrawMode = fdmSmush) then
        wC := SmushChars(wJ, LastChar.Width + wCharCount+1, wCI)
      else  if (FDrawMode=fdmSmushU) then
        wC := SmushChars(wJ, -1, wCI)
      else
        wC := wS[wCI];
      if wC=#0 then
        wC := wS[wCI];
      if (wC <> ' ') and (wC <> Hardblank) and (wC <>#0) then
        wTemp[aStartPos + wCharCount] := wC;
      Inc(wCharCount);
    end;
    Output[wJ] := wTemp;
  end;
  Result := wCharCount;
end;


procedure TFigletFont.DrawText(aText: string);
var
  wL: integer;
  wI: integer;
  wS: string;
  wCurrentPos: integer;
  wWidth:Integer;
  wCP: pansichar;
  wCPL: integer;
  wCharCode: integer;
  wPadCount: integer;
  wLineLength:integer;
begin
  //SmushMode:= SM_HARDBLANK or SM_EQUAL or SM_LOWLINE or SM_HIERARCHY or SM_PAIR or SM_BIGX;
  SmushMode:= FontSmushMode;
  wL := Length(aText);
  Output.Clear;
  wS := FLineStartText;
  wLineLength:= wL * Max_Length + wL*FAdditionalSpaces;
  if wLIneLength > MAX_LINE_LENGTH then
    wLineLength:=MAX_LINE_LENGTH;
  if wL > 0 then
    wS := wS + StringOfChar(' ', wLineLength);
  for wI := 1 to Height do
    Output.Add(wS);
  wCurrentPos := length(FLineStartText) + 1;
  CurrentChar.Init(0, Self);
  LastChar.Init(0, Self);
  wCP := PChar(Pointer(aText));
  while wL > 0 do
  begin
    wCharCode := Utf8decodeDGP(wCP, wCPL);
    wWidth:=DrawChar(wCharCode, wCurrentPos);
    if wWidth>0 then
      wCurrentPos := wCurrentPos + wWidth;
    if FDrawMode = fdmFull then
      wCurrentPos := wCurrentPos + FAdditionalSpaces;
    Inc(wCP, wCPL);
    Dec(wL, wCPL);
  end;

  wPadCount := FLineWidth + 1 - wCurrentPos - length(FLineEndText);
  if FLineWidth > 0 then
    wCurrentPos := min(wCurrentPos, FLineWidth + 1 - length(FLineEndText));
  for wI := 0 to Pred(Height) do
  begin
    if (FLineWidth <= 0) or (FAlign = faLeft) then
      Output[wI] := Copy(Output[wI], 1, wCurrentPos - 1) + FLineEndText
    else if FAlign = faCenter then
      Output[wI] := FLineStartText + StringOfChar(' ', wPadCount div 2) + Copy(Output[wI], 1 + Length(
        FLineStartText), wCurrentPos - 1 - Length(FLineStartText)) + StringOfChar(' ', wPadCount - (wPadCount div 2)) + FLineEndText
    else if FAlign = faRight then
      Output[wI] := FLineStartText + StringOfChar(' ', wPadCount) + Copy(Output[wI], 1 + Length(
        FLineStartText), wCurrentPos - 1 - Length(FLineStartText)) + FLineEndText
    else if FAlign = faLeft2R then
      Output[wI] := Copy(Output[wI], 1, wCurrentPos - 1) + StringOfChar(' ', wPadCount) + FLineEndText;
  end;
end;

end.
