{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.parser;

{$mode ObjFPC}{$H+}

interface

uses opengaze.constants, opengaze.types;

function ParseXML(const InputString: string): TRawTag;
function ParseStr(const ATag: string;
  AID: string; Values: TPairsDictionary = nil): string; overload;
function ParseStr(const ATag: string;
  AID: string; Values: array of string): string; overload;
function ParseStr(const ATag: TOpenGazeTag; Values: TTagPairs): string; overload;
function TagPairsToDict(const TagPairs : TTagPairs): TPairsDictionary;

implementation

uses
  Classes, SysUtils;

const
  AttributeDelimiter = #32;
  ParameterDelimiter = '=';
  ValueDelimiter = '"';
  XMLStart = '<';
  XMLEnd   = '>';
  XMLSlashEnd = '/'+XMLEnd;

function ParseXML(const InputString: string): TRawTag;
var
  AttrStart, AttrEnd, i, EqPos, ValStart, ValEnd: Integer;
begin
  Result := Default(TRawTag);
  if InputString.IsEmpty then Exit;
  // Set tag name
  Result.Tag := OpenGazeTagsMap[Copy(InputString, 2, 3)];
  // Find attributes section
  AttrStart := Pos(AttributeDelimiter, InputString, 2);
  AttrEnd := Pos(XMLSlashEnd, InputString)-4;
  // Process attributes
  i := AttrStart;
  while i < AttrEnd do begin
    // Skip whitespace
    while (i < AttrEnd) and (InputString[i] = AttributeDelimiter) do Inc(i);
    // Find the '=' character
    EqPos := Pos(ParameterDelimiter, InputString, i);
    if EqPos = 0 then Break;
    // Find the value delimiters and extract the value
    ValStart := Pos(ValueDelimiter, InputString, EqPos)+1;
    ValEnd := Pos(ValueDelimiter, InputString, ValStart);
    // Add the attribute to the result
    SetLength(Result.Pairs, Length(Result.Pairs) + 1);
    with Result.Pairs[High(Result.Pairs)] do begin
      Key := Copy(InputString, i, EqPos - i);
      Value := Copy(InputString, ValStart, ValEnd - ValStart);
      // Check if it's the ID attribute
      if Key = ID then begin
        try
          Result.ID := OpenGazeIDsMap[Value];
        except
          Result.ID := NONE;
        end;
      end;
    end;
    // Move to the next attribute
    i := ValEnd + 1;
  end;
end;

function ParseStr(const ATag: TOpenGazeTag; Values: TTagPairs): string;
var
  i : integer;
  LTag : string;
begin
  WriteStr(LTag, ATag);
  // Create the start of the formatted string.
  Result := Concat(
    XMLStart,
    LTag.Replace('_', ''),
    AttributeDelimiter);

  // Add the values for each parameter.
  for i := Low(Values) to High(Values) do begin
    Result := Concat(
      Result,
      Values[i].Key,
      ParameterDelimiter,
      ValueDelimiter,
      Values[i].Value,
      ValueDelimiter,
      AttributeDelimiter);
  end;

  Result := Result + XMLSlashEnd + LineEnding;
end;

function TagPairsToDict(const TagPairs: TTagPairs): TPairsDictionary;
var
  Pair : TTagPair;
begin
  Result := TPairsDictionary.Create;
  for Pair in TagPairs do begin
    Result.Add(Pair.Key, Pair.Value);
  end;
end;

function ParseStr(const ATag: string; AID: string;
  Values: TPairsDictionary): string;
var
  Parameter: string;
begin
  // Create the start of the formatted string.
  Result := Concat(
    XMLStart,
    ATag,
    AttributeDelimiter,
    ID,
    ParameterDelimiter,
    ValueDelimiter,
    AID,
    ValueDelimiter,
    AttributeDelimiter);

  // Add the values for each parameter.
  if Values <> nil then begin
    for Parameter in Values.Keys do begin
      Result := Concat(
        Result,
        Parameter,
        ParameterDelimiter,
        ValueDelimiter,
        Values[Parameter],
        ValueDelimiter,
        AttributeDelimiter);
    end;
  end;

  Result := Concat(Result, XMLSlashEnd, LineEnding);
end;

function ParseStr(const ATag: string; AID: string;
  Values: array of string): string;
var
  i : integer;
begin
  // Create the start of the formatted string.
  Result := Concat(
    XMLStart,
    ATag,
    AttributeDelimiter,
    ID,
    ParameterDelimiter,
    ValueDelimiter,
    AID,
    ValueDelimiter,
    AttributeDelimiter);

  // Add the values for each parameter.
  for i := Low(Values) to High(Values) -1 do begin
    Result := Concat(
      Result,
      Values[i],
      ParameterDelimiter,
      ValueDelimiter,
      Values[i+1],
      ValueDelimiter,
      AttributeDelimiter);
  end;

  Result := Result + XMLSlashEnd + LineEnding;
end;

{$IFDEF DEBUG}
procedure TestParseXML;
var
  InputString: string;
  RawTag: TRawTag;
  Pair : TTagPair;
begin
  // Example XML-like tag string
  InputString := '<ACK ID="CALIBRATE_ADDPOINT" PTS="6" X1="0.50000" Y1="0.50000" X2="0.85000" Y2="0.15000" X3="0.85000" Y3="0.85000" X4="0.15000" Y4="0.85000" X5="0.15000" Y5="0.15000" X6="0.50000" Y6="0.10000" />'+LineEnding;

  // Parse the XML-like tag
  RawTag := ParseXML(InputString);

  // Display parsed tag information
  WriteLn('opengaze.parser');
  WriteLn('Tag:', RawTag.Tag);
  WriteLn('Attributes:');
  for Pair in RawTag.Pairs do
    WriteLn(Pair.Key, ' = ', Pair.Value);
  WriteLn('---------------------------------------------------');
end;

initialization
  TestParseXML;
{$ENDIF}

end.

