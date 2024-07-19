{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.logger.calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, opengaze.types, choreographies;

procedure LogLine(Pairs : TTagPairs); overload;
procedure LogLine(Dictionary : TPairsDictionary); overload;
procedure LogLine(Points : TNormalizedPoints); overload;
function ToPythonDict(Dict: TPairsDictionary): string; overload;
function ToPythonDict(Points: TNormalizedPoints): string; overload;
procedure BeginUpdateData;
procedure EndUpdateData;
procedure FlushToDataFile;

var
  DataHeader : string;
  DataFilename : string = 'default_calibration_log.txt';

implementation

uses opengaze.helpers;

const
  TabDelimiter = #9;

var
  OutputDataFile : TextFile;

function ToPythonDict(Dict: TPairsDictionary): string;
var
  Key: string;
  StringArray : TStringArray = nil;
  i : integer = 0;
begin
  SetLength(StringArray, Dict.Count);
  for Key in Dict.Keys do begin
    StringArray[i] := Format('%s:%s', [Key, Dict[Key]]);
    Inc(i);
  end;

  Result := '{' + String.Join(',', StringArray) + '}';
end;


function ToPythonDict(Points: TNormalizedPoints): string;
var
  StringArray : TStringArray = nil;
  i : integer = 0;
  j : integer = 0;
begin
  SetLength(StringArray, Length(Points)*2);
  repeat
    StringArray[j] :=
      Format('X%s:%s', [(i+1).ToString, Points[i].X.ToDefaultString]);
    StringArray[j+1] :=
      Format('Y%s:%s', [(i+1).ToString, Points[i].Y.ToDefaultString]);
    Inc(i);
    Inc(j, 2);
  until i > High(Points);

  Result := '{' + String.Join(',', StringArray) + '}';
end;

procedure LogLine(Pairs: TTagPairs);
var
  i: Integer;
  LastColumn : Integer;
begin
  LastColumn := High(Pairs);
  for i := 0 to LastColumn-1 do begin
    Write(OutputDataFile, Pairs[i].Value + TabDelimiter);
  end;
  Write(OutputDataFile, Pairs[LastColumn].Value + LineEnding);
end;

procedure LogLine(Dictionary: TPairsDictionary);
begin
  WriteLn(OutputDataFile, ToPythonDict(Dictionary));
end;

procedure LogLine(Points: TNormalizedPoints);
begin
  WriteLn(OutputDataFile, ToPythonDict(Points));
end;

procedure BeginUpdateData;
begin
  AssignFile(OutputDataFile, DataFilename);
  System.Rewrite(OutputDataFile);
  {$IF FPC_FULLVERSION >= 30200}
  SetTextCodePage(OutputDataFile, CP_UTF8);
  {$ELSE}
  SetTextCodePage(OutputDataFile, 65001);
  {$ENDIF}
  WriteLn(OutputDataFile, DataHeader);
end;

procedure EndUpdateData;
begin
  System.Close(OutputDataFile);
end;

procedure FlushToDataFile;
begin
  System.Flush(OutputDataFile);
end;

end.

