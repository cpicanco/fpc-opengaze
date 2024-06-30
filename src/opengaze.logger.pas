{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.logger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, opengaze.types;

procedure LogLine(Pairs : TTagPairs);
procedure BeginUpdateData;
procedure EndUpdateData;
procedure FlushToDataFile;

var
  Data : TStringList;
  DataHeader : string;
  DataFilename : string = 'default_log.txt';

implementation

const
  TabDelimiter = #9;

procedure LogLine(Pairs: TTagPairs);
var
  Line : TStringArray = nil;
  i: Integer;
begin
  SetLength(Line, Length(Pairs));
  for i := Low(Pairs) to High(Pairs) do begin
    Line[i] := Pairs[i].Value;
  end;
  Data.Append(String.Join(TabDelimiter, Line));
end;

procedure BeginUpdateData;
begin
  Data.Sorted := False;
  Data.Duplicates := dupIgnore;
  Data.BeginUpdate;
  Data.Clear;
  Data.Append(DataHeader);
end;

procedure EndUpdateData;
begin
  Data.EndUpdate;
  Data.SaveToFile(DataFilename, TEncoding.UTF8);
end;

procedure FlushToDataFile;
begin
  Data.EndUpdate;
  Data.SaveToFile(DataFilename, TEncoding.UTF8);
  Data.BeginUpdate;
end;

initialization
  Data := TStringList.Create;

finalization
  Data.Free;

end.

