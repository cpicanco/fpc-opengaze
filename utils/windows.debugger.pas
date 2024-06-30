{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit windows.debugger;

{$mode ObjFPC}{$H+}

interface

function GetTimestampedFilename: string;

implementation

uses
  SysUtils, DateUtils;

var
  DebugFile: TextFile;
  Filename: string;

function GetTimestampedFilename: string;
var
  Year, Month, Day, Hour, Minute, Second, MilliSecond: Word;
begin
  DecodeDateTime(Now, Year, Month, Day, Hour, Minute, Second, MilliSecond);
  Result := Format('OpenGaze-debug-%d-%0.2d-%0.2d-%0.2d-%0.2d-%0.2d.txt',
                   [Year, Month, Day, Hour, Minute, Second]);
end;

initialization
  Filename := GetTimestampedFilename;
  AssignFile(DebugFile, Filename);
  Rewrite(DebugFile);
  Output := DebugFile;

finalization
  CloseFile(DebugFile);

end.

