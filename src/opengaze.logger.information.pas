{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.logger.information;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, opengaze.types;

procedure LogLine(Title, Value : string);
procedure BeginUpdateData;
procedure EndUpdateData;
procedure FlushToDataFile;

var
  DataHeader : string;
  DataFilename : string = 'default_information_log.txt';

implementation

uses opengaze.helpers;

var
  OutputDataFile : TextFile;

procedure LogLine(Title, Value: string);
begin
  WriteLn(OutputDataFile, Title + ':' + Value);
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

