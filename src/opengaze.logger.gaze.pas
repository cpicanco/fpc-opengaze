{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.logger.gaze;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, opengaze.types, opengaze.events;

type

  { TOpenGazeLogger }

  TOpenGazeLogger = class
  private
    FEvents : TOpenGazeEvents;
    FFilename: string;
    FHeader: string;
    FOnDataReceived: TGazeDataEvent;
    FOutputDataFile : TextFile;
    FDataReceivedEvent : TGazeDataEvent;
    function GetHeader: string;
    procedure DoNothing(Sender: TObject; RawTag: TRawTag);
    procedure LogLine(Sender: TObject; RawTag : TRawTag);
    procedure SetOnDataReceived(AValue: TGazeDataEvent);
  public
    constructor Create(AEvents : TOpenGazeEvents);
    procedure BeginUpdateData;
    procedure EndUpdateData;
    procedure FlushToDataFile;
    property DataReceivedEvent : TGazeDataEvent read FDataReceivedEvent;
    property Header : string read GetHeader;
    property Filename : string read FFilename write FFilename;
    property OnDataReceived : TGazeDataEvent read FOnDataReceived write SetOnDataReceived;
  end;

implementation

uses opengaze.commands;

const
  TabDelimiter = #9;

{ TOpenGazeLogger }

constructor TOpenGazeLogger.Create(AEvents: TOpenGazeEvents);
begin
  inherited Create;
  FEvents := AEvents;
  FDataReceivedEvent := @LogLine;
  FOnDataReceived := @DoNothing;
  FFilename := 'default_log.txt';
end;

procedure TOpenGazeLogger.LogLine(Sender: TObject; RawTag: TRawTag);
var
  i: Integer;
  LastColumn : Integer;
begin
  LastColumn := High(RawTag.Pairs);
  for i := 0 to LastColumn-1 do begin
    Write(FOutputDataFile, RawTag.Pairs[i].Value + TabDelimiter);
  end;
  Write(FOutputDataFile, RawTag.Pairs[LastColumn].Value + LineEnding);
  FOnDataReceived(Sender, RawTag);
end;

procedure TOpenGazeLogger.SetOnDataReceived(AValue: TGazeDataEvent);
begin
  if FOnDataReceived = AValue then Exit;
  FOnDataReceived := AValue;
end;

function TOpenGazeLogger.GetHeader: string;
begin
  Result := GetDataHeader;
end;

procedure TOpenGazeLogger.DoNothing(Sender: TObject; RawTag: TRawTag);
begin
  { do nothing }
end;

procedure TOpenGazeLogger.BeginUpdateData;
begin
  AssignFile(FOutputDataFile, FFilename);
  System.Rewrite(FOutputDataFile);
  {$IF FPC_FULLVERSION >= 30200}
  SetTextCodePage(FOutputDataFile, CP_UTF8);
  {$ELSE}
  SetTextCodePage(OutputDataFile, 65001);
  {$ENDIF}
  WriteLn(FOutputDataFile, GetDataHeader);
  FEvents.OnDataReceived := DataReceivedEvent;
end;

procedure TOpenGazeLogger.EndUpdateData;
begin
  FEvents.OnDataReceived := @DoNothing;
  System.Close(FOutputDataFile);
end;

procedure TOpenGazeLogger.FlushToDataFile;
begin
  System.Flush(FOutputDataFile);
end;

end.

