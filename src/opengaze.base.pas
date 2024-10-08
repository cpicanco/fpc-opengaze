{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opengaze.types,
  opengaze.socket,
  opengaze.events;

type

  { TOpenGazeBase }

  TOpenGazeBase = class
  private
    FSocket: TOpenGazeSocket;
    function GetServer: TOpenGazeServer;
  protected
    FEvents : TOpenGazeEvents;
    procedure SendCommand(Command: string; Blocking : Boolean = True);
    function RequestCommand(Command: string) : TPairsDictionary;
    function RequestKey(Command: string; Key: string): string;
  public
    constructor Create(ASocket: TOpenGazeSocket; AEvents: TOpenGazeEvents);
    property Socket : TOpenGazeSocket read FSocket;
    property Events : TOpenGazeEvents read FEvents;
    property Server : TOpenGazeServer read GetServer;
  end;

implementation

uses opengaze.parser;

{ TOpenGazeBase }

function TOpenGazeBase.GetServer: TOpenGazeServer;
begin
  Result := FSocket.Server;
end;

procedure TOpenGazeBase.SendCommand(Command: string; Blocking: Boolean);
var
  Reply: TRawTag;
begin
  if Blocking then begin
    Reply := Default(TRawTag);
    FSocket.Request(Command, Reply);
    FEvents.ProcessTag(Self, Reply);
  end else begin
    FSocket.Send(Command);
  end;
end;

function TOpenGazeBase.RequestCommand(Command: string): TPairsDictionary;
var
  Reply: TRawTag;
begin
  Reply := Default(TRawTag);
  FSocket.Request(Command, Reply);
  FEvents.ProcessTag(Self, Reply);
  Result := TagPairsToDict(Reply.Pairs);
end;

function TOpenGazeBase.RequestKey(Command: string; Key: string): string;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Command);
  try
    Result := Dictionary[Key];
  finally
    Dictionary.Free;
  end;
end;

constructor TOpenGazeBase.Create(ASocket: TOpenGazeSocket;
  AEvents: TOpenGazeEvents);
begin
  inherited Create;
  FSocket := ASocket;
  FEvents := AEvents;
end;

end.

