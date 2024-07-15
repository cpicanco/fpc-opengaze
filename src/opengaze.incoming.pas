{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.incoming;

interface

uses
  Classes, SysUtils, opengaze.socket, opengaze.types, opengaze.queues;

type

  { TIncomingThread }

  TIncomingThread = class(TThread)
  private
    FSocket: TOpenGazeSocket;
    FCommands : TThreadStringQueue;
  protected
    procedure Execute; override;
  public
    constructor Create(Socket: TOpenGazeSocket);
    destructor Destroy; override;
    property Commands : TThreadStringQueue read FCommands write FCommands;
  end;

implementation

{ TIncomingThread }

constructor TIncomingThread.Create(Socket: TOpenGazeSocket);
begin
  inherited Create(True);  // Create suspended
  FSocket := Socket;
  FreeOnTerminate := True;
end;

destructor TIncomingThread.Destroy;
begin
  inherited Destroy;
end;

procedure TIncomingThread.Execute;
var
  RXString, RXPartial, Command: String;
  DelimiterIndex: SizeInt;
begin
  RXPartial := '';
  while not Terminated do begin
    RXString := '';
    RXString := FSocket.Receive;

    if not RXPartial.IsEmpty then begin
      RXString := RXPartial + RXString;
      RXPartial := '';
    end;

    repeat
      DelimiterIndex := Pos(#13#10, RXString);
      if DelimiterIndex > 0 then begin
        Command := '';
        Command := Copy(RXString, 1, DelimiterIndex+1);
        FCommands.Enqueue(Command);
        Delete(RXString, 1, DelimiterIndex+1);
      end;
    until DelimiterIndex = 0;

    RXPartial := RXString;

    if FCommands.IsWaiting and FCommands.HasData then begin
      FCommands.SetEvent;
    end;
  end;
end;

end.

