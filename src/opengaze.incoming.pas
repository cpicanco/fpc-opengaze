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
  Classes, SysUtils, SyncObjs,
  opengaze.socket, opengaze.types, opengaze.queues;

type

  { TIncomingThread }

  TIncomingThread = class(TThread)
  private
    FOnReceive: TGazeDataEvent;
    FSocket: TOpenGazeSocket;
    RXStrings : TThreadStringQueue;
    procedure Reply;
    procedure DebugString;
    procedure SetOnReceive(AValue: TGazeDataEvent);
  protected
    procedure Execute; override;
  public
    constructor Create(Socket: TOpenGazeSocket); reintroduce;
    destructor Destroy; override;
    property OnReceive : TGazeDataEvent read FOnReceive write SetOnReceive;
  end;

implementation

uses synsock, OpenGaze.parser;

{ TIncomingThread }

constructor TIncomingThread.Create(Socket: TOpenGazeSocket);
begin
  inherited Create(True);  // Create suspended
  RXStrings := TThreadStringQueue.Create;
  FSocket := Socket;
  FreeOnTerminate := True;
end;

destructor TIncomingThread.Destroy;
begin
  RXStrings.Free;
  inherited Destroy;
end;

procedure TIncomingThread.Reply;
var
  RXString , Command: string;
  DelimiterIndex: SizeInt;
begin
  RXString := '';
  RXString := RXStrings.Dequeue;
  if RXString.IsEmpty then Exit;

  repeat
    DelimiterIndex := Pos(#13#10, RXString);
    if DelimiterIndex > 0 then begin
      Command := Copy(RXString, 1, DelimiterIndex+1);
      {$IFDEF DEBUG}
      Write(Command);
      {$ENDIF}
      OnReceive(Self, ParseXML(Command));
      Delete(RXString, 1, DelimiterIndex + 1);
    end;
  until DelimiterIndex = 0;
end;

procedure TIncomingThread.DebugString;
begin
  Write(RXStrings.Dequeue);
end;

procedure TIncomingThread.SetOnReceive(AValue: TGazeDataEvent);
begin
  if FOnReceive = AValue then Exit;
  FOnReceive := AValue;
end;

procedure TIncomingThread.Execute;
var
  RawTagReceived : TRawTag = (Tag: ERR; ID: NONE; Pairs: nil);
  RawTagRequested : TRawTag = (Tag: ERR; ID: NONE; Pairs: nil);
  Commmand: string;
begin
  while not Terminated do begin
    Commmand := '';
    Commmand := FSocket.Receive;
    if FSocket.IsBlocked then begin
      RawTagReceived := ParseXML(Commmand);
      RawTagRequested := FSocket.LastBlockedRawTag;
      case RawTagReceived.Tag of
        ACK: begin
          if RawTagRequested.ID = RawTagReceived.ID then begin
            FSocket.LastBlockedRawTag := RawTagReceived;
            FSocket.Event.SetEvent;
            Continue;
          end;
        end;

        otherwise begin
          { do nothing }
        end;
      end;
    end;

    RXStrings.Enqueue(Commmand);
    Queue(@Reply);
  end;
end;

end.

