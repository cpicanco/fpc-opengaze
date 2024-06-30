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
  Classes, SysUtils, SyncObjs, opengaze.socket, opengaze.types;

type

  { TIncomingThread }

  TIncomingThread = class(TThread)
  private
    FOnReceive: TGazeDataEvent;
    FRawTagReceived : TRawTag;
    FSocket: TOpenGazeSocket;
    FSocketLock : TCriticalSection;
    FLock : TCriticalSection;
    procedure Reply;
    procedure Debug;
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
  FSocket := Socket;
  FSocketLock := TCriticalSection.Create;
  FLock := TCriticalSection.Create;
  FreeOnTerminate := True;
end;

destructor TIncomingThread.Destroy;
begin
  FSocketLock.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TIncomingThread.Reply;
var
  RawTag : TRawTag = (Tag: ERR; ID: NONE; Pairs: nil);
begin
  FLock.Acquire;
  try
    RawTag := FRawTagReceived;
  finally
    FLock.Release;
  end;

  OnReceive(Self, RawTag);
end;

procedure TIncomingThread.Debug;
var
  LTag : string;
  RawTag : TRawTag = (Tag: ERR; ID: NONE; Pairs: nil);
  Pair: TTagPair;
begin
  FLock.Acquire;
  try
    RawTag := FRawTagReceived;
  finally
    FLock.Release;
  end;

  WriteStr(LTag, RawTag.Tag);
  for Pair in RawTag.Pairs do begin
    WriteLn(Format('%s, %s=%s', [LTag, Pair.Key, Pair.Value]));
  end;
  WriteLn('---------------------------------------------------');
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
begin
  while not Terminated do begin
    RawTagReceived := FSocket.Receive;

    FLock.Acquire;
    try
      FRawTagReceived := RawTagReceived;
    finally
      FLock.Release;
    end;

    {$IFDEF DEBUG}
    Queue(@Debug);
    {$ENDIF}

    // reply blocking commands (Socket.Request)
    RawTagRequested := FSocket.LastBlockingRawTag;
    case RawTagReceived.Tag of
      ACK: begin
        if RawTagRequested.ID = RawTagReceived.ID then begin
          FSocket.LastBlockingRawTag := RawTagReceived;
          FSocket.Event.SetEvent;
          Continue;
        end;
      end;

      otherwise begin
        { do nothing }
      end;
    end;

    // receive non-blocking commands (Socket.Send)
    Queue(@Reply);
  end;
end;

end.

