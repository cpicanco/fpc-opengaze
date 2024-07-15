{
  fpc-OpenGaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.socket;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, blcksock, synsock, synautil,
  opengaze.types;

type

  { TOpenGazeSocket }

  TOpenGazeSocket = class
  private
    FEvent : TEvent;
    FOpenGazeServer: TOpenGazeServer;
    FTCP: TTCPBlockSocket;
    FLock: TCriticalSection;
    FConnected : Boolean;
    FIP: string;
    FPort: string;
    FIsBlocked : Boolean;
    FLastBlockedCommand: string;
    function GetConnected: Boolean;
    function GetIsBlocked: Boolean;
    procedure SeTOpenGazeServer(AValue: TOpenGazeServer);
    procedure SetIsBlocked(AValue: Boolean);
    function Block(TimeOut : integer) : TWaitResult;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Unblock;
    procedure Send(Command : string); overload;
    procedure Request(Command : string; var Reply : TRawTag;
      Timeout : Integer = MaxInt); overload;
    function Receive(Timeout: Integer = MaxInt): string;
    property IP: string read FIP write FIP;
    property Port: string read FPort write FPort;
    property Lock: TCriticalSection read FLock;
    property Event: TEvent read FEvent write FEvent;
    property IsBlocked : Boolean read GetIsBlocked write SetIsBlocked;
    property Connected: Boolean read GetConnected;
    property Server : TOpenGazeServer read FOpenGazeServer write SeTOpenGazeServer;
    property LastBlockedCommand : string read FLastBlockedCommand write FLastBlockedCommand;
  end;


implementation

uses Dialogs, opengaze.parser;

{ TOpenGazeSocket }

function TOpenGazeSocket.GetConnected: Boolean;
begin
  FLock.Acquire;
  try
    Result := FConnected;
  finally
    FLock.Release;
  end;
end;

function TOpenGazeSocket.GetIsBlocked: Boolean;
begin
  FLock.Acquire;
  try
    Result := FIsBlocked;
  finally
    FLock.Release;
  end;
end;

procedure TOpenGazeSocket.SeTOpenGazeServer(AValue: TOpenGazeServer);
begin
  if FConnected then Exit;
  if FOpenGazeServer = AValue then Exit;
  FOpenGazeServer := AValue;
  case FOpenGazeServer of
    OpenGazeControlServer : FPort := '4242';
    OpenGazeAnalysisServer : FPort := '8442';
  end;
end;

procedure TOpenGazeSocket.SetIsBlocked(AValue: Boolean);
begin
  FLock.Acquire;
  try
    FIsBlocked := AValue;
  finally
    FLock.Release;
  end;
end;

function TOpenGazeSocket.Block(TimeOut: integer): TWaitResult;
begin
  Result := Event.WaitFor(Timeout);
end;

constructor TOpenGazeSocket.Create;
begin
  inherited Create;
  FEvent := TEvent.Create;
  FLock := TCriticalSection.Create;
  FTCP := TTCPBlockSocket.Create;
  FTCP.ConnectionTimeout := 5000;
  FTCP.SetTimeout(MaxInt);
  FIP := '127.0.0.1';
  FPort := '4242';
  FOpenGazeServer := OpenGazeControlServer;
  FConnected := False;
end;

destructor TOpenGazeSocket.Destroy;
begin
  FEvent.Free;
  FLock.Free;
  FTCP.Free;
  inherited Destroy;
end;

procedure TOpenGazeSocket.Connect;
begin
  FTCP.Connect(FIP, FPort);
  case FTCP.LastError of
    0 : begin
      FConnected := True;
    end;

    WSAETIMEDOUT: begin
      ShowMessage('Connection timed-out');
    end

    otherwise begin
      ShowMessage(FTCP.LastErrorDesc);
    end;
  end;
end;

procedure TOpenGazeSocket.Disconnect;
begin
  FLock.Acquire;
  try
    FConnected := False;
    FTCP.CloseSocket;
  finally
    FLock.Release;
  end;
end;

procedure TOpenGazeSocket.Unblock;
begin
  FEvent.SetEvent;
  FLock.Acquire;
  try
    FIsBlocked := False;
  finally
    FLock.Release;
  end;
end;

procedure TOpenGazeSocket.Send(Command: string);
begin
  {$IFDEF DEBUG}
  WriteLn(Command);
  {$ENDIF}
  FTCP.SendString(Command);
end;

procedure TOpenGazeSocket.Request(Command: string; var Reply: TRawTag;
  Timeout: Integer);
begin
  {$IFDEF DEBUG}
  Write(Command);
  {$ENDIF}

  FLock.Acquire;
  try
    FLastBlockedCommand := Command;
    FIsBlocked := True;
  finally
    FLock.Release;
  end;

  FTCP.SendString(Command);
  case Block(Timeout) of
    wrSignaled : begin
      FLock.Acquire;
      try
        Reply := ParseXML(FLastBlockedCommand);
        {$IFDEF DEBUG}
        Write(ParseStr(Reply.Tag, Reply.Pairs));
        {$ENDIF}
        FLastBlockedCommand := '';
      finally
        FLock.Release;
      end;
    end;

    wrTimeout: begin
      ShowMessage('Request:  wrTimeout');
    end;

    wrAbandoned: begin
      ShowMessage('Request:  wrAbandoned');
    end;

    wrError: begin
      ShowMessage('Request:  wrError');
    end;
  end;
  FEvent.ResetEvent;
end;

function TOpenGazeSocket.Receive(Timeout: Integer): string;
begin
  Result := '';
  Result := FTCP.RecvPacket(Timeout);
end;

end.
