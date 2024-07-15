unit opengaze.worker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, opengaze.socket, opengaze.queues, opengaze.types;

type

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FOnReceive: TGazeDataEvent;
    FSocket: TOpenGazeSocket;
    FCommands: TThreadStringQueue;
    procedure Reply;
    procedure SetOnReceive(AValue: TGazeDataEvent);
  protected
    procedure Execute; override;
  public
    constructor Create(Socket: TOpenGazeSocket);
    destructor Destroy; override;
    property Commands : TThreadStringQueue read FCommands write FCommands;
    property OnReceive : TGazeDataEvent read FOnReceive write SetOnReceive;
  end;

implementation

uses opengaze.parser;

{ TWorkerThread }

procedure TWorkerThread.Reply;
var
  RawTagReceived : TRawTag;
  Command: String;
begin
  FCommands.BeginUpdate;
  while FCommands.HasData do begin
    Command := '';
    Command := FCommands.Dequeue;
    RawTagReceived := ParseXML(Command);
    OnReceive(Self, RawTagReceived);
  end;
  FCommands.EndUpdate;
end;

procedure TWorkerThread.SetOnReceive(AValue: TGazeDataEvent);
begin
  if FOnReceive = AValue then Exit;
  FOnReceive := AValue;
end;

procedure TWorkerThread.Execute;
var
  RequestedCommand: string;
begin
  while not Terminated do begin
    FCommands.WaitForData;
    if FSocket.IsBlocked then begin
      RequestedCommand := '';
      RequestedCommand := FSocket.LastBlockedCommand;
      if FCommands.Dequeue(RequestedCommand) then begin
        FSocket.LastBlockedCommand := RequestedCommand;
        FSocket.Unblock;
      end;
    end else begin
      if (not FCommands.IsUpdating) and FCommands.HasData then begin
        Queue(@Reply);
      end;
    end;
    FCommands.ResetEvent;
  end;
end;

constructor TWorkerThread.Create(Socket: TOpenGazeSocket);
begin
  inherited Create(True);  // Create suspended
  FSocket := Socket;

  FreeOnTerminate := True;
end;

destructor TWorkerThread.Destroy;
begin
  inherited Destroy;
end;

end.

