{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opengaze.types,
  opengaze.socket,
  opengaze.incoming,
  opengaze.worker,
  opengaze.worker.raw,
  opengaze.events,
  opengaze.queues;

type

  { TOpenGazeBaseClient }

  TOpenGazeBaseClient = class
  private
    FCommands : TThreadStringQueue;
    FWorkerThread : TWorkerThread;
    FIncomingThread : TIncomingThread;
  protected
    FSocket: TOpenGazeSocket;
    FEvents : TOpenGazeEvents;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    property Events : TOpenGazeEvents read FEvents;
  end;


implementation

uses Dialogs, OpenGaze.commands, OpenGaze.logger;

constructor TOpenGazeBaseClient.Create;
begin
  inherited Create;
  FSocket := TOpenGazeSocket.Create;
  FCommands := TThreadStringQueue.Create;
  FEvents := TOpenGazeEvents.Create;

  FIncomingThread := nil;
  FWorkerThread := nil;
end;

destructor TOpenGazeBaseClient.Destroy;
begin
  FEvents.Free;

  Disconnect;

  FCommands.Free;
  FSocket.Free;
  inherited Destroy;
end;

procedure TOpenGazeBaseClient.Connect;
begin
  if FSocket.Connected then Exit;
  FSocket.Connect;
  if FSocket.Connected then begin
    FIncomingThread := TIncomingThread.Create(FSocket);
    FIncomingThread.Commands := FCommands;

    FWorkerThread := TWorkerThread.Create(FSocket);
    FWorkerThread.Commands := FCommands;

    FEvents.WorkerThread := FWorkerThread;

    FWorkerThread.Start;
    FIncomingThread.Start;
  end;
end;

procedure TOpenGazeBaseClient.Disconnect;
begin
  if Assigned(FWorkerThread) then begin
    if FWorkerThread.Suspended then begin
      FWorkerThread.Start;
    end;

    FWorkerThread.Terminate;
    FCommands.SetEvent;
  end;

  FWorkerThread := nil;

  if Assigned(FIncomingThread) then begin
    if FIncomingThread.Suspended then begin
      FIncomingThread.Start;
    end;

    FIncomingThread.Terminate;
    FSocket.Send(KillClient);
    while not FIncomingThread.Finished do begin
      Sleep(1);
    end;
    FIncomingThread := nil;
  end;
  FSocket.Disconnect;
end;

end.
