{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.queues;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, syncobjs, opengaze.types;

type

  TThreadStringList = specialize TThreadList<string>;

  { TThreadStringQueue }

  TThreadStringQueue = class
  private
    FIsWaiting : Boolean;
    FDataAvailable: TEvent;
    FUpdating: TEvent;
    FIsUpdating : Boolean;
    FThreadList: TThreadStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Dequeue : string; overload;
    function Dequeue(var Command: string) : Boolean; overload;
    function Oldest : string;
    function IsWaiting : Boolean;
    function HasData : Boolean;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating : Boolean;
    procedure DeleteOldest;
    procedure Enqueue(const AString: string);
    procedure ResetEvent;
    procedure SetEvent;
    procedure WaitForData;
    property Commands : TThreadStringList read FThreadList;
  end;

implementation

uses opengaze.parser;

constructor TThreadStringQueue.Create;
begin
  inherited Create;
  FThreadList := TThreadStringList.Create;
  FDataAvailable := TEvent.Create;
  FUpdating := TEvent.Create;
  FIsWaiting := False;
end;

destructor TThreadStringQueue.Destroy;
begin
  FUpdating.Free;
  FDataAvailable.Free;
  FThreadList.Free;
  inherited Destroy;
end;

procedure TThreadStringQueue.Enqueue(const AString: string);
begin
  FThreadList.Add(AString);
end;

procedure TThreadStringQueue.ResetEvent;
begin
  FDataAvailable.ResetEvent;
  FDataAvailable.Acquire;
  try
    FIsWaiting := False;
  finally
    FDataAvailable.Release;
  end;
end;

procedure TThreadStringQueue.SetEvent;
begin
  FDataAvailable.SetEvent;
end;

procedure TThreadStringQueue.DeleteOldest;
var
  List: specialize TList<string>;
begin
  List := FThreadList.LockList;
  try
    List.Delete(0);
  finally
    FThreadList.UnlockList;
  end;
end;

function TThreadStringQueue.Dequeue: string;
var
  List: specialize TList<string>;
begin
  Result := '';
  List := FThreadList.LockList;
  try
    if List.Count > 0 then begin
      Result := List.First;
      List.Delete(0);
    end;
  finally
    FThreadList.UnlockList;
  end;
end;

function TThreadStringQueue.Dequeue(var Command: string): Boolean;
var
  List: specialize TList<string>;
  LCommand : string;
  RawTagReceived : TRawTag;
  RawTagRequested : TRawTag;
  i : integer;
begin
  Result := False;
  RawTagReceived := Default(TRawTag);
  RawTagRequested := ParseXML(Command);

  List := FThreadList.LockList;
  try
    for i := 0 to List.Count -1 do begin
      LCommand := List.Items[i];
      RawTagReceived := ParseXML(LCommand);
      if RawTagReceived.ID = RawTagRequested.ID then begin
        Result := True;
        Command := LCommand;
        List.Delete(i);
        Break;
      end;
    end;
  finally
    FThreadList.UnlockList;
  end;
end;

function TThreadStringQueue.Oldest: string;
var
  List: specialize TList<string>;
begin
  Result := '';
  List := FThreadList.LockList;
  try
    if List.Count > 0 then begin
      Result := List.First;
    end;
  finally
    FThreadList.UnlockList;
  end;
end;

function TThreadStringQueue.IsWaiting: Boolean;
begin
  FDataAvailable.Acquire;
  try
    Result := FIsWaiting;
  finally
    FDataAvailable.Release;
  end;
end;

function TThreadStringQueue.HasData: Boolean;
var
  List: specialize TList<string>;
begin
  List := FThreadList.LockList;
  try
    Result := List.Count > 0;
  finally
    FThreadList.UnlockList;
  end;
end;

procedure TThreadStringQueue.Clear;
begin
  FThreadList.Clear;
end;

procedure TThreadStringQueue.BeginUpdate;
begin
  FUpdating.Acquire;
  try
    FIsUpdating := True;
  finally
    FUpdating.Release;
  end;
end;

procedure TThreadStringQueue.EndUpdate;
begin
  FUpdating.Acquire;
  try
    FIsUpdating := False;
  finally
    FUpdating.Release;
  end;
end;

function TThreadStringQueue.IsUpdating: Boolean;
begin
  FUpdating.Acquire;
  try
    Result := FIsUpdating;
  finally
    FUpdating.Release;
  end;
end;

procedure TThreadStringQueue.WaitForData;
begin
  FDataAvailable.Acquire;
  try
    FIsWaiting := True;
  finally
    FDataAvailable.Release;
  end;
  FDataAvailable.WaitFor(INFINITE);
  FDataAvailable.Acquire;
  try
    FIsWaiting := False;
  finally
    FDataAvailable.Release;
  end;
end;

end.

