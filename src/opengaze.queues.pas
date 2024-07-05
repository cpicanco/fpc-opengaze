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
  Classes, SysUtils, Generics.Collections;

type

  TThreadStringList = specialize TThreadList<string>;

  TThreadStringQueue = class
  private
    FThreadList: TThreadStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const AString: string);
    function Dequeue: string;
    procedure Clear;
  end;

implementation

constructor TThreadStringQueue.Create;
begin
  inherited Create;
  FThreadList := TThreadStringList.Create;
end;

destructor TThreadStringQueue.Destroy;
begin
  FThreadList.Free;
  inherited Destroy;
end;

procedure TThreadStringQueue.Enqueue(const AString: string);
begin
  FThreadList.Add(AString);
end;

function TThreadStringQueue.Dequeue: string;
var
  List: specialize TList<string>;
begin
  Result := '';
  List := FThreadList.LockList;
  try
    if List.Count > 0 then
    begin
      Result := List.First;
      List.Delete(0);
    end;
  finally
    FThreadList.UnlockList;
  end;
end;

procedure TThreadStringQueue.Clear;
begin
  FThreadList.Clear;
end;

end.

