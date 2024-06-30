{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, blcksock,
  opengaze.base,
  opengaze.socket,
  opengaze.events,
  opengaze.types;

type

  { TOpenGazeCalibration }

  // A wrapper around outgoing calibration messages
  TOpenGazeCalibration = class(TOpenGazeBase)
    private
      function GetOnResult: TOpenGazeEvent;
      function GetOnResultSummary: TOpenGazeEvent;
      function GetStarted: Boolean;
      procedure SetOnResult(AValue: TOpenGazeEvent);
      procedure SetOnResultSummary(AValue: TOpenGazeEvent);
    public
      constructor Create(ASocket : TOpenGazeSocket; AEvents : TOpenGazeEvents);
      destructor Destroy; override;
      procedure Show(Blocking : Boolean = True);
      procedure Hide(Blocking : Boolean = True);
      procedure Start(Blocking : Boolean = True);
      procedure Stop(Blocking : Boolean = True);
      procedure Clear(Blocking : Boolean = True);
      function ResultSummary : string;
      property Started : Boolean read GetStarted;
      property OnResult : TOpenGazeEvent read GetOnResult write SetOnResult;
      property OnResultSummary : TOpenGazeEvent read GetOnResultSummary write SetOnResultSummary;
  end;

implementation

uses OpenGaze.constants, OpenGaze.commands;

{ TOpenGazeCalibration }

function TOpenGazeCalibration.GetStarted: Boolean;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.Started);
  try
    Result := Dictionary[STATE] = _TRUE_;
  finally
    Dictionary.Free;
  end;
end;

function TOpenGazeCalibration.GetOnResult: TOpenGazeEvent;
begin
  Result := nil;
  if Assigned(FEvents) then begin
    Result := FEvents.OnCalibrationResult;
  end;
end;

function TOpenGazeCalibration.GetOnResultSummary: TOpenGazeEvent;
begin
  Result := nil;
  if Assigned(FEvents) then begin
    Result := FEvents.OnCalibrationResultSummary;
  end;
end;

procedure TOpenGazeCalibration.SetOnResult(AValue: TOpenGazeEvent);
begin
  if FEvents = nil then Exit;
  if FEvents.OnCalibrationResult = AValue then Exit;
  FEvents.OnCalibrationResult := AValue;
end;

procedure TOpenGazeCalibration.SetOnResultSummary(AValue: TOpenGazeEvent);
begin
  if FEvents = nil then Exit;
  if FEvents.OnCalibrationResultSummary = AValue then Exit;
  FEvents.OnCalibrationResultSummary := AValue;
end;

constructor TOpenGazeCalibration.Create(ASocket: TOpenGazeSocket;
  AEvents: TOpenGazeEvents);
begin
  inherited Create(ASocket, AEvents);
end;

destructor TOpenGazeCalibration.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenGazeCalibration.Show(Blocking: Boolean);
begin
  SendCommand(Calibration.Show, Blocking);
end;

procedure TOpenGazeCalibration.Hide(Blocking: Boolean);
begin
  SendCommand(Calibration.Hide, Blocking);
end;

procedure TOpenGazeCalibration.Start(Blocking: Boolean);
begin
  SendCommand(Calibration.Start, Blocking);
end;

procedure TOpenGazeCalibration.Stop(Blocking: Boolean);
begin
  SendCommand(Calibration.Stop, Blocking);
end;

procedure TOpenGazeCalibration.Clear(Blocking: Boolean);
begin
  SendCommand(Calibration.Clear, Blocking);
end;

function TOpenGazeCalibration.ResultSummary: string;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.ResultSummary);
  try
    Result := Dictionary[AVE_ERROR];
  finally
    Dictionary.Free;
  end;
end;

end.

