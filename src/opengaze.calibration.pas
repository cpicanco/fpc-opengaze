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
  opengaze.types,
  opengaze.calibration.points,
  opengaze.calibration.choreography;

type

  { TOpenGazeCalibration }

  // A wrapper around outgoing calibration messages
  TOpenGazeCalibration = class(TOpenGazeBase)
    private
      FLoggerAlive : Boolean;
      FChoreography : TOpenGazeCalibrationChoreography;
      FOnFailed: TNotifyEvent;
      FOnSuccess: TNotifyEvent;
      FOnResult : TOpenGazeEvent;
      FPoints: TOpenGazeCalibrationPoints;
      function GetOnResultSummary: TOpenGazeEvent;
      function GetRemote: Boolean;
      function GetStarted: Boolean;
      procedure SetChoreography(AValue: TOpenGazeCalibrationChoreography);
      procedure SetOnFailed(AValue: TNotifyEvent);
      procedure SetOnSuccess(AValue: TNotifyEvent);
      procedure SetOnResult(AValue: TOpenGazeEvent);
      procedure SetOnResultSummary(AValue: TOpenGazeEvent);
      procedure SetRemote(AValue: Boolean);
      procedure StartPointAnimation(Sender : TObject; Event : TPairsDictionary);
      procedure EndPointTimeout(Sender : TObject; Event : TPairsDictionary);
      procedure DoCalibrationResult(Sender: TObject; Event : TPairsDictionary);
    public
      constructor Create(ASocket : TOpenGazeSocket; AEvents : TOpenGazeEvents);
      destructor Destroy; override;
      procedure Show(Blocking : Boolean = True);
      procedure Hide(Blocking : Boolean = True);
      procedure Start(Blocking : Boolean = True);
      procedure Stop(Blocking : Boolean = True);
      procedure SelectNextScreen;
      procedure SetScreen(AX, AY, AW, AH : integer);
      procedure SetupDataFile(AFilename : string);
      procedure StartLogger;
      procedure StopLogger;
      function ResultSummary : string;
      function PointDelay : string;
      function PointDuration : string;
      property Started : Boolean read GetStarted;
      property OnResult : TOpenGazeEvent read FOnResult write SetOnResult;
      property OnResultSummary : TOpenGazeEvent read GetOnResultSummary write SetOnResultSummary;
      property OnSuccess : TNotifyEvent read FOnSuccess write SetOnSuccess;
      property OnFailed : TNotifyEvent read FOnFailed write SetOnFailed;
      property Points : TOpenGazeCalibrationPoints read FPoints;
      property Choreography : TOpenGazeCalibrationChoreography read FChoreography write SetChoreography;
      property UseCustomChoreography : Boolean read GetRemote write SetRemote;
  end;

implementation

uses
  opengaze.constants,
  opengaze.commands,
  opengaze.helpers,
  opengaze.logger.calibration,
  choreographies;

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

procedure TOpenGazeCalibration.SetChoreography(
  AValue: TOpenGazeCalibrationChoreography);
begin
  if FChoreography = AValue then Exit;
  FChoreography := AValue;
end;

procedure TOpenGazeCalibration.SetOnFailed(AValue: TNotifyEvent);
begin
  if FOnFailed = AValue then Exit;
  FOnFailed := AValue;
end;

procedure TOpenGazeCalibration.SetOnSuccess(
  AValue: TNotifyEvent);
begin
  if FOnSuccess = AValue then Exit;
  FOnSuccess := AValue;
end;

function TOpenGazeCalibration.GetOnResultSummary: TOpenGazeEvent;
begin
  Result := nil;
  if Assigned(FEvents) then begin
    Result := FEvents.OnCalibrationResultSummary;
  end;
end;

function TOpenGazeCalibration.GetRemote: Boolean;
begin
  Result := FChoreography.Animation.WaitForRemote;
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

procedure TOpenGazeCalibration.SetRemote(AValue: Boolean);
begin
  if FChoreography.Animation.WaitForRemote = AValue then Exit;
  FChoreography.Animation.WaitForRemote := AValue;

  if FChoreography.Animation.WaitForRemote then begin
    FChoreography.OnPointStart := FEvents.OnCalibrationPointStart;
    FEvents.OnCalibrationPointStart := @StartPointAnimation;

    FChoreography.OnPointEnd := FEvents.OnCalibrationPointResult;
    FEvents.OnCalibrationPointResult := @EndPointTimeout;
  end else begin
    FEvents.OnCalibrationPointStart := FChoreography.OnPointStart;
    FEvents.OnCalibrationPointResult := FChoreography.OnPointEnd;
  end;
end;

procedure TOpenGazeCalibration.StartPointAnimation(Sender: TObject;
  Event: TPairsDictionary);
begin
  FChoreography.Animation.StartPointAnimation;
  if Assigned(FChoreography.OnPointStart) then begin
    FChoreography.OnPointStart(Sender, Event);
  end;
end;

procedure TOpenGazeCalibration.EndPointTimeout(Sender: TObject;
  Event: TPairsDictionary);
begin
  FChoreography.Animation.EndPointTimeout;
  if Assigned(FChoreography.OnPointEnd) then begin
    FChoreography.OnPointEnd(Sender, Event);
  end;
end;

procedure TOpenGazeCalibration.DoCalibrationResult(Sender: TObject;
  Event: TPairsDictionary);
begin
  if FLoggerAlive then begin
    LogLine(Event);
  end;

  if Assigned(FOnResult) then begin
    FOnResult(Sender, Event);
  end;

  if Assigned(FOnFailed) and
     Assigned(FOnSuccess) then begin
    if ResultSummary.ToFloat > 40 { pixels } then begin
      FOnFailed(Sender);
    end else begin
      FOnSuccess(Sender);
    end;
  end;
end;

constructor TOpenGazeCalibration.Create(ASocket: TOpenGazeSocket;
  AEvents: TOpenGazeEvents);
begin
  inherited Create(ASocket, AEvents);
  FLoggerAlive := False;
  FEvents.OnCalibrationPointStart := nil;
  FEvents.OnCalibrationPointResult := nil;
  FEvents.OnCalibrationResult := @DoCalibrationResult;

  FPoints := TOpenGazeCalibrationPoints.Create(ASocket, AEvents);

  FChoreography := OpenGazeCalibrationChoreography;
  FChoreography.Events := FEvents;
end;

destructor TOpenGazeCalibration.Destroy;
begin
  FPoints.Free;
  inherited Destroy;
end;

procedure TOpenGazeCalibration.Show(Blocking: Boolean);
var
  NormalizedPoints : TNormalizedPoints;
  Point : TNormalizedPoint;
begin
  if UseCustomChoreography then begin
    if FChoreography.Visible then Exit;
    Points.Delay := 1.0;
    Points.Timeout := 1.5;
    Points.Clear;

    FChoreography.Animation.Delay := PointDelay.ToTimeInteger-3000000;
    FChoreography.Animation.Duration := PointDuration.ToTimeInteger;

    NormalizedPoints := FChoreography.GetPoints;
    for Point in NormalizedPoints do begin
      Points.Add(Point.X, Point.Y);
    end;

    if FLoggerAlive then begin
      LogLine(NormalizedPoints);
    end;

    FChoreography.Show;
  end else begin
    SendCommand(Calibration.Show, Blocking);
  end;
end;

procedure TOpenGazeCalibration.Hide(Blocking: Boolean);
begin
  if UseCustomChoreography then begin
    FChoreography.Hide;
  end else begin
    SendCommand(Calibration.Hide, Blocking);
  end;
end;

procedure TOpenGazeCalibration.Start(Blocking: Boolean);
begin
  SendCommand(Calibration.Start, Blocking);
end;

procedure TOpenGazeCalibration.Stop(Blocking: Boolean);
begin
  SendCommand(Calibration.Stop, Blocking);
  if UseCustomChoreography then begin
    FChoreography.Stop;
  end;
end;

procedure TOpenGazeCalibration.SelectNextScreen;
begin
  Choreography.SelectNextScreen;

  with Choreography.BoundsRect do begin
    SendCommand(Calibration.SetScreenSize(Left, Top, Width, Height));
  end;
end;

procedure TOpenGazeCalibration.SetScreen(AX, AY, AW, AH: integer);
begin
  Choreography.SetScreen(AX, AY, AW, AH);
  with Choreography.BoundsRect do begin
    SendCommand(Calibration.SetScreenSize(Left, Top, Width, Height));
  end;
end;

procedure TOpenGazeCalibration.SetupDataFile(AFilename: string);
begin
  DataFilename := AFilename + '.gaze.calibration';
end;

procedure TOpenGazeCalibration.StartLogger;
begin
  BeginUpdateData;
  FLoggerAlive := True;
end;

procedure TOpenGazeCalibration.StopLogger;
begin
  FLoggerAlive := False;
  EndUpdateData;
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

function TOpenGazeCalibration.PointDelay: string;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.GetDelay);
  try
    Result := Dictionary[VALUE];
  finally
    Dictionary.Free;
  end;
end;

function TOpenGazeCalibration.PointDuration: string;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.GetTimeOut);
  try
    Result := Dictionary[VALUE];
  finally
    Dictionary.Free;
  end;
end;

end.

