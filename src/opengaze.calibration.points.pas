{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.calibration.points;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  Math,
  opengaze.base,
  opengaze.socket,
  opengaze.events,
  opengaze.types;

type

  { TOpenGazeCalibrationPoints }

  TOpenGazeCalibrationPoints = class(TOpenGazeBase)
  private
    FPoints : TCalibrationPoints;
    function GetDelay: Float;
    function GetTimeout: Float;
    procedure SetDelay(AValue: Float);
    procedure SetTimeout(AValue: Float);
    procedure AssignPoints(Dictionary : TPairsDictionary);
  public
    constructor Create(ASocket: TOpenGazeSocket; AEvents: TOpenGazeEvents);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(X, Y : Float);
    function Get : TCalibrationPoints;
    property Timeout : Float read GetTimeout write SetTimeout;
    property Delay   : Float read GetDelay write SetDelay;
  end;

implementation

uses opengaze.constants, opengaze.commands, opengaze.helpers;

{ TOpenGazeCalibrationPoints }

function TOpenGazeCalibrationPoints.GetDelay: Float;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.GetDelay);
  try
    Result := Dictionary[VALUE].ToFloat;
  finally
    Dictionary.Free;
  end;
end;

function TOpenGazeCalibrationPoints.GetTimeout: Float;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.GetTimeOut);
  try
    Result := Dictionary[VALUE].ToFloat;
  finally
    Dictionary.Free;
  end;
end;

procedure TOpenGazeCalibrationPoints.SetDelay(AValue: Float);
begin
  SendCommand(Calibration.SetDelay(AValue));
end;

procedure TOpenGazeCalibrationPoints.SetTimeout(AValue: Float);
begin
  SendCommand(Calibration.SetTimeOut(AValue));
end;

procedure TOpenGazeCalibrationPoints.AssignPoints(Dictionary: TPairsDictionary);
var
  Count, i: Integer;
begin
  Count := Dictionary[PTS].ToInteger;
  SetLength(FPoints, Count);
  for i := Low(FPoints) to High(FPoints) do begin
    FPoints[i].X := Dictionary[XCOORDENATE+(i+1).ToString].ToFloat;
    FPoints[i].Y := Dictionary[YCOORDENATE+(i+1).ToString].ToFloat;
  end;
end;

constructor TOpenGazeCalibrationPoints.Create(ASocket: TOpenGazeSocket;
  AEvents: TOpenGazeEvents);
begin
  inherited Create(ASocket, AEvents);
  SetLength(FPoints, 0);
end;

destructor TOpenGazeCalibrationPoints.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenGazeCalibrationPoints.Clear;
begin
  SendCommand(Calibration.Clear, True);
  Get;
end;

procedure TOpenGazeCalibrationPoints.Add(X, Y: Float);
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.AddPoint(X, Y));
  try
    AssignPoints(Dictionary);
  finally
    Dictionary.Free;
  end;
end;

function TOpenGazeCalibrationPoints.Get: TCalibrationPoints;
var
  Dictionary : TPairsDictionary;
begin
  Dictionary := RequestCommand(Calibration.GetPoints);
  try
    AssignPoints(Dictionary);
    Result := FPoints;
  finally
    Dictionary.Free;
  end;
end;

end.

