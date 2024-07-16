{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.control;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opengaze.types,
  opengaze.client,
  opengaze.calibration,
  opengaze.recording,
  opengaze.events;

type

  { TOpenGazeControlClient }

  TOpenGazeControlClient = class(TOpenGazeBaseClient)
  private
    FCalibration : TOpenGazeCalibration;
    FRecording   : TOpenGazeRecording;
    function GetIP: string;
    procedure SetIP(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartCalibration;
    property Events : TOpenGazeEvents read FEvents;
    property Calibration : TOpenGazeCalibration read FCalibration;
    property Recording : TOpenGazeRecording read FRecording;
    property IP : string read GetIP write SetIP;
  end;


implementation

function TOpenGazeControlClient.GetIP: string;
begin
  Result := FSocket.IP;
end;

procedure TOpenGazeControlClient.SetIP(AValue: string);
begin
  if FSocket.Connected then Exit;
  if FSocket.IP = AValue then Exit;
  FSocket.IP := AValue;
end;

constructor TOpenGazeControlClient.Create;
begin
  inherited Create;
  FSocket.Server := OpenGazeControlServer;
  FCalibration := TOpenGazeCalibration.Create(FSocket, FEvents);
  FRecording := TOpenGazeRecording.Create(FSocket, FEvents);
end;

destructor TOpenGazeControlClient.Destroy;
begin
  FRecording.Free;
  FCalibration.Free;
  inherited Destroy;
end;

procedure TOpenGazeControlClient.StartCalibration;
begin
  Calibration.Show;
  Calibration.Start;
end;

end.
