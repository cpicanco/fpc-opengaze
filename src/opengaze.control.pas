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
  opengaze.information,
  opengaze.calibration,
  opengaze.recording,
  opengaze.events;

type

  { TOpenGazeControlClient }

  TOpenGazeControlClient = class(TOpenGazeBaseClient)
  private
    FInformation : TOpenGazeInformation;
    FCalibration : TOpenGazeCalibration;
    FRecording   : TOpenGazeRecording;
    function GetIP: string;
    function GetIsLocal: Boolean;
    function GetProductID: string;
    procedure SetIP(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartCalibration;
    procedure StopCalibration;
    procedure SetupForRemoteServer;
    procedure SetupDataFiles(AFilename : string);
    procedure StartRecording;
    procedure StopRecording;
    property Events : TOpenGazeEvents read FEvents;
    property Calibration : TOpenGazeCalibration read FCalibration;
    property Recording : TOpenGazeRecording read FRecording;
    property Information : TOpenGazeInformation read FInformation;
    property IP : string read GetIP write SetIP;
    property IsLocal : Boolean read GetIsLocal;
    property ProductID : string read GetProductID;
  end;


implementation

function TOpenGazeControlClient.GetIP: string;
begin
  Result := FSocket.IP;
end;

function TOpenGazeControlClient.GetIsLocal: Boolean;
begin
  Result := FSocket.IsLocal;
end;

function TOpenGazeControlClient.GetProductID: string;
begin
  Result := Information.ProductID;
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
  FInformation := TOpenGazeInformation.Create(FSocket, FEvents);
  FCalibration := TOpenGazeCalibration.Create(FSocket, FEvents);
  FRecording := TOpenGazeRecording.Create(FSocket, FEvents);
end;

destructor TOpenGazeControlClient.Destroy;
begin
  FInformation.Free;
  FRecording.Free;
  FCalibration.Free;
  inherited Destroy;
end;

procedure TOpenGazeControlClient.StartCalibration;
begin
  Calibration.Show;
  Calibration.Start;
end;

procedure TOpenGazeControlClient.StopCalibration;
begin
  Calibration.Stop;
  Calibration.Hide;
end;

procedure TOpenGazeControlClient.SetupForRemoteServer;
begin
  Calibration.UseCustomChoreography := True;
end;

procedure TOpenGazeControlClient.SetupDataFiles(AFilename: string);
begin
  Information.SetupDataFile(AFilename);
  Calibration.SetupDataFile(AFilename);
  Recording.SetupDataFile(AFilename);
end;

procedure TOpenGazeControlClient.StartRecording;
begin
  Information.SaveToFile;
  Calibration.StartLogger;
  Recording.Start;
end;

procedure TOpenGazeControlClient.StopRecording;
begin
  Recording.Stop;
  Calibration.StopLogger;
end;

end.
