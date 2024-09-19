{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.information;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, blcksock,
  opengaze.base,
  opengaze.socket,
  opengaze.events,
  opengaze.types;

type

  { TOpenGazeInformation }

  // A wrapper around outgoing information messages
  TOpenGazeInformation = class(TOpenGazeBase)
  private
    function GetProductID: string;
    public
      constructor Create(ASocket : TOpenGazeSocket; AEvents : TOpenGazeEvents);
      procedure SaveToFile;
      procedure SetupDataFile(AFilename : string);
      property ProductID : string read GetProductID;
  end;

implementation

uses
  opengaze.constants,
  opengaze.commands,
  opengaze.helpers,
  opengaze.logger.information;

{ TOpenGazeInformation }

function TOpenGazeInformation.GetProductID: string;
var
  Dictionary: TPairsDictionary;
begin
  Dictionary := RequestCommand(Information.ProductID);
  try
    Result := Dictionary[VALUE];
  finally
    Dictionary.Free;
  end;
end;

constructor TOpenGazeInformation.Create(ASocket: TOpenGazeSocket;
  AEvents: TOpenGazeEvents);
begin
  inherited Create(ASocket, AEvents);
end;

procedure TOpenGazeInformation.SaveToFile;
var
  Dictionary : TPairsDictionary;

  TimeTickFrequency : string = '';
  CameraWidth : string = '';
  CameraHeight : string = '';
  LProductID : string = '';
  ProductBus : string = '';
  ProductRate : string = '';
  SerialID : string = '';
  CompanyID : string = '';
  APIID : string = '';

begin
  Dictionary := RequestCommand(Information.TimeTickFrequency);
  try
    TimeTickFrequency := Dictionary[FREQ];
  finally
    Dictionary.Free;
  end;

  Dictionary := RequestCommand(Information.CameraSize);
  try
    CameraWidth := Dictionary[CAMERA_WIDTH];
    CameraHeight := Dictionary[CAMERA_HEIGHT];
  finally
    Dictionary.Free;
  end;

  Dictionary := RequestCommand(Information.ProductID);
  try
    LProductID := Dictionary[VALUE];
    ProductBus := Dictionary[BUS];
    ProductRate := Dictionary[RATE];
  finally
    Dictionary.Free;
  end;

  Dictionary := RequestCommand(Information.SerialID);
  try
    SerialID := Dictionary[VALUE];
  finally
    Dictionary.Free;
  end;

  Dictionary := RequestCommand(Information.CompanyID);
  try
    CompanyID := Dictionary[VALUE];
  finally
    Dictionary.Free;
  end;

  Dictionary := RequestCommand(Information.APIID);
  try
    APIID := Dictionary[VALUE];
  finally
    Dictionary.Free;
  end;

  BeginUpdateData;
  LogLine('ClientVersion', '1');
  LogLine('TimeTickFrequency', TimeTickFrequency);
  LogLine('CameraWidth', CameraWidth);
  LogLine('CameraHeight', CameraHeight);
  LogLine('ProductID', LProductID);
  LogLine('ProductBus', ProductBus);
  LogLine('ProductRate', ProductRate);
  LogLine('SerialID', SerialID);
  LogLine('CompanyID', CompanyID);
  LogLine('APIID', APIID);
  EndUpdateData;
end;

procedure TOpenGazeInformation.SetupDataFile(AFilename: string);
begin
  DataFilename := AFilename + '.gaze.info';
end;

end.
