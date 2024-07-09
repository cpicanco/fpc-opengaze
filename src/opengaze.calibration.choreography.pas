{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.calibration.choreography;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, opengaze.types, choreographies;

type

  { TOpenGazeCalibrationChoreography }

  TOpenGazeCalibrationChoreography = class(TCalibrationChoreography)
  private
    FOnPointStart : TOpenGazeEvent;
    FOnPointEnd   : TOpenGazeEvent;
    procedure SetOnPointEnd(AValue: TOpenGazeEvent);
    procedure SetOnPointStart(AValue: TOpenGazeEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SelectNextScreen;
    property OnPointStart : TOpenGazeEvent read FOnPointStart write SetOnPointStart;
    property OnPointEnd : TOpenGazeEvent read FOnPointEnd write SetOnPointEnd;
  end;


var
  OpenGazeCalibrationChoreography : TOpenGazeCalibrationChoreography;

implementation

uses Forms, Controls, forms.background;

{ TOpenGazeCalibrationChoreography }

procedure TOpenGazeCalibrationChoreography.SetOnPointEnd(
  AValue: TOpenGazeEvent);
begin
  if FOnPointEnd = AValue then Exit;
  FOnPointEnd := AValue;
end;

procedure TOpenGazeCalibrationChoreography.SetOnPointStart(
  AValue: TOpenGazeEvent);
begin
  if FOnPointStart = AValue then Exit;
  FOnPointStart := AValue;
end;

constructor TOpenGazeCalibrationChoreography.Create;
begin
  FormBackground := TFormBackground.CreateNew(nil);
  FormBackground.BorderStyle := bsNone;
  FormBackground.WindowState := wsFullScreen;
  FormBackground.Color := 0;
  FormBackground.Width := Screen.Width;
  FormBackground.Height := Screen.Height;
  inherited Create(FormBackground);
end;

destructor TOpenGazeCalibrationChoreography.Destroy;
begin
  FormBackground.Free;
  inherited Destroy;
end;

procedure TOpenGazeCalibrationChoreography.SelectNextScreen;
begin
  FormBackground.MoveToNextMonitor;
end;

end.

