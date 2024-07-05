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
  Classes, SysUtils, choreographies;

type

  { TOpenGazeCalibrationChoreography }

  TOpenGazeCalibrationChoreography = class(TCalibrationChoreography)
  public
    constructor Create;
    destructor Destroy; override;
    procedure NextMonitor;
  end;


var
  OpenGazeCalibrationChoreography : TOpenGazeCalibrationChoreography;

implementation

uses Forms, Controls, forms.background;

{ TOpenGazeCalibrationChoreography }

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

procedure TOpenGazeCalibrationChoreography.NextMonitor;
begin
  FormBackground.MoveToNextMonitor;
  FormBackground.Width := Screen.Width;
  FormBackground.Height := Screen.Height;
end;

end.

