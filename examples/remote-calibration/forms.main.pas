{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit forms.main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  opengaze.types;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonStartCalibration: TButton;
    ButtonConnecTOpenGazeAnalysis: TButton;
    ButtonDisconnecTOpenGazeAnalysis: TButton;
    ButtonStartRecordingOpenGazeAnalysis: TButton;
    ButtonStopRecordingOpenGazeAnalysis: TButton;
    CheckBoxUseCustomChoreography: TCheckBox;
    ListBox1: TListBox;
    procedure ButtonStartCalibrationClick(Sender: TObject);
    procedure CheckBoxUseCustomChoreographyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoCalibrationResult(Sender : TObject; Event: TPairsDictionary);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  opengaze, opengaze.constants,
  opengaze.helpers, opengaze.logger, windows.timestamps;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGazeControl.Events.OnCalibrationResult := @DoCalibrationResult;
  OpenGazeControl.Calibration.UseCustomChoreography :=
    CheckBoxUseCustomChoreography.Checked;
end;

procedure TForm1.DoCalibrationResult(Sender: TObject;
  Event: TPairsDictionary);
var
  Key : string;
begin
  OpenGazeControl.Calibration.Hide;

  for Key in Event.Keys do begin
    ListBox1.Items.Append(Key+'='+Event[Key]);
  end;
end;

procedure TForm1.ButtonStartCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.Connect;
  OpenGazeControl.Calibration.Show;
  OpenGazeControl.Calibration.Start;
end;

procedure TForm1.CheckBoxUseCustomChoreographyChange(Sender: TObject);
begin
  OpenGazeControl.Calibration.UseCustomChoreography :=
    CheckBoxUseCustomChoreography.Checked;
end;

end.

