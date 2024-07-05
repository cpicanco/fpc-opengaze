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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  opengaze.types;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonStopCalibrationRemote: TButton;
    ButtonStartCalibrationRemote: TButton;
    ButtonStopCalibration: TButton;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonStartRecording: TButton;
    ButtonStopRecording: TButton;
    ButtonStartCalibration: TButton;
    ListBox1: TListBox;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonStartCalibrationClick(Sender: TObject);
    procedure ButtonStartCalibrationRemoteClick(Sender: TObject);
    procedure ButtonStartRecordingClick(Sender: TObject);
    procedure ButtonStopCalibrationClick(Sender: TObject);
    procedure ButtonStopCalibrationRemoteClick(Sender: TObject);
    procedure ButtonStopRecordingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateList(Sender: TObject; Event : TPairsDictionary);
  public

  end;

var
  Form1: TForm1;

implementation

uses opengaze, opengaze.logger;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  OpenGazeControl.Connect;
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  OpenGazeControl.Disconnect;
end;

procedure TForm1.ButtonStartCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.Show;
  OpenGazeControl.Calibration.Start;
end;

procedure TForm1.ButtonStartCalibrationRemoteClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.UseCustomChoreography := True;
  ButtonStartCalibrationClick(Self);
end;

procedure TForm1.ButtonStartRecordingClick(Sender: TObject);
begin
  OpenGazeControl.Recording.Start;
end;

procedure TForm1.ButtonStopCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.Stop;
end;

procedure TForm1.ButtonStopCalibrationRemoteClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.Stop;
end;

procedure TForm1.ButtonStopRecordingClick(Sender: TObject);
begin
  OpenGazeControl.Recording.Stop;
  ListBox1.Items.AddStrings(Data as TStrings);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGazeControl.Events.OnStartCalibration := @UpdateList;
  OpenGazeControl.Events.OnEnableSendData := @UpdateList;
  OpenGazeControl.Events.OnDisableSendData := @UpdateList;
  OpenGazeControl.Events.OnCalibrationPointResult := @UpdateList;
  OpenGazeControl.Events.OnCalibrationPointStart := @UpdateList;
  OpenGazeControl.Events.OnStartRecording := @UpdateList;
  OpenGazeControl.Events.OnStopRecording := @UpdateList;
end;

procedure TForm1.UpdateList(Sender: TObject; Event: TPairsDictionary);
var
  Key: String;
begin
  for Key in Event.Keys do begin
    ListBox1.Items.Add(Key + '=' + Event[Key]);
  end;
end;

end.

