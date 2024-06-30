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
    ButtonConnect: TButton;
    ButtonConnecTOpenGazeAnalysis: TButton;
    ButtonDisconnect: TButton;
    ButtonDisconnecTOpenGazeAnalysis: TButton;
    ButtonStartCalibration: TButton;
    ButtonStartRecording: TButton;
    ButtonStartRecordingOpenGazeAnalysis: TButton;
    ButtonStopRecording: TButton;
    ButtonStopRecordingOpenGazeAnalysis: TButton;
    ButtonUserEvent: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ListBoxRecording: TListBox;
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonStartCalibrationClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonStartRecordingClick(Sender: TObject);
    procedure ButtonStopRecordingClick(Sender: TObject);
    procedure ButtonUserEventClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoCalibrationResult(Sender : TObject; Event: TPairsDictionary);
    procedure DoCalibrationResultSummary(Sender : TObject; Event: TPairsDictionary);
    procedure DoStartRecording(Sender: TObject; Event: TPairsDictionary);
    procedure DoStopRecording(Sender: TObject; Event: TPairsDictionary);
  public

  end;

var
  Form1: TForm1;

implementation

uses opengaze, opengaze.constants, opengaze.logger, windows.timestamps;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGazeControl.Events.OnCalibrationResult := @DoCalibrationResult;
  OpenGazeControl.Events.OnCalibrationResultSummary := @DoCalibrationResultSummary;
  OpenGazeControl.Events.OnEnableSendData := @DoStartRecording;
  OpenGazeControl.Events.OnDisableSendData := @DoStopRecording;
end;

procedure TForm1.DoCalibrationResult(Sender: TObject;
  Event: TPairsDictionary);
begin
  OpenGazeControl.Calibration.Hide;
  ListBoxRecording.Items.Append(Event[ID]);
  ListBoxRecording.Items.Append(OpenGazeControl.Calibration.ResultSummary);
end;

procedure TForm1.DoCalibrationResultSummary(Sender: TObject;
  Event: TPairsDictionary);
begin
  ListBoxRecording.Items.Append(Event[ID]+#32+Event[AVE_ERROR]);
end;

procedure TForm1.DoStartRecording(Sender: TObject; Event: TPairsDictionary);
begin
  ListBoxRecording.Items.Append(Event[ID]+#32+Event[STATE]);
  ListBoxRecording.Items.Append(
    'Receiving data from the server.');
end;

procedure TForm1.DoStopRecording(Sender: TObject; Event: TPairsDictionary);
begin
  ListBoxRecording.Items.Append(Event[ID]+#32+Event[STATE]);
  ListBoxRecording.Items.Append(
    'Stop receiving data from the server.');
end;

procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  if Sender = ButtonConnect then begin
    OpenGazeControl.Connect;
  end;

  if Sender = ButtonConnecTOpenGazeAnalysis then begin
    OpenGazeAnalysis.Connect;
  end;
end;

procedure TForm1.ButtonStartRecordingClick(Sender: TObject);
begin
  if Sender = ButtonStartRecording then begin
    OpenGazeControl.Recording.Start;
  end;

  if Sender = ButtonStartRecordingOpenGazeAnalysis then begin
    OpenGazeAnalysis.Recording.Start;
  end;
end;

procedure TForm1.ButtonStopRecordingClick(Sender: TObject);
begin
  if Sender = ButtonStopRecording then begin
    OpenGazeControl.Recording.Stop;
  end;

  if Sender = ButtonStopRecordingOpenGazeAnalysis then begin
    OpenGazeAnalysis.Recording.Stop;
  end;

  ListBoxRecording.Items.AddStrings(Data as TStrings);
end;

procedure TForm1.ButtonUserEventClick(Sender: TObject);
begin
  ListBoxRecording.Items.Append(ClockMonotonic.ToString);
end;

procedure TForm1.ButtonStartCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.StartCalibration;
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  if Sender = ButtonDisconnect then begin
    OpenGazeControl.Disconnect;
  end;

  if Sender = ButtonDisconnecTOpenGazeAnalysis then begin
    OpenGazeAnalysis.Disconnect;
  end;
end;

end.

