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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  opengaze.types;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonFlush: TButton;
    ButtonHideCalibration: TButton;
    ButtonShowCalibration: TButton;
    ButtonNextScreen: TButton;
    ButtonStopCalibration: TButton;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonStartRecording: TButton;
    ButtonStopRecording: TButton;
    ButtonStartCalibration: TButton;
    CheckBoxUseCustomCalibration: TCheckBox;
    CheckBoxUseRemoteServer: TCheckBox;
    EditIP: TEdit;
    ListBox1: TListBox;
    MenuItemSaveToFile: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonFlushClick(Sender: TObject);
    procedure ButtonHideCalibrationClick(Sender: TObject);
    procedure ButtonNextScreenClick(Sender: TObject);
    procedure ButtonShowCalibrationClick(Sender: TObject);
    procedure ButtonStartCalibrationClick(Sender: TObject);
    procedure ButtonStartRecordingClick(Sender: TObject);
    procedure ButtonStopCalibrationClick(Sender: TObject);
    procedure ButtonStopRecordingClick(Sender: TObject);
    procedure CheckBoxUseRemoteServerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemSaveToFileClick(Sender: TObject);
  private
    procedure UpdateList(Sender: TObject; Event : TPairsDictionary);
    procedure CalibrationFailed(Sender: TObject);
    procedure CalibrationSuccess(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

uses opengaze, opengaze.logger, opengaze.helpers;

{$R *.lfm}

{ TForm1 }

var
  FailedCalibrations : integer = 0;

procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  OpenGazeControl.Connect;
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  OpenGazeControl.Disconnect;
end;

procedure TForm1.ButtonFlushClick(Sender: TObject);
begin
  ListBox1.Items.AddStrings(Data as TStrings);
  Data.Clear;
end;

procedure TForm1.ButtonHideCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.Hide;
end;

procedure TForm1.ButtonNextScreenClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.SelectNextScreen;
end;

procedure TForm1.ButtonShowCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.Show;
end;

procedure TForm1.ButtonStartCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.UseCustomChoreography :=
    CheckBoxUseCustomCalibration.Checked;
  OpenGazeControl.IP := EditIP.Text;

  OpenGazeControl.StartCalibration;
end;

procedure TForm1.ButtonStartRecordingClick(Sender: TObject);
begin
  OpenGazeControl.StartRecording;
end;

procedure TForm1.ButtonStopCalibrationClick(Sender: TObject);
begin
  OpenGazeControl.StopCalibration;
end;

procedure TForm1.ButtonStopRecordingClick(Sender: TObject);
begin
  OpenGazeControl.StopRecording;
end;

procedure TForm1.CheckBoxUseRemoteServerChange(Sender: TObject);
begin
  if CheckBoxUseRemoteServer.Checked then begin
    //EditIP.Text := '169.254.150.247';
    CheckBoxUseCustomCalibration.Checked := True;
    CheckBoxUseCustomCalibration.Enabled := False;
    EditIP.Enabled := True;
  end else begin
    CheckBoxUseCustomCalibration.Enabled := True;
    EditIP.Text := '127.0.0.1';
    EditIP.Enabled := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGazeControl.Calibration.OnResult := @UpdateList;
  OpenGazeControl.Calibration.OnFailed := @CalibrationFailed;
  OpenGazeControl.Calibration.OnSuccess := @CalibrationSuccess;
end;

procedure TForm1.MenuItemSaveToFileClick(Sender: TObject);
begin
  ListBox1.Items.SaveToFile('ListBox1.txt');
end;

procedure TForm1.UpdateList(Sender: TObject; Event: TPairsDictionary);
var
  Key: String;
begin
  for Key in Event.Keys do begin
    ListBox1.Items.Add(Key + '=' + Event[Key]);
  end;
end;

procedure TForm1.CalibrationFailed(Sender: TObject);
begin
  if OpenGazeControl.Calibration.UseCustomChoreography then begin
    OpenGazeControl.StopCalibration;
  end;

  Inc(FailedCalibrations);
  if FailedCalibrations > 2 then begin
    Exit;
  end else begin
    ButtonStartCalibrationClick(Sender);
  end;
end;

procedure TForm1.CalibrationSuccess(Sender: TObject);
begin
  FailedCalibrations := 0;
end;

end.

