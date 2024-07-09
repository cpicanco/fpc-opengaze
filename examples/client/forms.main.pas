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
    ButtonNextScreen: TButton;
    ButtonStopCalibrationRemote: TButton;
    ButtonStartCalibrationRemote: TButton;
    ButtonStopCalibration: TButton;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonStartRecording: TButton;
    ButtonStopRecording: TButton;
    ButtonStartCalibration: TButton;
    ListBox1: TListBox;
    MenuItemSaveToFile: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonNextScreenClick(Sender: TObject);
    procedure ButtonStartCalibrationClick(Sender: TObject);
    procedure ButtonStartCalibrationRemoteClick(Sender: TObject);
    procedure ButtonStartRecordingClick(Sender: TObject);
    procedure ButtonStopCalibrationClick(Sender: TObject);
    procedure ButtonStopCalibrationRemoteClick(Sender: TObject);
    procedure ButtonStopRecordingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemSaveToFileClick(Sender: TObject);
  private
    procedure UpdateList(Sender: TObject; Event : TPairsDictionary);
    procedure EndCalibration(Sender: TObject; Event : TPairsDictionary);
  public

  end;

var
  Form1: TForm1;

implementation

uses opengaze, opengaze.logger;

{$R *.lfm}

{ TForm1 }

var
  Source : string;

procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  OpenGazeControl.Connect;
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  OpenGazeControl.Disconnect;
end;

procedure TForm1.ButtonNextScreenClick(Sender: TObject);
begin
  OpenGazeControl.Calibration.Choreography.SelectNextScreen;
end;

procedure TForm1.ButtonStartCalibrationClick(Sender: TObject);
begin
  Source := 'Gazepoint';
  OpenGazeControl.Calibration.UseCustomChoreography := False;
  OpenGazeControl.Calibration.Show;
  OpenGazeControl.Calibration.Start;
end;

procedure TForm1.ButtonStartCalibrationRemoteClick(Sender: TObject);
begin
  Source := 'Custom';
  OpenGazeControl.Calibration.UseCustomChoreography := True;
  OpenGazeControl.Calibration.Show;
  OpenGazeControl.Calibration.Start;
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
  //OpenGazeControl.Events.OnStartCalibration := @UpdateList;
  //OpenGazeControl.Events.OnEnableSendData := @UpdateList;
  //OpenGazeControl.Events.OnDisableSendData := @UpdateList;
  //OpenGazeControl.Events.OnCalibrationPointResult := @UpdateList;
  //OpenGazeControl.Events.OnCalibrationPointStart := @UpdateList;
  //OpenGazeControl.Events.OnStartRecording := @UpdateList;
  //OpenGazeControl.Events.OnStopRecording := @UpdateList;
  OpenGazeControl.Events.OnCalibrationResult := @EndCalibration;
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

procedure TForm1.EndCalibration(Sender: TObject; Event: TPairsDictionary);
begin
  //UpdateList(Sender, Event);
  OpenGazeControl.Calibration.Stop;
  OpenGazeControl.Calibration.Hide;
  ListBox1.Items.Add(Source + #9 + OpenGazeControl.Calibration.ResultSummary);

  Source := '';
end;

end.

