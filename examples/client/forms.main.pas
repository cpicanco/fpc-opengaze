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
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonStartRecording: TButton;
    ButtonStopRecording: TButton;
    ButtonStartCalibration: TButton;
    ListBox1: TListBox;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonStartCalibrationClick(Sender: TObject);
    procedure ButtonStartRecordingClick(Sender: TObject);
    procedure ButtonStopRecordingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Update(Sender: TObject; Event : TPairsDictionary);
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

procedure TForm1.ButtonStartRecordingClick(Sender: TObject);
begin
  OpenGazeControl.Recording.Start;
end;

procedure TForm1.ButtonStopRecordingClick(Sender: TObject);
begin
  OpenGazeControl.Recording.Stop;
  ListBox1.Items.AddStrings(Data as TStrings);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGazeControl.Events.OnStartCalibration := @Update;
  OpenGazeControl.Events.OnEnableSendData := @Update;
  OpenGazeControl.Events.OnDisableSendData := @Update;
  OpenGazeControl.Events.OnCalibrationPointResult := @Update;
  OpenGazeControl.Events.OnCalibrationPointStart := @Update;
end;

procedure TForm1.Update(Sender: TObject; Event: TPairsDictionary);
var
  Key: String;
begin
  for Key in Event.Keys do begin
    ListBox1.items.Add(Key + '=' + Event[Key]);
  end;
end;

end.

