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
  Classes, SysUtils, opengaze.types, opengaze.events, choreographies;

type

  { TOpenGazeCalibrationChoreography }

  TOpenGazeCalibrationChoreography = class(TCalibrationChoreography)
  private
    FOnPointStart : TOpenGazeEvent;
    FOnPointEnd   : TOpenGazeEvent;
    FEvents : TOpenGazeEvents;
    function GetVisible: Boolean;
    procedure SetEvents(AValue: TOpenGazeEvents);
    procedure SetOnPointEnd(AValue: TOpenGazeEvent);
    procedure SetOnPointStart(AValue: TOpenGazeEvent);
    procedure ReceiveGazePoint(Sender: TObject; RawTag : TRawTag);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SelectNextScreen;
    property OnPointStart : TOpenGazeEvent read FOnPointStart write SetOnPointStart;
    property OnPointEnd : TOpenGazeEvent read FOnPointEnd write SetOnPointEnd;
    property Events : TOpenGazeEvents read FEvents write SetEvents;
    property Visible : Boolean read GetVisible write SetVisible;
  end;


var
  OpenGazeCalibrationChoreography : TOpenGazeCalibrationChoreography;

implementation

uses Forms, Controls, forms.background, opengaze.helpers, screen.helpers;

{ TOpenGazeCalibrationChoreography }

procedure TOpenGazeCalibrationChoreography.SetOnPointEnd(
  AValue: TOpenGazeEvent);
begin
  if FOnPointEnd = AValue then Exit;
  FOnPointEnd := AValue;
end;

procedure TOpenGazeCalibrationChoreography.SetEvents(AValue: TOpenGazeEvents);
begin
  if FEvents = AValue then Exit;
  FEvents := AValue;
  FEvents.OnDataReceived := @ReceiveGazePoint;
end;

function TOpenGazeCalibrationChoreography.GetVisible: Boolean;
begin
  Result := FormBackground.Visible;
end;

procedure TOpenGazeCalibrationChoreography.SetOnPointStart(
  AValue: TOpenGazeEvent);
begin
  if FOnPointStart = AValue then Exit;
  FOnPointStart := AValue;
end;

procedure TOpenGazeCalibrationChoreography.ReceiveGazePoint(
  Sender: TObject; RawTag: TRawTag);
const
  BPOGX = 14; { if you want a dictionary }
  BPOGY = 15; { use TOpenGazeEvent instead of TGazeDataEvent}
var
  LX : integer;
  LY : integer;
begin
  LX := RawTag.Pairs[BPOGX].Value.ToFloat.Denormalize(FormBackground.Width);
  LY := RawTag.Pairs[BPOGY].Value.ToFloat.Denormalize(FormBackground.Height);
  FAnimation.Gaze := Point(LX, LY);
  FAnimation.Paint;
end;

procedure TOpenGazeCalibrationChoreography.SetVisible(AValue: Boolean);
begin
  if FormBackground.Visible = AValue then Exit;
  FormBackground.Visible := AValue;
end;

constructor TOpenGazeCalibrationChoreography.Create;
begin
  FormBackground := TFormBackground.CreateNew(nil);
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

