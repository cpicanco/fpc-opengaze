{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.recording;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opengaze.base,
  opengaze.constants,
  opengaze.types,
  opengaze.socket,
  opengaze.events;

type

  { TOpenGazeRecording }

  TOpenGazeRecording = class(TOpenGazeBase)
    private
      FDisabledCommands: TStringArray;
      FEnabledCommands: TStringArray;
      FOnDataReceived: TGazeDataEvent;
      procedure SetDisabledCommands(AValue: TStringArray);
      procedure SetEnabledCommands(AValue: TStringArray);
      procedure SetOnDataReceived(AValue: TGazeDataEvent);
      function GetStartCommand : string;
      function GetStopCommand : string;
      procedure Send(DoEnable, Blocking: Boolean);
      procedure ParseSendCommands;
    public
      constructor Create(ASocket : TOpenGazeSocket; AEvents : TOpenGazeEvents);
      destructor Destroy; override;
      procedure Start(Blocking : Boolean = True);
      procedure Stop(Blocking : Boolean = True);
      property EnabledCommands : TStringArray read FEnabledCommands write SetEnabledCommands;
      property DisabledCommands : TStringArray read FDisabledCommands write SetDisabledCommands;
  end;

implementation

uses opengaze.commands, opengaze.logger;

{ TOpenGazeRecording }

procedure TOpenGazeRecording.SetOnDataReceived(AValue: TGazeDataEvent);
begin
  if FOnDataReceived = AValue then Exit;
  FOnDataReceived := AValue;
end;

procedure TOpenGazeRecording.SetEnabledCommands(AValue: TStringArray);
begin
  if FEnabledCommands = AValue then Exit;
  FEnabledCommands := AValue;
end;

procedure TOpenGazeRecording.SetDisabledCommands(AValue: TStringArray);
begin
  if FDisabledCommands = AValue then Exit;
  FDisabledCommands := AValue;
end;

constructor TOpenGazeRecording.Create(ASocket: TOpenGazeSocket; AEvents : TOpenGazeEvents);
begin
  inherited Create(ASocket, AEvents);
  with Recording do begin
    //FDisabledCommands := TStringArray.Create(
    //  DisableSendTime,
    //  DisableSendPupilLeft,
    //  DisableSendPupilRight,
    //  DisableSend3DEyeLeft,
    //  DisableSend3DEyeRight,
    //  DisableSendCursor,
    //  DisableSendBlink,
    //  DisableSendUserData);

    SetLength(FDisabledCommands, 0);

    //FEnabledCommands := TStringArray.Create(
    //  EnableSendCounter,
    //  EnableSendTimeTick,
    //  EnableSendPointOfGazeFixation,
    //  EnableSendPointOfGazeLeft,
    //  EnableSendPointOfGazeRight,
    //  EnableSendPointOfGazeBest);

    FEnabledCommands := TStringArray.Create(
      EnableSendCounter,
      EnableSendTime,
      EnableSendTimeTick,
      EnableSendPointOfGazeFixation,
      EnableSendPointOfGazeLeft,
      EnableSendPointOfGazeRight,
      EnableSendPointOfGazeBest,
      EnableSendAssistiveCommunicationPointOfGaze,
      EnableSendPupilLeft,
      EnableSendPupilRight,
      EnableSend3DEyeLeft,
      EnableSend3DEyeRight,
      EnableSendCursor,
      EnableSendKeyboardInput,
      EnableSendBlink,
      EnableSendDial,
      EnableSendGalvanicSkinResponse,
      EnableSendHeartRate,
      EnableSendHeartRatePulse,
      EnableSendHeartBeatInterbeatInterval,
      EnableSendTransistorTransistorLogicInputOutput,
      EnableSendPupilInMillimeters,
      EnableSendUserData
      );
  end;
end;

destructor TOpenGazeRecording.Destroy;
begin
  inherited Destroy;
end;

function TOpenGazeRecording.GetStartCommand: string;
begin
  case Server of
    OpenGazeControlServer  : Result := Recording.EnableSendData;
    OpenGazeAnalysisServer : Result := RecordingOpenGazeAnalysis.Start;
  end;
end;

function TOpenGazeRecording.GetStopCommand: string;
begin
  case Server of
    OpenGazeControlServer  : Result := Recording.DisableSendData;
    OpenGazeAnalysisServer : Result := RecordingOpenGazeAnalysis.Stop;
  end;
end;

procedure TOpenGazeRecording.Send(DoEnable, Blocking: Boolean);
var
  Command : string;
begin
  if DoEnable then begin
    Command := GetStartCommand;
  end else begin
    Command := GetStopCommand;
  end;

  SendCommand(Command, Blocking);
end;

procedure TOpenGazeRecording.ParseSendCommands;
var
  Command : string;
begin
  for Command in Concat(EnabledCommands, DisabledCommands) do begin
    SendCommand(Command);
  end;
end;

procedure TOpenGazeRecording.Start(Blocking: Boolean);
begin
  ParseSendCommands;
  DataHeader := GetDataHeader;
  BeginUpdateData;
  Send(True, Blocking);
end;

procedure TOpenGazeRecording.Stop(Blocking: Boolean);
begin
  Send(False, Blocking);
  EndUpdateData;
end;

end.

