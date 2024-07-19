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
  opengaze.events,
  opengaze.logger.gaze;

type

  { TOpenGazeRecording }

  TOpenGazeRecording = class(TOpenGazeBase)
    private
      FLogger : TOpenGazeLogger;
      FDisabledCommands: TStringArray;
      FEnabledCommands: TStringArray;
      function GetOnDataReceived: TGazeDataEvent;
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
      procedure SetupDataFile(AFilename : string);
      property EnabledCommands : TStringArray read FEnabledCommands write SetEnabledCommands;
      property DisabledCommands : TStringArray read FDisabledCommands write SetDisabledCommands;
      property OnDataReceived : TGazeDataEvent read GetOnDataReceived write SetOnDataReceived;
  end;

implementation

uses opengaze.commands;

{ TOpenGazeRecording }

procedure TOpenGazeRecording.SetEnabledCommands(AValue: TStringArray);
begin
  if FEnabledCommands = AValue then Exit;
  FEnabledCommands := AValue;
end;

procedure TOpenGazeRecording.SetOnDataReceived(AValue: TGazeDataEvent);
begin
  if FLogger.OnDataReceived = AValue then Exit;
  FLogger.OnDataReceived := AValue;
end;

procedure TOpenGazeRecording.SetDisabledCommands(AValue: TStringArray);
begin
  if FDisabledCommands = AValue then Exit;
  FDisabledCommands := AValue;
end;

function TOpenGazeRecording.GetOnDataReceived: TGazeDataEvent;
begin
  Result := FLogger.OnDataReceived;
end;

constructor TOpenGazeRecording.Create(ASocket: TOpenGazeSocket; AEvents : TOpenGazeEvents);
begin
  inherited Create(ASocket, AEvents);
  FLogger := TOpenGazeLogger.Create(AEvents);
  with Recording do begin
    // Enable all
    // SetLength(FDisabledCommands, 0);
    //FEnabledCommands := TStringArray.Create(
    //  EnableSendCounter,
    //  EnableSendTime,
    //  EnableSendTimeTick,
    //  EnableSendPointOfGazeFixation,
    //  EnableSendPointOfGazeLeft,
    //  EnableSendPointOfGazeRight,
    //  EnableSendPointOfGazeBest,
    //  EnableSendAssistiveCommunicationPointOfGaze,
    //  EnableSendPupilLeft,
    //  EnableSendPupilRight,
    //  EnableSend3DEyeLeft,
    //  EnableSend3DEyeRight,
    //  EnableSendCursor,
    //  EnableSendKeyboardInput,
    //  EnableSendBlink,
    //  EnableSendDial,
    //  EnableSendGalvanicSkinResponse,
    //  EnableSendHeartRate,
    //  EnableSendHeartRatePulse,
    //  EnableSendHeartBeatInterbeatInterval,
    //  EnableSendTransistorTransistorLogicInputOutput,
    //  EnableSendPupilInMillimeters,
    //  EnableSendUserData);

    FDisabledCommands := TStringArray.Create(
      DisableSendTime,
      DisableSendAssistiveCommunicationPointOfGaze,
      DisableSendPupilLeft,
      DisableSendPupilRight,
      DisableSend3DEyeLeft,
      DisableSend3DEyeRight,
      DisableSendCursor,
      DisableSendKeyboardInput,
      DisableSendBlink,
      DisableSendDial,
      DisableSendGalvanicSkinResponse,
      DisableSendHeartRate,
      DisableSendHeartRatePulse,
      DisableSendHeartBeatInterbeatInterval,
      DisableSendTransistorTransistorLogicInputOutput,
      DisableSendPupilInMillimeters,
      DisableSendUserData);

    FEnabledCommands := TStringArray.Create(
      EnableSendCounter,
      EnableSendTimeTick,
      EnableSendPointOfGazeFixation,
      EnableSendPointOfGazeLeft,
      EnableSendPointOfGazeRight,
      EnableSendPointOfGazeBest);
  end;
end;

destructor TOpenGazeRecording.Destroy;
begin
  FLogger.Free;
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
  FLogger.BeginUpdateData;
  Send(True, Blocking);
end;

procedure TOpenGazeRecording.Stop(Blocking: Boolean);
begin
  Send(False, Blocking);
  FLogger.EndUpdateData;
end;

procedure TOpenGazeRecording.SetupDataFile(AFilename: string);
begin
  FLogger.Filename := AFilename + '.gaze';
end;

end.

