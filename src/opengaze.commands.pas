{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.commands;

{$mode ObjFPC}{$H+}

{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, math, opengaze.parser, opengaze.types;

type

  { TCalibrationCommands }

  TCalibrationCommands = record
    function Show : string;
    function Hide : string;
    function Start : string;
    function Stop : string;
    function Visible : string;
    function Started : string;
    function Clear : string;
    function ResultSummary : string;
    function SetTimeOut(AValue : Float) : string;
    function GetTimeOut : string;
    function SetDelay(AValue : Float) : string;
    function GetDelay : string;
    function SetReset : string;
    function GetReset : string;
    function AddPoint(X, Y : Float) : string;
    function SetScreenSize(Left, Top, Width, Height : integer) : string;
    function GetScreenSize : string;
    function GetPoints : string;
  end;

  { TRecordingCommands }

  TRecordingCommands = record
    function EnableSendData : string;
    function DisableSendData : string;
    function EnableSendCounter : string;
    function DisableSendCounter : string;
    function EnableSendTime : string;
    function DisableSendTime : string;
    function EnableSendTimeTick : string;
    function DisableSendTimeTick : string;
    function EnableSendPointOfGazeFixation : string;
    function DisableSendPointOfGazeFixation : string;
    function EnableSendPointOfGazeLeft : string;
    function DisableSendPointOfGazeLeft : string;
    function EnableSendPointOfGazeRight : string;
    function DisableSendPointOfGazeRight : string;
    function EnableSendPointOfGazeBest : string;
    function DisableSendPointOfGazeBest : string;
    function EnableSendAssistiveCommunicationPointOfGaze : string;
    function DisableSendAssistiveCommunicationPointOfGaze : string;
    function EnableSendPupilLeft : string;
    function DisableSendPupilLeft : string;
    function EnableSendPupilRight : string;
    function DisableSendPupilRight : string;
    function EnableSend3DEyeLeft : string;
    function DisableSend3DEyeLeft : string;
    function EnableSend3DEyeRight : string;
    function DisableSend3DEyeRight : string;
    function EnableSendCursor : string;
    function DisableSendCursor : string;
    function EnableSendKeyboardInput : string;
    function DisableSendKeyboardInput : string;
    function EnableSendBlink : string;
    function DisableSendBlink : string;
    function EnableSendPupilInMillimeters : string;
    function DisableSendPupilInMillimeters : string;
    function EnableSendDial : string;
    function DisableSendDial : string;
    function EnableSendGalvanicSkinResponse : string;
    function DisableSendGalvanicSkinResponse : string;
    function EnableSendHeartRate : string;
    function DisableSendHeartRate : string;
    function EnableSendHeartRatePulse : string;
    function DisableSendHeartRatePulse : string;
    function EnableSendHeartBeatInterbeatInterval : string;
    function DisableSendHeartBeatInterbeatInterval : string;
    function EnableSendTransistorTransistorLogicInputOutput : string;
    function DisableSendTransistorTransistorLogicInputOutput : string;
    function EnableSendPixelConversionFactor : string;
    function DisableSendPixelConversionFacotr : string;
    function EnableSendUserData : string;
    function DisableSendUserData : string;
  end;

  // read only

  { TInformationCommands }

  TInformationCommands = record
    function TimeTickFrequency : string;
    function CameraSize : string;
    function ProductID : string;
    function SerialID : string;
    function CompanyID : string;
    function APIID : string;
  end;

  { TRecordingCommandsOpenGazeAnalysis }

  TRecordingCommandsOpenGazeAnalysis = record
    function Start : string;
    function Stop : string;
  end;

  function KillClient : string;
  function GetDataHeader : string;

var
  Calibration : TCalibrationCommands;
  Recording : TRecordingCommands;
  Information : TInformationCommands;
  RecordingOpenGazeAnalysis : TRecordingCommandsOpenGazeAnalysis;

implementation

uses opengaze.constants, opengaze.helpers;

var
  EnabledSendDataCommands : TOpenGazeDataCommands;

function KillClient: string;
begin
  Result := ParseStr(CLIENT_SET, USER_DATA, VALUE_KILL);
end;

function GetDataHeader: string;
const
  TAB = #9;
var
  Command : TOpenGazeID;
  S : string;
begin
  Result := '';
  for Command in EnabledSendDataCommands do begin
    case Command of
      TOpenGazeID.ENABLE_SEND_COUNTER :
        s := String.Join(TAB, HEADER_CNT);

      TOpenGazeID.ENABLE_SEND_TIME :
        s := String.Join(TAB, HEADER_TIME);

      TOpenGazeID.ENABLE_SEND_TIME_TICK :
        s := String.Join(TAB, HEADER_TIME_TICK);

      TOpenGazeID.ENABLE_SEND_POG_FIX :
        s := String.Join(TAB, HEADER_FIXATION_POINT_OF_GAZE);

      TOpenGazeID.ENABLE_SEND_POG_LEFT :
        s := String.Join(TAB, HEADER_LEFT_POINT_OF_GAZE);

      TOpenGazeID.ENABLE_SEND_POG_RIGHT :
        s := String.Join(TAB, HEADER_RIGHT_POINT_OF_GAZE);

      TOpenGazeID.ENABLE_SEND_POG_BEST :
        s := String.Join(TAB, HEADER_BEST_POINT_OF_GAZE);

      TOpenGazeID.ENABLE_SEND_POG_AAC:
        s := String.Join(TAB, HEADER_AAC_POINT_OF_GAZE);

      TOpenGazeID.ENABLE_SEND_PUPIL_LEFT :
        s := String.Join(TAB, HEADER_LEFT_PUPIL);

      TOpenGazeID.ENABLE_SEND_PUPIL_RIGHT :
        s := String.Join(TAB, HEADER_RIGHT_PUPIL);

      TOpenGazeID.ENABLE_SEND_EYE_LEFT :
        s := String.Join(TAB, HEADER_LEFT_3D_EYE);

      TOpenGazeID.ENABLE_SEND_EYE_RIGHT :
        s := String.Join(TAB, HEADER_RIGHT_3D_EYE);

      TOpenGazeID.ENABLE_SEND_CURSOR :
        s := String.Join(TAB, HEADER_CURSOR_POSITION);

      TOpenGazeID.ENABLE_SEND_KB :
        s := String.Join(TAB, HEADER_KB);

      TOpenGazeID.ENABLE_SEND_BLINK :
        s := String.Join(TAB, HEADER_BLINK_DATA);

      TOpenGazeID.ENABLE_SEND_PUPILMM :
        s := String.Join(TAB, HEADER_PUPILMM);

      TOpenGazeID.ENABLE_SEND_DIAL :
        s := String.Join(TAB, HEADER_DIAL);

      TOpenGazeID.ENABLE_SEND_GSR :
        s := String.Join(TAB, HEADER_GSR);

      TOpenGazeID.ENABLE_SEND_HR :
        s := String.Join(TAB, HEADER_HR);

      TOpenGazeID.ENABLE_SEND_HR_PULSE :
        s := String.Join(TAB, HEADER_HR_PULSE);

      TOpenGazeID.ENABLE_SEND_HR_IBI :
        s := String.Join(TAB, HEADER_HR_IBI);

      TOpenGazeID.ENABLE_SEND_TTL :
        s := String.Join(TAB, HEADER_TTL);

      TOpenGazeID.ENABLE_SEND_PIX :
        s := String.Join(TAB, HEADER_PIX);

      TOpenGazeID.ENABLE_SEND_USER_DATA :
              s := String.Join(TAB, HEADER_USER_DATA);

      otherwise begin
        raise Exception.Create('Uknown command.');
      end;
    end;

    if Result.IsEmpty then begin
      Result := String.Join(TAB, [S]);
    end else begin
      Result := String.Join(TAB, [Result, S]);
    end;
  end;
end;

{ TCalibrationCommands }

function TCalibrationCommands.Show: string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_SHOW, True.ToStateArray);
end;

function TCalibrationCommands.Hide: string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_SHOW, False.ToStateArray);
end;

function TCalibrationCommands.Start: string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_START, True.ToStateArray);
end;

function TCalibrationCommands.Stop: string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_START, False.ToStateArray);
end;

function TCalibrationCommands.Visible: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_SHOW, []);
end;

function TCalibrationCommands.Started: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_START, []);
end;

function TCalibrationCommands.Clear: string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_CLEAR, []);
end;

function TCalibrationCommands.ResultSummary: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_RESULT_SUMMARY, []);
end;

function TCalibrationCommands.SetTimeOut(AValue: Float): string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_TIMEOUT, AValue.ToValueArray);
end;

function TCalibrationCommands.GetTimeOut: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_TIMEOUT, []);
end;

function TCalibrationCommands.SetDelay(AValue: Float): string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_DELAY, AValue.ToValueArray);
end;

function TCalibrationCommands.GetDelay: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_DELAY, []);
end;

function TCalibrationCommands.SetReset: string;
begin
  Result := ParseStr(CLIENT_SET, CALIBRATE_RESET, []);
end;

function TCalibrationCommands.GetReset: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_RESET, []);
end;

function TCalibrationCommands.AddPoint(X, Y: Float): string;
begin
  Result :=
    ParseStr(CLIENT_SET, CALIBRATE_ADDPOINT, Concat(X.ToXArray, Y.ToYArray));
end;

function TCalibrationCommands.SetScreenSize(Left, Top,
  Width, Height: integer): string;
begin
  Result :=
    ParseStr(CLIENT_SET, SCREEN_SIZE, Concat(
      Left.ToXArray, Top.ToYArray, Width.ToWidthArray, Height.ToHeightArray));
end;

function TCalibrationCommands.GetScreenSize: string;
begin
  Result := ParseStr(CLIENT_GET, SCREEN_SIZE, []);
end;

function TCalibrationCommands.GetPoints: string;
begin
  Result := ParseStr(CLIENT_GET, CALIBRATE_ADDPOINT, []);
end;

{ TRecordingCommands }

function TRecordingCommands.EnableSendData: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_DATA, True.ToStateArray);
end;

function TRecordingCommands.DisableSendData: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_DATA, False.ToStateArray);
end;

function TRecordingCommands.EnableSendCounter: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_COUNTER, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_COUNTER];
end;

function TRecordingCommands.DisableSendCounter: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_COUNTER, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_COUNTER];
end;

function TRecordingCommands.EnableSendTime: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_TIME, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_TIME];
end;

function TRecordingCommands.DisableSendTime: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_TIME, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_TIME];
end;

function TRecordingCommands.EnableSendTimeTick: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_TIME_TICK, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_TIME_TICK];
end;

function TRecordingCommands.DisableSendTimeTick: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_TIME_TICK, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_TIME_TICK];
end;

function TRecordingCommands.EnableSendPointOfGazeFixation: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_FIX, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_POG_FIX];
end;

function TRecordingCommands.DisableSendPointOfGazeFixation: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_FIX, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_POG_FIX];
end;

function TRecordingCommands.EnableSendPointOfGazeLeft: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_LEFT, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_POG_LEFT];
end;

function TRecordingCommands.DisableSendPointOfGazeLeft: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_LEFT, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_POG_LEFT];
end;

function TRecordingCommands.EnableSendPointOfGazeRight: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_RIGHT, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_POG_RIGHT];
end;

function TRecordingCommands.DisableSendPointOfGazeRight: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_RIGHT, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_POG_RIGHT];
end;

function TRecordingCommands.EnableSendPointOfGazeBest: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_BEST, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_POG_BEST];
end;

function TRecordingCommands.DisableSendPointOfGazeBest: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_BEST, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_POG_BEST];
end;

function TRecordingCommands.EnableSendAssistiveCommunicationPointOfGaze: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_AAC, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_POG_AAC];
end;

function TRecordingCommands.DisableSendAssistiveCommunicationPointOfGaze: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_POG_AAC, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_POG_AAC];
end;

function TRecordingCommands.EnableSendPupilLeft: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PUPIL_LEFT, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_PUPIL_LEFT];
end;

function TRecordingCommands.DisableSendPupilLeft: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PUPIL_LEFT, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_PUPIL_LEFT];
end;

function TRecordingCommands.EnableSendPupilRight: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PUPIL_RIGHT, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_PUPIL_RIGHT];
end;

function TRecordingCommands.DisableSendPupilRight: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PUPIL_RIGHT, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_PUPIL_RIGHT];
end;

function TRecordingCommands.EnableSend3DEyeLeft: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_EYE_LEFT, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_EYE_LEFT];
end;

function TRecordingCommands.DisableSend3DEyeLeft: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_EYE_LEFT, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_EYE_LEFT];
end;

function TRecordingCommands.EnableSend3DEyeRight: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_EYE_RIGHT, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_EYE_RIGHT];
end;

function TRecordingCommands.DisableSend3DEyeRight: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_EYE_RIGHT, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_EYE_RIGHT];
end;

function TRecordingCommands.EnableSendCursor: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_CURSOR, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_CURSOR];
end;

function TRecordingCommands.DisableSendCursor: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_CURSOR, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_CURSOR];
end;

function TRecordingCommands.EnableSendKeyboardInput: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_KB, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_KB];
end;

function TRecordingCommands.DisableSendKeyboardInput: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_KB, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_KB];
end;

function TRecordingCommands.EnableSendBlink: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_BLINK, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_BLINK];
end;

function TRecordingCommands.DisableSendBlink: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_BLINK, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_BLINK];
end;

function TRecordingCommands.EnableSendPupilInMillimeters: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PUPILMM, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_PUPILMM];
end;

function TRecordingCommands.DisableSendPupilInMillimeters: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PUPILMM, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_PUPILMM];
end;

function TRecordingCommands.EnableSendDial: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_DIAL, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_DIAL];
end;

function TRecordingCommands.DisableSendDial: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_DIAL, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_DIAL];
end;

function TRecordingCommands.EnableSendGalvanicSkinResponse: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_GSR, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_GSR];
end;

function TRecordingCommands.DisableSendGalvanicSkinResponse: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_GSR, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_GSR];
end;

function TRecordingCommands.EnableSendHeartRate: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_HR, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_HR];
end;

function TRecordingCommands.DisableSendHeartRate: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_HR, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_HR];
end;

function TRecordingCommands.EnableSendHeartRatePulse: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_HR_PULSE, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_HR_PULSE];
end;

function TRecordingCommands.DisableSendHeartRatePulse: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_HR_PULSE, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_HR_PULSE];
end;

function TRecordingCommands.EnableSendHeartBeatInterbeatInterval: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_HR_IBI, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_HR_IBI];
end;

function TRecordingCommands.DisableSendHeartBeatInterbeatInterval: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_HR_IBI, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_HR_IBI];
end;

function TRecordingCommands.EnableSendTransistorTransistorLogicInputOutput: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_TTL, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_TTL];
end;

function TRecordingCommands.DisableSendTransistorTransistorLogicInputOutput: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_TTL, False
  .ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_TTL];
end;

function TRecordingCommands.EnableSendPixelConversionFactor: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PIX, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_PIX];
end;

function TRecordingCommands.DisableSendPixelConversionFacotr: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_PIX, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_PIX];
end;

function TRecordingCommands.EnableSendUserData: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_USER_DATA, True.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands+[TOpenGazeID.ENABLE_SEND_USER_DATA];
end;

function TRecordingCommands.DisableSendUserData: string;
begin
  Result := ParseStr(CLIENT_SET, ENABLE_SEND_USER_DATA, False.ToStateArray);
  EnabledSendDataCommands := EnabledSendDataCommands-[TOpenGazeID.ENABLE_SEND_USER_DATA];
end;

{ TInformationCommands }

function TInformationCommands.TimeTickFrequency: string;
begin
  Result := ParseStr(CLIENT_GET, TIME_TICK_FREQUENCY, []);
end;

function TInformationCommands.CameraSize: string;
begin
  Result := ParseStr(CLIENT_GET, CAMERA_SIZE, []);
end;

function TInformationCommands.ProductID: string;
begin
  Result := ParseStr(CLIENT_GET, PRODUCT_ID, []);
end;

function TInformationCommands.SerialID: string;
begin
  Result := ParseStr(CLIENT_GET, SERIAL_ID, []);
end;

function TInformationCommands.CompanyID: string;
begin
  Result := ParseStr(CLIENT_GET, COMPANY_ID, []);
end;

function TInformationCommands.APIID: string;
begin
  Result := ParseStr(CLIENT_GET, API_ID, []);
end;

{ TRecordingCommandsOpenGazeAnalysis }

function TRecordingCommandsOpenGazeAnalysis.Start: string;
begin
  Result := ParseStr(CLIENT_SET, RECORD_START, True.ToStateArray);
end;

function TRecordingCommandsOpenGazeAnalysis.Stop: string;
begin
  Result := ParseStr(CLIENT_SET, RECORD_START, False.ToStateArray);
end;

initialization
  EnabledSendDataCommands := [];

end.
