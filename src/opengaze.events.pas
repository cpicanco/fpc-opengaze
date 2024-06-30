{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.events;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opengaze.constants,
  opengaze.types,
  opengaze.incoming;

type

  { TOpenGazeEvents }

  TOpenGazeEvents = class
    private
      FIncomingThread : TIncomingThread;
      FOnCalibrationResult: TOpenGazeEvent;
      FOnCalibrationResultSummary: TOpenGazeEvent;
      FOnDisableSendData: TOpenGazeEvent;
      FOnEnableSendData: TOpenGazeEvent;
      FOnDataReceived: TGazeDataEvent;
      FOnStartCalibration: TOpenGazeEvent;
      FOnStartRecording: TOpenGazeEvent;
      FOnStopRecording: TOpenGazeEvent;
      procedure SetIncomingThread(AValue: TIncomingThread);
      procedure SetOnCalibrationResult(AValue: TOpenGazeEvent);
      procedure SetOnCalibrationResultSummary(AValue: TOpenGazeEvent);
      procedure SetOnDisableSendData(AValue: TOpenGazeEvent);
      procedure SetOnEnableSendData(AValue: TOpenGazeEvent);
      procedure SetOnDataReceived(AValue: TGazeDataEvent);
      procedure SetOnStartCalibration(AValue: TOpenGazeEvent);
      procedure SetOnStartRecording(AValue: TOpenGazeEvent);
      procedure SetOnStopRecording(AValue: TOpenGazeEvent);
    public
      constructor Create;
      procedure ProcessTag(Sender: TObject; RawTag: TRawTag);
      destructor Destroy; override;
      property IncomingThread : TIncomingThread read FIncomingThread write SetIncomingThread;
      property OnStartCalibration : TOpenGazeEvent read FOnStartCalibration write SetOnStartCalibration;
      property OnCalibrationResult : TOpenGazeEvent read FOnCalibrationResult write SetOnCalibrationResult;
      property OnCalibrationResultSummary : TOpenGazeEvent read FOnCalibrationResultSummary write SetOnCalibrationResultSummary;
      property OnStartRecording : TOpenGazeEvent read FOnStartRecording write SetOnStartRecording;
      property OnStopRecording : TOpenGazeEvent read FOnStopRecording write SetOnStopRecording;
      property OnEnableSendData : TOpenGazeEvent read FOnEnableSendData write SetOnEnableSendData;
      property OnDisableSendData : TOpenGazeEvent read FOnDisableSendData write SetOnDisableSendData;
      property OnDataReceived : TGazeDataEvent read FOnDataReceived write SetOnDataReceived;
  end;

implementation

uses OpenGaze.commands, OpenGaze.parser, OpenGaze.logger;

{ TOpenGazeEvents }

procedure TOpenGazeEvents.ProcessTag(Sender: TObject; RawTag: TRawTag);
var
  Dictionary : TPairsDictionary;
begin
  case RawTag.Tag of
    ERR : Exit;

    ACK : begin
      Dictionary := TagPairsToDict(RawTag.Pairs);
      try
        case RawTag.ID of
          CALIBRATE_RESULT_SUMMARY : begin
            if Assigned(OnCalibrationResultSummary) then begin
              OnCalibrationResultSummary(Self, Dictionary);
            end;
          end;

          RECORD_START, ENABLE_SEND_DATA : begin
            if Dictionary[STATE] = _TRUE_ then begin
              if Assigned(OnStartRecording) then begin
                OnStartRecording(Self, Dictionary);
              end;
            end else begin
              if Assigned(OnStopRecording) then begin
                OnStopRecording(Self, Dictionary);
              end;
            end;
          end;

           { implement more ACK events here }

          otherwise begin
            { do nothing }
          end;
        end;
      finally
        Dictionary.Free;
      end;
    end;

    CAL : begin
      Dictionary := TagPairsToDict(RawTag.Pairs);
      try
        case RawTag.ID of
          CALIB_RESULT : begin
            if Assigned(OnCalibrationResult) then begin
              OnCalibrationResult(Self, Dictionary);
            end;
          end;

          { implement more calibration events here }

          otherwise begin
            { do nothing }
          end;
        end;
      finally
        Dictionary.Free;
      end;
    end;

    REC : begin
      { implement data logger events here }
      LogLine(RawTag.Pairs);
    end;

    NACK : begin
      { implement failed events here }
    end;

    otherwise begin

    end;
  end;
end;

procedure TOpenGazeEvents.SetIncomingThread(AValue: TIncomingThread);
begin
  if FIncomingThread = AValue then Exit;
  FIncomingThread := AValue;
  FIncomingThread.OnReceive := @ProcessTag
end;

procedure TOpenGazeEvents.SetOnCalibrationResult(AValue: TOpenGazeEvent);
begin
  if FOnCalibrationResult = AValue then Exit;
  FOnCalibrationResult := AValue;
end;

procedure TOpenGazeEvents.SetOnCalibrationResultSummary(
  AValue: TOpenGazeEvent);
begin
  if FOnCalibrationResultSummary = AValue then Exit;
  FOnCalibrationResultSummary := AValue;
end;

procedure TOpenGazeEvents.SetOnDisableSendData(AValue: TOpenGazeEvent);
begin
  if FOnDisableSendData = AValue then Exit;
  FOnDisableSendData := AValue;
end;

procedure TOpenGazeEvents.SetOnEnableSendData(AValue: TOpenGazeEvent);
begin
  if FOnEnableSendData = AValue then Exit;
  FOnEnableSendData := AValue;
end;

procedure TOpenGazeEvents.SetOnDataReceived(AValue: TGazeDataEvent);
begin
  if FOnDataReceived = AValue then Exit;
  FOnDataReceived := AValue;
end;

procedure TOpenGazeEvents.SetOnStartCalibration(AValue: TOpenGazeEvent);
begin
  if FOnStartCalibration = AValue then Exit;
  FOnStartCalibration := AValue;
end;

procedure TOpenGazeEvents.SetOnStartRecording(AValue: TOpenGazeEvent);
begin
  if FOnStartRecording = AValue then Exit;
  FOnStartRecording := AValue;
end;

procedure TOpenGazeEvents.SetOnStopRecording(AValue: TOpenGazeEvent);
begin
  if FOnStopRecording = AValue then Exit;
  FOnStopRecording := AValue;
end;

constructor TOpenGazeEvents.Create;
begin
  inherited Create;
  FOnStartRecording := nil;
  FOnStopRecording := nil;
  FOnDataReceived := nil;
  FOnStartCalibration := nil;
  FOnCalibrationResult := nil;
  FOnCalibrationResultSummary := nil;
end;

destructor TOpenGazeEvents.Destroy;
begin
  inherited Destroy;
end;

end.

