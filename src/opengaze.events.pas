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
  opengaze.worker;

type

  { TOpenGazeEvents }

  TOpenGazeEvents = class
    private
      FWorkerThread : TWorkerThread;
      FOnCalibrationPointResult: TOpenGazeEvent;
      FOnCalibrationPointStart: TOpenGazeEvent;
      FOnCalibrationResult: TOpenGazeEvent;
      FOnCalibrationResultSummary: TOpenGazeEvent;
      FOnDisableSendData: TOpenGazeEvent;
      FOnEnableSendData: TOpenGazeEvent;
      FOnDataReceived: TGazeDataEvent;
      FOnStartCalibration: TOpenGazeEvent;
      FOnStartRecording: TOpenGazeEvent;
      FOnStopCalibration: TOpenGazeEvent;
      FOnStopRecording: TOpenGazeEvent;
      procedure SetWorkerThread(AValue: TWorkerThread);
      procedure SetOnCalibrationPointResult(AValue: TOpenGazeEvent);
      procedure SetOnCalibrationPointStart(AValue: TOpenGazeEvent);
      procedure SetOnCalibrationResult(AValue: TOpenGazeEvent);
      procedure SetOnCalibrationResultSummary(AValue: TOpenGazeEvent);
      procedure SetOnDisableSendData(AValue: TOpenGazeEvent);
      procedure SetOnEnableSendData(AValue: TOpenGazeEvent);
      procedure SetOnDataReceived(AValue: TGazeDataEvent);
      procedure SetOnStartCalibration(AValue: TOpenGazeEvent);
      procedure SetOnStartRecording(AValue: TOpenGazeEvent);
      procedure SetOnStopCalibration(AValue: TOpenGazeEvent);
      procedure SetOnStopRecording(AValue: TOpenGazeEvent);
      procedure EmptyDataEvent(Sender : TObject; RawTag : TRawTag);
      procedure EmptyGazeEvent(Sender : TObject; Event : TPairsDictionary);
    public
      constructor Create;
      procedure ProcessTag(Sender: TObject; RawTag: TRawTag);
      destructor Destroy; override;
      property WorkerThread : TWorkerThread read FWorkerThread write SetWorkerThread;
      property OnStartCalibration : TOpenGazeEvent read FOnStartCalibration write SetOnStartCalibration;
      property OnStopCalibration : TOpenGazeEvent read FOnStopCalibration write SetOnStopCalibration;
      property OnCalibrationResult : TOpenGazeEvent read FOnCalibrationResult write SetOnCalibrationResult;
      property OnCalibrationResultSummary : TOpenGazeEvent read FOnCalibrationResultSummary write SetOnCalibrationResultSummary;
      property OnStartRecording : TOpenGazeEvent read FOnStartRecording write SetOnStartRecording;
      property OnStopRecording : TOpenGazeEvent read FOnStopRecording write SetOnStopRecording;
      property OnEnableSendData : TOpenGazeEvent read FOnEnableSendData write SetOnEnableSendData;
      property OnCalibrationPointStart : TOpenGazeEvent read FOnCalibrationPointStart write SetOnCalibrationPointStart;
      property OnCalibrationPointResult : TOpenGazeEvent read FOnCalibrationPointResult write SetOnCalibrationPointResult;
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
          CALIBRATE_START : begin
            if Dictionary[STATE] = _TRUE_ then begin
              FOnStartCalibration(Self, Dictionary);
            end else begin
              FOnStopCalibration(Self, Dictionary);
            end;
          end;

          CALIBRATE_RESULT_SUMMARY : begin
            FOnCalibrationResultSummary(Self, Dictionary);
          end;

          RECORD_START, ENABLE_SEND_DATA : begin
            if Dictionary[STATE] = _TRUE_ then begin
              FOnStartRecording(Self, Dictionary);
            end else begin
              FOnStopRecording(Self, Dictionary);
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
          CALIB_START_PT: begin
            FOnCalibrationPointStart(Self, Dictionary);
          end;

          CALIB_RESULT_PT: begin
            FOnCalibrationPointResult(Self, Dictionary);
          end;

          CALIB_RESULT : begin
            FOnCalibrationResult(Self, Dictionary);
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
      FOnDataReceived(Self, RawTag);
    end;

    NACK : begin
      { implement failed events here }
    end;

    otherwise begin

    end;
  end;
end;

procedure TOpenGazeEvents.SetWorkerThread(AValue: TWorkerThread);
begin
  if FWorkerThread = AValue then Exit;
  FWorkerThread := AValue;
  FWorkerThread.OnReceive := @ProcessTag
end;

procedure TOpenGazeEvents.SetOnCalibrationPointResult(AValue: TOpenGazeEvent);
begin
  if FOnCalibrationPointResult = AValue then Exit;
  FOnCalibrationPointResult := AValue;
end;

procedure TOpenGazeEvents.SetOnCalibrationPointStart(AValue: TOpenGazeEvent);
begin
  if FOnCalibrationPointStart = AValue then Exit;
  FOnCalibrationPointStart := AValue;
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

procedure TOpenGazeEvents.SetOnStopCalibration(AValue: TOpenGazeEvent);
begin
  if FOnStopCalibration = AValue then Exit;
  FOnStopCalibration := AValue;
end;

procedure TOpenGazeEvents.SetOnStopRecording(AValue: TOpenGazeEvent);
begin
  if FOnStopRecording = AValue then Exit;
  FOnStopRecording := AValue;
end;

procedure TOpenGazeEvents.EmptyDataEvent(Sender: TObject; RawTag: TRawTag);
begin
  { do nothing }
end;

procedure TOpenGazeEvents.EmptyGazeEvent(Sender: TObject;
  Event: TPairsDictionary);
begin
  { do nothing }
end;

constructor TOpenGazeEvents.Create;
begin
  inherited Create;
  FOnDataReceived:= @EmptyDataEvent;
  FOnCalibrationPointResult:= @EmptyGazeEvent;
  FOnCalibrationPointStart:= @EmptyGazeEvent;
  FOnCalibrationResult:= @EmptyGazeEvent;
  FOnCalibrationResultSummary:= @EmptyGazeEvent;
  FOnDisableSendData:= @EmptyGazeEvent;
  FOnEnableSendData:= @EmptyGazeEvent;
  FOnStartCalibration:= @EmptyGazeEvent;
  FOnStartRecording:= @EmptyGazeEvent;
  FOnStopCalibration:= @EmptyGazeEvent;
  FOnStopRecording:= @EmptyGazeEvent;
end;

destructor TOpenGazeEvents.Destroy;
begin
  inherited Destroy;
end;

end.

