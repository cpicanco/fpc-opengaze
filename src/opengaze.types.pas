{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections;

type
  TCalibrationPoint = record
    X : Float;
    Y : Float;
  end;

  TCalibrationPoints = array of TCalibrationPoint;

  TOpenGazeServer = (OpenGazeControlServer, OpenGazeAnalysisServer);

  TPairsDictionary = specialize TDictionary<string, string>;

  TOpenGazeTag = (
    ERR,
    SET_,
    GET_,
    ACK,
    NACK,
    CAL,
    REC);
  TOpenGazeTagsRange = ERR..REC;
  TOpenGazeTagsMap = specialize TDictionary<string, TOpenGazeTag>;

  TOpenGazeID = (
    NONE,
    CALIBRATE_START,
    CALIBRATE_SHOW,
    CALIBRATE_CLEAR,
    CALIBRATE_TIMEOUT,
    CALIBRATE_DELAY,
    CALIBRATE_RESULT_SUMMARY,
    CALIBRATE_RESET,
    CALIBRATE_ADDPOINT,
    CALIB_START_PT,
    CALIB_RESULT_PT,
    CALIB_ADD_PT,
    CALIB_RESULT,
    USER_DATA,
    RECORD_START,
    ENABLE_SEND_DATA,
    ENABLE_SEND_COUNTER,
    ENABLE_SEND_TIME,
    ENABLE_SEND_TIME_TICK,
    ENABLE_SEND_POG_FIX,
    ENABLE_SEND_POG_LEFT,
    ENABLE_SEND_POG_RIGHT,
    ENABLE_SEND_POG_BEST,
    ENABLE_SEND_PUPIL_LEFT,
    ENABLE_SEND_PUPIL_RIGHT,
    ENABLE_SEND_EYE_LEFT,
    ENABLE_SEND_EYE_RIGHT,
    ENABLE_SEND_CURSOR,
    ENABLE_SEND_BLINK,
    ENABLE_SEND_USER_DATA);

  TOpenGazeDataCommandsRange = ENABLE_SEND_COUNTER..ENABLE_SEND_USER_DATA;
  TOpenGazeDataCommands = set of TOpenGazeDataCommandsRange;

  TOpenGazeIDsRange = NONE..ENABLE_SEND_USER_DATA;
  TOpenGazeIDsMap = specialize TDictionary<string, TOpenGazeID>;

  TTagPair = record
    Key   : string;
    Value : string;
  end;

  TTagPairs = array of TTagPair;

  TRawTag = record
    Tag   : TOpenGazeTag;
    ID    : TOpenGazeID;
    Pairs : TTagPairs;
  end;

  TGazeDataEvent = procedure(Sender: TObject; RawTag: TRawTag) of object;
  TOpenGazeEvent = procedure(Sender: TObject; Event: TPairsDictionary) of object;

implementation

end.

