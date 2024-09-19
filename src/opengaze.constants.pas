{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.constants;

{$mode ObjFPC}{$H+}

interface

uses opengaze.types;

const
  KILL = '';
  _TRUE_ = '1';
  _FALSE_ = '0';

const
  CLIENT_SET  = 'SET'; // Client tag for setting values
  CLIENT_GET  = 'GET'; // Client tag for getting values
  SERVER_ERR  = ''; // Server tag, unknown
  SERVER_ACK  = 'ACK'; // Server tag, indicates a successful SET/GET command.
  SERVER_NACK = 'NAC'; // Server tag, it indicates a failed SET/GET command.
  SERVER_CAL  = 'CAL'; // Server tag, responses are generated based on calibration routines
  SERVER_REC  = 'REC'; // Server tag, holds the data transmitted
  //SERVER_UPDATE = 'UPDATE'; // not implemented yet here

  ID = 'ID'; // Parameter type: string
  NONE = '';
  CALIBRATE_START = 'CALIBRATE_START'; // Value of ID parameter
  CALIBRATE_SHOW = 'CALIBRATE_SHOW'; // Value of ID parameter
  CALIBRATE_CLEAR = 'CALIBRATE_CLEAR'; // Value of ID parameter
  CALIBRATE_TIMEOUT = 'CALIBRATE_TIMEOUT'; // Value of ID parameter
  CALIBRATE_DELAY = 'CALIBRATE_DELAY'; // Value of ID parameter
  CALIBRATE_RESULT_SUMMARY = 'CALIBRATE_RESULT_SUMMARY'; // Value of ID parameter
  CALIBRATE_RESET = 'CALIBRATE_RESET'; // Value of ID parameter
  CALIBRATE_ADDPOINT = 'CALIBRATE_ADDPOINT'; // Value of ID parameter
  CALIB_START_PT = 'CALIB_START_PT'; // Value of ID parameter
  CALIB_RESULT_PT = 'CALIB_RESULT_PT'; // Value of ID parameter
  CALIB_ADD_PT = 'CALIB_ADD_PT'; // Value of ID parameter
  CALIB_RESULT = 'CALIB_RESULT'; // Value of ID parameter

  STATE = 'STATE'; // Parameter type: Boolean (0 or 1)
  VALUE = 'VALUE'; // Parameter type: Numeric
  CALX = 'CALX'; // Parameter type: Numeric
  CALY = 'CALY'; // Parameter type: Numeric
  PTS = 'PTS'; // Parameter type: Numeric
  FREQ = 'FREQ';
  XCOORDENATE = 'X';
  YCOORDENATE = 'Y';
  CAMERA_WIDTH = 'WIDTH';
  CAMERA_HEIGHT = 'HEIGHT';
  SCREEN_WIDTH = 'WIDTH';
  SCREEN_HEIGHT = 'HEIGHT';
  AVE_ERROR = 'AVE_ERROR';  // Parameter type: Numeric
  PT = 'PT';

  USER_DATA = 'USER_DATA'; // Value of ID parameter
  TRACKER_DISPLAY = 'TRACKER_DISPLAY'; // Value of ID parameter
  TIME_TICK_FREQUENCY = 'TIME_TICK_FREQUENCY'; // Value of ID parameter
  SCREEN_SIZE = 'SCREEN_SIZE'; // Value of ID parameter
  CAMERA_SIZE = 'CAMERA_SIZE'; // Value of ID parameter
  PRODUCT_ID = 'PRODUCT_ID'; // Value of ID parameter
  BUS = 'BUS';
  RATE = 'RATE';
  SERIAL_ID = 'SERIAL_ID'; // Value of ID parameter
  COMPANY_ID = 'COMPANY_ID'; // Value of ID parameter
  API_ID = 'API_ID'; // Value of ID parameter
  TRACKER_ID = 'TRACKER_ID'; // Value of ID parameter
  ACTIVE_ID = 'ACTIVE_ID';
  MAX_ID = 'MAX_ID';
  SEARCH = 'SEARCH';
  MARKER_PIX = 'MARKER_PIX'; // Value of ID parameter
  AAC_FILTER = 'AAC_FILTER';
  TTL_WRITE = 'TTL_WRITE';
  CHANNEL = 'CHANNEL';

  ENABLE_SEND_DATA = 'ENABLE_SEND_DATA'; // Value of ID parameter
  ENABLE_SEND_COUNTER = 'ENABLE_SEND_COUNTER'; // Value of ID parameter
  ENABLE_SEND_TIME = 'ENABLE_SEND_TIME'; // Value of ID parameter
  ENABLE_SEND_TIME_TICK = 'ENABLE_SEND_TIME_TICK'; // Value of ID parameter
  ENABLE_SEND_POG_FIX = 'ENABLE_SEND_POG_FIX'; // Value of ID parameter
  ENABLE_SEND_POG_LEFT = 'ENABLE_SEND_POG_LEFT'; // Value of ID parameter
  ENABLE_SEND_POG_RIGHT = 'ENABLE_SEND_POG_RIGHT'; // Value of ID parameter
  ENABLE_SEND_POG_BEST = 'ENABLE_SEND_POG_BEST'; // Value of ID parameter
  ENABLE_SEND_POG_AAC = 'ENABLE_SEND_POG_AAC'; // Value of ID parameter
  ENABLE_SEND_PUPIL_LEFT = 'ENABLE_SEND_PUPIL_LEFT'; // Value of ID parameter
  ENABLE_SEND_PUPIL_RIGHT = 'ENABLE_SEND_PUPIL_RIGHT'; // Value of ID parameter
  ENABLE_SEND_EYE_LEFT = 'ENABLE_SEND_EYE_LEFT'; // Value of ID parameter
  ENABLE_SEND_EYE_RIGHT = 'ENABLE_SEND_EYE_RIGHT'; // Value of ID parameter
  ENABLE_SEND_CURSOR = 'ENABLE_SEND_CURSOR'; // Value of ID parameter
  ENABLE_SEND_KB = 'ENABLE_SEND_KB'; // Value of ID parameter
  ENABLE_SEND_BLINK = 'ENABLE_SEND_BLINK'; // Value of ID parameter
  ENABLE_SEND_PUPILMM = 'ENABLE_SEND_PUPILMM'; // Value of ID parameter
  ENABLE_SEND_DIAL = 'ENABLE_SEND_DIAL'; // Value of ID parameter
  ENABLE_SEND_GSR = 'ENABLE_SEND_GSR'; // Value of ID parameter
  ENABLE_SEND_HR = 'ENABLE_SEND_HR'; // Value of ID parameter
  ENABLE_SEND_HR_PULSE = 'ENABLE_SEND_HR_PULSE'; // Value of ID parameter
  ENABLE_SEND_HR_IBI = 'ENABLE_SEND_HR_IBI'; // Value of ID parameter
  ENABLE_SEND_TTL = 'ENABLE_SEND_TTL'; // Value of ID parameter
  ENABLE_SEND_PIX = 'ENABLE_SEND_PIX'; // Value of ID parameter
  ENABLE_SEND_USER_DATA = 'ENABLE_SEND_USER_DATA'; // Value of ID parameter

  // OpenGaze Analysis
  RECORD_START = 'RECORD_START';

  // 5.1 Counter

  CNT = 'CNT'; // Parameter type: Numeric. The counter data variable is incremented by 1 for each data record sent by the server.

  // 5.2 Time

  TIME = 'TIME'; // The time elapsed in seconds since the last system initialization or calibration. The time stamp is recorded at the end of the transmission of the image from camera to computer. Useful for synchronization and to determine if the server computer is processing the images at the full frame rate. For a 60 Hz camera, the TIME value should increment by 1/60 seconds.

  // 5.3 Time Tick

  TIME_TICK = 'TIME_TICK'; // This is a signed 64-bit integer which indicates the number of CPU time ticks. This allows high precision synchronization of gaze data with other data on the same CPU.

  // 5.4 Fixation POG

  FPOGX = 'FPOGX'; // Parameter type: Numeric. The X-coordinate of the fixation POG, as a fraction of the screen size. (0,0) is top left, (0.5,0.5) is the screen center, and (1.0,1.0) is bottom right.
  FPOGY = 'FPOGY'; // Parameter type: Numeric. The Y-coordinate of the fixation POG, as a fraction of the screen size. (0,0) is top left, (0.5,0.5) is the screen center, and (1.0,1.0) is bottom right.
  FPOGS = 'FPOGS'; // Parameter type: Numeric. The starting time of the fixation POG in seconds since the system initialization or calibration.
  FPOGD = 'FPOGD'; // Parameter type: Numeric. The duration of the fixation POG in seconds.
  FPOGID = 'FPOGID'; // Parameter type: Numeric. The fixation POG ID number.
  FPOGV = 'FPOGV'; // Parameter type: Boolean (0 or 1). The valid flag with value of 1 (TRUE) if the fixation POG data is valid, and 0 (FALSE) if it is not. FPOGV valid is TRUE ONLY when either one, or both, of the eyes are detected AND a fixation is detected. FPOGV is FALSE all other times, for example when the subject blinks, when there is no face in the field of view, when the eyes move to the next fixation (i.e. a saccade).

  // 5.5 Left Eye POG

  LPOGX = 'LPOGX'; // Parameter type: Numeric. The X-coordinate of the left eye POG, as a fraction of the screen size.
  LPOGY = 'LPOGY'; // Parameter type: Numeric. The Y-coordinate of the left eye POG, as a fraction of the screen size.
  LPOGV = 'LPOGV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.6 Right Eye POG

  RPOGX = 'RPOGX'; // Parameter type: Numeric. The X-coordinate of the right eye POG, as a fraction of the screen size.
  RPOGY = 'RPOGY'; // Parameter type: Numeric. The Y-coordinate of the right eye POG, as a fraction of the screen size.
  RPOGV = 'RPOGV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.7 Best POG

  BPOGX = 'BPOGX'; // Parameter type: Numeric. The X-coordinate of the best eye POG, as a fraction of the screen size.
  BPOGY = 'BPOGY'; // Parameter type: Numeric. The Y-coordinate of the best eye POG, as a fraction of the screen size.
  BPOGV = 'BPOGV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.8 Assistive Communication POG

  APOGX = 'APOGX';
  APOGY = 'APOGY';
  APOGV = 'APOGV';

  // 5.9 Left Eye Pupil

  LPCX = 'LPCX'; // Parameter type: Numeric. The X-coordinate of the left eye pupil in the camera image, as a fraction of the camera image size.
  LPCY = 'LPCY'; // Parameter type: Numeric. The Y-coordinate of the left eye pupil in the camera image, as a fraction of the camera image size.
  LPD = 'LPD'; // Parameter type: Numeric. The diameter of the left eye pupil in pixels.
  LPS = 'LPS'; // Parameter type: Numeric. The scale factor of the left eye pupil (unitless). Value equals 1 at calibration depth, is less than 1 when user is closer to the eye tracker and greater than 1 when user is further away.
  LPV = 'LPV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.10 Right Eye Pupil

  RPCX = 'RPCX'; // Parameter type: Numeric. The X-coordinate of the right eye pupil in the camera image, as a fraction of the camera image size.
  RPCY = 'RPCY'; // Parameter type: Numeric. The Y-coordinate of the right eye pupil in the camera image, as a fraction of the camera image size.
  RPD = 'RPD'; // Parameter type: Numeric. The diameter of the right eye pupil in pixels.
  RPS = 'RPS'; // Parameter type: Numeric. The scale factor of the right eye pupil (unitless). Value equals 1 at calibration depth, is less than 1 when user is closer to the eye tracker and greater than 1 when user is further away.
  RPV = 'RPV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.11 Left Eye 3D Data

  LEYEX = 'LEYEX'; // Parameter type: Numeric. The X-coordinate of the left eye with 3D space with respect to the camera focal point, in units of meters.
  LEYEY = 'LEYEY'; // Parameter type: Numeric. The Y-coordinate of the left eye with 3D space with respect to the camera focal point, in units of meters.
  LEYEZ = 'LEYEZ'; // Parameter type: Numeric. The Z-coordinate of the left eye with 3D space with respect to the camera focal point, in units of meters.
  LPUPILD = 'LPUPILD'; // Parameter type: Numeric. The diameter of the left eye pupil in units of meters.
  LPUPILV = 'LPUPILV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.12 Right Eye 3D Data

  REYEX = 'REYEX'; // Parameter type: Numeric. The X-coordinate of the right eye with 3D space with respect to the camera focal point, in units of meters.
  REYEY = 'REYEY'; // Parameter type: Numeric. The Ycoordinate of the right eye with 3D space with respect to the camera focal point, in units of meters.
  REYEZ = 'REYEZ'; // Parameter type: Numeric. The Z-coordinate of the right eye with 3D space with respect to the camera focal point, in units of meters.
  RPUPILD = 'RPUPILD'; // Parameter type: Numeric. The diameter of the right eye pupil in units of meters.
  RPUPILV = 'RPUPILV'; // Parameter type: Boolean. The valid flag with value of 1 if the data is valid, and 0 if it is not.

  // 5.13 Cursor position

  CS = 'CS'; // Parameter type: Numeric. 0 for idle, 1 for left mouse button down, 2 for right button down, 3 for left button up, 4 for right button up.
  CX = 'CX'; // Parameter type: Numeric. The X-coordinate of the mouse cursor, as percentage of the screen size.
  CY = 'CY'; // Parameter type: Numeric. The Y-coordinate of the mouse cursor, as percentage of the screen size.

  // 5.14 Keyboard Input

  KB = 'KB';
  KBS = 'KBS';

  // 5.15 Blink Data

  BKID = 'BKID'; // Parameter type: Numeric. Each blink is assigned an ID value and incremented by one. The BKID value equals 0 for every record where no blink has been detected.
  BKDUR = 'BKDUR'; // Parameter type: Numeric. The duration of the preceding blink in seconds.
  BKPMIN = 'BKPMIN'; // Parameter type: Numeric. The number of blinks in the previous 60 second period of time.

  // 5.16 Pupil Diameter (millimeters)

  LPMM = 'LPMM';
  LPMMV = 'LPMMV';
  RPMM = 'RPMM';
  RPMMV = 'RPMMV';

  // 5.17 Dial

  DIAL = 'DIAL';
  DIALV = 'DIALV';

  // 5.18 Galvanic Skin Response (GSR)

  GSR = 'GSR';
  GSRV = 'GSRV';

  // 5.19 Heart Rate

  HR = 'HR';
  HRV = 'HRV';

  // 5.20 Heart Rate Pulse

  HRP = 'HRP';

  // 5.21 Heart Rate Interbeat Interval

  HRIBI = 'HRIBI';

  // 5.22 TTL Input/Output

  TTL0 = 'TTL0';
  TTL1 = 'TTL1';
  TTLV = 'TTLV';

  // 5.23 Pixel Conversion Factor

  PIXX = 'PIXX';
  PIXY = 'PIXY';
  PIXS = 'PIXS';
  PIXV = 'PIXV';

  // 5.24 User data

  USER = 'USER'; // Parameter type: string. Synchronization markers.



const
  HEADER_CNT :
    array [0..0] of string = (CNT);

  HEADER_TIME :
    array [0..0] of string = (TIME);

  HEADER_TIME_TICK :
    array [0..0] of string = (TIME_TICK);

  HEADER_FIXATION_POINT_OF_GAZE :
    array [0..5] of string = (FPOGX, FPOGY, FPOGS, FPOGD, FPOGID, FPOGV);

  HEADER_LEFT_POINT_OF_GAZE :
    array [0..2] of string = (LPOGX, LPOGY, LPOGV);

  HEADER_RIGHT_POINT_OF_GAZE :
    array [0..2] of string = (RPOGX, RPOGY, RPOGV);

  HEADER_BEST_POINT_OF_GAZE :
    array [0..2] of string = (BPOGX, BPOGY, BPOGV);

  HEADER_AAC_POINT_OF_GAZE :
    array [0..2] of string = (APOGX, APOGY, APOGV);

  HEADER_LEFT_PUPIL :
    array [0..4] of string = (LPCX,  LPCY,  LPD,  LPS, LPV);

  HEADER_RIGHT_PUPIL :
    array [0..4] of string = (RPCX,  RPCY,  RPD,  RPS, RPV);

  HEADER_LEFT_3D_EYE :
    array [0..4] of string = (LEYEX, LEYEY, LEYEZ, LPUPILD, LPUPILV);

  HEADER_RIGHT_3D_EYE :
    array [0..4] of string = (REYEX, REYEY, REYEZ, RPUPILD, RPUPILV);

  HEADER_CURSOR_POSITION :
    array [0..2] of string = (CX, CY, CS);

  HEADER_KB :
    array [0..1] of string = (KB, KBS);

  HEADER_BLINK_DATA :
    array [0..2] of string = (BKID, BKDUR, BKPMIN);

  HEADER_PUPILMM :
    array [0..3] of string = (LPMM, LPMMV, RPMM, RPMMV);

  HEADER_DIAL :
    array [0..1] of string = (DIAL, DIALV);

  HEADER_GSR :
    array [0..1] of string = (GSR, GSRV);

  HEADER_HR :
    array [0..1] of string = (HR, HRV);

  HEADER_HR_PULSE :
    array [0..0] of string = (HRP);

  HEADER_HR_IBI :
    array [0..0] of string = (HRIBI);

  HEADER_TTL :
    array [0..2] of string = (TTL0, TTL1, TTLV);

  HEADER_PIX :
    array [0..3] of string = (PIXX, PIXY, PIXS, PIXV);

  HEADER_USER_DATA :
    array [0..0] of string = (USER);


const
  VALUE_KILL : array [0..1] of string = (VALUE, KILL);

var
  OpenGazeTagsMap : TOpenGazeTagsMap;
  OpenGazeIDsMap  : TOpenGazeIDsMap;

implementation

var
  OpenGazeTag : TOpenGazeTag;
  OpenGazeTags : array [TOpenGazeTagsRange] of string = (
    SERVER_ERR,
    CLIENT_SET,
    CLIENT_GET,
    SERVER_ACK,
    SERVER_NACK,
    SERVER_CAL,
    SERVER_REC);

  OpenGazeID : TOpenGazeID;
  OpenGazeIDs : array [TOpenGazeIDsRange] of string = (
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
      TRACKER_DISPLAY,
      TIME_TICK_FREQUENCY,
      SCREEN_SIZE,
      CAMERA_SIZE,
      PRODUCT_ID,
      SERIAL_ID,
      COMPANY_ID,
      API_ID,
      TRACKER_ID,
      MARKER_PIX,
      AAC_FILTER,
      TTL_WRITE,
      RECORD_START,
      ENABLE_SEND_DATA,
      ENABLE_SEND_COUNTER,
      ENABLE_SEND_TIME,
      ENABLE_SEND_TIME_TICK,
      ENABLE_SEND_POG_FIX,
      ENABLE_SEND_POG_LEFT,
      ENABLE_SEND_POG_RIGHT,
      ENABLE_SEND_POG_BEST,
      ENABLE_SEND_POG_AAC,
      ENABLE_SEND_PUPIL_LEFT,
      ENABLE_SEND_PUPIL_RIGHT,
      ENABLE_SEND_EYE_LEFT,
      ENABLE_SEND_EYE_RIGHT,
      ENABLE_SEND_CURSOR,
      ENABLE_SEND_KB,
      ENABLE_SEND_BLINK,
      ENABLE_SEND_PUPILMM,
      ENABLE_SEND_DIAL,
      ENABLE_SEND_GSR,
      ENABLE_SEND_HR,
      ENABLE_SEND_HR_PULSE,
      ENABLE_SEND_HR_IBI,
      ENABLE_SEND_TTL,
      ENABLE_SEND_PIX,
      ENABLE_SEND_USER_DATA);

initialization
  OpenGazeTagsMap := TOpenGazeTagsMap.Create;
  for OpenGazeTag in TOpenGazeTagsRange do begin
    OpenGazeTagsMap.Add(OpenGazeTags[OpenGazeTag], OpenGazeTag);
  end;

  OpenGazeIDsMap := TOpenGazeIDsMap.Create;
  for OpenGazeID in TOpenGazeIDsRange do begin
    OpenGazeIDsMap.Add(OpenGazeIDs[OpenGazeID], OpenGazeID);
  end;

finalization
  OpenGazeTagsMap.Free;
  OpenGazeIDsMap.Free;

end.

