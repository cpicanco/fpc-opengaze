fpc-opengaze
Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

fpc-opengaze is a Free Pascal client for OpenGaze API v2.0 (by [Gazepoint](https://gazept.com/)), which is as an open-source alternative to proprietary vendor formats for communicating with eye-tracking devices.

## Development enviroment
Lazarus 3.99 (rev main_3_99-1082-g0bd3e44eb4) FPC 3.3.1 x86_64-win64-win32/win64

## Dependencies
1. Synaser package;
2. LCL package;
3. TApplication queues

## Start up instructions
1. Open Lazarus and create a new Project/Application
2. Add the Synaser package using the Project Inpector and Online Package Manager
3. Make sure all units are visible in your project's settings path

## Main features
- blocking and non-blocking alternatives for sending messages
- event based approach for receiving messages
- multi-thread (safe)

## Exemples
See `examples/client/` for a working compilable example.

## Code snippets
```pascal
// main imports
uses opengaze, opengaze.types, opengaze.constants;

// connect to the server and wait for messages
OpenGazeControl.Connect;

// disconnect from the server
OpenGazeControl.Disconnect;

// start calibration
OpenGazeControl.Calibration.Show;
OpenGazeControl.Calibration.Start;

// start recording, send blocking message
OpenGazeControl.Recording.Start;

// start recording, send non-blocking message
OpenGazeControl.Recording.Start(False);

// events setup
procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGazeControl.Events.OnCalibrationResult := @StartRecording;
end;

// event implementation
procedure TForm1.StartRecording(Sender: TObject; Event: TPairsDictionary);
var
  AverageError : string;
begin
  AverageError := OpenGazeControl.Calibration.ResultSummary;
  ShowMessage(String.Join(#32, [AverageError, Event[ID], Event[STATE]]));
end;
```

## Structure of source units, src/
A base class for clients
\src\opengaze.client.pas

A class for Control clients
\src\opengaze.control.pas

A class for Analysis clients (not working yet)
\src\opengaze.analysis.pas

A wrapper for control and analysis instances
\src\opengaze.pas

A base class for Open Gaze commands
\src\opengaze.base.pas

A class to encapsulate Open Gaze API recording commands
\src\opengaze.recording.pas

A class to encapsulate Open Gaze API calibration commands
\src\opengaze.calibration.pas

A class to encapsulate fpc-opengaze events
\src\opengaze.events.pas

A class to encapsulate the incoming thread logic
\src\opengaze.incoming.pas

Convenience functions for parsing Open Gaze API commands.
\src\opengaze.commands.pas

Convenience constants for Open Gaze API tags, ids, parameters, ...
\src\opengaze.constants.pas

Convenience type convertion helpers
\src\opengaze.helpers.pas

A procedural logger
\src\opengaze.logger.pas

A procedural parser
\src\opengaze.parser.pas

A wrapper around Synaser TCP/IP block socket:
\src\opengaze.socket.pas

fpc-opengaze types
\src\opengaze.types.pas