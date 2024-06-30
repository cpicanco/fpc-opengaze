{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.analysis;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opengaze.types,
  opengaze.recording,
  opengaze.client;

type

  { TOpenGazeAnalysisClient }

  TOpenGazeAnalysisClient = class(TOpenGazeBaseClient)
  private
    FRecording   : TOpenGazeRecording;
  public
    constructor Create;
    destructor Destroy; override;
    property Recording : TOpenGazeRecording read FRecording;
  end;


implementation

uses Dialogs, opengaze.commands, opengaze.logger;

constructor TOpenGazeAnalysisClient.Create;
begin
  inherited Create;
  FSocket.Server := OpenGazeAnalysisServer;
  FRecording := TOpenGazeRecording.Create(FSocket, FIncomingEvents);
end;

destructor TOpenGazeAnalysisClient.Destroy;
begin
  FRecording.Free;
  inherited Destroy;
end;

end.
