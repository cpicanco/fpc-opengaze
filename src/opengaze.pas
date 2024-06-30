{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze;

{$mode ObjFPC}{$H+}

interface

uses
  opengaze.control, opengaze.analysis;

var
  OpenGazeControl  : TOpenGazeControlClient;
  OpenGazeAnalysis : TOpenGazeAnalysisClient;

implementation

initialization
  OpenGazeControl  := TOpenGazeControlClient.Create;
  OpenGazeAnalysis := TOpenGazeAnalysisClient.Create;

finalization
  OpenGazeControl.Free;
  OpenGazeAnalysis.Free;

end.

