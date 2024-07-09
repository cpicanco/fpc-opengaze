{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit choreographies;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Math, animation, screen.grids;

type
  TCoreography = array of integer;

  TNormalizedPoint = record
    X : Float;
    Y : Float;
  end;

  TNormalizedPoints = array of TNormalizedPoint;

  { TCalibrationChoreography }

  TCalibrationChoreography = class
  protected
    FBackground : TCustomControl;
    FAnimation  : TAnimation;
    procedure Paint(Sender: TObject); virtual;
  public
    constructor Create(Control : TCustomControl);
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure Start;
    procedure Stop;
    procedure SetPoints(NormalizedPoints : TNormalizedPoints);
    function GetPoints(AN: integer = 3;
      Coreography: TCoreography = nil): TNormalizedPoints;
    property Animation : TAnimation read FAnimation;
  end;

implementation

uses BGRABitmap, screen.helpers;

var
  DefaultCoreography : TCoreography;


{ TCalibrationChoreography }

procedure TCalibrationChoreography.Paint(Sender: TObject);
var
  Control : TCustomControl;
begin
  Control := Sender as TCustomControl;
  FAnimation.Bitmap.Draw(Control.Canvas, 0, 0, True);
end;

constructor TCalibrationChoreography.Create(Control: TCustomControl);
begin
  inherited Create;
  FBackground := Control;
  FBackground.OnPaint := @Paint;
  FAnimation := TAnimation.Create;
  FAnimation.Invalidate := @Control.Invalidate;
end;

destructor TCalibrationChoreography.Destroy;
begin
  FAnimation.Free;
  inherited Destroy;
end;

procedure TCalibrationChoreography.Show;
begin
  FBackground.Show;
end;

procedure TCalibrationChoreography.Hide;
begin
  FBackground.Hide;
end;

function TCalibrationChoreography.GetPoints(AN: integer;
  Coreography: TCoreography): TNormalizedPoints;
var
  Grid : TGrid;
  Distributed : Boolean = True;
  Cells : TCells;
  Cell : TCell;
  NormalizedPoints : TNormalizedPoints = nil;
  Width, Height : integer;
  i, j : integer;
begin
  if Coreography = nil then
    Coreography := DefaultCoreography;

  Width := FBackground.Width;
  Height := FBackground.Height;
  FAnimation.SetSize(Width, Height);
  FAnimation.ClearPoints;

  SetLength(NormalizedPoints, 0);

  Grid := GetCentralGrid(AN, Width, Height, FAnimation.OutSize, Distributed);
  Cells := CellsFromGrid(Coreography, Grid);
  for i := Low(Cells) to High(Cells) do begin
    Cell := Cells[i];
    FAnimation.Add(Cell.Rect.Left, Cell.Rect.Top);

    if i = 0 then begin
      Continue; // The first position is not a calibration point, skip it
    end;

    j := Length(NormalizedPoints);
    SetLength(NormalizedPoints, j+1);
    NormalizedPoints[j].X := Cell.Rect.CenterPoint.X.Normalize(Width);
    NormalizedPoints[j].Y := Cell.Rect.CenterPoint.Y.Normalize(Height);
  end;

  Result := NormalizedPoints;
end;

procedure TCalibrationChoreography.Start;
begin
  if FAnimation.WaitForRemote then Exit;
  FAnimation.StartPointAnimation;
end;

procedure TCalibrationChoreography.Stop;
begin
  FAnimation.Reset;
  FAnimation.ClearBackground;
end;

procedure TCalibrationChoreography.SetPoints(
  NormalizedPoints: TNormalizedPoints);
var
  Point : TNormalizedPoint;
  Width, Height : integer;
begin
  Width := FBackground.Width;
  Height := FBackground.Height;
  FAnimation.SetSize(Width, Height);
  FAnimation.ClearPoints;

  for Point in NormalizedPoints do begin
    FAnimation.Add(
      Point.X.Denormalize(Width),
      Point.Y.Denormalize(Height));
  end;
end;

initialization
  DefaultCoreography := TCoreography.Create(5, 1, 3, 9, 7, 5);

end.

