unit screen.grids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TCell = record
    Index: integer;
    Rect: TRect;
  end;

  TCells = array of TCell;

  TGrid = array of array of TCell;

function CellsFromGrid(const Cells: array of Integer;
  const Grid: TGrid): TCells;

function GetCentralGrid(AN, MonitorWidth, MonitorHeight : integer;
  ASide: Integer; Distribute: Boolean): TGrid;

implementation

uses
  Forms;

function CellsFromGrid( const Cells: array of Integer;
  const Grid: TGrid): TCells;
var
  i, Row, Col, Position: Integer;
begin
  Result := Default(TCells);
  SetLength(Result, Length(Cells));

  for i := Low(Cells) to High(Cells) do
  begin
    Position := Cells[i] - 1;
    Row := Position div Length(Grid);
    Col := Position mod Length(Grid);
    Result[i] := Grid[Row, Col];
  end;
end;

function GetPositionFromSegment(ASegment, AStep, ASteps, AStimulusSide,
  AInterStimulusSpace: integer): integer;
var
  LSize: integer;
begin
  LSize := AStimulusSide + AInterStimulusSpace;
  Result := Round(
    (LSize*AStep)-((LSize*ASteps)/2) + ((ASegment+AInterStimulusSpace)/2)
  );
end;

function InterSpace(ATotalSize, AElementSize, ACount: integer): integer;
begin
  Result := (ATotalSize - (AElementSize * ACount)) div ACount;
end;

function GetCentralGrid(AN, MonitorWidth, MonitorHeight : integer;
  ASide: Integer; Distribute: Boolean): TGrid;
var
  LIndex, LInterSpaceW, LInterSpaceH, j, i: integer;
begin
  Result := Default(TGrid);
  SetLength(Result, AN, AN);

  if Distribute then begin
    LInterSpaceW := InterSpace(MonitorWidth, ASide, AN);
    LInterSpaceH := InterSpace(MonitorHeight, ASide, AN);
  end else begin
    LInterSpaceW := InterSpace(Max(MonitorWidth, MonitorHeight), ASide, AN);
    LInterSpaceH := LInterSpaceW;
  end;

  LIndex := 0;
  for j := Low(Result) to High(Result) do begin
    for i := Low(Result[j]) to High(Result[j]) do begin
      with Result[j][i] do begin
        Index := LIndex;
        Rect.Top := GetPositionFromSegment(
          MonitorHeight, j, AN, ASide, LInterSpaceH);
        Rect.Left := GetPositionFromSegment(
          MonitorWidth, i, AN, ASide, LInterSpaceW);
        Rect.Width := ASide;
        Rect.Height := ASide;
      end;
      Inc(LIndex);
    end;
  end;
end;

end.
