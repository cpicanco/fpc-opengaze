{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit animation;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, ExtCtrls, Math,
  BGRABitmap, screen.lines;

type

  TProcedureOfObject = procedure of object;

  TPaintingState = (Waiting, PaintingOnset, PaintingPointDelay, PaintingPointDuration);

  { TAnimation }

  TTargets = array of TRect;

  TAnimation = class
    private
      FAnimating : Boolean;
      FBitmapBack : TBGRABitmap;
      FBitmap : TBGRABitmap;
      FGaze: TPoint;
      FInvalidate: TProcedureOfObject;
      FMustWaitForRemote : Boolean;
      FDelay: TLargeInteger;
      FDuration : TLargeInteger;
      FInSize: Integer;
      FOnStartPointDuration: TNotifyEvent;
      FOnStartPointAnimation: TNotifyEvent;
      FOutSize: Integer;
      FReducePointSize: Boolean;
      FState: TPaintingState;
      FTimer  : TTimer;
      FTargetA  : TRect;
      FTargetB : TRect;
      FTargets : TTargets;
      FStartAnimationTime : TLargeInteger;
      FStartPDurationTime : TLargeInteger;
      FMaxLineIndex : Integer;
      FLine : TBresenhamLine;
      FIndex : Integer;
      procedure AnimateDistance(Sender: TObject);
      procedure AnimatePoint(Sender : TObject);
      procedure SetupNextPoint;
      function AssignRect(ARect : TRect) : TRect; overload;
      function AssignRect(X, Y : Integer) : TRect; overload;
      function CalculateCentralMarker(ARect : TRect; ASize : integer) : TRect;
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClearBackground;
      procedure ClearForeground;
      procedure PaintBackPoint(X, Y : Integer);
      procedure Reset;
      procedure SetSize(Width, Height : Integer);
      procedure StartOnSet;
      procedure StartPointDuration;
      procedure StartPointAnimation;
      procedure EndPointTimeout;
      procedure ClearPoints;
      procedure Paint;
      procedure Add(X, Y : integer);
      function GetCentralMarker : TRect;
      property Delay : TLargeInteger read FDelay write FDelay;
      property Duration : TLargeInteger read FDuration write FDuration;
      property Target : TRect read FTargetA;
      property CentralMarker : TRect read GetCentralMarker;
      property OutSize : Integer read FOutSize write FOutSize;
      property InSize  : Integer read FInSize write FOutSize;
      property OnStartPointDuration : TNotifyEvent read FOnStartPointDuration write FOnStartPointDuration;
      property OnStartPointAnimation : TNotifyEvent read FOnStartPointAnimation write FOnStartPointAnimation;
      property Targets : TTargets read FTargets;
      property State : TPaintingState read FState;
      property Bitmap : TBGRABitmap read FBitmap write FBitmap;
      property BitmapBack : TBGRABitmap read FBitmapBack write FBitmapBack;
      property ReducePointSize : Boolean read FReducePointSize write FReducePointSize;
      property Invalidate : TProcedureOfObject read FInvalidate write FInvalidate;
      property WaitForRemote : Boolean read FMustWaitForRemote write FMustWaitForRemote;
      property Gaze : TPoint read FGaze write FGaze;
  end;

implementation

uses
  BGRACanvas, windows.timestamps, windows.methods, easings, screen.helpers;

{ TAnimation }

procedure TAnimation.AnimateDistance(Sender: TObject);
var
  NormalizedTime : Float;
  DeltaTime: TLargeInteger;
  LPoint : TPoint;
begin
  DeltaTime := ClockMonotonic - FStartAnimationTime;
  if DeltaTime >= FDelay then begin
    FStartPDurationTime := ClockMonotonic;
    //if FMustWaitForRemote then begin
    //  FTimer.Enabled := False;
    //end else begin
    //  StartPointDuration;
    //end;
    StartPointDuration;
    Exit;
  end;

  NormalizedTime := EaseInOutQuad(DeltaTime / FDelay);
  LPoint := FLine[NormalizedTime.Denormalize(FMaxLineIndex)];
  FTargetA := AssignRect(LPoint.X, LPoint.Y);
  Paint;
end;

procedure TAnimation.AnimatePoint(Sender : TObject);
var
  NormalizedTime : Float;
  DeltaTime: TLargeInteger;
  LSize : Integer;
begin
  DeltaTime := ClockMonotonic - FStartPDurationTime;
  if DeltaTime > FDuration then begin
    FAnimating := False;
    if FMustWaitForRemote then begin
      FTimer.Enabled := False;
    end else begin
      StartPointAnimation;
    end;
    Exit;
  end;

  if ReducePointSize then begin
    NormalizedTime := easeInOutExpo(DeltaTime / FDuration);
    LSize := NormalizedTime.DenormalizeI(FOutSize, FInSize);
    FTargetA := CalculateCentralMarker(FTargetA, LSize);
  end;
  Paint;
end;

procedure TAnimation.SetupNextPoint;
begin
  FTargetA := FTargets[FIndex];
  Inc(FIndex);
  FTargetB := FTargets[FIndex];
  FLine := BresenhamLine(
    FTargetA.Left, FTargetA.Top, FTargetB.Left, FTargetB.Top);
  FMaxLineIndex := High(FLine);
end;

procedure TAnimation.Paint;
var
  Canvas : TBGRACanvas;

  procedure DrawGaze;
  begin
    Canvas.Pen.Color := clGreen;
    Canvas.Brush.Color := clNone;
    Canvas.EllipseC(Gaze.X, Gaze.Y, InSize, InSize);
  end;

  procedure DrawCalibrationPoint(Color : TColor);
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Ellipse(Target);

    Canvas.Pen.Color := Color;
    Canvas.Brush.Color := Color;
    Canvas.Ellipse(CentralMarker);

    //Canvas.TextRect(Target,
    //  Target.CenterPoint.X - (Canvas.TextWidth(LText) div 2),
    //  Target.CenterPoint.Y - (Canvas.TextHeight(LText) div 2),
    //  LText);
  end;

begin
  Canvas := FBitmap.CanvasBGRA;
  Canvas.Draw(0, 0, FBitmapBack);
  case FState of
    Waiting : begin
      DrawGaze;
    end;

    PaintingOnset : begin
      DrawCalibrationPoint(clRed);
    end;

    PaintingPointDelay : begin
      DrawCalibrationPoint(clBlack);
    end;

    PaintingPointDuration : begin
      DrawCalibrationPoint(clRed);
    end;
  end;

  Invalidate;
end;

function TAnimation.AssignRect(ARect: TRect): TRect;
begin
  Result := Rect(
    ARect.Left, ARect.Top, ARect.Left+FOutSize, ARect.Top+FOutSize);
end;

function TAnimation.AssignRect(X, Y: Integer): TRect;
begin
  Result := Rect(X, Y, X+FOutSize, Y+FOutSize);
end;

function TAnimation.CalculateCentralMarker(ARect: TRect; ASize: integer): TRect;
var
  LHalfSize : integer;
begin
  LHalfSize := ASize div 2;
  Result := Rect(
    ARect.CenterPoint.X - LHalfSize,
    ARect.CenterPoint.Y - LHalfSize,
    ARect.CenterPoint.X + LHalfSize,
    ARect.CenterPoint.Y + LHalfSize);
end;

constructor TAnimation.Create;
begin
  inherited Create;
  FAnimating := False;

  FGaze.X := 0;
  FGaze.Y := 0;

  FBitmap := TBGRABitmap.Create;
  FBitmapBack := TBGRABitmap.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 1000 div GetRefreshRate;
  FTimer.OnTimer := @AnimateDistance;

  FDuration := 20000000;
  FDelay    := 10000000;
  FInSize := 10;
  FOutSize := 50;

  SetLength(FTargets, 0);
  FIndex := 0;

  FReducePointSize := False;
  FMustWaitForRemote := False;
end;

destructor TAnimation.Destroy;
begin
  FTimer.Free;
  FBitmap.Free;
  FBitmapBack.Free;

  inherited Destroy;
end;

procedure TAnimation.ClearBackground;
var
  Canvas : TBGRACanvas;
begin
  Canvas := FBitmapBack.CanvasBGRA;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect(0, 0, Canvas.Width, Canvas.Height));
end;

procedure TAnimation.ClearForeground;
var
  Canvas : TBGRACanvas;
begin
  Canvas := FBitmap.CanvasBGRA;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect(0, 0, Canvas.Width, Canvas.Height));
end;

procedure TAnimation.PaintBackPoint(X, Y: Integer);
var
  Canvas : TBGRACanvas;
begin
  Canvas := FBitmapBack.CanvasBGRA;

  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clRed;
  Canvas.EllipseC(X, Y, 2, 2);
end;

procedure TAnimation.Reset;
begin
  FTimer.Enabled := False;
  FIndex := 0;
  FState := Waiting;
  Invalidate;
end;

procedure TAnimation.SetSize(Width, Height: Integer);
begin
  FBitmap.SetSize(Width, Height);
  FBitmapBack.SetSize(Width, Height);
  ClearBackground;
end;

procedure TAnimation.StartOnSet;
begin
  FIndex := Low(FTargets);
  FTargetA := FTargets[FIndex];
  FState := PaintingOnset;
  Paint;
end;

procedure TAnimation.StartPointAnimation;
begin
  if FIndex = High(FTargets) then begin
    FState := Waiting;
    FIndex := 0;
    FTimer.Enabled := False;
    Exit;
  end;
  if not FAnimating then begin
    FAnimating := True;
    SetupNextPoint;
    FTimer.Enabled := False;
    FTimer.OnTimer := @AnimateDistance;
    FTimer.Enabled := True;
    FStartAnimationTime := ClockMonotonic;
    FState := PaintingPointDelay;
    if Assigned(FOnStartPointAnimation) then begin
      FOnStartPointAnimation(Self);
    end;
  end;
end;

procedure TAnimation.EndPointTimeout;
begin
  { do nothing }
end;

procedure TAnimation.ClearPoints;
begin
  SetLength(FTargets, 0);
end;

procedure TAnimation.StartPointDuration;
begin
  FTimer.Enabled := False;
  FTimer.OnTimer := @AnimatePoint;
  FTimer.Enabled := True;
  FStartPDurationTime := ClockMonotonic;
  FState := PaintingPointDuration;
  if Assigned(FOnStartPointDuration) then begin
    FOnStartPointDuration(Self);
  end;
end;

procedure TAnimation.Add(X, Y: integer);
var
  LPoint : TPoint;
  i : Integer;
begin
  i := Length(FTargets);
  LPoint.X := X;
  LPoint.Y := Y;
  SetLength(FTargets, i+1);
  FTargets[i] := AssignRect(LPoint.X, LPoint.Y);
  if i = 0 then begin
    FTargetA := FTargets[i];
  end;
end;

function TAnimation.GetCentralMarker: TRect;
begin
  Result := CalculateCentralMarker(FTargetA, FInSize);
end;

end.

