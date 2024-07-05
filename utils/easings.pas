unit easings;

{$mode ObjFPC}{$H+}

interface

uses
  Math;

function EaseInOutExpo(x: Float): Float;
function EaseOutBounce(x: Float): Float;
function EaseInOutQuad(x: Float): Float;
function EaseInOutQuart(x: Float): Float;

implementation

function easeInOutExpo(x: Float): Float;
begin
  if x = 0 then begin
    Result := 0
  end else begin
    if x = 1 then begin
      Result := 1
    end else begin
      if x < 0.5 then begin
        Result := Power(2, (20 * x) - 10) / 2;
      end else begin
        Result := (2 - Power(2, (-20 * x) + 10)) / 2;
      end;
    end;
  end;
end;

function EaseOutBounce(x: Float): Float;
const
  n1 = 7.5625;
  d1 = 2.75;
begin
  if x < 1 / d1 then begin
    Result := n1 * x * x;
  end else begin
    if x < 2 / d1 then begin
      x := x - 1.5 / d1;
      Result := n1 * x * x + 0.75;
    end else begin
      if x < 2.5 / d1 then begin
        x := x - 2.25 / d1;
        Result := n1 * x * x + 0.9375;
      end else begin
        x := x - 2.625 / d1;
        Result := n1 * x * x + 0.984375;
      end;
    end;
  end;
end;

function EaseInOutQuart(x: Float): Float;
begin
  if x < 0.5 then begin
    Result := 8 * x * x * x * x;
  end else begin
    Result := 1 - Power(-2 * x + 2, 4) / 2;
  end;
end;

function EaseInOutQuad(x: Float): Float;
begin
  if x < 0.5 then begin
    Result := 2 * x * x;
  end else begin
    Result := 1 - Power(-2 * x + 2, 2) / 2;
  end;
end;




end.

