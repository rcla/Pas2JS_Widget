
unit Canvasctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  Graphics,
  Controls;

type

  TBorderWidth = 0..Maxint;

  { TCustomContainCanvas }

  TCustomContainCanvas = class(TCustomControl)
  private
    FBorderColor: TColor;
    FBorderWidth: TBorderWidth;
    FTopSpace: Boolean;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: TBorderWidth);
    procedure SetTopSpace(Avalue: Boolean); 
  protected
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth;
    property TopSpace: Boolean read FTopSpace write SetTopSpace default False;
  end;


implementation

{ TCustomContainCanvas }
class function TCustomContainCanvas.GetControlClassDefaultSize: TSize;
begin
  Result.Cx := 170;
  Result.Cy := 50;
end;

constructor TCustomContainCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderColor := clSilver;
  FBorderWidth := 1;
  FTopSpace := False; 
end;

destructor TCustomContainCanvas.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomContainCanvas.SetBorderColor(AValue: TColor);
begin
  if (FBorderColor <> AValue) then
  begin
    FBorderColor := AValue;
  end;
end;

procedure TCustomContainCanvas.SetBorderWidth(AValue: TBorderWidth);
begin
  if (FBorderWidth <> AValue) then
  begin
    FBorderWidth := AValue;
  end;
end;

procedure TCustomContainCanvas.SetTopSpace(AValue: Boolean);
begin
  if (FTopSpace <> AValue) then
  begin
    FTopSpace := AValue;
  end;
end;

end.
