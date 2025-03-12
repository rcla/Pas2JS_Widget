
unit Canvasctrl;

{$I pas2js_widget.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  Web,
  Graphics,
  Controls;

type
  
  TBorderWidth = 0..Maxint;
  
  { TCustomContainCanvas }

  TCustomContainCanvas = class(TCustomControl)
  private
    FBorderColor: TColor;
    FBorderWidth: TBorderWidth;
    FCanvasElement: TJSHTMLCanvasElement;
    FTopSpace: Boolean;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: TBorderWidth);
    procedure SetCanvasElement(AValue: TJSHTMLCanvasElement);
    procedure SetTopSpace(Avalue: Boolean);
  protected
    procedure Changed; override;
    function CreateHandleElement: TJSHTMLElement; override;
  protected
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth;
    property CanvasElement: TJSHTMLCanvasElement read FCanvasElement write SetCanvasElement;
    property TopSpace: Boolean read FTopSpace write SetTopSpace default False; 
  end;


implementation

{ TCustomContainCanvas }
function TCustomContainCanvas.CreateHandleElement: TJSHTMLElement;
begin
  Result := TJSHTMLElement(Document.CreateElement('div'));
  FCanvasElement := TJSHTMLCanvasElement(Document.CreateElement('canvas'));
  FCanvasElement.height := 100;
  FCanvasElement.width := 100;
  Result.append(FCanvasElement);
end;

procedure TCustomContainCanvas.Changed;
begin
  inherited Changed;
  if (not IsUpdating) and not (csLoading in ComponentState) then
  begin
    with HandleElement do
    begin
      /// Border
      Style.SetProperty('border-width', IntToStr(FBorderWidth) + 'px');
      Style.SetProperty('border-style', 'solid');
      Style.SetProperty('border-color', JSColor(FBorderColor));
      /// Position
      Style.SetProperty('position', 'relative');      
      /// Focus highlight
      Style.SetProperty('outline', 'none');
      /// Prevent text selection
      Style.SetProperty('user-select', 'none');
      Style.SetProperty('-moz-user-select', 'none');
      Style.SetProperty('-ms-user-select', 'none');
      Style.SetProperty('-khtml-user-select', 'none');
      Style.SetProperty('-webkit-user-select', 'none');
      /// Padding
      Style.SetProperty('padding', '10px');
      if FTopSpace then Style.SetProperty('padding-top', '40px'); 
      /// Overflow
      Style.SetProperty('overflow', 'auto');
      /// Text align
      Style.SetProperty('text-align', 'center');
    end;
  end;
end;

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
    Changed;
  end;
end;

procedure TCustomContainCanvas.SetBorderWidth(AValue: TBorderWidth);
begin
  if (FBorderWidth <> AValue) then
  begin
    FBorderWidth := AValue;
    Changed;
  end;
end;

procedure TCustomContainCanvas.SetCanvasElement(AValue: TJSHTMLCanvasElement);
begin
  if (FCanvasElement <> AValue) then
  begin
    FCanvasElement := AValue;
    Changed;
  end;
end;

procedure TCustomContainCanvas.SetTopSpace(AValue: Boolean);
begin
  if (FTopSpace <> AValue) then
  begin
    FTopSpace := AValue;
    Changed;
  end;
end;


end.
