unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, ComCtrls;

type
  Channel = record
    R, G, B: Byte;
  end;
  ChannelDouble = record
    R, G, B: Real;
  end;

  Kernel = array[-1..1, -1..1] of Real;
  KernelInt = array[-1..1, -1..1] of Integer;
  SE = array[-1..1, -1..1] of Byte;
  BitmapColor = array[0..301, 0..301] of Channel;
  BitmapGrayscale = array[0..301, 0..301] of Byte;
  BitmapBinary = array[0..301, 0..301] of Boolean;
  FourWays = array[1..4] of KernelInt;

  { TFormMain }

  TFormMain = class(TForm)
    ButtonPattern1: TButton;
    ButtonTexture1: TButton;
    ButtonBackground: TButton;
    ButtonPattern2: TButton;
    ButtonExecute: TButton;
    ButtonSave: TButton;
    EditBinarization: TEdit;
    Image1: TImage;
    ImagePattern1: TImage;
    ImageTexture1: TImage;
    ImageTexture2: TImage;
    ImagePattern2: TImage;
    ImageResult: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControl: TPageControl;
    Panel1: TPanel;
    SavePictureDialog1: TSavePictureDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonPattern2Click(Sender: TObject);
    procedure ButtonPattern1Click(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonTexture1Click(Sender: TObject);
    procedure ButtonBackgroundClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitImageDimension(imgWidth: Integer; imgHeight: Integer);
    function InitImageBitmap(image: TImage): BitmapColor;
    procedure ShowImageFromBitmap(bitmap: BitmapColor);
    procedure ShowImageFromBitmap(bitmap: BitmapGrayscale);
    procedure ShowImageFromBitmap(bitmap: BitmapBinary);
    function Constrain(value: Integer): Byte;
    function Grayscaling(bitmap: BitmapColor): BitmapGrayscale;
    function Invers(bitmap: BitmapGrayscale): BitmapGrayscale;
    function Invers(bitmap: BitmapBinary): BitmapBinary;
    function Binarization(bitmap: BitmapGrayscale; threshold: Byte): BitmapBinary;
    function PaddingBitmap(bitmap: BitmapColor): BitmapColor;
    function PaddingBitmap(bitmap: BitmapGrayscale): BitmapGrayscale;
    function PaddingBitmap(bitmap: BitmapBinary): BitmapBinary;
    function Convolution(padBitmap: BitmapColor; K: Kernel): BitmapColor;
    function CompassKernel(): FourWays;
    function EdgeDetection(padBitmap: BitmapGrayscale; K: FourWays; ways: Integer): BitmapGrayscale;
    function Dilation(padBitmap: BitmapBinary; loop: Integer): BitmapBinary;
    function Erosion(padBitmap: BitmapBinary; loop: Integer): BitmapBinary;
    function BoolToByte(value: Boolean): Byte;
    function BinaryToGrayscale(bitmap: BitmapBinary): BitmapGrayscale;
    function ArithmeticAnd(bitmap1: BitmapBinary; bitmap2: BitmapBinary): BitmapBinary;
    function ArithmeticAddition(bitmap1: BitmapGrayscale; bitmap2: BitmapGrayscale): BitmapGrayscale;
    function ArithmeticAddition(bitmap1: BitmapColor; bitmap2: BitmapColor): BitmapColor;
    function ArithmeticAddition(bitmap1: BitmapColor; bitmap2: BitmapGrayscale): BitmapColor;
    function ArithmeticMultiply(bitmap1: BitmapGrayscale; bitmap2: BitmapColor): BitmapColor;

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

uses
  windows;

var
  BitmapPattern1, BitmapTexture1, BitmapTexture2, BitmapPattern2: BitmapColor;
  imageWidth: Integer = 300;
  imageHeight: Integer = 300;
  LPFKernel: Kernel = ((1/9, 1/9, 1/9), (1/9, 1/9, 1/9), (1/9, 1/9, 1/9));
  HPFKernel: Kernel = ((-1, -1, -1), (-1, 9, -1), (-1, -1, -1));
  StructElement: SE = ((1, 1, 1), (1, 1, 1), (1, 1, 1));

procedure TFormMain.ButtonPattern1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImagePattern1.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapPattern1:= InitImageBitmap(ImagePattern1);
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
  begin
    ImageResult.Picture.SaveToFile(SavePictureDialog1.FileName);
  end;
end;

procedure TFormMain.ButtonPattern2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImagePattern2.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapPattern2:= InitImageBitmap(ImagePattern2);
  end;
end;

// EXECUTION
procedure TFormMain.ButtonExecuteClick(Sender: TObject);
var
  Pattern1Compass: BitmapGrayscale;
  Pattern2Binary, Pattern1AndPattern2, Pattern2CompassBinary: BitmapBinary;
  Texture1HPF: BitmapColor;
  Texture1HPFMultiplyPattern1AndPattern2: BitmapColor;
  Pattern1AndPattern2MultiplyTexture2: BitmapColor;
  SubFinal, Final: BitmapColor;
begin
  // Pattern1Compass:= EdgeDetection(PaddingBitmap(BinaryToGrayscale(Erosion(Dilation(Invers(Binarization(Grayscaling(BitmapPattern1),148)),2),2))),CompassKernel(),4);
  Pattern1Compass:= BinaryToGrayscale(Binarization(EdgeDetection(PaddingBitmap(Grayscaling(BitmapPattern1)), CompassKernel(), 4), StrToInt(EditBinarization.Text)));
  Pattern2Binary:= Binarization(Grayscaling(BitmapPattern2), 229);

  Pattern1AndPattern2:= ArithmeticAnd(Binarization(Pattern1Compass, 127), Pattern2Binary);

  Texture1HPF:= Convolution(PaddingBitmap(Convolution(PaddingBitmap(BitmapTexture1), LPFKernel)), HPFKernel);

  Texture1HPFMultiplyPattern1AndPattern2:= ArithmeticMultiply(BinaryToGrayscale(Pattern1AndPattern2), Texture1HPF);

  Pattern1AndPattern2MultiplyTexture2:= ArithmeticMultiply(BinaryToGrayscale(Invers(Pattern1AndPattern2)), BitmapTexture2);

  SubFinal:= ArithmeticAddition(Pattern1AndPattern2MultiplyTexture2, Texture1HPFMultiplyPattern1AndPattern2);
  Pattern2CompassBinary:= Binarization(EdgeDetection(PaddingBitmap(Grayscaling(BitmapPattern2)), CompassKernel(), 4), 169);
  Final:= ArithmeticAddition(SubFinal, BinaryToGrayscale(Pattern2CompassBinary));

  ShowImageFromBitmap(Final);
end;

procedure TFormMain.ButtonTexture1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageTexture1.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapTexture1:= InitImageBitmap(ImageTexture1);
  end;
end;

procedure TFormMain.ButtonBackgroundClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageTexture2.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapTexture2:= InitImageBitmap(ImageTexture2);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitImageDimension(imageWidth, imageHeight);
  PageControl.ActivePageIndex:= 0;
end;

function TFormMain.InitImageBitmap(image: TImage): BitmapColor;
var
  x, y: Integer;
  bitmap: BitmapColor;
begin
  for y:= 1 to imageWidth do
  begin
    for x:= 1 to imageHeight do
    begin
      bitmap[x, y].R:= GetRValue(image.Canvas.Pixels[x-1, y-1]);
      bitmap[x, y].G:= GetGValue(image.Canvas.Pixels[x-1, y-1]);
      bitmap[x, y].B:= GetBValue(image.Canvas.Pixels[x-1, y-1]);
    end;
  end;
  InitImageBitmap:= bitmap
end;

procedure TFormMain.InitImageDimension(imgWidth: Integer; imgHeight: Integer);
begin
  ImagePattern1.Width:= imgWidth;
  ImageTexture1.Width:= imgWidth;
  ImageTexture2.Width:= imgWidth;
  ImagePattern2.Width:= imgWidth;
  ImageResult.Width:= imgWidth;

  ImagePattern1.Height:= imgHeight;
  ImageTexture1.Height:= imgHeight;
  ImageTexture2.Height:= imgHeight;
  ImagePattern2.Height:= imgHeight;
  ImageResult.Height:= imgHeight;
end;

procedure TFormMain.ShowImageFromBitmap(bitmap: BitmapColor);
var
  x, y: Integer;
begin
  for y:= 1 to imageWidth do
  begin
    for x:= 1 to imageHeight do
    begin
      ImageResult.Canvas.Pixels[x-1, y-1]:= RGB(bitmap[x, y].R, bitmap[x, y].G, bitmap[x, y].B);
    end;
  end;
end;

procedure TFormMain.ShowImageFromBitmap(bitmap: BitmapGrayscale);
var
  x, y: Integer;
begin
  for y:= 1 to imageWidth do
  begin
    for x:= 1 to imageHeight do
    begin
      ImageResult.Canvas.Pixels[x-1, y-1]:= RGB(bitmap[x, y], bitmap[x, y], bitmap[x, y]);
    end;
  end;
end;

procedure TFormMain.ShowImageFromBitmap(bitmap: BitmapBinary);
var
  x, y: Integer;
  pixel: Byte;
begin
  for y:= 1 to imageWidth do
  begin
    for x:= 1 to imageHeight do
    begin
      pixel:= BoolToByte(bitmap[x, y]) * 255;
      ImageResult.Canvas.Pixels[x-1, y-1]:= RGB(pixel, pixel, pixel);
    end;
  end;
end;

function TFormMain.Grayscaling(bitmap: BitmapColor): BitmapGrayscale;
var
  x, y: Integer;
  BitmapTemp: BitmapGrayscale;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      BitmapTemp[x, y]:= (bitmap[x, y].R + bitmap[x, y].G + bitmap[x, y].B) div 3
    end;
  end;
  Grayscaling:= BitmapTemp;
end;

function TFormMain.Invers(bitmap: BitmapGrayscale): BitmapGrayscale;
var
  x, y: Integer;
  BitmapTemp: BitmapGrayscale;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      BitmapTemp[x, y]:= 255 - bitmap[x, y];
    end;
  end;
  Invers:= BitmapTemp;
end;

function TFormMain.Invers(bitmap: BitmapBinary): BitmapBinary;
var
  x, y: Integer;
  BitmapTemp: BitmapBinary;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      BitmapTemp[x, y]:= NOT bitmap[x, y];
    end;
  end;
  Invers:= BitmapTemp;
end;

function TFormMain.Binarization(bitmap: BitmapGrayscale; threshold: Byte): BitmapBinary;
var
  x, y: Integer;
  BitmapTemp: BitmapBinary;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      if bitmap[x, y] <= threshold then
        BitmapTemp[x, y]:= false
      else
        BitmapTemp[x, y]:= true;
    end;
  end;
  Binarization:= BitmapTemp;
end;

function TFormMain.PaddingBitmap(bitmap: BitmapColor): BitmapColor;
var
  x, y: Integer;
  BitmapTemp: BitmapColor;
begin
  BitmapTemp:= bitmap;
  for y:= 1 to imageHeight do
  begin
    BitmapTemp[0, y]:= bitmap[1, y];
    BitmapTemp[imageWidth+1, y]:= bitmap[imageWidth, y];
  end;

  for x:= 0 to imageWidth+1 do
  begin
    BitmapTemp[x, 0]:= BitmapTemp[x, 1];
    BitmapTemp[x, imageHeight+1]:= BitmapTemp[x, imageHeight];
  end;
  PaddingBitmap:= BitmapTemp;
end;

function TFormMain.PaddingBitmap(bitmap: BitmapGrayscale): BitmapGrayscale;
var
  x, y: Integer;
  BitmapTemp: BitmapGrayscale;
begin
  BitmapTemp:= bitmap;
  for y:= 1 to imageHeight do
  begin
    BitmapTemp[0, y]:= bitmap[1, y];
    BitmapTemp[imageWidth+1, y]:= bitmap[imageWidth, y];
  end;

  for x:= 0 to imageWidth+1 do
  begin
    BitmapTemp[x, 0]:= BitmapTemp[x, 1];
    BitmapTemp[x, imageHeight+1]:= BitmapTemp[x, imageHeight];
  end;
  PaddingBitmap:= BitmapTemp;
end;

function TFormMain.PaddingBitmap(bitmap: BitmapBinary): BitmapBinary;
var
  x, y: Integer;
  BitmapTemp: BitmapBinary;
begin
  BitmapTemp:= bitmap;
  for y:= 1 to imageHeight do
  begin
    BitmapTemp[0, y]:= bitmap[1, y];
    BitmapTemp[imageWidth+1, y]:= bitmap[imageWidth, y];
  end;

  for x:= 0 to imageWidth+1 do
  begin
    BitmapTemp[x, 0]:= BitmapTemp[x, 1];
    BitmapTemp[x, imageHeight+1]:= BitmapTemp[x, imageHeight];
  end;
  PaddingBitmap:= BitmapTemp;
end;

function TFormMain.Convolution(padBitmap: BitmapColor; K: Kernel): BitmapColor;
var
  x, y: Integer;
  kx, ky: Integer;
  ResultBitmap: BitmapColor;
  pixel: ChannelDouble;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      pixel.R:= 0;
      pixel.G:= 0;
      pixel.B:= 0;
      for ky:= -1 to 1 do
      begin
        for kx:= -1 to 1 do
        begin
          pixel.R:= pixel.R + (padBitmap[x-kx, y-ky].R * K[kx, ky]);
          pixel.G:= pixel.G + (padBitmap[x-kx, y-ky].G * K[kx, ky]);
          pixel.B:= pixel.B + (padBitmap[x-kx, y-ky].B * K[kx, ky]);
        end;
      end;
      ResultBitmap[x, y].R:= Constrain(Round(pixel.R));
      ResultBitmap[x, y].G:= Constrain(Round(pixel.G));
      ResultBitmap[x, y].B:= Constrain(Round(pixel.B));
    end;
  end;
  Convolution:= ResultBitmap;
end;

function TFormMain.EdgeDetection(padBitmap: BitmapGrayscale; K: FourWays; ways: Integer): BitmapGrayscale;
var
  ResultBitmap: BitmapGrayscale;
  grays: array[1..4] of Integer;
  way, gray: Integer;
  x, y, kx, ky: Integer;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      for way:= 1 to ways do
      begin
        grays[way]:= 0;
      end;
      for ky:= -1 to 1 do
      begin
        for kx:= -1 to 1 do
        begin
          for way:= 1 to ways do
          begin
            grays[way]:= grays[way] + (padBitmap[x-kx, y-ky] * K[way, kx, ky]);
          end;
        end;
      end;
      gray:= 0;
      for way:= 1 to ways do
      begin
        if grays[way] >= gray then
           gray:= grays[way];
      end;
      ResultBitmap[x, y]:= Constrain(Round(gray));
    end;
  end;
  EdgeDetection:= ResultBitmap;
end;

function TFormMain.Constrain(value: Integer): byte;
begin
  if value < 0 then Constrain := 0
  else if value > 255 then Constrain := 255
  else Constrain := value;
end;

function TFormMain.CompassKernel(): FourWays;
var
  cS: KernelInt = ((-1, -1, -1), (1, -2, 1), (1, 1, 1));
  cW: KernelInt = ((1, 1, -1), (1, -2, -1), (1, 1, -1));
  cN: KernelInt = ((1, 1, 1), (1, -2, 1), (-1, -1, -1));
  cE: KernelInt = ((-1, 1, 1), (-1, -2, 1), (-1, 1, 1));
  K: FourWays;
begin
  K[1]:= cS;
  K[2]:= cW;
  K[3]:= cN;
  K[4]:= cE;
  CompassKernel:= K;
end;

function TFormMain.Dilation(padBitmap: BitmapBinary; loop: Integer): BitmapBinary;
var
  x, y: Integer;
  kx, ky: Integer;
  m: Integer;
  BitmapTemp: BitmapBinary;
  BitmapInput: BitmapBinary;
begin
  BitmapInput:= PaddingBitmap(padBitmap);
  for m:= 1 to loop do
  begin
    for y:= 1 to imageHeight do
    begin
      for x:= 1 to imageWidth do
      begin
        BitmapTemp[x, y]:= false;
        for ky:= -1 to 1 do
        begin
          for kx:= -1 to 1 do
          begin
            BitmapTemp[x, y]:= BitmapTemp[x, y] OR (BoolToByte(BitmapInput[x-kx, y-ky]) = StructElement[kx, ky]);
          end;
        end;
      end;
    end;
    BitmapInput:= PaddingBitmap(BitmapTemp);
  end;
  Dilation:= BitmapInput;
end;

function TFormMain.Erosion(padBitmap: BitmapBinary; loop: Integer): BitmapBinary;
var
  x, y: Integer;
  kx, ky: Integer;
  m: Integer;
  BitmapTemp: BitmapBinary;
  BitmapInput: BitmapBinary;
begin
  BitmapInput:= padBitmap;
  for m:= 1 to loop do
  begin
    for y:= 1 to imageHeight do
    begin
      for x:= 1 to imageWidth do
      begin
        BitmapTemp[x, y]:= true;
        for ky:= -1 to 1 do
        begin
          for kx:= -1 to 1 do
          begin
            BitmapTemp[x, y]:= BitmapTemp[x, y] AND (BoolToByte(BitmapInput[x-kx, y-ky]) = StructElement[kx, ky]);
          end;
        end;
      end;
    end;
    BitmapInput:= BitmapTemp;
  end;
  Erosion:= BitmapInput;
end;

function TFormMain.ArithmeticAnd(bitmap1: BitmapBinary; bitmap2: BitmapBinary): BitmapBinary;
var
  x, y: Integer;
  ResultBitmap: BitmapBinary;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      ResultBitmap[x, y]:= bitmap1[x, y] AND bitmap2[x, y];
    end;
  end;
  ArithmeticAnd:= ResultBitmap;
end;

function TFormMain.ArithmeticAddition(bitmap1: BitmapGrayscale; bitmap2: BitmapGrayscale): BitmapGrayscale;
var
  x, y: Integer;
  ResultBitmap: BitmapGrayscale;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      ResultBitmap[x, y]:= Constrain(bitmap1[x, y] + bitmap2[x, y]);
    end;
  end;
  ArithmeticAddition:= ResultBitmap;
end;

function TFormMain.ArithmeticAddition(bitmap1: BitmapColor; bitmap2: BitmapColor): BitmapColor;
var
  x, y: Integer;
  ResultBitmap: BitmapColor;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      ResultBitmap[x, y].R:= Constrain(bitmap1[x, y].R + bitmap2[x, y].R);
      ResultBitmap[x, y].G:= Constrain(bitmap1[x, y].G + bitmap2[x, y].G);
      ResultBitmap[x, y].B:= Constrain(bitmap1[x, y].B + bitmap2[x, y].B);
    end;
  end;
  ArithmeticAddition:= ResultBitmap;
end;

function TFormMain.ArithmeticAddition(bitmap1: BitmapColor; bitmap2: BitmapGrayscale): BitmapColor;
var
  x, y: Integer;
  ResultBitmap: BitmapColor;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      ResultBitmap[x, y].R:= Constrain(bitmap1[x, y].R + bitmap2[x, y]);
      ResultBitmap[x, y].G:= Constrain(bitmap1[x, y].G + bitmap2[x, y]);
      ResultBitmap[x, y].B:= Constrain(bitmap1[x, y].B + bitmap2[x, y]);
    end;
  end;
  ArithmeticAddition:= ResultBitmap;
end;

function TFormMain.BoolToByte(value: Boolean): Byte;
begin
  if value then BoolToByte:= 1 else BoolToByte:= 0;
end;

function TFormMain.BinaryToGrayscale(bitmap: BitmapBinary): BitmapGrayscale;
var
  x, y: Integer;
  ResultBitmap: BitmapGrayscale;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      ResultBitmap[x, y]:= BoolToByte(bitmap[x, y]) * 255;
    end;
  end;
  BinaryToGrayscale:= ResultBitmap;
end;

function TFormMain.ArithmeticMultiply(bitmap1: BitmapGrayscale; bitmap2: BitmapColor): BitmapColor;
var
  x, y: Integer;
  ResultBitmap: BitmapColor;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      ResultBitmap[x, y].R:= Constrain(Round((bitmap1[x, y] / 255) * (bitmap2[x, y].R / 255) * 255));
      ResultBitmap[x, y].G:= Constrain(Round((bitmap1[x, y] / 255) * (bitmap2[x, y].G / 255) * 255));
      ResultBitmap[x, y].B:= Constrain(Round((bitmap1[x, y] / 255) * (bitmap2[x, y].B / 255) * 255));
    end;
  end;
  ArithmeticMultiply:= ResultBitmap;
end;

end.

