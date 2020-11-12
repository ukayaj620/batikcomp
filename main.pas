unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs;

type
  Channel = record
    R, G, B: Byte;
  end;
  ChannelDouble = record
    R, G, B: Real;
  end;

  Kernel = array[-1..1, -1..1] of Real;
  KernelInt = array[-1..1, -1..1] of Integer;
  SE = array[-1..1, -1..1] of Integer;
  BitmapColor = array[0..1000, 0..1000] of Channel;
  BitmapGrayscale = array[0..1000, 0..1000] of Byte;
  FourWays = array[1..4] of KernelInt;

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    ButtonPattern: TButton;
    ButtonTexture1: TButton;
    ButtonTexture2: TButton;
    ButtonObject: TButton;
    ButtonExecute: TButton;
    ButtonSave: TButton;
    Edit1: TEdit;
    ImagePattern: TImage;
    ImageTexture1: TImage;
    ImageTexture2: TImage;
    ImageObject: TImage;
    ImageResult: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonObjectClick(Sender: TObject);
    procedure ButtonPatternClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonTexture1Click(Sender: TObject);
    procedure ButtonTexture2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitImageDimension(imgWidth: Integer; imgHeight: Integer);
    function InitImageBitmap(image: TImage): BitmapColor;
    procedure ShowImageFromBitmap(bitmap: BitmapColor);
    procedure ShowImageFromBitmap(bitmap: BitmapGrayscale);
    function Constrain(value: Integer): Byte;
    function Grayscaling(bitmap: BitmapColor): BitmapGrayscale;
    function Invers(bitmap: BitmapGrayscale): BitmapGrayscale;
    function Binarization(bitmap: BitmapGrayscale; threshold: Byte): BitmapGrayscale;
    function PaddingBitmap(bitmap: BitmapColor): BitmapColor;
    function PaddingBitmap(bitmap: BitmapGrayscale): BitmapGrayscale;
    function Convolution(padBitmap: BitmapColor; K: Kernel): BitmapColor;
    function CompassKernel(): FourWays;
    function EdgeDetection(padBitmap: BitmapGrayscale; K: FourWays; ways: Integer): BitmapGrayscale;

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
  BitmapPattern, BitmapTexture1, BitmapTexture2, BitmapObject: BitmapColor;
  BitmapBinaryImage: BitmapGrayscale;
  imageWidth: Integer = 300;
  imageHeight: Integer = 300;
  KSize: Integer = 3;
  LPFKernel: Kernel = ((1/9, 1/9, 1/9), (1/9, 1/9, 1/9), (1/9, 1/9, 1/9));
  HPFKernel: Kernel = ((-1, -1, -1), (-1, 9, -1), (-1, -1, -1));

procedure TFormMain.ButtonPatternClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImagePattern.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapPattern:= InitImageBitmap(ImagePattern);
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
  begin
    ImageResult.Picture.SaveToFile(SavePictureDialog1.FileName);
  end;
end;

procedure TFormMain.ButtonObjectClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageObject.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapObject:= InitImageBitmap(ImageObject);
  end;
end;

procedure TFormMain.ButtonExecuteClick(Sender: TObject);
var
  BitmapGray: BitmapGrayscale;
begin
  BitmapGray:= Grayscaling(BitmapPattern);
  ShowImageFromBitmap(EdgeDetection(PaddingBitmap(BitmapGray), CompassKernel(), 4));
end;

procedure TFormMain.ButtonTexture1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageTexture1.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    BitmapTexture1:= InitImageBitmap(ImageTexture1);
  end;
end;

procedure TFormMain.ButtonTexture2Click(Sender: TObject);
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
  ImagePattern.Width:= imgWidth;
  ImageTexture1.Width:= imgWidth;
  ImageTexture2.Width:= imgWidth;
  ImageObject.Width:= imgWidth;
  ImageResult.Width:= imgWidth;

  ImagePattern.Height:= imgHeight;
  ImageTexture1.Height:= imgHeight;
  ImageTexture2.Height:= imgHeight;
  ImageObject.Height:= imgHeight;
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

function TFormMain.Binarization(bitmap: BitmapGrayscale; threshold: Byte): BitmapGrayscale;
var
  x, y: Integer;
  BitmapTemp: BitmapGrayscale;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      if bitmap[x, y] <= threshold then
        BitmapTemp[x, y]:= 0
      else
        BitmapTemp[x, y]:= 255;
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
    BitmapTemp[0, y]:= BitmapTemp[1, y];
    BitmapTemp[imageWidth+1, y]:= BitmapTemp[imageWidth, y];
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
    BitmapTemp[0, y]:= BitmapTemp[1, y];
    BitmapTemp[imageWidth+1, y]:= BitmapTemp[imageWidth, y];
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

end.

