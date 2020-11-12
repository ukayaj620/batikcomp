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
  Kernel = array[1..3, 1..3] of Integer;
  BitmapColor = array[0..1000, 0..1000] of Channel;
  BitmapBinary = array[0..1000, 0..1000] of Boolean;

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    ButtonPattern: TButton;
    ButtonTexture1: TButton;
    ButtonTexture2: TButton;
    ButtonObject: TButton;
    ButtonExecute: TButton;
    ButtonSave: TButton;
    ImagePattern: TImage;
    ImageTexture1: TImage;
    ImageTexture2: TImage;
    ImageObject: TImage;
    Image5: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    procedure ButtonPatternClick(Sender: TObject);
  private
    procedure InitImageDimension(imgWidth: Integer; imgHeight: Integer);
    procedure InitImageBitmap(image: TImage; bitmap: BitmapColor);

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
  BitmapBinaryImage: BitmapBinary;
  imageWidth, imageHeight: Integer;

procedure TFormMain.ButtonPatternClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImagePattern.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    imageWidth:= ImagePattern.Width;
    imageHeight:= ImagePattern.Height;
    InitImageDimension(imageWidth, imageHeight);

    InitImageBitmap(ImagePattern, BitmapPattern);
  end;
end;

procedure TFormMain.InitImageBitmap(image: TImage; bitmap: BitmapColor);
var
  x, y: Integer;
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
end;

procedure TFormMain.InitImageDimension(imgWidth: Integer; imgHeight: Integer);
begin
  ImageTexture1.Width:= imgWidth;
  ImageTexture2.Width:= imgWidth;
  ImageObject.Width:= imgWidth;

  ImageTexture1.Height:= imgHeight;
  ImageTexture2.Height:= imgHeight;
  ImageObject.Height:= imgHeight;
end;

end.

