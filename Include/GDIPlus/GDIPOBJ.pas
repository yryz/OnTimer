{******************************************************************}
{ GDI+ Class                                                       }
{                                                                  }
{ home page : http://www.progdigy.com                              }
{ email     : hgourvest@progdigy.com                               }
{                                                                  }
{ date      : 15-02-2002                                           }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ *****************************************************************}

unit GDIPOBJ;

interface
uses
  windows,
  ActiveX,
  DirectDraw,
  GDIPAPI;

(**************************************************************************\
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

function GetImageDecodersSize(out numDecoders, size: UINT): TStatus;
function GetImageDecoders(numDecoders, size: UINT;
  decoders: PImageCodecInfo): TStatus;
function GetImageEncodersSize(out numEncoders, size: UINT): TStatus;
function GetImageEncoders(numEncoders, size: UINT;
  encoders: PImageCodecInfo): TStatus;

(**************************************************************************\
*
*   Private GDI+ header file.
*
\**************************************************************************)

//---------------------------------------------------------------------------
// GDI+ classes for forward reference
//---------------------------------------------------------------------------

type
  TGPGraphics = class;
  TGPPen = class;
  TGPBrush = class;
  TGPMatrix = class;
  TGPBitmap = class;
  TGPMetafile = class;
  TGPFontFamily = class;
  TGPGraphicsPath = class;
  TGPRegion = class;
  TGPImage = class;
  TGPHatchBrush = class;
  TGPSolidBrush = class;
  TGPLinearGradientBrush = class;
  TGPPathGradientBrush = class;
  TGPFont = class;
  TGPFontCollection = class;
  TGPInstalledFontCollection = class;
  TGPPrivateFontCollection = class;
  TGPImageAttributes = class;
  TGPCachedBitmap = class;

  (**************************************************************************\
  *
  *   GDI+ Region, Font, Image, CustomLineCap class definitions.
  *
  \**************************************************************************)

  TGPRegion = class(TGdiplusBase)
  protected
    nativeRegion: GpRegion;
    lastResult: TStatus;
    function SetStatus(Status: TStatus): TStatus;
    procedure SetNativeRegion(nativeRegion: GpRegion);
    constructor Create(nativeRegion: GpRegion); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(Rect: TGPRectF); reintroduce; overload;
    constructor Create(Rect: TGPRect); reintroduce; overload;
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create(regionData: PBYTE; size: Integer); reintroduce; overload;
    constructor Create(hRgn: hRgn); reintroduce; overload;
    function FromHRGN(hRgn: hRgn): TGPRegion;
    destructor destroy; override;
    function Clone: TGPRegion;
    function MakeInfinite: TStatus;
    function MakeEmpty: TStatus;
    function GetDataSize: UINT;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    function GetData(buffer: PBYTE; bufferSize: UINT;
      sizeFilled: PUINT = nil): TStatus;
    function Intersect(const Rect: TGPRect): TStatus; overload;
    function Intersect(const Rect: TGPRectF): TStatus; overload;
    function Intersect(path: TGPGraphicsPath): TStatus; overload;
    function Intersect(region: TGPRegion): TStatus; overload;
    function Union(const Rect: TGPRect): TStatus; overload;
    function Union(const Rect: TGPRectF): TStatus; overload;
    function Union(path: TGPGraphicsPath): TStatus; overload;
    function Union(region: TGPRegion): TStatus; overload;
    function Xor_(const Rect: TGPRect): TStatus; overload;
    function Xor_(const Rect: TGPRectF): TStatus; overload;
    function Xor_(path: TGPGraphicsPath): TStatus; overload;
    function Xor_(region: TGPRegion): TStatus; overload;
    function Exclude(const Rect: TGPRect): TStatus; overload;
    function Exclude(const Rect: TGPRectF): TStatus; overload;
    function Exclude(path: TGPGraphicsPath): TStatus; overload;
    function Exclude(region: TGPRegion): TStatus; overload;
    function Complement(const Rect: TGPRect): TStatus; overload;
    function Complement(const Rect: TGPRectF): TStatus; overload;
    function Complement(path: TGPGraphicsPath): TStatus; overload;
    function Complement(region: TGPRegion): TStatus; overload;
    function Translate(dx, dy: Single): TStatus; overload;
    function Translate(dx, dy: Integer): TStatus; overload;
    function Transform(matrix: TGPMatrix): TStatus;
    function GetBounds(out Rect: TGPRect; G: TGPGraphics): TStatus; overload;
    function GetBounds(out Rect: TGPRectF; G: TGPGraphics): TStatus; overload;
    function GetHRGN(G: TGPGraphics): hRgn;
    function IsEmpty(G: TGPGraphics): BOOL;
    function IsInfinite(G: TGPGraphics): BOOL;
    function IsVisible(X, Y: Integer; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const point: TGPPoint; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(X, Y: Single; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const point: TGPPointF; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(X, Y, Width, Height: Integer; G: TGPGraphics): BOOL; overload;
    function IsVisible(const Rect: TGPRect; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(X, Y, Width, Height: Single; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const Rect: TGPRectF; G: TGPGraphics = nil): BOOL; overload;
    function Equals(region: TGPRegion; G: TGPGraphics): BOOL;
    function GetRegionScansCount(matrix: TGPMatrix): UINT;
    function GetRegionScans(matrix: TGPMatrix; rects: PGPRectF; out Count: Integer): TStatus; overload;
    function GetRegionScans(matrix: TGPMatrix; rects: PGPRect; out Count: Integer): TStatus; overload;
    function GetLastStatus: TStatus;
  end;

  //--------------------------------------------------------------------------
  // FontFamily
  //--------------------------------------------------------------------------

  TGPFontFamily = class(TGdiplusBase)
  protected
    nativeFamily: GpFontFamily;
    lastResult: TStatus;
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(nativeOrig: GpFontFamily;
      Status: TStatus); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(name: WideString; fontCollection: TGPFontCollection = nil); reintroduce; overload;
    destructor destroy; override;
    class function GenericSansSerif: TGPFontFamily;
    class function GenericSerif: TGPFontFamily;
    class function GenericMonospace: TGPFontFamily;
    function GetFamilyName(out name: string; language: LANGID = 0): TStatus;
    function Clone: TGPFontFamily;
    function IsAvailable: BOOL;
    function IsStyleAvailable(style: Integer): BOOL;
    function GetEmHeight(style: Integer): UINT16;
    function GetCellAscent(style: Integer): UINT16;
    function GetCellDescent(style: Integer): UINT16;
    function GetLineSpacing(style: Integer): UINT16;
    function GetLastStatus: TStatus;
  end;

  //--------------------------------------------------------------------------
  // Font Collection
  //--------------------------------------------------------------------------

  TGPFontCollection = class(TGdiplusBase)
  protected
    nativeFontCollection: GpFontCollection;
    lastResult: TStatus;
    function SetStatus(Status: TStatus): TStatus;
  public
    constructor Create;
    destructor destroy; override;
    function GetFamilyCount: Integer;
    function GetFamilies(numSought: Integer; out gpfamilies: array of TGPFontFamily;
      out numFound: Integer): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPInstalledFontCollection = class(TGPFontCollection)
  public
    constructor Create; reintroduce;
    destructor destroy; override;
  end;

  TGPPrivateFontCollection = class(TGPFontCollection)
  public
    constructor Create; reintroduce;
    destructor destroy; override;
    function AddFontFile(filename: WideString): TStatus;
    function AddMemoryFont(memory: Pointer; Length: Integer): TStatus;
  end;

  //--------------------------------------------------------------------------
  // TFont
  //--------------------------------------------------------------------------

  TGPFont = class(TGdiplusBase)
  protected
    nativeFont: GpFont;
    lastResult: TStatus;
    procedure SetNativeFont(font: GpFont);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(font: GpFont; Status: TStatus); overload;
  public
    constructor Create(hdc: hdc); reintroduce; overload;
    constructor Create(hdc: hdc; logfont: PLOGFONTA); reintroduce; overload;
    constructor Create(hdc: hdc; logfont: PLOGFONTW); reintroduce; overload;
    constructor Create(hdc: hdc; hfont: hfont); reintroduce; overload;
    constructor Create(family: TGPFontFamily; emSize: Single;
      style: TFontStyle = FontStyleRegular;
      Unit_: TUnit = UnitPoint); reintroduce; overload;
    constructor Create(familyName: WideString; emSize: Single;
      style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint;
      fontCollection: TGPFontCollection = nil); reintroduce; overload;
    function GetLogFontA(G: TGPGraphics; out logfontA: TLogFontA): TStatus;
    function GetLogFontW(G: TGPGraphics; out logfontW: TLogFontW): TStatus;
    function Clone: TGPFont;
    destructor destroy; override;
    function IsAvailable: BOOL;
    function GetStyle: Integer;
    function GetSize: Single;
    function GetUnit: TUnit;
    function GetLastStatus: TStatus;
    function GetHeight(graphics: TGPGraphics): Single; overload;
    function GetHeight(dpi: Single): Single; overload;
    function GetFamily(family: TGPFontFamily): TStatus;
  end;

  //--------------------------------------------------------------------------
  // Abstract base class for Image and Metafile
  //--------------------------------------------------------------------------

  TGPImage = class(TGdiplusBase)
  protected
    nativeImage: GpImage;
    lastResult: TStatus;
    loadStatus: TStatus;
    procedure SetNativeImage(nativeImage: GpImage);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(nativeImage: GpImage; Status: TStatus); reintroduce; overload;
  public
    constructor Create(filename: WideString; useEmbeddedColorManagement: BOOL = False); reintroduce; overload;
    constructor Create(stream: ISTREAM; useEmbeddedColorManagement: BOOL = False); reintroduce; overload;
    function FromFile(filename: WideString; useEmbeddedColorManagement: BOOL = False): TGPImage;
    function FromStream(stream: ISTREAM; useEmbeddedColorManagement: BOOL = False): TGPImage;
    destructor destroy; override;
    function Clone: TGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID;
      encoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(stream: ISTREAM; const clsidEncoder: TGUID;
      encoderParams: PEncoderParameters = nil): TStatus; overload;
    function SaveAdd(encoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(newImage: TGPImage; encoderParams: PEncoderParameters): TStatus; overload;
    function GetType: TImageType;
    function GetPhysicalDimension(out size: TGPSizeF): TStatus;
    function GetBounds(out srcRect: TGPRectF; out srcUnit: TUnit): TStatus;
    function GetWidth: UINT;
    function GetHeight: UINT;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetFlags: UINT;
    function GetRawFormat(out Format: TGUID): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(palette: PColorPalette; size: Integer): TStatus;
    function SetPalette(palette: PColorPalette): TStatus;
    function GetThumbnailImage(thumbWidth, thumbHeight: UINT;
      callback: GetThumbnailImageAbort = nil; callbackData: Pointer = nil): TGPImage;
    function GetFrameDimensionsCount: UINT;
    function GetFrameDimensionsList(dimensionIDs: PGUID; Count: UINT): TStatus;
    function GetFrameCount(const dimensionID: TGUID): UINT;
    function SelectActiveFrame(const dimensionID: TGUID; frameIndex: UINT): TStatus;
    function RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
    function GetPropertyCount: UINT;
    function GetPropertyIdList(numOfProperty: UINT; list: PPROPID): TStatus;
    function GetPropertyItemSize(PROPID: PROPID): UINT;
    function GetPropertyItem(PROPID: PROPID; propSize: UINT; buffer: PPropertyItem): TStatus;
    function GetPropertySize(out totalBufferSize, numProperties: UINT): TStatus;
    function GetAllPropertyItems(totalBufferSize, numProperties: UINT;
      allItems: PPropertyItem): TStatus;
    function RemovePropertyItem(PROPID: TPROPID): TStatus;
    function SetPropertyItem(const item: TPropertyItem): TStatus;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID): UINT;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: UINT;
      buffer: PEncoderParameters): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPBitmap = class(TGPImage)
  protected
    constructor Create(nativeBitmap: GpBitmap); reintroduce; overload;
  public
    constructor Create(filename: WideString; useEmbeddedColorManagement: BOOL = False); reintroduce; overload;
    constructor Create(stream: ISTREAM; useEmbeddedColorManagement: BOOL = False); reintroduce; overload;
    function FromFile(filename: WideString; useEmbeddedColorManagement: BOOL = False): TGPBitmap;
    function FromStream(stream: ISTREAM; useEmbeddedColorManagement: BOOL = False): TGPBitmap;
    constructor Create(Width, Height, Stride: Integer; Format: TPixelFormat; Scan0: PBYTE); reintroduce; overload;
    constructor Create(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppARGB); reintroduce; overload;
    constructor Create(Width, Height: Integer; target: TGPGraphics); reintroduce; overload;
    function Clone(Rect: TGPRect; Format: TPixelFormat): TGPBitmap; overload;
    function Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): TGPBitmap; overload;
    function Clone(Rect: TGPRectF; Format: TPixelFormat): TGPBitmap; overload;
    function Clone(X, Y, Width, Height: Single; Format: TPixelFormat): TGPBitmap; overload;
    function LockBits(Rect: TGPRect; Flags: UINT; Format: TPixelFormat; out lockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(var lockedBitmapData: TBitmapData): TStatus;
    function GetPixel(X, Y: Integer; out color: TGPColor): TStatus;
    function SetPixel(X, Y: Integer; color: TGPColor): TStatus;
    function SetResolution(xdpi, ydpi: Single): TStatus;
    constructor Create(surface: IDirectDrawSurface7); reintroduce; overload;
    constructor Create(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer); reintroduce; overload;
    constructor Create(hbm: HBITMAP; hpal: HPALETTE); reintroduce; overload;
    constructor Create(hicon: hicon); reintroduce; overload;
    constructor Create(hInstance: HMODULE; bitmapName: WideString); reintroduce; overload;
    function FromDirectDrawSurface7(surface: IDirectDrawSurface7): TGPBitmap;
    function FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer): TGPBitmap;
    function FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE): TGPBitmap;
    function FromHICON(hicon: hicon): TGPBitmap;
    function FromResource(hInstance: HMODULE; bitmapName: WideString): TGPBitmap;
    function GetHBITMAP(colorBackground: TGPColor; out hbmReturn: HBITMAP): TStatus;
    function GetHICON(out hicon: hicon): TStatus;
  end;

  TGPCustomLineCap = class(TGdiplusBase)
  protected
    nativeCap: GpCustomLineCap;
    lastResult: TStatus;
    procedure SetNativeCap(nativeCap: GpCustomLineCap);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(nativeCap: GpCustomLineCap;
      Status: TStatus); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(fillPath, strokePath: TGPGraphicsPath;
      baseCap: TLineCap = LineCapFlat;
      baseInset: Single = 0); reintroduce; overload;
    destructor destroy; override;
    function Clone: TGPCustomLineCap;
    function SetStrokeCap(strokeCap: TLineCap): TStatus;
    function SetStrokeCaps(startCap, endCap: TLineCap): TStatus;
    function GetStrokeCaps(out startCap, endCap: TLineCap): TStatus;
    function SetStrokeJoin(LineJoin: TLineJoin): TStatus;
    function GetStrokeJoin: TLineJoin;
    function SetBaseCap(baseCap: TLineCap): TStatus;
    function GetBaseCap: TLineCap;
    function SetBaseInset(inset: Single): TStatus;
    function GetBaseInset: Single;
    function SetWidthScale(widthScale: Single): TStatus;
    function GetWidthScale: Single;
    function GetLastStatus: TStatus;
  end;

  TGPCachedBitmap = class(TGdiplusBase)
  protected
    nativeCachedBitmap: GpCachedBitmap;
    lastResult: TStatus;
  public
    constructor Create(bitmap: TGPBitmap; graphics: TGPGraphics); reintroduce;
    destructor destroy; override;
    function GetLastStatus: TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ Image Attributes used with Graphics.DrawImage
  *
  * There are 5 possible sets of color adjustments:
  *          ColorAdjustDefault,
  *          ColorAdjustBitmap,
  *          ColorAdjustBrush,
  *          ColorAdjustPen,
  *          ColorAdjustText,
  *
  * Bitmaps, Brushes, Pens, and Text will all use any color adjustments
  * that have been set into the default ImageAttributes until their own
  * color adjustments have been set.  So as soon as any "Set" method is
  * called for Bitmaps, Brushes, Pens, or Text, then they start from
  * scratch with only the color adjustments that have been set for them.
  * Calling Reset removes any individual color adjustments for a type
  * and makes it revert back to using all the default color adjustments
  * (if any).  The SetToIdentity method is a way to force a type to
  * have no color adjustments at all, regardless of what previous adjustments
  * have been set for the defaults or for that type.
  *
  \********************************************************************F******)

  TGPImageAttributes = class(TGdiplusBase)
  protected
    nativeImageAttr: GpImageAttributes;
    lastResult: TStatus;
    procedure SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(imageattr: GpImageAttributes;
      Status: GpStatus); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    destructor destroy; override;
    function Clone: TGPImageAttributes;
    function SetToIdentity(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function Reset(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrix(const ColorMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrix(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrices(const ColorMatrix: TColorMatrix; const grayMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrices(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetThreshold(threshold: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearThreshold(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetGamma(gamma: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorKey(colorLow, colorHigh: TGPColor; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorKey(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannel(channelFlags: TColorChannelFlags; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannel(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannelColorProfile(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetRemapTable(mapSize: Cardinal; map: PColorMap; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearRemapTable(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetBrushRemapTable(mapSize: Cardinal; map: PColorMap): TStatus;
    function ClearBrushRemapTable: TStatus;
    function SetWrapMode(wrap: TWrapMode; color: TGPColor = aclBlack; clamp: BOOL = False): TStatus;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TStatus;
    function GetLastStatus: TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ Matrix class
  *
  \**************************************************************************)

  TMatrixArray = array[0..5] of Single;

  TGPMatrix = class(TGdiplusBase)
  protected
    nativeMatrix: GpMatrix;
    lastResult: GpStatus;
    procedure SetNativeMatrix(nativeMatrix: GpMatrix);
    function SetStatus(Status: GpStatus): TStatus;
    constructor Create(nativeMatrix: GpMatrix); reintroduce; overload;
  public
    // Default constructor is set to identity matrix.
    constructor Create; reintroduce; overload;
    constructor Create(m11, m12, m21, m22, dx, dy: Single); reintroduce; overload;
    constructor Create(const Rect: TGPRectF; const dstplg: TGPPointF); reintroduce; overload;
    constructor Create(const Rect: TGPRect; const dstplg: TGPPoint); reintroduce; overload;
    destructor destroy; override;
    function Clone: TGPMatrix;
    function GetElements(const m: TMatrixArray): TStatus;
    function SetElements(m11, m12, m21, m22, dx, dy: Single): TStatus;
    function offsetX: Single;
    function offsetY: Single;
    function Reset: TStatus;
    function Multiply(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Translate(offsetX, offsetY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function scale(scaleX, scaleY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Rotate(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function RotateAt(angle: Single; const center: TGPPointF; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Shear(shearX, shearY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Invert: TStatus;           // ok

    function TransformPoints(pts: PGPPointF; Count: Integer = 1): TStatus; overload;
    function TransformPoints(pts: PGPPoint; Count: Integer = 1): TStatus; overload;

    function TransformVectors(pts: PGPPointF; Count: Integer = 1): TStatus; overload;
    function TransformVectors(pts: PGPPoint; Count: Integer = 1): TStatus; overload;

    function IsInvertible: BOOL;
    function IsIdentity: BOOL;
    function Equals(matrix: TGPMatrix): BOOL;
    function GetLastStatus: TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ Brush class
  *
  \**************************************************************************)

    //--------------------------------------------------------------------------
    // Abstract base class for various brush types
    //--------------------------------------------------------------------------

  TGPBrush = class(TGdiplusBase)
  protected
    nativeBrush: GpBrush;
    lastResult: TStatus;
    procedure SetNativeBrush(nativeBrush: GpBrush);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(nativeBrush: GpBrush; Status: TStatus); overload;
  public
    constructor Create; overload;
    destructor destroy; override;
    function Clone: TGPBrush; virtual;
    function GetType: TBrushType;
    function GetLastStatus: TStatus;
  end;

  //--------------------------------------------------------------------------
  // Solid Fill Brush Object
  //--------------------------------------------------------------------------

  TGPSolidBrush = class(TGPBrush)
  public
    constructor Create(color: TGPColor); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function GetColor(out color: TGPColor): TStatus;
    function SetColor(color: TGPColor): TStatus;
  end;

  //--------------------------------------------------------------------------
  // Texture Brush Fill Object
  //--------------------------------------------------------------------------

  TGPTextureBrush = class(TGPBrush)
  public
    constructor Create(image: TGPImage; WrapMode: TWrapMode = WrapModeTile); reintroduce; overload;
    constructor Create(image: TGPImage; WrapMode: TWrapMode; dstrect: TGPRectF); reintroduce; overload;
    constructor Create(image: TGPImage; dstrect: TGPRectF; imageAttributes: TGPImageAttributes = nil); reintroduce; overload;
    constructor Create(image: TGPImage; dstrect: TGPRect; imageAttributes: TGPImageAttributes = nil); reintroduce; overload;
    constructor Create(image: TGPImage; WrapMode: TWrapMode; dstrect: TGPRect); reintroduce; overload;
    constructor Create(image: TGPImage; WrapMode: TWrapMode; dstx, dsty, dstwidth,
      dstheight: Single); reintroduce; overload;
    constructor Create(image: TGPImage; WrapMode: TWrapMode; dstx, dsty, dstwidth,
      dstheight: Integer); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: MatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
    function GetImage: TGPImage;
  end;

  //--------------------------------------------------------------------------
  // Linear Gradient Brush Object
  //--------------------------------------------------------------------------

  TGPLinearGradientBrush = class(TGPBrush)
  public
    constructor Create; reintroduce; overload;
    constructor Create(const point1, point2: TGPPointF; color1,
      color2: TGPColor); reintroduce; overload;
    constructor Create(const point1, point2: TGPPoint; color1,
      color2: TGPColor); reintroduce; overload;
    constructor Create(Rect: TGPRectF; color1, color2: TGPColor;
      mode: TLinearGradientMode); reintroduce; overload;
    constructor Create(Rect: TGPRect; color1, color2: TGPColor;
      mode: TLinearGradientMode); reintroduce; overload;
    constructor Create(Rect: TGPRectF; color1, color2: TGPColor; angle: Single;
      isAngleScalable: BOOL = False); overload;
    constructor Create(Rect: TGPRect; color1, color2: TGPColor; angle: Single;
      isAngleScalable: BOOL = False); overload;
    function SetLinearColors(color1, color2: TGPColor): TStatus;
    function GetLinearColors(out color1, color2: TGPColor): TStatus;
    function GetRectangle(out Rect: TGPRectF): TStatus; overload;
    function GetRectangle(out Rect: TGPRect): TStatus; overload;
    function SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
    function GetGammaCorrection: BOOL;
    function GetBlendCount: Integer;
    function SetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
    function GetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(presetColors: PGPColor; blendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColors(presetColors: PGPColor; blendPositions: PSingle; Count: Integer): TStatus;
    function SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
    function SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
  end;

  //--------------------------------------------------------------------------
  // Hatch Brush Object
  //--------------------------------------------------------------------------

  TGPHatchBrush = class(TGPBrush)
  public
    constructor Create; reintroduce; overload;
    constructor Create(HatchStyle: THatchStyle; foreColor: TGPColor; backColor: TGPColor = aclBlack); reintroduce; overload; // ok
    function GetHatchStyle: THatchStyle;
    function GetForegroundColor(out color: TGPColor): TStatus;
    function GetBackgroundColor(out color: TGPColor): TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ Pen class
  *
  \**************************************************************************)

  //--------------------------------------------------------------------------
  // Pen class
  //--------------------------------------------------------------------------

  TGPPen = class(TGdiplusBase)
  protected
    nativePen: GpPen;
    lastResult: TStatus;
    procedure SetNativePen(nativePen: GpPen);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(nativePen: GpPen; Status: TStatus); reintroduce; overload;
  public
    constructor Create(color: TGPColor; Width: Single = 1.0); reintroduce; overload;
    constructor Create(brush: TGPBrush; Width: Single = 1.0); reintroduce; overload;
    destructor destroy; override;
    function Clone: TGPPen;
    function SetWidth(Width: Single): TStatus;
    function GetWidth: Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function SetLineCap(startCap, endCap: TLineCap; DashCap: TDashCap): TStatus;
    function SetStartCap(startCap: TLineCap): TStatus;
    function SetEndCap(endCap: TLineCap): TStatus;
    function SetDashCap(DashCap: TDashCap): TStatus;
    function GetStartCap: TLineCap;
    function GetEndCap: TLineCap;
    function GetDashCap: TDashCap;
    function SetLineJoin(LineJoin: TLineJoin): TStatus;
    function GetLineJoin: TLineJoin;
    function SetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
    function GetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
    function SetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
    function GetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
    function SetMiterLimit(miterLimit: Single): TStatus;
    function GetMiterLimit: Single;
    function SetAlignment(PenAlignment: TPenAlignment): TStatus;
    function GetAlignment: TPenAlignment;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetPenType: TPenType;
    function SetColor(color: TGPColor): TStatus;
    function SetBrush(brush: TGPBrush): TStatus;
    function GetColor(out color: TGPColor): TStatus;
    function GetBrush: TGPBrush;
    function GetDashStyle: TDashStyle;
    function SetDashStyle(DashStyle: TDashStyle): TStatus;
    function GetDashOffset: Single;
    function SetDashOffset(dashOffset: Single): TStatus;
    function SetDashPattern(dashArray: PSingle; Count: Integer): TStatus;
    function GetDashPatternCount: Integer;
    function GetDashPattern(dashArray: PSingle; Count: Integer): TStatus;
    function SetCompoundArray(compoundArray: PSingle; Count: Integer): TStatus;
    function GetCompoundArrayCount: Integer;
    function GetCompoundArray(compoundArray: PSingle; Count: Integer): TStatus;
    function GetLastStatus: TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ StringFormat class
  *
  \**************************************************************************)

  TGPStringFormat = class(TGdiplusBase)
  protected
    nativeFormat: GpStringFormat;
    lastError: TStatus;
    function SetStatus(newStatus: GpStatus): TStatus;
    procedure Assign(source: TGPStringFormat);
    constructor Create(clonedStringFormat: GpStringFormat; Status: TStatus); reintroduce; overload;
  public
    constructor Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL); reintroduce; overload;
    constructor Create(Format: TGPStringFormat); reintroduce; overload;
    destructor destroy; override;
    class function GenericDefault: TGPStringFormat;
    class function GenericTypographic: TGPStringFormat;
    function Clone: TGPStringFormat;
    function SetFormatFlags(Flags: Integer): TStatus;
    function GetFormatFlags: Integer;
    function SetAlignment(align: TStringAlignment): TStatus;
    function GetAlignment: TStringAlignment;
    function SetLineAlignment(align: TStringAlignment): TStatus;
    function GetLineAlignment: TStringAlignment;
    function SetHotkeyPrefix(HotkeyPrefix: THotkeyPrefix): TStatus;
    function GetHotkeyPrefix: THotkeyPrefix;
    function SetTabStops(firstTabOffset: Single; Count: Integer; tabStops: PSingle): TStatus;
    function GetTabStopCount: Integer;
    function GetTabStops(Count: Integer; firstTabOffset, tabStops: PSingle): TStatus;
    function SetDigitSubstitution(language: LANGID; substitute: TStringDigitSubstitute): TStatus;
    function GetDigitSubstitutionLanguage: LANGID;
    function GetDigitSubstitutionMethod: TStringDigitSubstitute;
    function SetTrimming(trimming: TStringTrimming): TStatus;
    function GetTrimming: TStringTrimming;
    function SetMeasurableCharacterRanges(rangeCount: Integer; ranges: PCharacterRange): TStatus;
    function GetMeasurableCharacterRangeCount: Integer;
    function GetLastStatus: TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ Graphics Path class
  *
  \**************************************************************************)

  TGPGraphicsPath = class(TGdiplusBase)
  protected
    nativePath: GpPath;
    lastResult: TStatus;
    procedure SetNativePath(nativePath: GpPath);
    function SetStatus(Status: TStatus): TStatus;
    constructor Create(nativePath: GpPath); reintroduce; overload;
  public
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create(FillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    constructor Create(Points: PGPPointF; Types: PBYTE; Count: Integer;
      FillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    constructor Create(Points: PGPPoint; Types: PBYTE; Count: Integer;
      FillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    destructor destroy; override;
    function Clone: TGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function Reset: TStatus;
    function GetFillMode: TFillMode;
    function SetFillMode(FillMode: TFillMode): TStatus;
    function GetPathData(pathData: TPathData): TStatus;
    function StartFigure: TStatus;
    function CloseFigure: TStatus;
    function CloseAllFigures: TStatus;
    function SetMarker: TStatus;
    function ClearMarkers: TStatus;
    function Reverse: TStatus;
    function GetLastPoint(out lastPoint: TGPPointF): TStatus;

    function AddLine(const pt1, pt2: TGPPointF): TStatus; overload;
    function AddLine(x1, y1, x2, y2: Single): TStatus; overload;
    function AddLines(Points: PGPPointF; Count: Integer): TStatus; overload;
    function AddLine(const pt1, pt2: TGPPoint): TStatus; overload;
    function AddLine(x1, y1, x2, y2: Integer): TStatus; overload;
    function AddLines(Points: PGPPoint; Count: Integer): TStatus; overload;

    function AddArc(Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function AddArc(X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus; overload;
    function AddArc(Rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function AddArc(X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    function AddBezier(pt1, pt2, pt3, pt4: TGPPointF): TStatus; overload;
    function AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus; overload;
    function AddBeziers(Points: PGPPointF; Count: Integer): TStatus; overload;
    function AddBezier(pt1, pt2, pt3, pt4: TGPPoint): TStatus; overload;
    function AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus; overload;
    function AddBeziers(Points: PGPPoint; Count: Integer): TStatus; overload;

    function AddCurve(Points: PGPPointF; Count: Integer): TStatus; overload;
    function AddCurve(Points: PGPPointF; Count: Integer; tension: Single): TStatus; overload;
    function AddCurve(Points: PGPPointF; Count, offset, numberOfSegments: Integer; tension: Single): TStatus; overload;
    function AddCurve(Points: PGPPoint; Count: Integer): TStatus; overload;
    function AddCurve(Points: PGPPoint; Count: Integer; tension: Single): TStatus; overload;
    function AddCurve(Points: PGPPoint; Count, offset, numberOfSegments: Integer; tension: Single): TStatus; overload;

    function AddClosedCurve(Points: PGPPointF; Count: Integer): TStatus; overload;
    function AddClosedCurve(Points: PGPPointF; Count: Integer; tension: Single): TStatus; overload;
    function AddClosedCurve(Points: PGPPoint; Count: Integer): TStatus; overload;
    function AddClosedCurve(Points: PGPPoint; Count: Integer; tension: Single): TStatus; overload;

    function AddRectangle(Rect: TGPRectF): TStatus; overload;
    function AddRectangles(rects: PGPRectF; Count: Integer): TStatus; overload;
    function AddRectangle(Rect: TGPRect): TStatus; overload;
    function AddRectangles(rects: PGPRect; Count: Integer): TStatus; overload;

    function AddEllipse(Rect: TGPRectF): TStatus; overload;
    function AddEllipse(X, Y, Width, Height: Single): TStatus; overload;
    function AddEllipse(Rect: TGPRect): TStatus; overload;
    function AddEllipse(X, Y, Width, Height: Integer): TStatus; overload;

    function AddPie(Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function AddPie(X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus; overload;
    function AddPie(Rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function AddPie(X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    function AddPolygon(Points: PGPPointF; Count: Integer): TStatus; overload;
    function AddPolygon(Points: PGPPoint; Count: Integer): TStatus; overload;

    function AddPath(addingPath: TGPGraphicsPath; connect: BOOL): TStatus;

    function AddString(string_: WideString; Length: Integer; family: TGPFontFamily;
      style: Integer; emSize: Single; origin: TGPPointF; Format: TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; Length: Integer; family: TGPFontFamily;
      style: Integer; emSize: Single; layoutRect: TGPRectF; Format: TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; Length: Integer; family: TGPFontFamily;
      style: Integer; emSize: Single; origin: TGPPoint; Format: TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; Length: Integer; family: TGPFontFamily;
      style: Integer; emSize: Single; layoutRect: TGPRect; Format: TGPStringFormat): TStatus; overload;

    function Transform(matrix: TGPMatrix): TStatus;

    // This is not always the tightest bounds.
    function GetBounds(out bounds: TGPRectF; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus; overload;
    function GetBounds(out bounds: TGPRect; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus; overload;
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function Flatten(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
    function Widen(pen: TGPPen; matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
    function Outline(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function Warp(destPoints: PGPPointF; Count: Integer; srcRect: TGPRectF;
      matrix: TGPMatrix = nil; WarpMode: TWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault): TStatus;
    function GetPointCount: Integer;
    function GetPathTypes(Types: PBYTE; Count: Integer): TStatus;
    function GetPathPoints(Points: PGPPointF; Count: Integer): TStatus; overload;
    function GetPathPoints(Points: PGPPoint; Count: Integer): TStatus; overload;
    function GetLastStatus: TStatus;

    function IsVisible(point: TGPPointF; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(X, Y: Single; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(point: TGPPoint; G: TGPGraphics = nil): BOOL; overload;
    function IsVisible(X, Y: Integer; G: TGPGraphics = nil): BOOL; overload;

    function IsOutlineVisible(point: TGPPointF; pen: TGPPen; G: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(X, Y: Single; pen: TGPPen; G: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(point: TGPPoint; pen: TGPPen; G: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(X, Y: Integer; pen: TGPPen; G: TGPGraphics = nil): BOOL; overload;
  end;

  //--------------------------------------------------------------------------
  // GraphisPathIterator class
  //--------------------------------------------------------------------------

  TGPGraphicsPathIterator = class(TGdiplusBase)
  protected
    nativeIterator: GpPathIterator;
    lastResult: TStatus;
    procedure SetNativeIterator(nativeIterator: GpPathIterator);
    function SetStatus(Status: TStatus): TStatus;
  public
    constructor Create(path: TGPGraphicsPath); reintroduce;
    destructor destroy; override;
    function NextSubpath(out startIndex, endIndex: Integer; out isClosed: BOOL): Integer; overload;
    function NextSubpath(path: TGPGraphicsPath; out isClosed: BOOL): Integer; overload;
    function NextPathType(out pathType: TPathPointType; out startIndex, endIndex: Integer): Integer;
    function NextMarker(out startIndex, endIndex: Integer): Integer; overload;
    function NextMarker(path: TGPGraphicsPath): Integer; overload;
    function GetCount: Integer;
    function GetSubpathCount: Integer;
    function hasCurve: BOOL;
    procedure Rewind;
    function Enumerate(Points: PGPPointF; Types: PBYTE; Count: Integer): Integer;
    function CopyData(Points: PGPPointF; Types: PBYTE; startIndex, endIndex: Integer): Integer;
    function GetLastStatus: TStatus;
  end;

  //--------------------------------------------------------------------------
  // Path Gradient Brush
  //--------------------------------------------------------------------------

  TGPPathGradientBrush = class(TGPBrush)
  public
    constructor Create(Points: PGPPointF; Count: Integer;
      WrapMode: TWrapMode = WrapModeClamp); reintroduce; overload;
    constructor Create(Points: PGPPoint; Count: Integer;
      WrapMode: TWrapMode = WrapModeClamp); reintroduce; overload;
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function GetCenterColor(out color: TGPColor): TStatus;
    function SetCenterColor(color: TGPColor): TStatus;
    function GetPointCount: Integer;
    function GetSurroundColorCount: Integer;
    function GetSurroundColors(colors: PARGB; var Count: Integer): TStatus;
    function SetSurroundColors(colors: PARGB; var Count: Integer): TStatus;
    function GetGraphicsPath(path: TGPGraphicsPath): TStatus;
    function SetGraphicsPath(path: TGPGraphicsPath): TStatus;
    function GetCenterPoint(out point: TGPPointF): TStatus; overload;
    function GetCenterPoint(out point: TGPPoint): TStatus; overload;
    function SetCenterPoint(point: TGPPointF): TStatus; overload;
    function SetCenterPoint(point: TGPPoint): TStatus; overload;
    function GetRectangle(out Rect: TGPRectF): TStatus; overload;
    function GetRectangle(out Rect: TGPRect): TStatus; overload;
    function SetGammaCorrection(useGammaCorrection: BOOL): TStatus; overload;
    function GetGammaCorrection: BOOL; overload;
    function GetBlendCount: Integer;
    function GetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
    function SetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(presetColors: PARGB; blendPositions: PSingle;
      Count: Integer): TStatus;
    function GetInterpolationColors(presetColors: PARGB;
      blendPositions: PSingle; Count: Integer): TStatus;
    function SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
    function SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetFocusScales(out xScale, yScale: Single): TStatus;
    function SetFocusScales(xScale, yScale: Single): TStatus;
    function GetWrapMode: TWrapMode;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
  end;

  (**************************************************************************\
  *
  *   GDI+ Graphics Object
  *
  \**************************************************************************)

  TGPGraphics = class(TGdiplusBase)
  protected
    nativeGraphics: GpGraphics;
    lastResult: TStatus;
    procedure SetNativeGraphics(graphics: GpGraphics);
    function SetStatus(Status: TStatus): TStatus;
    function GetNativeGraphics: GpGraphics;
    function GetNativePen(pen: TGPPen): GpPen;
    constructor Create(graphics: GpGraphics); reintroduce; overload;
  public
    function FromHDC(hdc: hdc): TGPGraphics; overload;
    function FromHDC(hdc: hdc; hDevice: THandle): TGPGraphics; overload;
    function FromHWND(HWND: HWND; icm: BOOL = False): TGPGraphics;
    function FromImage(image: TGPImage): TGPGraphics;
    constructor Create(hdc: hdc); reintroduce; overload;
    constructor Create(hdc: hdc; hDevice: THandle); reintroduce; overload;
    constructor Create(HWND: HWND; icm: BOOL { = FALSE}); reintroduce; overload;
    constructor Create(image: TGPImage); reintroduce; overload;
    destructor destroy; override;
    procedure Flush(intention: TFlushIntention = FlushIntentionFlush);
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function GetHDC: hdc;
    procedure ReleaseHDC(hdc: hdc);
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    function SetRenderingOrigin(X, Y: Integer): TStatus;
    function GetRenderingOrigin(out X, Y: Integer): TStatus;
    function SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
    function GetCompositingMode: TCompositingMode;
    function SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
    function GetCompositingQuality: TCompositingQuality;
    function SetTextRenderingHint(newMode: TTextRenderingHint): TStatus;
    function GetTextRenderingHint: TTextRenderingHint;
    function SetTextContrast(contrast: UINT): TStatus; // 0..12
    function GetTextContrast: UINT;
    function GetInterpolationMode: TInterpolationMode;
    function SetInterpolationMode(InterpolationMode: TInterpolationMode): TStatus;
    function GetSmoothingMode: TSmoothingMode;
    function SetSmoothingMode(SmoothingMode: TSmoothingMode): TStatus;
    function GetPixelOffsetMode: TPixelOffsetMode;
    function SetPixelOffsetMode(PixelOffsetMode: TPixelOffsetMode): TStatus;
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    function SetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function SetPageUnit(Unit_: TUnit): TStatus;
    function SetPageScale(scale: Single): TStatus;
    function GetPageUnit: TUnit;
    function GetPageScale: Single;
    function GetDpiX: Single;
    function GetDpiY: Single;
    function TransformPoints(destSpace: TCoordinateSpace; srcSpace: TCoordinateSpace;
      pts: PGPPointF; Count: Integer): TStatus; overload;
    function TransformPoints(destSpace: TCoordinateSpace; srcSpace: TCoordinateSpace;
      pts: PGPPoint; Count: Integer): TStatus; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    function GetNearestColor(var color: TGPColor): TStatus;

    // DrawLine(s)
    function DrawLine(pen: TGPPen; x1, y1, x2, y2: Single): TStatus; overload;
    function DrawLine(pen: TGPPen; const pt1, pt2: TGPPointF): TStatus; overload;
    function DrawLines(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus; overload;
    function DrawLine(pen: TGPPen; x1, y1, x2, y2: Integer): TStatus; overload;
    function DrawLine(pen: TGPPen; const pt1, pt2: TGPPoint): TStatus; overload;
    function DrawLines(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus; overload;

    // DrawArc
    function DrawArc(pen: TGPPen; X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; const Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; const Rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;

    // DrawBezier(s)
    function DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus; overload;
    function DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPointF): TStatus; overload;
    function DrawBeziers(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus; overload;
    function DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus; overload;
    function DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPoint): TStatus; overload;
    function DrawBeziers(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus; overload;

    // DrawRectangle(s)
    function DrawRectangle(pen: TGPPen; const Rect: TGPRectF): TStatus; overload;
    function DrawRectangle(pen: TGPPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawRectangles(pen: TGPPen; rects: PGPRectF; Count: Integer): TStatus; overload;
    function DrawRectangle(pen: TGPPen; const Rect: TGPRect): TStatus; overload;
    function DrawRectangle(pen: TGPPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawRectangles(pen: TGPPen; rects: PGPRect; Count: Integer): TStatus; overload;

    // DrawEllipse
    function DrawEllipse(pen: TGPPen; const Rect: TGPRectF): TStatus; overload;
    function DrawEllipse(pen: TGPPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawEllipse(pen: TGPPen; const Rect: TGPRect): TStatus; overload;
    function DrawEllipse(pen: TGPPen; X, Y, Width, Height: Integer): TStatus; overload;

    // DrawPie
    function DrawPie(pen: TGPPen; const Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; const Rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    // DrawPolygon
    function DrawPolygon(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus; overload;
    function DrawPolygon(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus; overload;

    // DrawPath
    function DrawPath(pen: TGPPen; path: TGPGraphicsPath): TStatus;

    // DrawCurve
    function DrawCurve(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus; overload;
    function DrawCurve(pen: TGPPen; Points: PGPPointF; Count: Integer; tension: Single): TStatus; overload;
    function DrawCurve(pen: TGPPen; Points: PGPPointF; Count, offset,
      numberOfSegments: Integer; tension: Single = 0.5): TStatus; overload;
    function DrawCurve(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus; overload;
    function DrawCurve(pen: TGPPen; Points: PGPPoint; Count: Integer; tension: Single): TStatus; overload;
    function DrawCurve(pen: TGPPen; Points: PGPPoint; Count, offset, numberOfSegments: Integer;
      tension: Single = 0.5): TStatus; overload;

    // DrawClosedCurve
    function DrawClosedCurve(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; Points: PGPPointF; Count: Integer; tension: Single): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; Points: PGPPoint; Count: Integer; tension: Single): TStatus; overload;

    // Clear
    function Clear(color: TGPColor): TStatus;

    // FillRectangle(s)
    function FillRectangle(brush: TGPBrush; const Rect: TGPRectF): TStatus; overload;
    function FillRectangle(brush: TGPBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillRectangles(brush: TGPBrush; rects: PGPRectF; Count: Integer): TStatus; overload;
    function FillRectangle(brush: TGPBrush; const Rect: TGPRect): TStatus; overload;
    function FillRectangle(brush: TGPBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillRectangles(brush: TGPBrush; rects: PGPRect; Count: Integer): TStatus; overload;

    // FillPolygon
    function FillPolygon(brush: TGPBrush; Points: PGPPointF; Count: Integer): TStatus; overload;
    function FillPolygon(brush: TGPBrush; Points: PGPPointF; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillPolygon(brush: TGPBrush; Points: PGPPoint; Count: Integer): TStatus; overload;
    function FillPolygon(brush: TGPBrush; Points: PGPPoint; Count: Integer; FillMode: TFillMode): TStatus; overload;

    // FillEllipse
    function FillEllipse(brush: TGPBrush; const Rect: TGPRectF): TStatus; overload;
    function FillEllipse(brush: TGPBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillEllipse(brush: TGPBrush; const Rect: TGPRect): TStatus; overload;
    function FillEllipse(brush: TGPBrush; X, Y, Width, Height: Integer): TStatus; overload;

    // FillPie
    function FillPie(brush: TGPBrush; const Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; const Rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    // FillPath
    function fillPath(brush: TGPBrush; path: TGPGraphicsPath): TStatus;

    // FillClosedCurve
    function FillClosedCurve(brush: TGPBrush; Points: PGPPointF; Count: Integer): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; Points: PGPPointF; Count: Integer;
      FillMode: TFillMode; tension: Single = 0.5): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; Points: PGPPoint; Count: Integer): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; Points: PGPPoint; Count: Integer;
      FillMode: TFillMode; tension: Single = 0.5): TStatus; overload;

    // FillRegion
    function FillRegion(brush: TGPBrush; region: TGPRegion): TStatus;

    // DrawString
    function DrawString(string_: WideString; Length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus; overload;
    function DrawString(string_: WideString; Length: Integer; font: TGPFont;
      const origin: TGPPointF; brush: TGPBrush): TStatus; overload;
    function DrawString(string_: WideString; Length: Integer; font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus; overload;

    // MeasureString
    function MeasureString(string_: WideString; Length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat; out BoundingBox: TGPRectF;
      codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(string_: WideString; Length: Integer; font: TGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: TGPStringFormat; out size: TGPSizeF;
      codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(string_: WideString; Length: Integer; font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat;
      out BoundingBox: TGPRectF): TStatus; overload;
    function MeasureString(string_: WideString; Length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; out BoundingBox: TGPRectF): TStatus; overload;
    function MeasureString(string_: WideString; Length: Integer; font: TGPFont;
      const origin: TGPPointF; out BoundingBox: TGPRectF): TStatus; overload;

    // MeasureCharacterRanges
    function MeasureCharacterRanges(string_: WideString; Length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat; regionCount: Integer;
      const regions: array of TGPRegion): TStatus; overload;

    // DrawDriverString
    function DrawDriverString(text: PUINT16; Length: Integer; font: TGPFont;
      brush: TGPBrush; positions: PGPPointF; Flags: Integer; matrix: TGPMatrix): TStatus;

    // MeasureDriverString
    function MeasureDriverString(text: PUINT16; Length: Integer; font: TGPFont;
      positions: PGPPointF; Flags: Integer; matrix: TGPMatrix;
      out BoundingBox: TGPRectF): TStatus;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function DrawCachedBitmap(cb: TGPCachedBitmap; X, Y: Integer): TStatus;
    function DrawImage(image: TGPImage; const point: TGPPointF): TStatus; overload;
    function DrawImage(image: TGPImage; X, Y: Single): TStatus; overload;
    function DrawImage(image: TGPImage; const Rect: TGPRectF): TStatus; overload;
    function DrawImage(image: TGPImage; X, Y, Width, Height: Single): TStatus; overload;
    function DrawImage(image: TGPImage; const point: TGPPoint): TStatus; overload;
    function DrawImage(image: TGPImage; X, Y: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; const Rect: TGPRect): TStatus; overload;
    function DrawImage(image: TGPImage; X, Y, Width, Height: Integer): TStatus; overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    //     destPoints[0] <=> top-left corner of the source rectangle
    //     destPoints[1] <=> top-right corner
    //     destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    //     destPoints[3] <=> bottom-right corner
    function DrawImage(image: TGPImage; destPoints: PGPPointF; Count: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPoint; Count: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; X, Y, srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit): TStatus; overload;
    function DrawImage(image: TGPImage; const destRect: TGPRectF; srcx, srcy,
      srcwidth, srcheight: Single; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPointF; Count: Integer;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;
    function DrawImage(image: TGPImage; X, Y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TUnit): TStatus; overload;
    function DrawImage(image: TGPImage; const destRect: TGPRect; srcx, srcy,
      srcwidth, srcheight: Integer; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPoint;
      Count, srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPoint;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRect;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
      Count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
      Count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
      const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil
      ): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPoint;
      const srcRect: TGPRect; srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil
      ): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
      const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect, srcRect: TGPRect;
      srcUnit: TUnit; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
      Count: Integer; const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
      Count: Integer; const srcRect: TGPRect; srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus; overload;

    // SetClip
    function SetClip(G: TGPGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TGPRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TGPRect; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(path: TGPGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(region: TGPRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function SetClip(hRgn: hRgn; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;

    // IntersectClip
    function IntersectClip(const Rect: TGPRectF): TStatus; overload;
    function IntersectClip(const Rect: TGPRect): TStatus; overload;
    function IntersectClip(region: TGPRegion): TStatus; overload;
    // ExcludeClip
    function ExcludeClip(const Rect: TGPRectF): TStatus; overload;
    function ExcludeClip(const Rect: TGPRect): TStatus; overload;
    function ExcludeClip(region: TGPRegion): TStatus; overload;

    function ResetClip: TStatus;

    function TranslateClip(dx, dy: Single): TStatus; overload;
    function TranslateClip(dx, dy: Integer): TStatus; overload;

    function GetClip(region: TGPRegion): TStatus;

    function GetClipBounds(out Rect: TGPRectF): TStatus; overload;
    function GetClipBounds(out Rect: TGPRect): TStatus; overload;

    function IsClipEmpty: BOOL;

    function GetVisibleClipBounds(out Rect: TGPRectF): TStatus; overload;
    function GetVisibleClipBounds(out Rect: TGPRect): TStatus; overload;

    function IsVisibleClipEmpty: BOOL;

    function IsVisible(X, Y: Integer): BOOL; overload;
    function IsVisible(const point: TGPPoint): BOOL; overload;
    function IsVisible(X, Y, Width, Height: Integer): BOOL; overload;
    function IsVisible(const Rect: TGPRect): BOOL; overload;
    function IsVisible(X, Y: Single): BOOL; overload;
    function IsVisible(const point: TGPPointF): BOOL; overload;
    function IsVisible(X, Y, Width, Height: Single): BOOL; overload;
    function IsVisible(const Rect: TGPRectF): BOOL; overload;

    function Save: GraphicsState;
    function Restore(gstate: GraphicsState): TStatus;

    function BeginContainer(const dstrect, srcRect: TGPRectF; Unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer(const dstrect, srcRect: TGPRect; Unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer: GraphicsContainer; overload;
    function EndContainer(state: GraphicsContainer): TStatus;

    // Only valid when recording metafiles.
    function AddMetafileComment(data: PBYTE; sizeData: UINT): TStatus;

    function GetHalftonePalette: HPALETTE;
    function GetLastStatus: TStatus;
  end;


  (**************************************************************************\
  *
  *   GDI+ CustomLineCap APIs
  *
  \**************************************************************************)

  TGPAdjustableArrowCap = class(TGPCustomLineCap)
  public
    constructor Create(Height, Width: Single; isFilled: BOOL = True);
    function SetHeight(Height: Single): TStatus;
    function GetHeight: Single;
    function SetWidth(Width: Single): TStatus;
    function GetWidth: Single;
    function SetMiddleInset(middleInset: Single): TStatus;
    function GetMiddleInset: Single;
    function SetFillState(isFilled: BOOL): TStatus;
    function isFilled: BOOL;
  end;

  (**************************************************************************\
  *
  *   GDI+ Metafile class
  *
  \**************************************************************************)

  TGPMetafile = class(TGPImage)
  public
    // Playback a metafile from a HMETAFILE
    // If deleteWmf is TRUE, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hWmf: HMETAFILE; var WmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      deleteWmf: BOOL = False); overload;
    // Playback a metafile from a HENHMETAFILE
    // If deleteEmf is TRUE, then when the metafile is deleted,
    // the hEmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hEmf: HENHMETAFILE; deleteEmf: BOOL = False); overload;
    constructor Create(filename: WideString); overload;
    // Playback a WMF metafile from a file.
    constructor Create(filename: WideString; var WmfPlaceableFileHeader: TWmfPlaceableFileHeader); overload;
    constructor Create(stream: ISTREAM); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: hdc; type_: TEmfType = EmfTypeEmfPlusDual;
      description: PWCHAR = nil); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: hdc; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: hdc; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(filename: WideString; referenceHdc: hdc;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(filename: WideString; referenceHdc: hdc; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(filename: WideString; referenceHdc: hdc; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(stream: ISTREAM; referenceHdc: hdc;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(stream: ISTREAM; referenceHdc: hdc; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(stream: ISTREAM; referenceHdc: hdc; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create; reintroduce; overload;
    function GetMetafileHeader(hWmf: HMETAFILE; var WmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      Header: TMetafileHeader): TStatus; overload;
    function GetMetafileHeader(hEmf: HENHMETAFILE; Header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(filename: WideString; Header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(stream: ISTREAM; Header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(Header: TMetafileHeader): TStatus; overload; // ok
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.                                              // ok
    function GetHENHMETAFILE: HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    function PlayRecord(recordType: TEmfPlusRecordType; Flags, dataSize: UINT; data: PBYTE): TStatus;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: UINT): TStatus;
    function GetDownLevelRasterizationLimit: UINT;
    function EmfToWmfBits(hEmf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
  end;

  ////////////////////////////////////////////////////////////////////////////////

var
  GenericSansSerifFontFamily: TGPFontFamily = nil;
  GenericSerifFontFamily: TGPFontFamily = nil;
  GenericMonospaceFontFamily: TGPFontFamily = nil;

  GenericTypographicStringFormatBuffer: TGPStringFormat = nil;
  GenericDefaultStringFormatBuffer: TGPStringFormat = nil;

  StartupInput      : TGdiplusStartupInput;
  gdiplusToken      : ULONG;

  ////////////////////////////////////////////////////////////////////////////////

implementation

(**************************************************************************\
*
* Image Attributes
*
* Abstract:
*
*   GDI+ Image Attributes used with Graphics.DrawImage
*
* There are 5 possible sets of color adjustments:
*          ColorAdjustDefault,
*          ColorAdjustBitmap,
*          ColorAdjustBrush,
*          ColorAdjustPen,
*          ColorAdjustText,
*
* Bitmaps, Brushes, Pens, and Text will all use any color adjustments
* that have been set into the default ImageAttributes until their own
* color adjustments have been set.  So as soon as any "Set" method is
* called for Bitmaps, Brushes, Pens, or Text, then they start from
* scratch with only the color adjustments that have been set for them.
* Calling Reset removes any individual color adjustments for a type
* and makes it revert back to using all the default color adjustments
* (if any).  The SetToIdentity method is a way to force a type to
* have no color adjustments at all, regardless of what previous adjustments
* have been set for the defaults or for that type.
*
\********************************************************************F******)

constructor TGPImageAttributes.Create;
begin
  nativeImageAttr := nil;
  lastResult := GdipCreateImageAttributes(nativeImageAttr);
end;

destructor TGPImageAttributes.destroy;
begin
  GdipDisposeImageAttributes(nativeImageAttr);
  inherited destroy;
end;

function TGPImageAttributes.Clone: TGPImageAttributes;
var Clone           : GpImageAttributes;
begin
  SetStatus(GdipCloneImageAttributes(nativeImageAttr, Clone));
  Result := TGPImageAttributes.Create(Clone, lastResult);
end;

function TGPImageAttributes.SetToIdentity(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesToIdentity(nativeImageAttr, type_));
end;

function TGPImageAttributes.Reset(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipResetImageAttributes(nativeImageAttr, type_));
end;

function TGPImageAttributes.SetColorMatrix(const ColorMatrix: TColorMatrix;
  mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr,
    type_, True, @ColorMatrix, nil, mode));
end;

function TGPImageAttributes.ClearColorMatrix(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
    False, nil, nil, ColorMatrixFlagsDefault));
end;


function TGPImageAttributes.SetColorMatrices(const ColorMatrix: TColorMatrix;
  const grayMatrix: TColorMatrix; mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
    True, @ColorMatrix, @grayMatrix, mode));
end;

function TGPImageAttributes.ClearColorMatrices(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr,
    type_, False, nil, nil, ColorMatrixFlagsDefault));
end;

function TGPImageAttributes.SetThreshold(threshold: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(nativeImageAttr, type_,
    True, threshold));
end;

function TGPImageAttributes.ClearThreshold(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(nativeImageAttr, type_,
    False, 0.0));
end;

function TGPImageAttributes.SetGamma(gamma: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(nativeImageAttr, type_, True, gamma));
end;

function TGPImageAttributes.ClearGamma(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(nativeImageAttr, type_, False, 0.0));
end;

function TGPImageAttributes.SetNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(nativeImageAttr, type_, True));
end;

function TGPImageAttributes.ClearNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(nativeImageAttr, type_, False));
end;

function TGPImageAttributes.SetColorKey(colorLow, colorHigh: TGPColor;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(nativeImageAttr, type_,
    True, colorLow, colorHigh));
end;

function TGPImageAttributes.ClearColorKey(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(nativeImageAttr, type_,
    False, 0, 0));
end;

function TGPImageAttributes.SetOutputChannel(channelFlags: TColorChannelFlags;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(nativeImageAttr,
    type_, True, channelFlags));
end;

function TGPImageAttributes.ClearOutputChannel(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(nativeImageAttr,
    type_, False, ColorChannelFlagsLast));
end;

function TGPImageAttributes.SetOutputChannelColorProfile(colorProfileFilename: WideString;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(nativeImageAttr,
    type_, True, PWideChar(colorProfileFilename)));
end;

function TGPImageAttributes.ClearOutputChannelColorProfile(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(nativeImageAttr,
    type_, False, nil));
end;

function TGPImageAttributes.SetRemapTable(mapSize: Cardinal; map: PColorMap;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(nativeImageAttr, type_,
    True, mapSize, map));
end;

function TGPImageAttributes.ClearRemapTable(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(nativeImageAttr, type_,
    False, 0, nil));
end;

function TGPImageAttributes.SetBrushRemapTable(mapSize: Cardinal; map: PColorMap): TStatus;
begin
  Result := SetRemapTable(mapSize, map, ColorAdjustTypeBrush);
end;

function TGPImageAttributes.ClearBrushRemapTable: TStatus;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TGPImageAttributes.SetWrapMode(wrap: TWrapMode; color: TGPColor = aclBlack;
  clamp: BOOL = False): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesWrapMode(nativeImageAttr, wrap, color, clamp));
end;

// The flags of the palette are ignored.

function TGPImageAttributes.GetAdjustedPalette(ColorPalette: PColorPalette;
  ColorAdjustType: TColorAdjustType): TStatus;
begin
  Result := SetStatus(GdipGetImageAttributesAdjustedPalette(nativeImageAttr,
    ColorPalette, ColorAdjustType));
end;

function TGPImageAttributes.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPImageAttributes.Create(imageattr: GpImageAttributes; Status: TStatus);
begin
  SetNativeImageAttr(imageattr);
  lastResult := Status;
end;

procedure TGPImageAttributes.SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
begin
  Self.nativeImageAttr := nativeImageAttr;
end;

function TGPImageAttributes.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

(**************************************************************************\
*
*   GDI+ Matrix class
*
\**************************************************************************)

  // Default constructor is set to identity matrix.

constructor TGPMatrix.Create;
var matrix          : GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix(matrix);
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(m11, m12, m21, m22, dx, dy: Single);
var matrix          : GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix2(m11, m12, m21, m22, dx, dy, matrix);
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(const Rect: TGPRectF; const dstplg: TGPPointF);
var matrix          : GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix3(@Rect, @dstplg, matrix);
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(const Rect: TGPRect; const dstplg: TGPPoint);
var matrix          : GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix3I(@Rect, @dstplg, matrix);
  SetNativeMatrix(matrix);
end;

destructor TGPMatrix.destroy;
begin
  GdipDeleteMatrix(nativeMatrix);
end;

function TGPMatrix.Clone: TGPMatrix;
var cloneMatrix     : GpMatrix;
begin
  cloneMatrix := nil;
  SetStatus(GdipCloneMatrix(nativeMatrix, cloneMatrix));
  if (lastResult <> Ok) then begin
    Result := nil;
    exit;
  end;
  Result := TGPMatrix.Create(cloneMatrix);
end;

function TGPMatrix.GetElements(const m: TMatrixArray): TStatus;
begin
  Result := SetStatus(GdipGetMatrixElements(nativeMatrix, @m));
end;

function TGPMatrix.SetElements(m11, m12, m21, m22, dx, dy: Single): TStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(nativeMatrix,
    m11, m12, m21, m22, dx, dy));
end;

function TGPMatrix.offsetX: Single;
var elements        : TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[4]
  else
    Result := 0.0;
end;

function TGPMatrix.offsetY: Single;
var elements        : TMatrixArray;
begin
  if (GetElements(elements) = Ok) then Result := elements[5]
  else Result := 0.0;
end;

function TGPMatrix.Reset: TStatus;
begin
  // set identity matrix elements
  Result := SetStatus(GdipSetMatrixElements(nativeMatrix, 1.0, 0.0, 0.0, 1.0,
    0.0, 0.0));
end;

function TGPMatrix.Multiply(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyMatrix(nativeMatrix, matrix.nativeMatrix, order));
end;

function TGPMatrix.Translate(offsetX, offsetY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateMatrix(nativeMatrix, offsetX, offsetY, order));
end;

function TGPMatrix.scale(scaleX, scaleY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleMatrix(nativeMatrix, scaleX, scaleY, order));
end;

function TGPMatrix.Rotate(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
end;

function TGPMatrix.RotateAt(angle: Single; const center: TGPPointF; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  if (order = MatrixOrderPrepend) then begin
    SetStatus(GdipTranslateMatrix(nativeMatrix, center.X, center.Y, order));
    SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
    Result := SetStatus(GdipTranslateMatrix(nativeMatrix, -center.X, -center.Y,
      order));
  end
  else begin
    SetStatus(GdipTranslateMatrix(nativeMatrix, -center.X, -center.Y, order));
    SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
    Result := SetStatus(GdipTranslateMatrix(nativeMatrix, center.X, center.Y,
      order));
  end;
end;

function TGPMatrix.Shear(shearX, shearY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipShearMatrix(nativeMatrix, shearX, shearY, order));
end;

function TGPMatrix.Invert: TStatus;
begin
  Result := SetStatus(GdipInvertMatrix(nativeMatrix));
end;

// float version

function TGPMatrix.TransformPoints(pts: PGPPointF; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPoints(nativeMatrix, pts, Count));
end;

function TGPMatrix.TransformPoints(pts: PGPPoint; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPointsI(nativeMatrix, pts, Count));
end;

function TGPMatrix.TransformVectors(pts: PGPPointF; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPoints(nativeMatrix, pts, Count));
end;

function TGPMatrix.TransformVectors(pts: PGPPoint; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPointsI(nativeMatrix, pts, Count));
end;

function TGPMatrix.IsInvertible: BOOL;
begin
  Result := False;
  SetStatus(GdipIsMatrixInvertible(nativeMatrix, Result));
end;

function TGPMatrix.IsIdentity: BOOL;
begin
  Result := False;
  SetStatus(GdipIsMatrixIdentity(nativeMatrix, Result));
end;

function TGPMatrix.Equals(matrix: TGPMatrix): BOOL;
begin
  Result := False;
  SetStatus(GdipIsMatrixEqual(nativeMatrix, matrix.nativeMatrix, Result));
end;

function TGPMatrix.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPMatrix.Create(nativeMatrix: GpMatrix);
begin
  lastResult := Ok;
  SetNativeMatrix(nativeMatrix);
end;

procedure TGPMatrix.SetNativeMatrix(nativeMatrix: GpMatrix);
begin
  Self.nativeMatrix := nativeMatrix;
end;

function TGPMatrix.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;


(**************************************************************************\
*
*   GDI+ StringFormat class
*
\**************************************************************************)

constructor TGPStringFormat.Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL);
begin
  nativeFormat := nil;
  lastError := GdipCreateStringFormat(formatFlags, language, nativeFormat);
end;

class function TGPStringFormat.GenericDefault: TGPStringFormat;
begin
  if not Assigned(GenericDefaultStringFormatBuffer) then begin
    GenericDefaultStringFormatBuffer := TGPStringFormat.Create;
    GenericDefaultStringFormatBuffer.lastError :=
      GdipStringFormatGetGenericDefault(GenericDefaultStringFormatBuffer.nativeFormat);
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TGPStringFormat.GenericTypographic: TGPStringFormat;
begin
  if not Assigned(GenericTypographicStringFormatBuffer) then begin
    GenericTypographicStringFormatBuffer := TGPStringFormat.Create;
    GenericTypographicStringFormatBuffer.lastError :=
      GdipStringFormatGetGenericTypographic(GenericTypographicStringFormatBuffer.nativeFormat);
  end;
  Result := GenericTypographicStringFormatBuffer;
end;

constructor TGPStringFormat.Create(Format: TGPStringFormat);
var gpstf           : GpStringFormat;
begin
  nativeFormat := nil;
  if Assigned(Format) then gpstf := Format.nativeFormat
  else gpstf := nil;
  lastError := GdipCloneStringFormat(gpstf, nativeFormat);
end;

function TGPStringFormat.Clone: TGPStringFormat;
var
  clonedStringFormat: GpStringFormat;
begin
  clonedStringFormat := nil;
  lastError := GdipCloneStringFormat(nativeFormat, clonedStringFormat);
  if (lastError = Ok) then
    Result := TGPStringFormat.Create(clonedStringFormat, lastError)
  else
    Result := nil;
end;

destructor TGPStringFormat.destroy;
begin
  GdipDeleteStringFormat(nativeFormat);
end;

function TGPStringFormat.SetFormatFlags(Flags: Integer): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatFlags(nativeFormat, Flags));
end;

function TGPStringFormat.GetFormatFlags: Integer;
begin
  SetStatus(GdipGetStringFormatFlags(nativeFormat, Result));
end;

function TGPStringFormat.SetAlignment(align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatAlign(nativeFormat, align));
end;

function TGPStringFormat.GetAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatAlign(nativeFormat, Result));
end;

function TGPStringFormat.SetLineAlignment(align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatLineAlign(nativeFormat, align));
end;

function TGPStringFormat.GetLineAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatLineAlign(nativeFormat, Result));
end;

function TGPStringFormat.SetHotkeyPrefix(HotkeyPrefix: THotkeyPrefix): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatHotkeyPrefix(nativeFormat, Integer(HotkeyPrefix)));
end;

function TGPStringFormat.GetHotkeyPrefix: THotkeyPrefix;
var HotkeyPrefix    : Integer;
begin
  SetStatus(GdipGetStringFormatHotkeyPrefix(nativeFormat, HotkeyPrefix));
  Result := THotkeyPrefix(HotkeyPrefix);
end;

function TGPStringFormat.SetTabStops(firstTabOffset: Single; Count: Integer; tabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTabStops(nativeFormat, firstTabOffset, Count, tabStops));
end;

function TGPStringFormat.GetTabStopCount: Integer;
begin
  SetStatus(GdipGetStringFormatTabStopCount(nativeFormat, Result));
end;

function TGPStringFormat.GetTabStops(Count: Integer; firstTabOffset, tabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipGetStringFormatTabStops(nativeFormat, Count, firstTabOffset, tabStops));
end;

function TGPStringFormat.SetDigitSubstitution(language: LANGID; substitute: TStringDigitSubstitute): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatDigitSubstitution(nativeFormat, language, substitute));
end;

function TGPStringFormat.GetDigitSubstitutionLanguage: LANGID;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(nativeFormat, @Result, nil));
end;

function TGPStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(nativeFormat, nil, @Result));
end;

function TGPStringFormat.SetTrimming(trimming: TStringTrimming): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTrimming(nativeFormat, trimming));
end;

function TGPStringFormat.GetTrimming: TStringTrimming;
begin
  SetStatus(GdipGetStringFormatTrimming(nativeFormat, Result));
end;

function TGPStringFormat.SetMeasurableCharacterRanges(rangeCount: Integer;
  ranges: PCharacterRange): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges(nativeFormat,
    rangeCount, ranges));
end;

function TGPStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(nativeFormat, Result));
end;

function TGPStringFormat.GetLastStatus: TStatus;
begin
  Result := lastError;
  lastError := Ok;
end;

function TGPStringFormat.SetStatus(newStatus: GpStatus): TStatus;
begin
  if (newStatus <> Ok) then lastError := newStatus;
  Result := newStatus;
end;

// operator =

procedure TGPStringFormat.Assign(source: TGPStringFormat);
begin
  assert(Assigned(source));
  GdipDeleteStringFormat(nativeFormat);
  lastError := GdipCloneStringFormat(source.nativeFormat, nativeFormat);
end;

constructor TGPStringFormat.Create(clonedStringFormat: GpStringFormat; Status: TStatus);
begin
  lastError := Status;
  nativeFormat := clonedStringFormat;
end;



// ---------------------------------------------------------------------------
//  TAdjustableArrowCap
// ---------------------------------------------------------------------------

constructor TGPAdjustableArrowCap.Create(Height, Width: Single; isFilled: BOOL = True);
var cap             : GpAdjustableArrowCap;
begin
  cap := nil;
  lastResult := GdipCreateAdjustableArrowCap(Height, Width, isFilled, cap);
  SetNativeCap(cap);
end;

function TGPAdjustableArrowCap.SetHeight(Height: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(nativeCap), Height));
end;

function TGPAdjustableArrowCap.GetHeight: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap(nativeCap), Result));
end;

function TGPAdjustableArrowCap.SetWidth(Width: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(nativeCap), Width));
end;

function TGPAdjustableArrowCap.GetWidth: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap(nativeCap), Result));
end;

function TGPAdjustableArrowCap.SetMiddleInset(middleInset: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(nativeCap), middleInset));
end;

function TGPAdjustableArrowCap.GetMiddleInset: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapMiddleInset(
    GpAdjustableArrowCap(nativeCap), Result));
end;

function TGPAdjustableArrowCap.SetFillState(isFilled: BOOL): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(nativeCap), isFilled));
end;

function TGPAdjustableArrowCap.isFilled: BOOL;
begin
  SetStatus(GdipGetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(nativeCap), Result));
end;

(**************************************************************************\
*
*   GDI+ Metafile class
*
\**************************************************************************)

    // Playback a metafile from a HMETAFILE
    // If deleteWmf is TRUE, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.

constructor TGPMetafile.Create(hWmf: HMETAFILE;
  var WmfPlaceableFileHeader: TWmfPlaceableFileHeader; deleteWmf: BOOL = False);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromWmf(hWmf, deleteWmf, @WmfPlaceableFileHeader, metafile);
  SetNativeImage(metafile);
end;

// Playback a metafile from a HENHMETAFILE
// If deleteEmf is TRUE, then when the metafile is deleted,
// the hEmf will also be deleted.  Otherwise, it won't be.

constructor TGPMetafile.Create(hEmf: HENHMETAFILE; deleteEmf: BOOL = False);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromEmf(hEmf, deleteEmf, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromFile(PWideChar(filename), metafile);
  SetNativeImage(metafile);
end;

// Playback a WMF metafile from a file.

constructor TGPMetafile.Create(filename: WideString; var WmfPlaceableFileHeader: TWmfPlaceableFileHeader);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromWmfFile(PWideChar(filename), @WmfPlaceableFileHeader, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: ISTREAM);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromStream(stream, metafile);
  SetNativeImage(metafile);
end;

// Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: hdc; type_: TEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafile(referenceHdc, type_, nil, MetafileFrameUnitGdi,
    description, metafile);
  SetNativeImage(metafile);
end;

// Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: hdc; frameRect: TGPRectF;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var metafile        : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafile(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile);
  SetNativeImage(metafile);
end;

// Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: hdc; frameRect: TGPRect;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileI(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString; referenceHdc: hdc;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileFileName(PWideChar(filename),
    referenceHdc, type_, nil, MetafileFrameUnitGdi, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString; referenceHdc: hdc; frameRect: TGPRectF;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileFileName(PWideChar(filename), referenceHdc,
    type_, @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString; referenceHdc: hdc; frameRect: TGPRect;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileFileNameI(PWideChar(filename),
    referenceHdc, type_, @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: ISTREAM; referenceHdc: hdc;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileStream(stream, referenceHdc, type_, nil,
    MetafileFrameUnitGdi, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: ISTREAM; referenceHdc: hdc; frameRect: TGPRectF;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileStream(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: ISTREAM; referenceHdc: hdc; frameRect: TGPRect;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = nil);
var
  metafile          : GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileStreamI(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

function TGPMetafile.GetMetafileHeader(hWmf: HMETAFILE;
  var WmfPlaceableFileHeader: TWmfPlaceableFileHeader; Header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromWmf(hWmf, @WmfPlaceableFileHeader, @Header.type_);
end;

function TGPMetafile.GetMetafileHeader(hEmf: HENHMETAFILE; Header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromEmf(hEmf, @Header.type_);
end;

function TGPMetafile.GetMetafileHeader(filename: WideString; Header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromFile(PWideChar(filename), @Header.type_);
end;

function TGPMetafile.GetMetafileHeader(stream: ISTREAM; Header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromStream(stream, @Header.type_);
end;

function TGPMetafile.GetMetafileHeader(Header: TMetafileHeader): TStatus;
begin
  Result := SetStatus(GdipGetMetafileHeaderFromMetafile(GpMetafile(nativeImage),
    @Header.type_));
end;

// Once this method is called, the Metafile object is in an invalid state
// and can no longer be used.  It is the responsiblity of the caller to
// invoke DeleteEnhMetaFile to delete this hEmf.

function TGPMetafile.GetHENHMETAFILE: HENHMETAFILE;
begin
  SetStatus(GdipGetHemfFromMetafile(GpMetafile(nativeImage), Result));
end;

// Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
// The data must be DWORD aligned if it's an EMF or EMF+.  It must be
// WORD aligned if it's a WMF.

function TGPMetafile.PlayRecord(recordType: TEmfPlusRecordType; Flags, dataSize: UINT;
  data: PBYTE): TStatus;
begin
  Result := SetStatus(GdipPlayMetafileRecord(GpMetafile(nativeImage),
    recordType, Flags, dataSize, data));
end;

// If you're using a printer HDC for the metafile, but you want the
// metafile rasterized at screen resolution, then use this API to set
// the rasterization dpi of the metafile to the screen resolution,
// e.g. 96 dpi or 120 dpi.

function TGPMetafile.SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: UINT): TStatus;
begin
  Result := SetStatus(GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(nativeImage), metafileRasterizationLimitDpi));
end;

function TGPMetafile.GetDownLevelRasterizationLimit: UINT;
var metafileRasterizationLimitDpi: UINT;
begin
  metafileRasterizationLimitDpi := 0;
  SetStatus(GdipGetMetafileDownLevelRasterizationLimit(
    GpMetafile(nativeImage), metafileRasterizationLimitDpi));
  Result := metafileRasterizationLimitDpi;
end;

function TGPMetafile.EmfToWmfBits(hEmf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
  iMapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
begin
  Result := GdipEmfToWmfBits(hEmf, cbData16, pData16, iMapMode, Integer(eFlags));
end;

constructor TGPMetafile.Create;
begin
  SetNativeImage(nil);
  lastResult := Ok;
end;

(**************************************************************************\
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

function GetImageDecodersSize(out numDecoders, size: UINT): TStatus;
begin
  Result := GdipGetImageDecodersSize(numDecoders, size);
end;

function GetImageDecoders(numDecoders, size: UINT;
  decoders: PImageCodecInfo): TStatus;
begin
  Result := GdipGetImageDecoders(numDecoders, size, decoders);
end;


function GetImageEncodersSize(out numEncoders, size: UINT): TStatus;
begin
  Result := GdipGetImageEncodersSize(numEncoders, size);
end;

function GetImageEncoders(numEncoders, size: UINT;
  encoders: PImageCodecInfo): TStatus;
begin
  Result := GdipGetImageEncoders(numEncoders, size, encoders);
end;

(**************************************************************************\
*
*   GDI+ Region class implementation
*
\**************************************************************************)

constructor TGPRegion.Create;
var
  region            : GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegion(region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(Rect: TGPRectF);
var
  region            : GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionRect(@Rect, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(Rect: TGPRect);
var
  region            : GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionRectI(@Rect, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(path: TGPGraphicsPath);
var
  region            : GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionPath(path.nativePath, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(regionData: PBYTE; size: Integer);
var
  region            : GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionRgnData(regionData, size, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(hRgn: hRgn);
var
  region            : GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionHrgn(hRgn, region);
  SetNativeRegion(region);
end;

function TGPRegion.FromHRGN(hRgn: hRgn): TGPRegion;
var
  region            : GpRegion;
begin
  region := nil;
  if (GdipCreateRegionHrgn(hRgn, region) = Ok) then begin
    Result := TGPRegion.Create(region);
    if (Result = nil) then
      GdipDeleteRegion(region);
    exit;
  end
  else
    Result := nil;
end;

destructor TGPRegion.destroy;
begin
  GdipDeleteRegion(nativeRegion);
end;

function TGPRegion.Clone: TGPRegion;
var region          : GpRegion;
begin
  region := nil;
  SetStatus(GdipCloneRegion(nativeRegion, region));
  Result := TGPRegion.Create(region);
end;

function TGPRegion.MakeInfinite: TStatus;
begin
  Result := SetStatus(GdipSetInfinite(nativeRegion));
end;

function TGPRegion.MakeEmpty: TStatus;
begin
  Result := SetStatus(GdipSetEmpty(nativeRegion));
end;

// Get the size of the buffer needed for the GetData method

function TGPRegion.GetDataSize: UINT;
var bufferSize      : UINT;
begin
  bufferSize := 0;
  SetStatus(GdipGetRegionDataSize(nativeRegion, bufferSize));
  Result := bufferSize;
end;


// buffer     - where to put the data
// bufferSize - how big the buffer is (should be at least as big as GetDataSize())
// sizeFilled - if not nil, this is an OUT param that says how many bytes
//              of data were written to the buffer.

function TGPRegion.GetData(buffer: PBYTE; bufferSize: UINT; sizeFilled: PUINT = nil): TStatus;
begin
  Result := SetStatus(GdipGetRegionData(nativeRegion, buffer, bufferSize, sizeFilled));
end;

function TGPRegion.Intersect(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @Rect, CombineModeIntersect));
end;

function TGPRegion.Intersect(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @Rect, CombineModeIntersect));
end;

function TGPRegion.Intersect(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
    CombineModeIntersect));
end;

function TGPRegion.Intersect(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion, region.nativeRegion,
    CombineModeIntersect));
end;

function TGPRegion.Union(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @Rect, CombineModeUnion));
end;

function TGPRegion.Union(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @Rect, CombineModeUnion));
end;

function TGPRegion.Union(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath, CombineModeUnion));
end;

function TGPRegion.Union(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion, region.nativeRegion,
    CombineModeUnion));
end;

function TGPRegion.Xor_(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @Rect, CombineModeXor));
end;

function TGPRegion.Xor_(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @Rect, CombineModeXor));
end;

function TGPRegion.Xor_(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath, CombineModeXor));
end;

function TGPRegion.Xor_(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion, region.nativeRegion,
    CombineModeXor));
end;

function TGPRegion.Exclude(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @Rect, CombineModeExclude));
end;

function TGPRegion.Exclude(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @Rect, CombineModeExclude));
end;

function TGPRegion.Exclude(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath, CombineModeExclude));
end;

function TGPRegion.Exclude(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
    region.nativeRegion,
    CombineModeExclude));
end;

function TGPRegion.Complement(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @Rect,
    CombineModeComplement));
end;

function TGPRegion.Complement(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @Rect,
    CombineModeComplement));
end;

function TGPRegion.Complement(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion,
    path.nativePath,
    CombineModeComplement));
end;

function TGPRegion.Complement(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
    region.nativeRegion,
    CombineModeComplement));
end;

function TGPRegion.Translate(dx, dy: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateRegion(nativeRegion, dx, dy));
end;

function TGPRegion.Translate(dx, dy: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateRegionI(nativeRegion, dx, dy));
end;

function TGPRegion.Transform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipTransformRegion(nativeRegion,
    matrix.nativeMatrix));
end;

function TGPRegion.GetBounds(out Rect: TGPRect; G: TGPGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBoundsI(nativeRegion,
    G.nativeGraphics,
    @Rect));
end;

function TGPRegion.GetBounds(out Rect: TGPRectF; G: TGPGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBounds(nativeRegion,
    G.nativeGraphics,
    @Rect));
end;

function TGPRegion.GetHRGN(G: TGPGraphics): hRgn;
begin
  SetStatus(GdipGetRegionHRgn(nativeRegion, G.nativeGraphics, Result));
end;

function TGPRegion.IsEmpty(G: TGPGraphics): BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsEmptyRegion(nativeRegion, G.nativeGraphics, booln));
  Result := booln;
end;

function TGPRegion.IsInfinite(G: TGPGraphics): BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsInfiniteRegion(nativeRegion, G.nativeGraphics, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(X, Y: Integer; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionPointI(nativeRegion, X, Y, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const point: TGPPoint; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionPointI(nativeRegion, point.X, point.Y, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(X, Y: Single; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionPoint(nativeRegion, X, Y, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const point: TGPPointF; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionPoint(nativeRegion, point.X, point.Y, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(X, Y, Width, Height: Integer; G: TGPGraphics): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionRectI(nativeRegion,
    X,
    Y,
    Width,
    Height,
    GPX,
    booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const Rect: TGPRect; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionRectI(nativeRegion,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    GPX,
    booln));
  Result := booln;
end;

function TGPRegion.IsVisible(X, Y, Width, Height: Single; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionRect(nativeRegion, X,
    Y, Width,
    Height,
    GPX,
    booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const Rect: TGPRectF; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  GPX               : GpGraphics;
begin
  booln := False;
  if Assigned(G) then GPX := G.nativeGraphics else GPX := nil;
  SetStatus(GdipIsVisibleRegionRect(nativeRegion, Rect.X,
    Rect.Y, Rect.Width,
    Rect.Height,
    GPX,
    booln));
  Result := booln;
end;

function TGPRegion.Equals(region: TGPRegion; G: TGPGraphics): BOOL;
var
  booln             : BOOL;
begin
  booln := False;
  SetStatus(GdipIsEqualRegion(nativeRegion,
    region.nativeRegion,
    G.nativeGraphics,
    booln));
  Result := booln;
end;

function TGPRegion.GetRegionScansCount(matrix: TGPMatrix): UINT;
var Count           : UINT;
begin
  Count := 0;
  SetStatus(GdipGetRegionScansCount(nativeRegion, Count, matrix.nativeMatrix));
  Result := Count;
end;

// If rects is nil, result := the count of rects in the region.
// Otherwise, assume rects is big enough to hold all the region rects
// and fill them in and result := the number of rects filled in.
// The rects are result :=ed in the units specified by the matrix
// (which is typically a world-to-device transform).
// Note that the number of rects result :=ed can vary, depending on the
// matrix that is used.

function TGPRegion.GetRegionScans(matrix: TGPMatrix; rects: PGPRectF; out Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScans(nativeRegion,
    rects,
    Count,
    matrix.nativeMatrix));
end;

function TGPRegion.GetRegionScans(matrix: TGPMatrix; rects: PGPRect; out Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScansI(nativeRegion,
    rects,
    Count,
    matrix.nativeMatrix));
end;

function TGPRegion.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

function TGPRegion.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

constructor TGPRegion.Create(nativeRegion: GpRegion);
begin
  SetNativeRegion(nativeRegion);
end;

procedure TGPRegion.SetNativeRegion(nativeRegion: GpRegion);
begin
  Self.nativeRegion := nativeRegion;
end;

(**************************************************************************\
*
*   GDI+ CustomLineCap APIs
*
\**************************************************************************)

constructor TGPCustomLineCap.Create(fillPath, strokePath: TGPGraphicsPath;
  baseCap: TLineCap = LineCapFlat; baseInset: Single = 0);
var nativeFillPath, nativeStrokePath: GpPath;
begin
  nativeCap := nil;
  nativeFillPath := nil;
  nativeStrokePath := nil;

  if Assigned(fillPath) then nativeFillPath := fillPath.nativePath;
  if Assigned(strokePath) then nativeStrokePath := strokePath.nativePath;

  lastResult := GdipCreateCustomLineCap(nativeFillPath, nativeStrokePath,
    baseCap, baseInset, nativeCap);
end;

destructor TGPCustomLineCap.destroy;
begin
  GdipDeleteCustomLineCap(nativeCap);
end;

function TGPCustomLineCap.Clone: TGPCustomLineCap;
var newNativeLineCap: GpCustomLineCap;
begin
  newNativeLineCap := nil;
  SetStatus(GdipCloneCustomLineCap(nativeCap, newNativeLineCap));
  if (lastResult = Ok) then begin
    Result := TGPCustomLineCap.Create(newNativeLineCap, lastResult);
    if (Result = nil) then
      SetStatus(GdipDeleteCustomLineCap(newNativeLineCap));
  end
  else
    Result := nil;
end;

// This changes both the start and end cap.

function TGPCustomLineCap.SetStrokeCap(strokeCap: TLineCap): TStatus;
begin
  Result := SetStrokeCaps(strokeCap, strokeCap);
end;

function TGPCustomLineCap.SetStrokeCaps(startCap, endCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeCaps(nativeCap, startCap, endCap));
end;

function TGPCustomLineCap.GetStrokeCaps(out startCap, endCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipGetCustomLineCapStrokeCaps(nativeCap, startCap, endCap));
end;

function TGPCustomLineCap.SetStrokeJoin(LineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeJoin(nativeCap, LineJoin));
end;

function TGPCustomLineCap.GetStrokeJoin: TLineJoin;
begin
  SetStatus(GdipGetCustomLineCapStrokeJoin(nativeCap, Result));
end;

function TGPCustomLineCap.SetBaseCap(baseCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseCap(nativeCap, baseCap));
end;

function TGPCustomLineCap.GetBaseCap: TLineCap;
begin
  SetStatus(GdipGetCustomLineCapBaseCap(nativeCap, Result));
end;

function TGPCustomLineCap.SetBaseInset(inset: Single): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseInset(nativeCap, inset));
end;

function TGPCustomLineCap.GetBaseInset: Single;
begin
  SetStatus(GdipGetCustomLineCapBaseInset(nativeCap, Result));
end;

function TGPCustomLineCap.SetWidthScale(widthScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapWidthScale(nativeCap, widthScale));
end;

function TGPCustomLineCap.GetWidthScale: Single;
begin
  SetStatus(GdipGetCustomLineCapWidthScale(nativeCap, Result));
end;

function TGPCustomLineCap.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPCustomLineCap.Create;
begin
  nativeCap := nil;
  lastResult := Ok;
end;

constructor TGPCustomLineCap.Create(nativeCap: GpCustomLineCap; Status: TStatus);
begin
  lastResult := Status;
  SetNativeCap(nativeCap);
end;

procedure TGPCustomLineCap.SetNativeCap(nativeCap: GpCustomLineCap);
begin
  Self.nativeCap := nativeCap;
end;

function TGPCustomLineCap.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

(**************************************************************************
*
* CachedBitmap class definition
*
*   GDI+ CachedBitmap is a representation of an accelerated drawing
*   that has restrictions on what operations are allowed in order
*   to accelerate the drawing to the destination.
*
**************************************************************************)

constructor TGPCachedBitmap.Create(bitmap: TGPBitmap; graphics: TGPGraphics);
begin
  nativeCachedBitmap := nil;
  lastResult := GdipCreateCachedBitmap(
    GpBitmap(bitmap.nativeImage),
    graphics.nativeGraphics,
    nativeCachedBitmap);
end;

destructor TGPCachedBitmap.destroy;
begin
  GdipDeleteCachedBitmap(nativeCachedBitmap);
end;

function TGPCachedBitmap.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

(**************************************************************************\
*
*   GDI+ Pen class
*
\**************************************************************************)

  //--------------------------------------------------------------------------
  // Pen class
  //--------------------------------------------------------------------------

constructor TGPPen.Create(color: TGPColor; Width: Single = 1.0);
var Unit_           : TUnit;
begin
  Unit_ := UnitWorld;
  nativePen := nil;
  lastResult := GdipCreatePen1(color, Width, Unit_, nativePen);
end;

constructor TGPPen.Create(brush: TGPBrush; Width: Single = 1.0);
var Unit_           : TUnit;
begin
  Unit_ := UnitWorld;
  nativePen := nil;
  lastResult := GdipCreatePen2(brush.nativeBrush, Width, Unit_, nativePen);
end;

destructor TGPPen.destroy;
begin
  GdipDeletePen(nativePen);
end;

function TGPPen.Clone: TGPPen;
var clonepen        : GpPen;
begin
  clonepen := nil;
  lastResult := GdipClonePen(nativePen, clonepen);
  Result := TGPPen.Create(clonepen, lastResult);
end;

function TGPPen.SetWidth(Width: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenWidth(nativePen, Width));
end;

function TGPPen.GetWidth: Single;
begin
  SetStatus(GdipGetPenWidth(nativePen, Result));
end;

// Set/get line caps: start, end, and dash
// Line cap and join APIs by using LineCap and LineJoin enums.

function TGPPen.SetLineCap(startCap, endCap: TLineCap; DashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenLineCap197819(nativePen, startCap, endCap, DashCap));
end;

function TGPPen.SetStartCap(startCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenStartCap(nativePen, startCap));
end;

function TGPPen.SetEndCap(endCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenEndCap(nativePen, endCap));
end;

function TGPPen.SetDashCap(DashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenDashCap197819(nativePen, DashCap));
end;

function TGPPen.GetStartCap: TLineCap;
begin
  SetStatus(GdipGetPenStartCap(nativePen, Result));
end;

function TGPPen.GetEndCap: TLineCap;
begin
  SetStatus(GdipGetPenEndCap(nativePen, Result));
end;

function TGPPen.GetDashCap: TDashCap;
begin
  SetStatus(GdipGetPenDashCap197819(nativePen, Result));
end;

function TGPPen.SetLineJoin(LineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetPenLineJoin(nativePen, LineJoin));
end;

function TGPPen.GetLineJoin: TLineJoin;
begin
  SetStatus(GdipGetPenLineJoin(nativePen, Result));
end;

function TGPPen.SetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
var nativeCap       : GpCustomLineCap;
begin
  nativeCap := nil;
  if Assigned(customCap) then nativeCap := customCap.nativeCap;
  Result := SetStatus(GdipSetPenCustomStartCap(nativePen, nativeCap));
end;

function TGPPen.GetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
begin
  if (customCap = nil) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCustomStartCap(nativePen, customCap.nativeCap));
end;

function TGPPen.SetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
var nativeCap       : GpCustomLineCap;
begin
  nativeCap := nil;
  if Assigned(customCap) then nativeCap := customCap.nativeCap;
  Result := SetStatus(GdipSetPenCustomEndCap(nativePen, nativeCap));
end;

function TGPPen.GetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
begin
  if (customCap = nil) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCustomEndCap(nativePen, customCap.nativeCap));
end;

function TGPPen.SetMiterLimit(miterLimit: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenMiterLimit(nativePen, miterLimit));
end;

function TGPPen.GetMiterLimit: Single;
begin
  SetStatus(GdipGetPenMiterLimit(nativePen, Result));
end;

function TGPPen.SetAlignment(PenAlignment: TPenAlignment): TStatus;
begin
  Result := SetStatus(GdipSetPenMode(nativePen, PenAlignment));
end;

function TGPPen.GetAlignment: TPenAlignment;
begin
  SetStatus(GdipGetPenMode(nativePen, Result));
end;

function TGPPen.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPenTransform(nativePen, matrix.nativeMatrix));
end;

function TGPPen.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPenTransform(nativePen, matrix.nativeMatrix));
end;

function TGPPen.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetPenTransform(nativePen));
end;

function TGPPen.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPenTransform(nativePen, matrix.nativeMatrix, order));
end;

function TGPPen.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePenTransform(nativePen, dx, dy, order));
end;

function TGPPen.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePenTransform(nativePen, sx, sy, order));
end;

function TGPPen.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePenTransform(nativePen, angle, order));
end;

function TGPPen.GetPenType: TPenType;
begin
  SetStatus(GdipGetPenFillType(nativePen, Result));
end;

function TGPPen.SetColor(color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipSetPenColor(nativePen, color));
end;

function TGPPen.SetBrush(brush: TGPBrush): TStatus;
begin
  Result := SetStatus(GdipSetPenBrushFill(nativePen, brush.nativeBrush));
end;

function TGPPen.GetColor(out color: TGPColor): TStatus;
var
  type_             : TPenType;
  ARGB              : DWORD;
begin
  type_ := GetPenType;

  if (type_ <> PenTypeSolidColor) then begin
    Result := WrongState;
    exit;
  end;

  SetStatus(GdipGetPenColor(nativePen, ARGB));
  if (lastResult = Ok) then color := ARGB;
  Result := lastResult;
end;

function TGPPen.GetBrush: TGPBrush;
var
  type_             : TPenType;
  brush             : TGPBrush;
  nativeBrush       : GpBrush;
begin
  type_ := GetPenType;
  brush := nil;

  case type_ of
    PenTypeSolidColor: brush := TGPSolidBrush.Create;
    PenTypeHatchFill: brush := TGPHatchBrush.Create;
    PenTypeTextureFill: brush := TGPTextureBrush.Create;
    PenTypePathGradient: brush := TGPBrush.Create;
    PenTypeLinearGradient: brush := TGPLinearGradientBrush.Create;
  end;

  if (brush <> nil) then begin
    SetStatus(GdipGetPenBrushFill(nativePen, nativeBrush));
    brush.SetNativeBrush(nativeBrush);
  end;

  Result := brush;
end;

function TGPPen.GetDashStyle: TDashStyle;
begin
  SetStatus(GdipGetPenDashStyle(nativePen, Result));
end;

function TGPPen.SetDashStyle(DashStyle: TDashStyle): TStatus;
begin
  Result := SetStatus(GdipSetPenDashStyle(nativePen, DashStyle));
end;

function TGPPen.GetDashOffset: Single;
begin
  SetStatus(GdipGetPenDashOffset(nativePen, Result));
end;

function TGPPen.SetDashOffset(dashOffset: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenDashOffset(nativePen, dashOffset));
end;

function TGPPen.SetDashPattern(dashArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenDashArray(nativePen, dashArray, Count));
end;

function TGPPen.GetDashPatternCount: Integer;
begin
  SetStatus(GdipGetPenDashCount(nativePen, Result));
end;

function TGPPen.GetDashPattern(dashArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPenDashArray(nativePen, dashArray, Count));
end;

function TGPPen.SetCompoundArray(compoundArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenCompoundArray(nativePen, compoundArray, Count));
end;

function TGPPen.GetCompoundArrayCount: Integer;
begin
  SetStatus(GdipGetPenCompoundCount(nativePen, Result));
end;

function TGPPen.GetCompoundArray(compoundArray: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCompoundArray(nativePen, compoundArray, Count));
end;

function TGPPen.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPPen.Create(nativePen: GpPen; Status: TStatus);
begin
  lastResult := Status;
  SetNativePen(nativePen);
end;

procedure TGPPen.SetNativePen(nativePen: GpPen);
begin
  Self.nativePen := nativePen;
end;

function TGPPen.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

(**************************************************************************\
*
*   GDI+ Brush class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Abstract base class for various brush types
//--------------------------------------------------------------------------

destructor TGPBrush.destroy;
begin
  GdipDeleteBrush(nativeBrush);
end;

function TGPBrush.Clone: TGPBrush;
var
  brush             : GpBrush;
  newBrush          : TGPBrush;
begin
  brush := nil;
  SetStatus(GdipCloneBrush(nativeBrush, brush));
  newBrush := TGPBrush.Create(brush, lastResult);
  if (newBrush = nil) then
    GdipDeleteBrush(brush);
  Result := newBrush;
end;

function TGPBrush.GetType: TBrushType;
var type_           : TBrushType;
begin
  type_ := TBrushType(-1);
  SetStatus(GdipGetBrushType(nativeBrush, type_));
  Result := type_;
end;

function TGPBrush.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPBrush.Create;
begin
  SetStatus(NotImplemented);
end;

constructor TGPBrush.Create(nativeBrush: GpBrush; Status: TStatus);
begin
  lastResult := Status;
  SetNativeBrush(nativeBrush);
end;

procedure TGPBrush.SetNativeBrush(nativeBrush: GpBrush);
begin
  Self.nativeBrush := nativeBrush;
end;

function TGPBrush.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

//--------------------------------------------------------------------------
// Solid Fill Brush Object
//--------------------------------------------------------------------------

constructor TGPSolidBrush.Create(color: TGPColor);
var
  brush             : GpSolidFill;
begin
  brush := nil;
  lastResult := GdipCreateSolidFill(color, brush);
  SetNativeBrush(brush);
end;

function TGPSolidBrush.GetColor(out color: TGPColor): TStatus;
begin
  SetStatus(GdipGetSolidFillColor(GpSolidFill(nativeBrush), color));
  Result := lastResult;
end;

function TGPSolidBrush.SetColor(color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipSetSolidFillColor(GpSolidFill(nativeBrush),
    color));
end;

constructor TGPSolidBrush.Create;
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Texture Brush Fill Object
//--------------------------------------------------------------------------

constructor TGPTextureBrush.Create(image: TGPImage; WrapMode: TWrapMode = WrapModeTile);
var texture         : GpTexture;
begin
  //texture := nil;
  lastResult := GdipCreateTexture(image.nativeImage, WrapMode, texture);
  SetNativeBrush(texture);
end;

// When creating a texture brush from a metafile image, the dstRect
// is used to specify the size that the metafile image should be
// rendered at in the device units of the destination graphics.
// It is NOT used to crop the metafile image, so only the width
// and height values matter for metafiles.

constructor TGPTextureBrush.Create(image: TGPImage; WrapMode: TWrapMode; dstrect: TGPRectF);
var texture         : GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2(image.nativeImage, WrapMode, dstrect.X,
    dstrect.Y, dstrect.Width, dstrect.Height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; dstrect: TGPRectF; imageAttributes: TGPImageAttributes = nil);
var texture         : GpTexture;
  ImgAtt            : GpImageAttributes;
begin
  texture := nil;
  if Assigned(imageAttributes) then ImgAtt := imageAttributes.nativeImageAttr
  else ImgAtt := nil;

  lastResult := GdipCreateTextureIA(image.nativeImage, ImgAtt, dstrect.X,
    dstrect.Y, dstrect.Width, dstrect.Height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; dstrect: TGPRect; imageAttributes: TGPImageAttributes = nil);
var texture         : GpTexture;
  ImgAtt            : GpImageAttributes;
begin
  texture := nil;
  if Assigned(imageAttributes) then ImgAtt := imageAttributes.nativeImageAttr
  else ImgAtt := nil;
  lastResult := GdipCreateTextureIAI(image.nativeImage, ImgAtt, dstrect.X,
    dstrect.Y, dstrect.Width, dstrect.Height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; WrapMode: TWrapMode; dstrect: TGPRect);
var texture         : GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2I(image.nativeImage, WrapMode, dstrect.X,
    dstrect.Y, dstrect.Width, dstrect.Height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; WrapMode: TWrapMode; dstx, dsty, dstwidth, dstheight: Single);
var texture         : GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2(image.nativeImage, WrapMode, dstx, dsty,
    dstwidth, dstheight, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; WrapMode: TWrapMode; dstx, dsty, dstwidth, dstheight: Integer);
var texture         : GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2I(image.nativeImage, WrapMode, dstx, dsty,
    dstwidth, dstheight, texture);
  SetNativeBrush(texture);
end;

function TGPTextureBrush.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetTextureTransform(GpTexture(nativeBrush),
    matrix.nativeMatrix));
end;

function TGPTextureBrush.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetTextureTransform(GpTexture(nativeBrush),
    matrix.nativeMatrix));
end;

function TGPTextureBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetTextureTransform(GpTexture(nativeBrush)));
end;

function TGPTextureBrush.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyTextureTransform(GpTexture(nativeBrush),
    matrix.nativeMatrix, order));
end;

function TGPTextureBrush.TranslateTransform(dx, dy: Single; order: MatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateTextureTransform(GpTexture(nativeBrush),
    dx, dy, order));
end;

function TGPTextureBrush.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleTextureTransform(GpTexture(nativeBrush),
    sx, sy, order));
end;

function TGPTextureBrush.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateTextureTransform(GpTexture(nativeBrush),
    angle, order));
end;

function TGPTextureBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetTextureWrapMode(GpTexture(nativeBrush), WrapMode));
end;

function TGPTextureBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetTextureWrapMode(GpTexture(nativeBrush), Result));
end;

function TGPTextureBrush.GetImage: TGPImage;
var image           : GpImage;
begin
  SetStatus(GdipGetTextureImage(GpTexture(nativeBrush), image));
  Result := TGPImage.Create(image, lastResult);
  if (Result = nil) then
    GdipDisposeImage(image);
end;

constructor TGPTextureBrush.Create;
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Linear Gradient Brush Object
//--------------------------------------------------------------------------

constructor TGPLinearGradientBrush.Create(const point1, point2: TGPPointF; color1, color2: TGPColor);
var brush           : GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrush(@point1, @point2, color1, color2, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(const point1, point2: TGPPoint; color1, color2: TGPColor);
var brush           : GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushI(@point1, @point2, color1,
    color2, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(Rect: TGPRectF; color1, color2: TGPColor; mode: TLinearGradientMode);
var brush           : GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRect(@Rect, color1,
    color2, mode, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(Rect: TGPRect; color1, color2: TGPColor; mode: TLinearGradientMode);
var brush           : GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRectI(@Rect, color1,
    color2, mode, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(Rect: TGPRectF; color1, color2: TGPColor; angle: Single; isAngleScalable: BOOL = False);
var brush           : GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRectWithAngle(@Rect, color1,
    color2, angle, isAngleScalable, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(Rect: TGPRect; color1, color2: TGPColor; angle: Single; isAngleScalable: BOOL = False);
var brush           : GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRectWithAngleI(@Rect, color1,
    color2, angle, isAngleScalable, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

function TGPLinearGradientBrush.SetLinearColors(color1, color2: TGPColor): TStatus;
begin
  Result := SetStatus(GdipSetLineColors(GpLineGradient(nativeBrush),
    color1, color2));
end;

function TGPLinearGradientBrush.GetLinearColors(out color1, color2: TGPColor): TStatus;
var colors          : array[0..1] of TGPColor;
begin
  SetStatus(GdipGetLineColors(GpLineGradient(nativeBrush), @colors));
  if (lastResult = Ok) then begin
    color1 := colors[0];
    color2 := colors[1];
  end;
  Result := lastResult;
end;

function TGPLinearGradientBrush.GetRectangle(out Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetLineRect(GpLineGradient(nativeBrush), @Rect));
end;

function TGPLinearGradientBrush.GetRectangle(out Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetLineRectI(GpLineGradient(nativeBrush), @Rect));
end;

function TGPLinearGradientBrush.SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
begin
  Result := SetStatus(GdipSetLineGammaCorrection(GpLineGradient(nativeBrush),
    useGammaCorrection));
end;

function TGPLinearGradientBrush.GetGammaCorrection: BOOL;
var useGammaCorrection: BOOL;
begin
  SetStatus(GdipGetLineGammaCorrection(GpLineGradient(nativeBrush),
    useGammaCorrection));
  Result := useGammaCorrection;
end;

function TGPLinearGradientBrush.GetBlendCount: Integer;
var Count           : Integer;
begin
  Count := 0;
  SetStatus(GdipGetLineBlendCount(GpLineGradient(nativeBrush), Count));
  Result := Count;
end;

function TGPLinearGradientBrush.SetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetLineBlend(GpLineGradient(nativeBrush),
    blendFactors, blendPositions, Count));
end;

function TGPLinearGradientBrush.GetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
begin
  if ((Count <= 0) or (blendFactors = nil) or (blendPositions = nil)) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLineBlend(GpLineGradient(nativeBrush), blendFactors,
      blendPositions, Count));
end;

function TGPLinearGradientBrush.GetInterpolationColorCount: Integer;
var Count           : Integer;
begin
  Count := 0;
  SetStatus(GdipGetLinePresetBlendCount(GpLineGradient(nativeBrush), Count));
  Result := Count;
end;

function TGPLinearGradientBrush.SetInterpolationColors(presetColors: PGPColor;
  blendPositions: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipSetLinePresetBlend(GpLineGradient(nativeBrush),
      PARGB(presetColors), blendPositions, Count));
end;

function TGPLinearGradientBrush.GetInterpolationColors(presetColors: PGPColor; blendPositions: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLinePresetBlend(GpLineGradient(nativeBrush),
      PARGB(presetColors), blendPositions, Count));
end;

function TGPLinearGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetLineSigmaBlend(GpLineGradient(nativeBrush), focus, scale));
end;

function TGPLinearGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetLineLinearBlend(GpLineGradient(nativeBrush), focus, scale));
end;

function TGPLinearGradientBrush.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetLineTransform(GpLineGradient(nativeBrush),
    matrix.nativeMatrix));
end;

function TGPLinearGradientBrush.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetLineTransform(GpLineGradient(nativeBrush),
    matrix.nativeMatrix));
end;

function TGPLinearGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetLineTransform(GpLineGradient(nativeBrush)));
end;

function TGPLinearGradientBrush.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyLineTransform(GpLineGradient(nativeBrush),
    matrix.nativeMatrix,
    order));
end;

function TGPLinearGradientBrush.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateLineTransform(GpLineGradient(nativeBrush),
    dx, dy, order));
end;

function TGPLinearGradientBrush.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleLineTransform(GpLineGradient(nativeBrush),
    sx, sy, order));
end;

function TGPLinearGradientBrush.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateLineTransform(GpLineGradient(nativeBrush),
    angle, order));
end;

function TGPLinearGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetLineWrapMode(GpLineGradient(nativeBrush), WrapMode));
end;

function TGPLinearGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetLineWrapMode(GpLineGradient(nativeBrush), Result));
end;

constructor TGPLinearGradientBrush.Create;
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Hatch Brush Object
//--------------------------------------------------------------------------

constructor TGPHatchBrush.Create(HatchStyle: THatchStyle; foreColor: TGPColor; backColor: TGPColor = aclBlack);
var
  brush             : GpHatch;
begin
  brush := nil;
  lastResult := GdipCreateHatchBrush(Integer(HatchStyle), foreColor, backColor, brush);
  SetNativeBrush(brush);
end;

function TGPHatchBrush.GetHatchStyle: THatchStyle;
begin
  SetStatus(GdipGetHatchStyle(GpHatch(nativeBrush), Result));
end;

function TGPHatchBrush.GetForegroundColor(out color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGetHatchForegroundColor(GpHatch(nativeBrush), color));
end;

function TGPHatchBrush.GetBackgroundColor(out color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGetHatchBackgroundColor(GpHatch(nativeBrush), color));
end;

constructor TGPHatchBrush.Create;
begin
end;

constructor TGPImage.Create(filename: WideString;
  useEmbeddedColorManagement: BOOL = False);
begin
  nativeImage := nil;
  if (useEmbeddedColorManagement) then begin
    lastResult := GdipLoadImageFromFileICM(
      PWideChar(filename),
      nativeImage
      );
  end
  else begin
    lastResult := GdipLoadImageFromFile(
      PWideChar(filename),
      nativeImage
      );
  end;
end;

constructor TGPImage.Create(stream: ISTREAM;
  useEmbeddedColorManagement: BOOL = False);
begin
  nativeImage := nil;
  if (useEmbeddedColorManagement) then
    lastResult := GdipLoadImageFromStreamICM(stream, nativeImage)
  else lastResult := GdipLoadImageFromStream(stream, nativeImage);
end;

function TGPImage.FromFile(filename: WideString;
  useEmbeddedColorManagement: BOOL = False): TGPImage;
begin
  Result := TGPImage.Create(
    PWideChar(filename),
    useEmbeddedColorManagement
    );
end;

function TGPImage.FromStream(stream: ISTREAM;
  useEmbeddedColorManagement: BOOL = False): TGPImage;
begin
  Result := TGPImage.Create(
    stream,
    useEmbeddedColorManagement
    );
end;

destructor TGPImage.destroy;
begin
  GdipDisposeImage(nativeImage);
end;

function TGPImage.Clone: TGPImage;
var cloneImage      : GpImage;
begin
  cloneImage := nil;
  SetStatus(GdipCloneImage(nativeImage, cloneImage));
  Result := TGPImage.Create(cloneImage, lastResult);
end;

function TGPImage.Save(filename: WideString; const clsidEncoder: TGUID;
  encoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToFile(nativeImage,
    PWideChar(filename),
    @clsidEncoder,
    encoderParams));
end;

function TGPImage.Save(stream: ISTREAM; const clsidEncoder: TGUID;
  encoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToStream(nativeImage,
    stream,
    @clsidEncoder,
    encoderParams));
end;

function TGPImage.SaveAdd(encoderParams: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipSaveAdd(nativeImage,
    encoderParams));
end;

function TGPImage.SaveAdd(newImage: TGPImage;
  encoderParams: PEncoderParameters): TStatus;
begin
  if (newImage = nil) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;
  Result := SetStatus(GdipSaveAddImage(nativeImage,
    newImage.nativeImage,
    encoderParams));
end;

function TGPImage.GetType: TImageType;
begin
  SetStatus(GdipGetImageType(nativeImage, Result));
end;

function TGPImage.GetPhysicalDimension(out size: TGPSizeF): TStatus;
var
  Width, Height     : Single;
  Status            : TStatus;
begin
  Status := SetStatus(GdipGetImageDimension(nativeImage, Width, Height));
  size.Width := Width;
  size.Height := Height;
  Result := Status;
end;

function TGPImage.GetBounds(out srcRect: TGPRectF; out srcUnit: TUnit): TStatus;
begin
  Result := SetStatus(GdipGetImageBounds(nativeImage, @srcRect, srcUnit));
end;

function TGPImage.GetWidth: UINT;
var Width           : UINT;
begin
  Width := 0;
  SetStatus(GdipGetImageWidth(nativeImage, Width));
  Result := Width;
end;

function TGPImage.GetHeight: UINT;
var Height          : UINT;
begin
  Height := 0;
  SetStatus(GdipGetImageHeight(nativeImage, Height));
  Result := Height;
end;

function TGPImage.GetHorizontalResolution: Single;
var resolution      : Single;
begin
  resolution := 0.0;
  SetStatus(GdipGetImageHorizontalResolution(nativeImage, resolution));
  Result := resolution;
end;

function TGPImage.GetVerticalResolution: Single;
var resolution      : Single;
begin
  resolution := 0.0;
  SetStatus(GdipGetImageVerticalResolution(nativeImage, resolution));
  Result := resolution;
end;

function TGPImage.GetFlags: UINT;
var Flags           : UINT;
begin
  Flags := 0;
  SetStatus(GdipGetImageFlags(nativeImage, Flags));
  Result := Flags;
end;

function TGPImage.GetRawFormat(out Format: TGUID): TStatus;
begin
  Result := SetStatus(GdipGetImageRawFormat(nativeImage, @Format));
end;

function TGPImage.GetPixelFormat: TPixelFormat;
begin
  SetStatus(GdipGetImagePixelFormat(nativeImage, Result));
end;

function TGPImage.GetPaletteSize: Integer;
var size            : Integer;
begin
  size := 0;
  SetStatus(GdipGetImagePaletteSize(nativeImage, size));
  Result := size;
end;

function TGPImage.GetPalette(palette: PColorPalette; size: Integer): TStatus;
begin
  Result := SetStatus(GdipGetImagePalette(nativeImage, palette, size));
end;

function TGPImage.SetPalette(palette: PColorPalette): TStatus;
begin
  Result := SetStatus(GdipSetImagePalette(nativeImage, palette));
end;

function TGPImage.GetThumbnailImage(thumbWidth, thumbHeight: UINT;
  callback: GetThumbnailImageAbort = nil;
  callbackData: Pointer = nil): TGPImage;
var
  thumbImage        : GpImage;
  newImage          : TGPImage;
begin
  thumbImage := nil;
  SetStatus(GdipGetImageThumbnail(nativeImage,
    thumbWidth, thumbHeight,
    thumbImage,
    callback, callbackData));

  newImage := TGPImage.Create(thumbImage, lastResult);
  if (newImage = nil) then
    GdipDisposeImage(thumbImage);

  Result := newImage;
end;

function TGPImage.GetFrameDimensionsCount: UINT;
var Count           : UINT;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameDimensionsCount(nativeImage, Count));
  Result := Count;
end;

function TGPImage.GetFrameDimensionsList(dimensionIDs: PGUID; Count: UINT): TStatus;
begin
  Result := SetStatus(GdipImageGetFrameDimensionsList(nativeImage, dimensionIDs, Count));
end;

function TGPImage.GetFrameCount(const dimensionID: TGUID): UINT;
var Count           : UINT;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameCount(nativeImage, @dimensionID, Count));
  Result := Count;
end;

function TGPImage.SelectActiveFrame(const dimensionID: TGUID; frameIndex: UINT): TStatus;
begin
  Result := SetStatus(GdipImageSelectActiveFrame(nativeImage,
    @dimensionID,
    frameIndex));
end;

function TGPImage.RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
begin
  Result := SetStatus(GdipImageRotateFlip(nativeImage,
    RotateFlipType));
end;

function TGPImage.GetPropertyCount: UINT;
var numProperty     : UINT;
begin
  numProperty := 0;
  SetStatus(GdipGetPropertyCount(nativeImage, numProperty));
  Result := numProperty;
end;

function TGPImage.GetPropertyIdList(numOfProperty: UINT; list: PPROPID): TStatus;
begin
  Result := SetStatus(GdipGetPropertyIdList(nativeImage, numOfProperty, list));
end;

function TGPImage.GetPropertyItemSize(PROPID: PROPID): UINT;
var size            : UINT;
begin
  size := 0;
  SetStatus(GdipGetPropertyItemSize(nativeImage, PROPID, size));
  Result := size;
end;

function TGPImage.GetPropertyItem(PROPID: PROPID; propSize: UINT;
  buffer: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetPropertyItem(nativeImage,
    PROPID, propSize, buffer));
end;

function TGPImage.GetPropertySize(out totalBufferSize, numProperties: UINT): TStatus;
begin
  Result := SetStatus(GdipGetPropertySize(nativeImage,
    totalBufferSize,
    numProperties));
end;

function TGPImage.GetAllPropertyItems(totalBufferSize, numProperties: UINT;
  allItems: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetAllPropertyItems(nativeImage,
    totalBufferSize,
    numProperties,
    allItems));
end;

function TGPImage.RemovePropertyItem(PROPID: TPROPID): TStatus;
begin
  Result := SetStatus(GdipRemovePropertyItem(nativeImage, PROPID));
end;

function TGPImage.SetPropertyItem(const item: TPropertyItem): TStatus;
begin
  Result := SetStatus(GdipSetPropertyItem(nativeImage, @item));
end;

function TGPImage.GetEncoderParameterListSize(const clsidEncoder: TGUID): UINT;
var size            : UINT;
begin
  size := 0;
  SetStatus(GdipGetEncoderParameterListSize(nativeImage, @clsidEncoder, size));
  Result := size;
end;

function TGPImage.GetEncoderParameterList(const clsidEncoder: TGUID; size: UINT;
  buffer: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipGetEncoderParameterList(nativeImage,
    @clsidEncoder,
    size,
    buffer));
end;

function TGPImage.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPImage.Create(nativeImage: GpImage; Status: TStatus);
begin
  SetNativeImage(nativeImage);
  lastResult := Status;
end;

procedure TGPImage.SetNativeImage(nativeImage: GpImage);
begin
  Self.nativeImage := nativeImage;
end;

function TGPImage.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

// TGPBitmap

constructor TGPBitmap.Create(filename: WideString; useEmbeddedColorManagement: BOOL = False);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  if (useEmbeddedColorManagement) then
    lastResult := GdipCreateBitmapFromFileICM(PWideChar(filename), bitmap)
  else
    lastResult := GdipCreateBitmapFromFile(PWideChar(filename), bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(stream: ISTREAM; useEmbeddedColorManagement: BOOL = False);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  if (useEmbeddedColorManagement) then
    lastResult := GdipCreateBitmapFromStreamICM(stream, bitmap)
  else
    lastResult := GdipCreateBitmapFromStream(stream, bitmap);
  SetNativeImage(bitmap);
end;

function TGPBitmap.FromFile(filename: WideString; useEmbeddedColorManagement: BOOL = False): TGPBitmap;
begin
  Result := TGPBitmap.Create(
    PWideChar(filename),
    useEmbeddedColorManagement
    );
end;

function TGPBitmap.FromStream(stream: ISTREAM; useEmbeddedColorManagement: BOOL = False): TGPBitmap;
begin
  Result := TGPBitmap.Create(
    stream,
    useEmbeddedColorManagement
    );
end;

constructor TGPBitmap.Create(Width, Height, Stride: Integer; Format: TPixelFormat; Scan0: PBYTE);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromScan0(Width,
    Height,
    Stride,
    Format,
    Scan0,
    bitmap);

  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppARGB);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromScan0(Width,
    Height,
    0,
    Format,
    nil,
    bitmap);

  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(Width, Height: Integer; target: TGPGraphics);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromGraphics(Width,
    Height,
    target.nativeGraphics,
    bitmap);

  SetNativeImage(bitmap);
end;

function TGPBitmap.Clone(Rect: TGPRect; Format: TPixelFormat): TGPBitmap;
begin
  Result := Clone(Rect.X, Rect.Y, Rect.Width, Rect.Height, Format);
end;

function TGPBitmap.Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): TGPBitmap;
var
  bitmap            : TGPBitmap;
  gpdstBitmap       : GpBitmap;
begin
  gpdstBitmap := nil;
  lastResult := GdipCloneBitmapAreaI(
    X,
    Y,
    Width,
    Height,
    Format,
    GpBitmap(nativeImage),
    gpdstBitmap);

  if (lastResult = Ok) then begin
    bitmap := TGPBitmap.Create(gpdstBitmap);
    if (bitmap = nil) then
      GdipDisposeImage(gpdstBitmap);
    Result := bitmap;
    exit;
  end
  else
    Result := nil;
end;

function TGPBitmap.Clone(Rect: TGPRectF; Format: TPixelFormat): TGPBitmap;
begin
  Result := Clone(Rect.X, Rect.Y, Rect.Width, Rect.Height, Format);
end;

function TGPBitmap.Clone(X, Y, Width, Height: Single; Format: TPixelFormat): TGPBitmap;
var
  bitmap            : TGPBitmap;
  gpdstBitmap       : GpBitmap;
begin
  gpdstBitmap := nil;
  SetStatus(GdipCloneBitmapArea(
    X,
    Y,
    Width,
    Height,
    Format,
    GpBitmap(nativeImage),
    gpdstBitmap));

  if (lastResult = Ok) then begin
    bitmap := TGPBitmap.Create(gpdstBitmap);
    if (bitmap = nil) then
      GdipDisposeImage(gpdstBitmap);
    Result := bitmap;
  end
  else
    Result := nil;
end;

function TGPBitmap.LockBits(Rect: TGPRect; Flags: UINT; Format: TPixelFormat;
  out lockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapLockBits(
    GpBitmap(nativeImage),
    @Rect,
    Flags,
    Format,
    @lockedBitmapData));
end;

function TGPBitmap.UnlockBits(var lockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapUnlockBits(
    GpBitmap(nativeImage),
    @lockedBitmapData));
end;

function TGPBitmap.GetPixel(X, Y: Integer; out color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipBitmapGetPixel(GpBitmap(nativeImage), X, Y, color));
end;

function TGPBitmap.SetPixel(X, Y: Integer; color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipBitmapSetPixel(
    GpBitmap(nativeImage),
    X, Y,
    color));
end;

function TGPBitmap.SetResolution(xdpi, ydpi: Single): TStatus;
begin
  Result := SetStatus(GdipBitmapSetResolution(
    GpBitmap(nativeImage),
    xdpi, ydpi));
end;

constructor TGPBitmap.Create(surface: IDirectDrawSurface7);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromDirectDrawSurface(surface, bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromGdiDib(@gdiBitmapInfo, gdiBitmapData, bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(hbm: HBITMAP; hpal: HPALETTE);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromHBITMAP(hbm, hpal, bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(hicon: hicon);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromHICON(hicon, bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(hInstance: HMODULE; bitmapName: WideString);
var bitmap          : GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromResource(hInstance, PWideChar(bitmapName), bitmap);
  SetNativeImage(bitmap);
end;

function TGPBitmap.FromDirectDrawSurface7(surface: IDirectDrawSurface7): TGPBitmap;
begin
  Result := TGPBitmap.Create(surface);
end;

function TGPBitmap.FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer): TGPBitmap;
begin
  Result := TGPBitmap.Create(gdiBitmapInfo, gdiBitmapData);
end;

function TGPBitmap.FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE): TGPBitmap;
begin
  Result := TGPBitmap.Create(hbm, hpal);
end;

function TGPBitmap.FromHICON(hicon: hicon): TGPBitmap;
begin
  Result := TGPBitmap.Create(hicon);
end;

function TGPBitmap.FromResource(hInstance: HMODULE; bitmapName: WideString): TGPBitmap;
begin
  Result := TGPBitmap.Create(hInstance, PWideChar(bitmapName));
end;

function TGPBitmap.GetHBITMAP(colorBackground: TGPColor; out hbmReturn: HBITMAP): TStatus;
begin
  Result := SetStatus(GdipCreateHBITMAPFromBitmap(
    GpBitmap(nativeImage),
    hbmReturn,
    colorBackground));
end;

function TGPBitmap.GetHICON(out hicon: hicon): TStatus;
begin
  Result := SetStatus(GdipCreateHICONFromBitmap(
    GpBitmap(nativeImage),
    hicon));
end;

constructor TGPBitmap.Create(nativeBitmap: GpBitmap);
begin
  lastResult := Ok;
  SetNativeImage(nativeBitmap);
end;

(**************************************************************************\
*
*   GDI+ Graphics Object
*
\**************************************************************************)

function TGPGraphics.FromHDC(hdc: hdc): TGPGraphics;
begin
  Result := TGPGraphics.Create(hdc);
end;

function TGPGraphics.FromHDC(hdc: hdc; hDevice: THandle): TGPGraphics;
begin
  Result := TGPGraphics.Create(hdc, hDevice);
end;

function TGPGraphics.FromHWND(HWND: HWND; icm: BOOL = False): TGPGraphics;
begin
  Result := TGPGraphics.Create(HWND, icm);
end;

function TGPGraphics.FromImage(image: TGPImage): TGPGraphics;
begin
  Result := TGPGraphics.Create(image);
end;

constructor TGPGraphics.Create(hdc: hdc);
var graphics        : GpGraphics;
begin
  graphics := nil;
  lastResult := GdipCreateFromHDC(hdc, graphics);
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(hdc: hdc; hDevice: THandle);
var graphics        : GpGraphics;
begin
  graphics := nil;
  lastResult := GdipCreateFromHDC2(hdc, hDevice, graphics);
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(HWND: HWND; icm: BOOL { = FALSE});
var graphics        : GpGraphics;
begin
  graphics := nil;
  if icm then lastResult := GdipCreateFromHWNDICM(HWND, graphics)
  else lastResult := GdipCreateFromHWND(HWND, graphics);
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(image: TGPImage);
var graphics        : GpGraphics;
begin
  graphics := nil;
  if (image <> nil) then
    lastResult := GdipGetImageGraphicsContext(image.nativeImage, graphics);
  SetNativeGraphics(graphics);
end;

destructor TGPGraphics.destroy;
begin
  GdipDeleteGraphics(nativeGraphics);
end;

procedure TGPGraphics.Flush(intention: TFlushIntention = FlushIntentionFlush);
begin
  GdipFlush(nativeGraphics, intention);
end;

//------------------------------------------------------------------------
// GDI Interop methods
//------------------------------------------------------------------------

// Locks the graphics until ReleaseDC is called

function TGPGraphics.GetHDC: hdc;
begin
  SetStatus(GdipGetDC(nativeGraphics, Result));
end;

procedure TGPGraphics.ReleaseHDC(hdc: hdc);
begin
  SetStatus(GdipReleaseDC(nativeGraphics, hdc));
end;

//------------------------------------------------------------------------
// Rendering modes
//------------------------------------------------------------------------

function TGPGraphics.SetRenderingOrigin(X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipSetRenderingOrigin(nativeGraphics, X, Y));
end;

function TGPGraphics.GetRenderingOrigin(out X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRenderingOrigin(nativeGraphics, X, Y));
end;

function TGPGraphics.SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
begin
  Result := SetStatus(GdipSetCompositingMode(nativeGraphics,
    CompositingMode));
end;

function TGPGraphics.GetCompositingMode: TCompositingMode;
begin
  SetStatus(GdipGetCompositingMode(nativeGraphics, Result));
end;

function TGPGraphics.SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
begin
  Result := SetStatus(GdipSetCompositingQuality(nativeGraphics, CompositingQuality));
end;

function TGPGraphics.GetCompositingQuality: TCompositingQuality;
begin
  SetStatus(GdipGetCompositingQuality(nativeGraphics, Result));
end;

function TGPGraphics.SetTextRenderingHint(newMode: TTextRenderingHint): TStatus;
begin
  Result := SetStatus(GdipSetTextRenderingHint(nativeGraphics, newMode));
end;

function TGPGraphics.GetTextRenderingHint: TTextRenderingHint;
begin
  SetStatus(GdipGetTextRenderingHint(nativeGraphics, Result));
end;

function TGPGraphics.SetTextContrast(contrast: UINT): TStatus;
begin
  Result := SetStatus(GdipSetTextContrast(nativeGraphics, contrast));
end;

function TGPGraphics.GetTextContrast: UINT;
begin
  SetStatus(GdipGetTextContrast(nativeGraphics, Result));
end;

function TGPGraphics.GetInterpolationMode: TInterpolationMode;
var mode            : TInterpolationMode;
begin
  mode := InterpolationModeInvalid;
  SetStatus(GdipGetInterpolationMode(nativeGraphics, mode));
  Result := mode;
end;

function TGPGraphics.SetInterpolationMode(InterpolationMode: TInterpolationMode): TStatus;
begin
  Result := SetStatus(GdipSetInterpolationMode(nativeGraphics,
    InterpolationMode));
end;

function TGPGraphics.GetSmoothingMode: TSmoothingMode;
var SmoothingMode   : TSmoothingMode;
begin
  SmoothingMode := SmoothingModeInvalid;
  SetStatus(GdipGetSmoothingMode(nativeGraphics, SmoothingMode));
  Result := SmoothingMode;
end;

function TGPGraphics.SetSmoothingMode(SmoothingMode: TSmoothingMode): TStatus;
begin
  Result := SetStatus(GdipSetSmoothingMode(nativeGraphics, SmoothingMode));
end;

function TGPGraphics.GetPixelOffsetMode: TPixelOffsetMode;
var PixelOffsetMode : TPixelOffsetMode;
begin
  PixelOffsetMode := PixelOffsetModeInvalid;
  SetStatus(GdipGetPixelOffsetMode(nativeGraphics, PixelOffsetMode));
  Result := PixelOffsetMode;
end;

function TGPGraphics.SetPixelOffsetMode(PixelOffsetMode: TPixelOffsetMode): TStatus;
begin
  Result := SetStatus(GdipSetPixelOffsetMode(nativeGraphics, PixelOffsetMode));
end;

//------------------------------------------------------------------------
// Manipulate current world transform
//------------------------------------------------------------------------

function TGPGraphics.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetWorldTransform(nativeGraphics, matrix.nativeMatrix));
end;

function TGPGraphics.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetWorldTransform(nativeGraphics));
end;

function TGPGraphics.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyWorldTransform(nativeGraphics,
    matrix.nativeMatrix,
    order));
end;

function TGPGraphics.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateWorldTransform(nativeGraphics,
    dx, dy, order));
end;

function TGPGraphics.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleWorldTransform(nativeGraphics,
    sx, sy, order));
end;

function TGPGraphics.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateWorldTransform(nativeGraphics,
    angle, order));
end;

function TGPGraphics.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetWorldTransform(nativeGraphics,
    matrix.nativeMatrix));
end;

function TGPGraphics.SetPageUnit(Unit_: TUnit): TStatus;
begin
  Result := SetStatus(GdipSetPageUnit(nativeGraphics,
    Unit_));
end;

function TGPGraphics.SetPageScale(scale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPageScale(nativeGraphics,
    scale));
end;

function TGPGraphics.GetPageUnit: TUnit;
begin
  SetStatus(GdipGetPageUnit(nativeGraphics, Result));
end;

function TGPGraphics.GetPageScale: Single;
begin
  SetStatus(GdipGetPageScale(nativeGraphics, Result));
end;

function TGPGraphics.GetDpiX: Single;
begin
  SetStatus(GdipGetDpiX(nativeGraphics, Result));
end;

function TGPGraphics.GetDpiY: Single;
begin
  SetStatus(GdipGetDpiY(nativeGraphics, Result));
end;

function TGPGraphics.TransformPoints(destSpace: TCoordinateSpace;
  srcSpace: TCoordinateSpace;
  pts: PGPPointF;
  Count: Integer): TStatus;
begin
  Result := SetStatus(GdipTransformPoints(nativeGraphics,
    destSpace,
    srcSpace,
    pts,
    Count));
end;

function TGPGraphics.TransformPoints(destSpace: TCoordinateSpace;
  srcSpace: TCoordinateSpace;
  pts: PGPPoint;
  Count: Integer): TStatus;
begin

  Result := SetStatus(GdipTransformPointsI(nativeGraphics,
    destSpace,
    srcSpace,
    pts,
    Count));
end;

//------------------------------------------------------------------------
// GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
//------------------------------------------------------------------------

function TGPGraphics.GetNearestColor(var color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGetNearestColor(nativeGraphics, @color));
end;

function TGPGraphics.DrawLine(pen: TGPPen; x1, y1, x2, y2: Single): TStatus;
begin
  Result := SetStatus(GdipDrawLine(nativeGraphics,
    pen.nativePen, x1, y1, x2,
    y2));
end;

function TGPGraphics.DrawLine(pen: TGPPen; const pt1, pt2: TGPPointF): TStatus;
begin
  Result := DrawLine(pen, pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TGPGraphics.DrawLines(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLines(nativeGraphics,
    pen.nativePen,
    Points, Count));
end;

function TGPGraphics.DrawLine(pen: TGPPen; x1, y1, x2, y2: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLineI(nativeGraphics,
    pen.nativePen,
    x1,
    y1,
    x2,
    y2));
end;

function TGPGraphics.DrawLine(pen: TGPPen; const pt1, pt2: TGPPoint): TStatus;
begin
  Result := DrawLine(pen,
    pt1.X,
    pt1.Y,
    pt2.X,
    pt2.Y);
end;

function TGPGraphics.DrawLines(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLinesI(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawArc(pen: TGPPen; X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawArc(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height,
    startAngle,
    sweepAngle));
end;

function TGPGraphics.DrawArc(pen: TGPPen; const Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawArc(pen, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    startAngle, sweepAngle);
end;

function TGPGraphics.DrawArc(pen: TGPPen; X, Y, Width, Height: Integer; startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawArcI(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height,
    startAngle,
    sweepAngle));
end;


function TGPGraphics.DrawArc(pen: TGPPen; const Rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawArc(pen,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus;
begin
  Result := SetStatus(GdipDrawBezier(nativeGraphics,
    pen.nativePen, x1, y1,
    x2, y2, x3, y3, x4, y4));
end;

function TGPGraphics.DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPointF): TStatus;
begin
  Result := DrawBezier(pen,
    pt1.X,
    pt1.Y,
    pt2.X,
    pt2.Y,
    pt3.X,
    pt3.Y,
    pt4.X,
    pt4.Y);
end;

function TGPGraphics.DrawBeziers(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawBeziers(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawBezierI(nativeGraphics,
    pen.nativePen,
    x1,
    y1,
    x2,
    y2,
    x3,
    y3,
    x4,
    y4));
end;

function TGPGraphics.DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPoint): TStatus;
begin
  Result := DrawBezier(pen,
    pt1.X,
    pt1.Y,
    pt2.X,
    pt2.Y,
    pt3.X,
    pt3.Y,
    pt4.X,
    pt4.Y);
end;

function TGPGraphics.DrawBeziers(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawBeziersI(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; const Rect: TGPRectF): TStatus;
begin
  Result := DrawRectangle(pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipDrawRectangle(nativeGraphics,
    pen.nativePen, X, Y,
    Width, Height));
end;

function TGPGraphics.DrawRectangles(pen: TGPPen; rects: PGPRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawRectangles(nativeGraphics,
    pen.nativePen,
    rects, Count));
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; const Rect: TGPRect): TStatus;
begin
  Result := DrawRectangle(pen,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height);
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawRectangleI(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphics.DrawRectangles(pen: TGPPen; rects: PGPRect; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawRectanglesI(nativeGraphics,
    pen.nativePen,
    rects,
    Count));
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; const Rect: TGPRectF): TStatus;
begin
  Result := DrawEllipse(pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipDrawEllipse(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; const Rect: TGPRect): TStatus;
begin
  Result := DrawEllipse(pen,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height);
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawEllipseI(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphics.DrawPie(pen: TGPPen; const Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawPie(pen,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawPie(pen: TGPPen; X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawPie(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height,
    startAngle,
    sweepAngle));
end;

function TGPGraphics.DrawPie(pen: TGPPen; const Rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawPie(pen,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawPie(pen: TGPPen; X, Y, Width, Height: Integer;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawPieI(nativeGraphics,
    pen.nativePen,
    X,
    Y,
    Width,
    Height,
    startAngle,
    sweepAngle));
end;

function TGPGraphics.DrawPolygon(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawPolygon(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawPolygon(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawPolygonI(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawPath(pen: TGPPen; path: TGPGraphicsPath): TStatus;
var
  nPen              : GpPen;
  nPath             : GpPath;
begin
  if Assigned(pen) then nPen := pen.nativePen else nPen := nil;
  if Assigned(path) then nPath := path.nativePath else nPath := nil;
  Result := SetStatus(GdipDrawPath(nativeGraphics, nPen, nPath));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawCurve(nativeGraphics,
    pen.nativePen, Points,
    Count));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; Points: PGPPointF; Count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawCurve2(nativeGraphics,
    pen.nativePen, Points,
    Count, tension));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; Points: PGPPointF; Count, offset,
  numberOfSegments: Integer; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipDrawCurve3(nativeGraphics,
    pen.nativePen, Points,
    Count, offset,
    numberOfSegments, tension));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawCurveI(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; Points: PGPPoint; Count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawCurve2I(nativeGraphics,
    pen.nativePen,
    Points,
    Count,
    tension));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; Points: PGPPoint; Count, offset,
  numberOfSegments: Integer; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipDrawCurve3I(nativeGraphics,
    pen.nativePen,
    Points,
    Count,
    offset,
    numberOfSegments,
    tension));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve(nativeGraphics,
    pen.nativePen,
    Points, Count));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; Points: PGPPointF; Count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve2(nativeGraphics,
    pen.nativePen,
    Points, Count,
    tension));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurveI(nativeGraphics,
    pen.nativePen,
    Points,
    Count));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; Points: PGPPoint;
  Count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve2I(nativeGraphics,
    pen.nativePen,
    Points,
    Count,
    tension));
end;

function TGPGraphics.Clear(color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGraphicsClear(
    nativeGraphics,
    color));
end;

function TGPGraphics.FillRectangle(brush: TGPBrush; const Rect: TGPRectF): TStatus;
begin
  Result := FillRectangle(brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphics.FillRectangle(brush: TGPBrush; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipFillRectangle(nativeGraphics,
    brush.nativeBrush, X, Y,
    Width, Height));
end;

function TGPGraphics.FillRectangles(brush: TGPBrush; rects: PGPRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangles(nativeGraphics,
    brush.nativeBrush,
    rects, Count));
end;

function TGPGraphics.FillRectangle(brush: TGPBrush; const Rect: TGPRect): TStatus;
begin
  Result := FillRectangle(brush,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height);
end;

function TGPGraphics.FillRectangle(brush: TGPBrush; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangleI(nativeGraphics,
    brush.nativeBrush,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphics.FillRectangles(brush: TGPBrush; rects: PGPRect; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectanglesI(nativeGraphics,
    brush.nativeBrush,
    rects,
    Count));
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := FillPolygon(brush, Points, Count, FillModeAlternate);
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; Points: PGPPointF; Count: Integer;
  FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygon(nativeGraphics,
    brush.nativeBrush,
    Points, Count, FillMode));
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := FillPolygon(brush, Points, Count, FillModeAlternate);
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; Points: PGPPoint; Count: Integer;
  FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygonI(nativeGraphics,
    brush.nativeBrush,
    Points, Count,
    FillMode));
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; const Rect: TGPRectF): TStatus;
begin
  Result := FillEllipse(brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipFillEllipse(nativeGraphics,
    brush.nativeBrush, X, Y,
    Width, Height));
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; const Rect: TGPRect): TStatus;
begin
  Result := FillEllipse(brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillEllipseI(nativeGraphics,
    brush.nativeBrush,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphics.FillPie(brush: TGPBrush; const Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
begin
  Result := FillPie(brush, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    startAngle, sweepAngle);
end;

function TGPGraphics.FillPie(brush: TGPBrush; X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPie(nativeGraphics,
    brush.nativeBrush, X, Y,
    Width, Height, startAngle,
    sweepAngle));
end;

function TGPGraphics.FillPie(brush: TGPBrush; const Rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
begin
  Result := FillPie(brush, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    startAngle, sweepAngle);
end;

function TGPGraphics.FillPie(brush: TGPBrush; X, Y, Width, Height: Integer; startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPieI(nativeGraphics,
    brush.nativeBrush,
    X,
    Y,
    Width,
    Height,
    startAngle,
    sweepAngle));
end;

function TGPGraphics.fillPath(brush: TGPBrush; path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipFillPath(nativeGraphics,
    brush.nativeBrush,
    path.nativePath));
end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve(nativeGraphics,
    brush.nativeBrush,
    Points, Count));

end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; Points: PGPPointF; Count: Integer;
  FillMode: TFillMode; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2(nativeGraphics,
    brush.nativeBrush,
    Points, Count,
    tension, FillMode));
end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurveI(nativeGraphics,
    brush.nativeBrush,
    Points,
    Count));
end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; Points: PGPPoint;
  Count: Integer; FillMode: TFillMode; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2I(nativeGraphics,
    brush.nativeBrush,
    Points, Count,
    tension, FillMode));
end;

function TGPGraphics.FillRegion(brush: TGPBrush; region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipFillRegion(nativeGraphics,
    brush.nativeBrush,
    region.nativeRegion));
end;


function TGPGraphics.DrawString(string_: WideString; Length: Integer; font: TGPFont;
  const layoutRect: TGPRectF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus;
var
  nFont             : GpFont;
  nStringFormat     : GpStringFormat;
  nBrush            : GpBrush;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(stringFormat) then nStringFormat := stringFormat.nativeFormat else nStringFormat := nil;
  if Assigned(brush) then nBrush := brush.nativeBrush else nBrush := nil;
  Result := SetStatus(GdipDrawString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @layoutRect,
    nStringFormat,
    nBrush));
end;

function TGPGraphics.DrawString(string_: WideString; Length: Integer; font: TGPFont;
  const origin: TGPPointF; brush: TGPBrush): TStatus;
var
  Rect              : TGPRectF;
  nFont             : GpFont;
  nBrush            : GpBrush;
begin
  Rect.X := origin.X;
  Rect.Y := origin.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(brush) then nBrush := brush.nativeBrush else nBrush := nil;
  Result := SetStatus(GdipDrawString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @Rect,
    nil,
    nBrush));
end;

function TGPGraphics.DrawString(string_: WideString; Length: Integer; font: TGPFont;
  const origin: TGPPointF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus;
var
  Rect              : TGPRectF;
  nFont             : GpFont;
  nStringFormat     : GpStringFormat;
  nBrush            : GpBrush;
begin
  Rect.X := origin.X;
  Rect.Y := origin.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(stringFormat) then nStringFormat := stringFormat.nativeFormat else nStringFormat := nil;
  if Assigned(brush) then nBrush := brush.nativeBrush else nBrush := nil;
  Result := SetStatus(GdipDrawString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @Rect,
    nStringFormat,
    nBrush));
end;


function TGPGraphics.MeasureString(string_: WideString; Length: Integer; font: TGPFont;
  const layoutRect: TGPRectF; stringFormat: TGPStringFormat; out BoundingBox: TGPRectF;
  codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus;
var
  nFont             : GpFont;
  nStringFormat     : GpStringFormat;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(stringFormat) then nStringFormat := stringFormat.nativeFormat else nStringFormat := nil;
  Result := SetStatus(GdipMeasureString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @layoutRect,
    nStringFormat,
    @BoundingBox,
    codepointsFitted,
    linesFilled
    ));
end;


function TGPGraphics.MeasureString(string_: WideString; Length: Integer; font: TGPFont;
  const layoutRectSize: TGPSizeF; stringFormat: TGPStringFormat; out size: TGPSizeF;
  codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus;
var
  layoutRect, BoundingBox: TGPRectF;
  Status            : TStatus;
  nFont             : GpFont;
  nStringFormat     : GpStringFormat;
begin
  layoutRect.X := 0;
  layoutRect.Y := 0;
  layoutRect.Width := layoutRectSize.Width;
  layoutRect.Height := layoutRectSize.Height;

  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(stringFormat) then nStringFormat := stringFormat.nativeFormat else nStringFormat := nil;

  Status := SetStatus(GdipMeasureString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @layoutRect,
    nStringFormat,
    @BoundingBox,
    codepointsFitted,
    linesFilled
    ));

  if (Status = Ok) then begin
    size.Width := BoundingBox.Width;
    size.Height := BoundingBox.Height;
  end;
  Result := Status;
end;


function TGPGraphics.MeasureString(string_: WideString; Length: Integer; font: TGPFont;
  const origin: TGPPointF; stringFormat: TGPStringFormat; out BoundingBox: TGPRectF): TStatus;
var
  Rect              : TGPRectF;
  nFont             : GpFont;
  nStringFormat     : GpStringFormat;
begin
  Rect.X := origin.X;
  Rect.Y := origin.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;

  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(stringFormat) then nStringFormat := stringFormat.nativeFormat else nStringFormat := nil;

  Result := SetStatus(GdipMeasureString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @Rect,
    nStringFormat,
    @BoundingBox,
    nil,
    nil
    ));
end;


function TGPGraphics.MeasureString(string_: WideString; Length: Integer; font: TGPFont;
  const layoutRect: TGPRectF; out BoundingBox: TGPRectF): TStatus;
var
  nFont             : GpFont;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  Result := SetStatus(GdipMeasureString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @layoutRect,
    nil,
    @BoundingBox,
    nil,
    nil
    ));
end;


function TGPGraphics.MeasureString(string_: WideString; Length: Integer; font: TGPFont;
  const origin: TGPPointF; out BoundingBox: TGPRectF): TStatus;
var
  nFont             : GpFont;
  Rect              : TGPRectF;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  Rect.X := origin.X;
  Rect.Y := origin.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;

  Result := SetStatus(GdipMeasureString(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @Rect,
    nil,
    @BoundingBox,
    nil,
    nil
    ));
end;



function TGPGraphics.MeasureCharacterRanges(string_: WideString; Length: Integer; font: TGPFont;
  const layoutRect: TGPRectF; stringFormat: TGPStringFormat; regionCount: Integer;
  const regions: array of TGPRegion): TStatus;
var
  nativeRegions     : Pointer;
  i                 : Integer;
  Status            : TStatus;
  nFont             : GpFont;
  nStringFormat     : GpStringFormat;
type
  TArrayGpRegion = array of GpRegion;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(stringFormat) then nStringFormat := stringFormat.nativeFormat else nStringFormat := nil;

  if (regionCount <= 0) then begin
    Result := InvalidParameter;
    exit;
  end;

  getmem(nativeRegions, sizeof(GpRegion) * regionCount);

  for i := 0 to regionCount - 1 do
    TArrayGpRegion(nativeRegions)[i] := regions[i].nativeRegion;

  Status := SetStatus(GdipMeasureCharacterRanges(
    nativeGraphics,
    PWideChar(string_),
    Length,
    nFont,
    @layoutRect,
    nStringFormat,
    regionCount,
    nativeRegions
    ));

  freemem(nativeRegions, sizeof(GpRegion) * regionCount);
  Result := Status;
end;

function TGPGraphics.DrawDriverString(text: PUINT16; Length: Integer; font: TGPFont
  ; brush: TGPBrush; positions: PGPPointF; Flags: Integer
  ; matrix: TGPMatrix): TStatus;
var
  nFont             : GpFont;
  nBrush            : GpBrush;
  nmatrix           : GpMatrix;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(brush) then nBrush := brush.nativeBrush else nBrush := nil;
  if Assigned(matrix) then nmatrix := matrix.nativeMatrix else nmatrix := nil;

  Result := SetStatus(GdipDrawDriverString(
    nativeGraphics,
    text,
    Length,
    nFont,
    nBrush,
    positions,
    Flags,
    nmatrix));
end;

function TGPGraphics.MeasureDriverString(text: PUINT16; Length: Integer; font: TGPFont;
  positions: PGPPointF; Flags: Integer; matrix: TGPMatrix;
  out BoundingBox: TGPRectF): TStatus;
var
  nFont             : GpFont;
  nmatrix           : GpMatrix;
begin
  if Assigned(font) then nFont := font.nativeFont else nFont := nil;
  if Assigned(matrix) then nmatrix := matrix.nativeMatrix else nmatrix := nil;

  Result := SetStatus(GdipMeasureDriverString(
    nativeGraphics,
    text,
    Length,
    nFont,
    positions,
    Flags,
    nmatrix,
    @BoundingBox
    ));
end;

// Draw a cached bitmap on this graphics destination offset by
// x, y. Note this will fail with WrongState if the CachedBitmap
// native format differs from this Graphics.

function TGPGraphics.DrawCachedBitmap(cb: TGPCachedBitmap; X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawCachedBitmap(
    nativeGraphics,
    cb.nativeCachedBitmap,
    X, Y
    ));
end;

function TGPGraphics.DrawImage(image: TGPImage; const point: TGPPointF): TStatus;
begin
  Result := DrawImage(image, point.X, point.Y);
end;

function TGPGraphics.DrawImage(image: TGPImage; X, Y: Single): TStatus;
var
  nImage            : GpImage;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImage(nativeGraphics, nImage, X, Y));
end;

function TGPGraphics.DrawImage(image: TGPImage; const Rect: TGPRectF): TStatus;
begin
  Result := DrawImage(image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphics.DrawImage(image: TGPImage; X, Y, Width, Height: Single): TStatus;
var
  nImage            : GpImage;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImageRect(nativeGraphics,
    nImage,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphics.DrawImage(image: TGPImage; const point: TGPPoint): TStatus;
begin
  Result := DrawImage(image, point.X, point.Y);
end;

function TGPGraphics.DrawImage(image: TGPImage; X, Y: Integer): TStatus;
var
  nImage            : GpImage;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImageI(nativeGraphics,
    nImage,
    X,
    Y));
end;

function TGPGraphics.DrawImage(image: TGPImage; const Rect: TGPRect): TStatus;
begin
  Result := DrawImage(image,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height);
end;

function TGPGraphics.DrawImage(image: TGPImage; X, Y, Width, Height: Integer): TStatus;
var
  nImage            : GpImage;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImageRectI(nativeGraphics,
    nImage,
    X,
    Y,
    Width,
    Height));
end;


// Affine Draw Image
// destPoints.length = 3: rect => parallelogram
//     destPoints[0] <=> top-left corner of the source rectangle
//     destPoints[1] <=> top-right corner
//     destPoints[2] <=> bottom-left corner
// destPoints.length = 4: rect => quad
//     destPoints[3] <=> bottom-right corner

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPointF; Count: Integer): TStatus;
var
  nImage            : GpImage;
begin
  if (((Count <> 3) and (Count <> 4)) or (destPoints = nil)) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImagePoints(nativeGraphics,
    nImage,
    destPoints, Count));
end;

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPoint; Count: Integer): TStatus;
var
  nImage            : GpImage;
begin
  if (((Count <> 3) and (Count <> 4)) or (destPoints = nil)) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;

  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImagePointsI(nativeGraphics,
    nImage,
    destPoints,
    Count));
end;

function TGPGraphics.DrawImage(image: TGPImage; X, Y, srcx, srcy, srcwidth, srcheight: Single;
  srcUnit: TUnit): TStatus;
var
  nImage            : GpImage;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImagePointRect(nativeGraphics,
    nImage,
    X, Y,
    srcx, srcy,
    srcwidth, srcheight, srcUnit));
end;

function TGPGraphics.DrawImage(image: TGPImage; const destRect: TGPRectF; srcx, srcy, srcwidth, srcheight: Single;
  srcUnit: TUnit; imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage            : GpImage;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipDrawImageRectRect(nativeGraphics,
    nImage,
    destRect.X,
    destRect.Y,
    destRect.Width,
    destRect.Height,
    srcx, srcy,
    srcwidth, srcheight,
    srcUnit,
    nimageAttributes,
    callback,
    callbackData));
end;

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPointF; Count: Integer;
  srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
  imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage            : GpImage;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipDrawImagePointsRect(nativeGraphics,
    nImage,
    destPoints, Count,
    srcx, srcy,
    srcwidth,
    srcheight,
    srcUnit,
    nimageAttributes,
    callback,
    callbackData));
end;

function TGPGraphics.DrawImage(image: TGPImage; X, Y, srcx, srcy, srcwidth, srcheight: Integer;
  srcUnit: TUnit): TStatus;
var
  nImage            : GpImage;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  Result := SetStatus(GdipDrawImagePointRectI(nativeGraphics,
    nImage,
    X,
    Y,
    srcx,
    srcy,
    srcwidth,
    srcheight,
    srcUnit));
end;

function TGPGraphics.DrawImage(image: TGPImage; const destRect: TGPRect; srcx, srcy, srcwidth,
  srcheight: Integer; srcUnit: TUnit; imageAttributes: TGPImageAttributes = nil;
  callback: DrawImageAbort = nil; callbackData: Pointer = nil): TStatus;
var
  nImage            : GpImage;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipDrawImageRectRectI(nativeGraphics,
    nImage,
    destRect.X,
    destRect.Y,
    destRect.Width,
    destRect.Height,
    srcx,
    srcy,
    srcwidth,
    srcheight,
    srcUnit,
    nimageAttributes,
    callback,
    callbackData));
end;

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPoint;
  Count, srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
  imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage            : GpImage;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(image) then nImage := image.nativeImage else nImage := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;

  Result := SetStatus(GdipDrawImagePointsRectI(nativeGraphics,
    nImage,
    destPoints,
    Count,
    srcx,
    srcy,
    srcwidth,
    srcheight,
    srcUnit,
    nimageAttributes,
    callback,
    callbackData));
end;

// The following methods are for playing an EMF+ to a graphics
// via the enumeration interface.  Each record of the EMF+ is
// sent to the callback (along with the callbackData).  Then
// the callback can invoke the Metafile::PlayRecord method
// to play the particular record.


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPoint(
    nativeGraphics,
    nMetafile,
    @destPoint,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPoint;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPointI(
    nativeGraphics,
    nMetafile,
    @destPoint,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestRect(
    nativeGraphics,
    nMetafile,
    @destRect,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRect;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestRectI(
    nativeGraphics,
    nMetafile,
    @destRect,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
  Count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPoints(
    nativeGraphics,
    nMetafile,
    destPoints,
    Count,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
  Count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPointsI(
    nativeGraphics,
    nMetafile,
    destPoints,
    Count,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
  const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoint(
    nativeGraphics,
    nMetafile,
    @destPoint,
    @srcRect,
    srcUnit,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPoint;
  const srcRect: TGPRect; srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointI(
    nativeGraphics,
    nMetafile,
    @destPoint,
    @srcRect,
    srcUnit,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
  const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestRect(
    nativeGraphics,
    nMetafile,
    @destRect,
    @srcRect,
    srcUnit,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect, srcRect: TGPRect;
  srcUnit: TUnit; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestRectI(
    nativeGraphics,
    nMetafile,
    @destRect,
    @srcRect,
    srcUnit,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
  Count: Integer; const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoints(
    nativeGraphics,
    nMetafile,
    destPoints,
    Count,
    @srcRect,
    srcUnit,
    callback,
    callbackData,
    nimageAttributes));
end;


function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
  Count: Integer; const srcRect: TGPRect; srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile         : GpMetafile;
  nimageAttributes  : GpImageAttributes;
begin
  if Assigned(metafile) then nMetafile := GpMetafile(metafile.nativeImage) else nMetafile := nil;
  if Assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointsI(
    nativeGraphics,
    nMetafile,
    destPoints,
    Count,
    @srcRect,
    srcUnit,
    callback,
    callbackData,
    nimageAttributes));
end;

function TGPGraphics.SetClip(G: TGPGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipGraphics(nativeGraphics,
    G.nativeGraphics,
    CombineMode));
end;

function TGPGraphics.SetClip(Rect: TGPRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(nativeGraphics,
    Rect.X, Rect.Y,
    Rect.Width, Rect.Height,
    CombineMode));
end;

function TGPGraphics.SetClip(Rect: TGPRect; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(nativeGraphics,
    Rect.X, Rect.Y,
    Rect.Width, Rect.Height,
    CombineMode));
end;

function TGPGraphics.SetClip(path: TGPGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipPath(nativeGraphics,
    path.nativePath,
    CombineMode));
end;

function TGPGraphics.SetClip(region: TGPRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(nativeGraphics,
    region.nativeRegion,
    CombineMode));
end;

// This is different than the other SetClip methods because it assumes
// that the HRGN is already in device units, so it doesn't transform
// the coordinates in the HRGN.

function TGPGraphics.SetClip(hRgn: hRgn; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipHrgn(nativeGraphics, hRgn,
    CombineMode));
end;

function TGPGraphics.IntersectClip(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(nativeGraphics,
    Rect.X, Rect.Y,
    Rect.Width, Rect.Height,
    CombineModeIntersect));
end;

function TGPGraphics.IntersectClip(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(nativeGraphics,
    Rect.X, Rect.Y,
    Rect.Width, Rect.Height,
    CombineModeIntersect));
end;

function TGPGraphics.IntersectClip(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(nativeGraphics,
    region.nativeRegion,
    CombineModeIntersect));
end;

function TGPGraphics.ExcludeClip(const Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(nativeGraphics,
    Rect.X, Rect.Y,
    Rect.Width, Rect.Height,
    CombineModeExclude));
end;

function TGPGraphics.ExcludeClip(const Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(nativeGraphics,
    Rect.X, Rect.Y,
    Rect.Width, Rect.Height,
    CombineModeExclude));
end;

function TGPGraphics.ExcludeClip(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(nativeGraphics,
    region.nativeRegion,
    CombineModeExclude));
end;

function TGPGraphics.ResetClip: TStatus;
begin
  Result := SetStatus(GdipResetClip(nativeGraphics));
end;

function TGPGraphics.TranslateClip(dx, dy: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateClip(nativeGraphics, dx, dy));
end;

function TGPGraphics.TranslateClip(dx, dy: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateClipI(nativeGraphics,
    dx, dy));
end;

function TGPGraphics.GetClip(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipGetClip(nativeGraphics,
    region.nativeRegion));
end;

function TGPGraphics.GetClipBounds(out Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetClipBounds(nativeGraphics, @Rect));
end;

function TGPGraphics.GetClipBounds(out Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetClipBoundsI(nativeGraphics, @Rect));
end;

function TGPGraphics.IsClipEmpty: BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsClipEmpty(nativeGraphics, @booln));
  Result := booln;
end;

function TGPGraphics.GetVisibleClipBounds(out Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBounds(nativeGraphics, @Rect));
end;

function TGPGraphics.GetVisibleClipBounds(out Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBoundsI(nativeGraphics, @Rect));
end;

function TGPGraphics.IsVisibleClipEmpty: BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsVisibleClipEmpty(nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(X, Y: Integer): BOOL;
var pt              : TGPPoint;
begin
  pt.X := X; pt.Y := Y;
  Result := IsVisible(pt);
end;

function TGPGraphics.IsVisible(const point: TGPPoint): BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsVisiblePointI(nativeGraphics,
    point.X,
    point.Y,
    booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(X, Y, Width, Height: Integer): BOOL;
var booln           : BOOL;
begin
  booln := True;
  SetStatus(GdipIsVisibleRectI(nativeGraphics,
    X,
    Y,
    Width,
    Height,
    booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(const Rect: TGPRect): BOOL;
var booln           : BOOL;
begin
  booln := True;
  SetStatus(GdipIsVisibleRectI(nativeGraphics,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(X, Y: Single): BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsVisiblePoint(nativeGraphics,
    X,
    Y,
    booln));

  Result := booln;
end;

function TGPGraphics.IsVisible(const point: TGPPointF): BOOL;
var booln           : BOOL;
begin
  booln := False;
  SetStatus(GdipIsVisiblePoint(nativeGraphics,
    point.X,
    point.Y,
    booln));

  Result := booln;
end;

function TGPGraphics.IsVisible(X, Y, Width, Height: Single): BOOL;
var booln           : BOOL;
begin
  booln := True;
  SetStatus(GdipIsVisibleRect(nativeGraphics,
    X,
    Y,
    Width,
    Height,
    booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(const Rect: TGPRectF): BOOL;
var booln           : BOOL;
begin
  booln := True;
  SetStatus(GdipIsVisibleRect(nativeGraphics,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    booln));
  Result := booln;
end;

function TGPGraphics.Save: GraphicsState;
begin
  SetStatus(GdipSaveGraphics(nativeGraphics, Result));
end;

function TGPGraphics.Restore(gstate: GraphicsState): TStatus;
begin
  Result := SetStatus(GdipRestoreGraphics(nativeGraphics,
    gstate));
end;

function TGPGraphics.BeginContainer(const dstrect, srcRect: TGPRectF; Unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainer(nativeGraphics, @dstrect,
    @srcRect, Unit_, Result));
end;

function TGPGraphics.BeginContainer(const dstrect, srcRect: TGPRect; Unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainerI(nativeGraphics, @dstrect,
    @srcRect, Unit_, Result));
end;

function TGPGraphics.BeginContainer: GraphicsContainer;
begin
  SetStatus(GdipBeginContainer2(nativeGraphics, Result));
end;

function TGPGraphics.EndContainer(state: GraphicsContainer): TStatus;
begin
  Result := SetStatus(GdipEndContainer(nativeGraphics, state));
end;

// Only valid when recording metafiles.

function TGPGraphics.AddMetafileComment(data: PBYTE; sizeData: UINT): TStatus;
begin
  Result := SetStatus(GdipComment(nativeGraphics, sizeData, data));
end;

function TGPGraphics.GetHalftonePalette: HPALETTE;
begin
  Result := GdipCreateHalftonePalette;
end;

function TGPGraphics.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPGraphics.Create(graphics: GpGraphics);
begin
  lastResult := Ok;
  SetNativeGraphics(graphics);
end;

procedure TGPGraphics.SetNativeGraphics(graphics: GpGraphics);
begin
  Self.nativeGraphics := graphics;
end;

function TGPGraphics.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

function TGPGraphics.GetNativeGraphics: GpGraphics;
begin
  Result := Self.nativeGraphics;
end;

function TGPGraphics.GetNativePen(pen: TGPPen): GpPen;
begin
  Result := pen.nativePen;
end;

(**************************************************************************\
*
*   GDI+ Font Family class
*
\**************************************************************************)

constructor TGPFontFamily.Create;
begin
  nativeFamily := nil;
  lastResult := Ok;
end;

constructor TGPFontFamily.Create(name: WideString; fontCollection: TGPFontCollection = nil);
var nfontCollection : GpFontCollection;
begin
  nativeFamily := nil;
  if Assigned(fontCollection) then nfontCollection := fontCollection.nativeFontCollection else nfontCollection := nil;
  lastResult := GdipCreateFontFamilyFromName(PWideChar(name), nfontCollection, nativeFamily);
end;

destructor TGPFontFamily.destroy;
begin
  GdipDeleteFontFamily(nativeFamily);
end;

class function TGPFontFamily.GenericSansSerif: TGPFontFamily;
var
  nFontFamily       : GpFontFamily;
begin
  if (GenericSansSerifFontFamily <> nil) then begin
    Result := GenericSansSerifFontFamily;
    exit;
  end;
  GenericSansSerifFontFamily := TGPFontFamily.Create;
  GenericSansSerifFontFamily.lastResult := GdipGetGenericFontFamilySansSerif(nFontFamily);
  GenericSansSerifFontFamily.nativeFamily := nFontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TGPFontFamily.GenericSerif: TGPFontFamily;
var nFontFamily     : GpFontFamily;
begin
  if (GenericSerifFontFamily <> nil) then begin
    Result := GenericSerifFontFamily;
    exit;
  end;

  GenericSerifFontFamily := TGPFontFamily.Create; // (GenericSerifFontFamilyBuffer);
  GenericSerifFontFamily.lastResult := GdipGetGenericFontFamilySerif(nFontFamily);
  GenericSerifFontFamily.nativeFamily := nFontFamily;
  Result := GenericSerifFontFamily;
end;

class function TGPFontFamily.GenericMonospace: TGPFontFamily;
var nFontFamily     : GpFontFamily;
begin
  if (GenericMonospaceFontFamily <> nil) then begin
    Result := GenericMonospaceFontFamily;
    exit;
  end;
  GenericMonospaceFontFamily := TGPFontFamily.Create; // (GenericMonospaceFontFamilyBuffer);
  GenericMonospaceFontFamily.lastResult := GdipGetGenericFontFamilyMonospace(nFontFamily);
  GenericMonospaceFontFamily.nativeFamily := nFontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TGPFontFamily.GetFamilyName(out name: string; language: LANGID = 0): TStatus;
var str             : array[0..LF_FACESIZE - 1] of WideChar;
begin
  Result := SetStatus(GdipGetFamilyName(nativeFamily, @str, language));
  name := str;
end;

function TGPFontFamily.Clone: TGPFontFamily;
var
  clonedFamily      : GpFontFamily;
begin
  clonedFamily := nil;
  SetStatus(GdipCloneFontFamily(nativeFamily, clonedFamily));
  Result := TGPFontFamily.Create(clonedFamily, lastResult);
end;

function TGPFontFamily.IsAvailable: BOOL;
begin
  Result := (nativeFamily <> nil);
end;

function TGPFontFamily.IsStyleAvailable(style: Integer): BOOL;
var
  StyleAvailable    : BOOL;
  Status            : TStatus;
begin
  Status := SetStatus(GdipIsStyleAvailable(nativeFamily, style, StyleAvailable));
  if (Status <> Ok) then StyleAvailable := False;
  Result := StyleAvailable;
end;

function TGPFontFamily.GetEmHeight(style: Integer): UINT16;
begin
  SetStatus(GdipGetEmHeight(nativeFamily, style, Result));
end;

function TGPFontFamily.GetCellAscent(style: Integer): UINT16;
begin
  SetStatus(GdipGetCellAscent(nativeFamily, style, Result));
end;

function TGPFontFamily.GetCellDescent(style: Integer): UINT16;
begin
  SetStatus(GdipGetCellDescent(nativeFamily, style, Result));
end;

function TGPFontFamily.GetLineSpacing(style: Integer): UINT16;
begin
  SetStatus(GdipGetLineSpacing(nativeFamily, style, Result));
end;

function TGPFontFamily.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

function TGPFontFamily.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

constructor TGPFontFamily.Create(nativeOrig: GpFontFamily; Status: TStatus);
begin
  lastResult := Status;
  nativeFamily := nativeOrig;
end;

(**************************************************************************\
*
*   GDI+ Font class
*
\**************************************************************************)

constructor TGPFont.Create(hdc: hdc);
var font            : GpFont;
begin
  font := nil;
  lastResult := GdipCreateFontFromDC(hdc, font);
  SetNativeFont(font);
end;

constructor TGPFont.Create(hdc: hdc; logfont: PLOGFONTA);
var font            : GpFont;
begin
  font := nil;
  if Assigned(logfont) then
    lastResult := GdipCreateFontFromLogfontA(hdc, logfont, font)
  else
    lastResult := GdipCreateFontFromDC(hdc, font);
  SetNativeFont(font);
end;

constructor TGPFont.Create(hdc: hdc; logfont: PLOGFONTW);
var font            : GpFont;
begin
  font := nil;
  if Assigned(logfont) then
    lastResult := GdipCreateFontFromLogfontW(hdc, logfont, font)
  else
    lastResult := GdipCreateFontFromDC(hdc, font);
  SetNativeFont(font);
end;

constructor TGPFont.Create(hdc: hdc; hfont: hfont);
var
  font              : GpFont;
  lf                : logfontA;
begin
  font := nil;
  if BOOL(hfont) then begin
    if (BOOL(GetObjectA(hfont, sizeof(logfontA), @lf))) then
      lastResult := GdipCreateFontFromLogfontA(hdc, @lf, font)
    else
      lastResult := GdipCreateFontFromDC(hdc, font);
  end
  else
    lastResult := GdipCreateFontFromDC(hdc, font);
  SetNativeFont(font);
end;

constructor TGPFont.Create(family: TGPFontFamily; emSize: Single;
  style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint);
var
  font              : GpFont;
  nFontFamily       : GpFontFamily;
begin
  font := nil;
  if Assigned(family) then nFontFamily := family.nativeFamily else nFontFamily := nil;
  lastResult := GdipCreateFont(nFontFamily, emSize, Integer(style), Integer(Unit_), font);
  SetNativeFont(font);
end;

constructor TGPFont.Create(familyName: WideString; emSize: Single;
  style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint;
  fontCollection: TGPFontCollection = nil);
var
  family            : TGPFontFamily;
  nativeFamily      : GpFontFamily;
begin
  nativeFont := nil;
  family := TGPFontFamily.Create(familyName, fontCollection);
  nativeFamily := family.nativeFamily;
  lastResult := family.GetLastStatus;
  if (lastResult <> Ok) then begin
    nativeFamily := TGPFontFamily.GenericSansSerif.nativeFamily;
    lastResult := TGPFontFamily.GenericSansSerif.lastResult;
    if (lastResult <> Ok) then begin
      family.Free;
      exit;
    end;
  end;

  lastResult := GdipCreateFont(nativeFamily,
    emSize,
    Integer(style),
    Integer(Unit_),
    nativeFont);

  if (lastResult <> Ok) then begin
    nativeFamily := TGPFontFamily.GenericSansSerif.nativeFamily;
    lastResult := TGPFontFamily.GenericSansSerif.lastResult;
    if (lastResult <> Ok) then begin
      family.Free;
      exit;
    end;

    lastResult := GdipCreateFont(
      nativeFamily,
      emSize,
      Integer(style),
      Integer(Unit_),
      nativeFont);
  end;
  family.Free;
end;

function TGPFont.GetLogFontA(G: TGPGraphics; out logfontA: TLogFontA): TStatus;
var nGraphics       : GpGraphics;
begin
  if Assigned(G) then nGraphics := G.nativeGraphics else nGraphics := nil;
  Result := SetStatus(GdipGetLogFontA(nativeFont, nGraphics, logfontA));
end;

function TGPFont.GetLogFontW(G: TGPGraphics; out logfontW: TLogFontW): TStatus;
var nGraphics       : GpGraphics;
begin
  if Assigned(G) then nGraphics := G.nativeGraphics else nGraphics := nil;
  Result := SetStatus(GdipGetLogFontW(nativeFont, nGraphics, logfontW));
end;

function TGPFont.Clone: TGPFont;
var cloneFont       : GpFont;
begin
  cloneFont := nil;
  SetStatus(GdipCloneFont(nativeFont, cloneFont));
  Result := TGPFont.Create(cloneFont, lastResult);
end;

destructor TGPFont.destroy;
begin
  GdipDeleteFont(nativeFont);
end;

function TGPFont.IsAvailable: BOOL;
begin
  Result := (nativeFont <> nil);
end;

function TGPFont.GetStyle: Integer;
begin
  SetStatus(GdipGetFontStyle(nativeFont, Result));
end;

function TGPFont.GetSize: Single;
begin
  SetStatus(GdipGetFontSize(nativeFont, Result));
end;

function TGPFont.GetUnit: TUnit;
begin
  SetStatus(GdipGetFontUnit(nativeFont, Result));
end;

function TGPFont.GetLastStatus: TStatus;
begin
  Result := lastResult;
end;

function TGPFont.GetHeight(graphics: TGPGraphics): Single;
var nGraphics       : GpGraphics;
begin
  if Assigned(graphics) then nGraphics := graphics.nativeGraphics else nGraphics := nil;
  SetStatus(GdipGetFontHeight(nativeFont, nGraphics, Result));
end;

function TGPFont.GetHeight(dpi: Single): Single;
begin
  SetStatus(GdipGetFontHeightGivenDPI(nativeFont, dpi, Result));
end;

function TGPFont.GetFamily(family: TGPFontFamily): TStatus;
var
  Status            : TStatus;
  nFamily           : GpFontFamily;
begin
  if (family = nil) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;

  Status := GdipGetFamily(nativeFont, nFamily);
  family.nativeFamily := nFamily;
  family.SetStatus(Status);
  Result := SetStatus(Status);
end;

constructor TGPFont.Create(font: GpFont; Status: TStatus);
begin
  lastResult := Status;
  SetNativeFont(font);
end;

procedure TGPFont.SetNativeFont(font: GpFont);
begin
  nativeFont := font;
end;

function TGPFont.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

(**************************************************************************\
*
*   Font collections (Installed and Private)
*
\**************************************************************************)

constructor TGPFontCollection.Create;
begin
  nativeFontCollection := nil;
end;

destructor TGPFontCollection.destroy;
begin
  inherited destroy;
end;

function TGPFontCollection.GetFamilyCount: Integer;
var
  numFound          : Integer;
begin
  numFound := 0;
  lastResult := GdipGetFontCollectionFamilyCount(nativeFontCollection, numFound);
  Result := numFound;
end;

function TGPFontCollection.GetFamilies(numSought: Integer; out gpfamilies: array of TGPFontFamily;
  out numFound: Integer): TStatus;
var
  nativeFamilyList  : Pointer;
  Status            : TStatus;
  i                 : Integer;
type
  ArrGpFontFamily = array of GpFontFamily;
begin
  if ((numSought <= 0) or (Length(gpfamilies) = 0)) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;
  numFound := 0;

  getmem(nativeFamilyList, numSought * sizeof(GpFontFamily));
  if nativeFamilyList = nil then begin
    Result := SetStatus(OutOfMemory);
    exit;
  end;

  Status := SetStatus(GdipGetFontCollectionFamilyList(
    nativeFontCollection,
    numSought,
    nativeFamilyList,
    numFound
    ));

  if (Status = Ok) then
    for i := 0 to numFound - 1 do
      GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i], gpfamilies[i].nativeFamily);
  freemem(nativeFamilyList, numSought * sizeof(GpFontFamily));
  Result := Status;
end;

function TGPFontCollection.GetLastStatus: TStatus;
begin
  Result := lastResult;
end;

function TGPFontCollection.SetStatus(Status: TStatus): TStatus;
begin
  lastResult := Status;
  Result := lastResult;
end;


constructor TGPInstalledFontCollection.Create;
begin
  nativeFontCollection := nil;
  lastResult := GdipNewInstalledFontCollection(nativeFontCollection);
end;

destructor TGPInstalledFontCollection.destroy;
begin
  inherited destroy;
end;

constructor TGPPrivateFontCollection.Create;
begin
  nativeFontCollection := nil;
  lastResult := GdipNewPrivateFontCollection(nativeFontCollection);
end;

destructor TGPPrivateFontCollection.destroy;
begin
  GdipDeletePrivateFontCollection(nativeFontCollection);
  inherited destroy;
end;

function TGPPrivateFontCollection.AddFontFile(filename: WideString): TStatus;
begin
  Result := SetStatus(GdipPrivateAddFontFile(nativeFontCollection, PWideChar(filename)));
end;

function TGPPrivateFontCollection.AddMemoryFont(memory: Pointer; Length: Integer): TStatus;
begin
  Result := SetStatus(GdipPrivateAddMemoryFont(
    nativeFontCollection,
    memory,
    Length));
end;

(**************************************************************************\
*
*   GDI+ Graphics Path class
*
\**************************************************************************)

constructor TGPGraphicsPath.Create(FillMode: TFillMode = FillModeAlternate);
begin
  nativePath := nil;
  lastResult := GdipCreatePath(FillMode, nativePath);
end;

constructor TGPGraphicsPath.Create(Points: PGPPointF; Types: PBYTE; Count: Integer;
  FillMode: TFillMode = FillModeAlternate);
begin
  nativePath := nil;
  lastResult := GdipCreatePath2(Points, Types, Count, FillMode, nativePath);
end;

constructor TGPGraphicsPath.Create(Points: PGPPoint; Types: PBYTE; Count: Integer;
  FillMode: TFillMode = FillModeAlternate);
begin
  nativePath := nil;
  lastResult := GdipCreatePath2I(Points, Types, Count, FillMode, nativePath);
end;

destructor TGPGraphicsPath.destroy;
begin
  GdipDeletePath(nativePath);
end;

function TGPGraphicsPath.Clone: TGPGraphicsPath;
var
  clonePath         : GpPath;
begin
  clonePath := nil;
  SetStatus(GdipClonePath(nativePath, clonePath));
  Result := TGPGraphicsPath.Create(clonePath);
end;

// Reset the path object to empty (and fill mode to FillModeAlternate)

function TGPGraphicsPath.Reset: TStatus;
begin
  Result := SetStatus(GdipResetPath(nativePath));
end;

function TGPGraphicsPath.GetFillMode: TFillMode;
var FMode           : TFillMode;
begin
  FMode := FillModeAlternate;
  SetStatus(GdipGetPathFillMode(nativePath, Result));
  Result := FMode;
end;

function TGPGraphicsPath.SetFillMode(FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipSetPathFillMode(nativePath, FillMode));
end;

function TGPGraphicsPath.GetPathData(pathData: TPathData): TStatus;
var
  Count             : Integer;
begin
  Count := GetPointCount;
  if ((Count <= 0) or ((pathData.Count > 0) and (pathData.Count < Count))) then begin
    pathData.Count := 0;
    if Assigned(pathData.Points) then begin
      freemem(pathData.Points);
      pathData.Points := nil;
    end;
    if Assigned(pathData.Types) then begin
      freemem(pathData.Types);
      pathData.Types := nil;
    end;
    if (Count <= 0) then begin
      Result := lastResult;
      exit;
    end;
  end;

  if (pathData.Count = 0) then begin
    getmem(pathData.Points, sizeof(TGPPointF) * Count);
    if (pathData.Points = nil) then begin
      Result := SetStatus(OutOfMemory);
      exit;
    end;
    getmem(pathData.Types, Count);
    if (pathData.Types = nil) then begin
      freemem(pathData.Points);
      pathData.Points := nil;
      Result := SetStatus(OutOfMemory);
      exit;
    end;
    pathData.Count := Count;
  end;

  Result := SetStatus(GdipGetPathData(nativePath, @pathData.Count));
end;

function TGPGraphicsPath.StartFigure: TStatus;
begin
  Result := SetStatus(GdipStartPathFigure(nativePath));
end;

function TGPGraphicsPath.CloseFigure: TStatus;
begin
  Result := SetStatus(GdipClosePathFigure(nativePath));
end;

function TGPGraphicsPath.CloseAllFigures: TStatus;
begin
  Result := SetStatus(GdipClosePathFigures(nativePath));
end;

function TGPGraphicsPath.SetMarker: TStatus;
begin
  Result := SetStatus(GdipSetPathMarker(nativePath));
end;

function TGPGraphicsPath.ClearMarkers: TStatus;
begin
  Result := SetStatus(GdipClearPathMarkers(nativePath));
end;

function TGPGraphicsPath.Reverse: TStatus;
begin
  Result := SetStatus(GdipReversePath(nativePath));
end;

function TGPGraphicsPath.GetLastPoint(out lastPoint: TGPPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathLastPoint(nativePath,
    @lastPoint));
end;

function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPointF): TStatus;
begin
  Result := AddLine(pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathLine(nativePath, x1, y1,
    x2, y2));
end;

function TGPGraphicsPath.AddLines(Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPoint): TStatus;
begin
  Result := AddLine(pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLineI(nativePath, x1, y1, x2, y2));
end;

function TGPGraphicsPath.AddLines(Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2I(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddArc(Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height,
    startAngle, sweepAngle);
end;

function TGPGraphicsPath.AddArc(X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArc(nativePath, X, Y, Width, Height, startAngle, sweepAngle));
end;

function TGPGraphicsPath.AddArc(Rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height, startAngle, sweepAngle);
end;

function TGPGraphicsPath.AddArc(X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArcI(nativePath, X, Y, Width, Height, startAngle, sweepAngle));
end;

function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPointF): TStatus;
begin
  Result := AddBezier(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
end;

function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathBezier(nativePath, x1, y1, x2, y2, x3, y3, x4, y4));
end;

function TGPGraphicsPath.AddBeziers(Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziers(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPoint): TStatus;
begin
  Result := AddBezier(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
end;

function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBezierI(nativePath, x1, y1, x2, y2, x3, y3, x4, y4));
end;

function TGPGraphicsPath.AddBeziers(Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziersI(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddCurve(Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddCurve(Points: PGPPointF; Count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2(nativePath, Points, Count, tension));
end;

function TGPGraphicsPath.AddCurve(Points: PGPPointF; Count, offset,
  numberOfSegments: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3(nativePath, Points, Count, offset,
    numberOfSegments, tension));
end;

function TGPGraphicsPath.AddCurve(Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurveI(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddCurve(Points: PGPPoint; Count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2I(nativePath, Points, Count, tension));
end;

function TGPGraphicsPath.AddCurve(Points: PGPPoint; Count, offset,
  numberOfSegments: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3I(nativePath, Points, Count, offset,
    numberOfSegments, tension));
end;

function TGPGraphicsPath.AddClosedCurve(Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddClosedCurve(Points: PGPPointF; Count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2(nativePath, Points, Count, tension));
end;

function TGPGraphicsPath.AddClosedCurve(Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurveI(nativePath, Points, Count));
end;


function TGPGraphicsPath.AddClosedCurve(Points: PGPPoint; Count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2I(nativePath, Points, Count, tension));
end;

function TGPGraphicsPath.AddRectangle(Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangle(nativePath,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height));
end;

function TGPGraphicsPath.AddRectangles(rects: PGPRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangles(nativePath,
    rects,
    Count));
end;

function TGPGraphicsPath.AddRectangle(Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangleI(nativePath,
    Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height));
end;

function TGPGraphicsPath.AddRectangles(rects: PGPRect; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectanglesI(nativePath,
    rects,
    Count));
end;

function TGPGraphicsPath.AddEllipse(Rect: TGPRectF): TStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphicsPath.AddEllipse(X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipse(nativePath,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphicsPath.AddEllipse(Rect: TGPRect): TStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGPGraphicsPath.AddEllipse(X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipseI(nativePath,
    X,
    Y,
    Width,
    Height));
end;

function TGPGraphicsPath.AddPie(Rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, startAngle,
    sweepAngle);
end;

function TGPGraphicsPath.AddPie(X, Y, Width, Height, startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPie(nativePath, X, Y, Width,
    Height, startAngle,
    sweepAngle));
end;

function TGPGraphicsPath.AddPie(Rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddPie(Rect.X,
    Rect.Y,
    Rect.Width,
    Rect.Height,
    startAngle,
    sweepAngle);
end;

function TGPGraphicsPath.AddPie(X, Y, Width, Height: Integer; startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPieI(nativePath,
    X,
    Y,
    Width,
    Height,
    startAngle,
    sweepAngle));
end;

function TGPGraphicsPath.AddPolygon(Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygon(nativePath, Points, Count));
end;

function TGPGraphicsPath.AddPolygon(Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygonI(nativePath, Points,
    Count));
end;

function TGPGraphicsPath.AddPath(addingPath: TGPGraphicsPath; connect: BOOL): TStatus;
var
  nativePath2       : GpPath;
begin
  nativePath2 := nil;
  if Assigned(addingPath) then nativePath2 := addingPath.nativePath;
  Result := SetStatus(GdipAddPathPath(nativePath, nativePath2, connect));
end;

function TGPGraphicsPath.AddString(
  string_: WideString; Length: Integer;
  family: TGPFontFamily;
  style: Integer;
  emSize: Single;                       // World units
  origin: TGPPointF;
  Format: TGPStringFormat): TStatus;
var
  Rect              : TGPRectF;
  gpff              : GpFontFamily;
  gpsf              : GpStringFormat;
begin
  Rect.X := origin.X;
  Rect.Y := origin.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;

  gpff := nil;
  gpsf := nil;
  if Assigned(family) then gpff := family.nativeFamily;
  if Assigned(Format) then gpsf := Format.nativeFormat;
  Result := SetStatus(GdipAddPathString(nativePath, PWideChar(string_), Length, gpff,
    style, emSize, @Rect, gpsf));
end;

function TGPGraphicsPath.AddString(
  string_: WideString;
  Length: Integer;
  family: TGPFontFamily;
  style: Integer;
  emSize: Single;                       // World units
  layoutRect: TGPRectF;
  Format: TGPStringFormat): TStatus;
var
  gpff              : GpFontFamily;
  gpsf              : GpStringFormat;
begin
  gpff := nil;
  gpsf := nil;
  if Assigned(family) then gpff := family.nativeFamily;
  if Assigned(Format) then gpsf := Format.nativeFormat;
  Result := SetStatus(GdipAddPathString(nativePath, PWideChar(string_), Length, gpff,
    style, emSize, @layoutRect, gpsf));
end;

function TGPGraphicsPath.AddString(
  string_: WideString;
  Length: Integer;
  family: TGPFontFamily;
  style: Integer;
  emSize: Single;                       // World units
  origin: TGPPoint;
  Format: TGPStringFormat): TStatus;
var
  Rect              : TGPRect;
  gpff              : GpFontFamily;
  gpsf              : GpStringFormat;
begin
  Rect.X := origin.X;
  Rect.Y := origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  gpff := nil;
  gpsf := nil;
  if Assigned(family) then gpff := family.nativeFamily;
  if Assigned(Format) then gpsf := Format.nativeFormat;
  Result := SetStatus(GdipAddPathStringI(nativePath, PWideChar(string_), Length, gpff,
    style, emSize, @Rect, gpsf));
end;

function TGPGraphicsPath.AddString(
  string_: WideString;
  Length: Integer;
  family: TGPFontFamily;
  style: Integer;
  emSize: Single;                       // World units
  layoutRect: TGPRect;
  Format: TGPStringFormat): TStatus;
var
  gpff              : GpFontFamily;
  gpsf              : GpStringFormat;
begin
  gpff := nil;
  gpsf := nil;
  if Assigned(family) then gpff := family.nativeFamily;
  if Assigned(Format) then gpsf := Format.nativeFormat;
  Result := SetStatus(GdipAddPathStringI(nativePath, PWideChar(string_), Length, gpff,
    style, emSize, @layoutRect, gpsf));
end;

function TGPGraphicsPath.Transform(matrix: TGPMatrix): TStatus;
begin
  if Assigned(matrix) then
    Result := SetStatus(GdipTransformPath(nativePath, matrix.nativeMatrix))
  else
    Result := Ok;
end;

// This is not always the tightest bounds.

function TGPGraphicsPath.GetBounds(out bounds: TGPRectF; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;
var
  nativeMatrix      : GpMatrix;
  nativePen         : GpPen;
begin
  nativeMatrix := nil;
  nativePen := nil;
  if Assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
  if Assigned(pen) then nativePen := pen.nativePen;

  Result := SetStatus(GdipGetPathWorldBounds(nativePath, @bounds, nativeMatrix, nativePen));
end;

function TGPGraphicsPath.GetBounds(out bounds: TGPRect; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;
var
  nativeMatrix      : GpMatrix;
  nativePen         : GpPen;
begin
  nativeMatrix := nil;
  nativePen := nil;
  if Assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
  if Assigned(pen) then nativePen := pen.nativePen;

  Result := SetStatus(GdipGetPathWorldBoundsI(nativePath, @bounds, nativeMatrix, nativePen));
end;

// Once flattened, the resultant path is made of line segments and
// the original path information is lost.  When matrix is nil the
// identity matrix is assumed.

function TGPGraphicsPath.Flatten(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
var nativeMatrix    : GpMatrix;
begin
  nativeMatrix := nil;
  if Assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipFlattenPath(nativePath, nativeMatrix, flatness));
end;

function TGPGraphicsPath.Widen(pen: TGPPen; matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
var nativeMatrix    : GpMatrix;
begin
  nativeMatrix := nil;
  if Assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipWidenPath(nativePath, pen.nativePen, nativeMatrix, flatness));
end;

function TGPGraphicsPath.Outline(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
var nativeMatrix    : GpMatrix;
begin
  nativeMatrix := nil;
  if Assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipWindingModeOutline(nativePath, nativeMatrix, flatness));
end;

// Once this is called, the resultant path is made of line segments and
// the original path information is lost.  When matrix is nil, the
// identity matrix is assumed.

function TGPGraphicsPath.Warp(destPoints: PGPPointF; Count: Integer; srcRect: TGPRectF;
  matrix: TGPMatrix = nil; WarpMode: TWarpMode = WarpModePerspective;
  flatness: Single = FlatnessDefault): TStatus;
var nativeMatrix    : GpMatrix;
begin
  nativeMatrix := nil;
  if Assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipWarpPath(nativePath, nativeMatrix, destPoints,
    Count, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height,
    WarpMode, flatness));
end;

function TGPGraphicsPath.GetPointCount: Integer;
var Count           : Integer;
begin
  Count := 0;
  SetStatus(GdipGetPointCount(nativePath, Count));
  Result := Count;
end;

function TGPGraphicsPath.GetPathTypes(Types: PBYTE; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathTypes(nativePath, Types, Count));
end;

function TGPGraphicsPath.GetPathPoints(Points: PGPPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPoints(nativePath, Points, Count));
end;

function TGPGraphicsPath.GetPathPoints(Points: PGPPoint; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPointsI(nativePath, Points, Count));
end;

function TGPGraphicsPath.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

function TGPGraphicsPath.IsVisible(point: TGPPointF; G: TGPGraphics = nil): BOOL;
begin
  Result := IsVisible(point.X, point.Y, G);
end;

function TGPGraphicsPath.IsVisible(X, Y: Single; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  nativeGraphics    : GpGraphics;
begin
  booln := False;
  nativeGraphics := nil;
  if Assigned(G) then nativeGraphics := G.nativeGraphics;
  SetStatus(GdipIsVisiblePathPoint(nativePath, X, Y, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsVisible(point: TGPPoint; G: TGPGraphics = nil): BOOL;
begin
  Result := IsVisible(point.X, point.Y, G);
end;

function TGPGraphicsPath.IsVisible(X, Y: Integer; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  nativeGraphics    : GpGraphics;
begin
  booln := False;
  nativeGraphics := nil;
  if Assigned(G) then nativeGraphics := G.nativeGraphics;
  SetStatus(GdipIsVisiblePathPointI(nativePath, X, Y, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsOutlineVisible(point: TGPPointF; pen: TGPPen; G: TGPGraphics = nil): BOOL;
begin
  Result := IsOutlineVisible(point.X, point.Y, pen, G);
end;

function TGPGraphicsPath.IsOutlineVisible(X, Y: Single; pen: TGPPen; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  nativeGraphics    : GpGraphics;
  nativePen         : GpPen;
begin
  booln := False;
  nativeGraphics := nil;
  nativePen := nil;
  if Assigned(G) then nativeGraphics := G.nativeGraphics;
  if Assigned(pen) then nativePen := pen.nativePen;
  SetStatus(GdipIsOutlineVisiblePathPoint(nativePath, X, Y, nativePen,
    nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsOutlineVisible(point: TGPPoint; pen: TGPPen; G: TGPGraphics = nil): BOOL;
begin
  Result := IsOutlineVisible(point.X, point.Y, pen, G);
end;

function TGPGraphicsPath.IsOutlineVisible(X, Y: Integer; pen: TGPPen; G: TGPGraphics = nil): BOOL;
var
  booln             : BOOL;
  nativeGraphics    : GpGraphics;
  nativePen         : GpPen;
begin
  booln := False;
  nativeGraphics := nil;
  nativePen := nil;
  if Assigned(G) then nativeGraphics := G.nativeGraphics;
  if Assigned(pen) then nativePen := pen.nativePen;
  SetStatus(GdipIsOutlineVisiblePathPointI(nativePath, X, Y, nativePen,
    nativeGraphics, booln));
  Result := booln;
end;

constructor TGPGraphicsPath.Create(path: TGPGraphicsPath);
var clonePath       : GpPath;
begin
  clonePath := nil;
  SetStatus(GdipClonePath(path.nativePath, clonePath));
  SetNativePath(clonePath);
end;

constructor TGPGraphicsPath.Create(nativePath: GpPath);
begin
  lastResult := Ok;
  SetNativePath(nativePath);
end;

procedure TGPGraphicsPath.SetNativePath(nativePath: GpPath);
begin
  Self.nativePath := nativePath;
end;

function TGPGraphicsPath.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

constructor TGPGraphicsPathIterator.Create(path: TGPGraphicsPath);
var
  nativePath        : GpPath;
  iter              : GpPathIterator;
begin
  nativePath := nil;
  if Assigned(path) then nativePath := path.nativePath;
  iter := nil;
  lastResult := GdipCreatePathIter(iter, nativePath);
  SetNativeIterator(iter);
end;

destructor TGPGraphicsPathIterator.destroy;
begin
  GdipDeletePathIter(nativeIterator);
end;


function TGPGraphicsPathIterator.NextSubpath(out startIndex, endIndex: Integer; out isClosed: BOOL): Integer;
begin
  SetStatus(GdipPathIterNextSubpath(nativeIterator, Result, startIndex, endIndex, isClosed));
end;

function TGPGraphicsPathIterator.NextSubpath(path: TGPGraphicsPath; out isClosed: BOOL): Integer;
var
  nativePath        : GpPath;
  resultCount       : Integer;
begin
  nativePath := nil;
  if Assigned(path) then nativePath := path.nativePath;
  SetStatus(GdipPathIterNextSubpathPath(nativeIterator, resultCount,
    nativePath, isClosed));
  Result := resultCount;
end;

function TGPGraphicsPathIterator.NextPathType(out pathType: TPathPointType; out startIndex, endIndex: Integer): Integer;
var
  resultCount       : Integer;
begin
  SetStatus(GdipPathIterNextPathType(nativeIterator, resultCount, @pathType,
    startIndex, endIndex));
  Result := resultCount;
end;

function TGPGraphicsPathIterator.NextMarker(out startIndex, endIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterNextMarker(nativeIterator, Result, startIndex, endIndex));
end;

function TGPGraphicsPathIterator.NextMarker(path: TGPGraphicsPath): Integer;
var nativePath      : GpPath;
begin
  nativePath := nil;
  if Assigned(path) then nativePath := path.nativePath;
  SetStatus(GdipPathIterNextMarkerPath(nativeIterator, Result, nativePath));
end;

function TGPGraphicsPathIterator.GetCount: Integer;
begin
  SetStatus(GdipPathIterGetCount(nativeIterator, Result));
end;

function TGPGraphicsPathIterator.GetSubpathCount: Integer;
begin
  SetStatus(GdipPathIterGetSubpathCount(nativeIterator, Result));
end;

function TGPGraphicsPathIterator.hasCurve: BOOL;
begin
  SetStatus(GdipPathIterHasCurve(nativeIterator, Result));
end;

procedure TGPGraphicsPathIterator.Rewind;
begin
  SetStatus(GdipPathIterRewind(nativeIterator));
end;

function TGPGraphicsPathIterator.Enumerate(Points: PGPPointF; Types: PBYTE;
  Count: Integer): Integer;
begin
  SetStatus(GdipPathIterEnumerate(nativeIterator, Result, Points, Types, Count));
end;

function TGPGraphicsPathIterator.CopyData(Points: PGPPointF; Types: PBYTE;
  startIndex, endIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterCopyData(nativeIterator, Result, Points, Types,
    startIndex, endIndex));
end;

function TGPGraphicsPathIterator.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

procedure TGPGraphicsPathIterator.SetNativeIterator(nativeIterator: GpPathIterator);
begin
  Self.nativeIterator := nativeIterator;
end;

function TGPGraphicsPathIterator.SetStatus(Status: TStatus): TStatus;
begin
  if (Status <> Ok) then lastResult := Status;
  Result := Status;
end;

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

constructor TGPPathGradientBrush.Create(Points: PGPPointF; Count: Integer; WrapMode: TWrapMode = WrapModeClamp);
var brush           : GpPathGradient;
begin
  brush := nil;
  lastResult := GdipCreatePathGradient(Points, Count, WrapMode, brush);
  SetNativeBrush(brush);
end;

constructor TGPPathGradientBrush.Create(Points: PGPPoint; Count: Integer; WrapMode: TWrapMode = WrapModeClamp);
var brush           : GpPathGradient;
begin
  brush := nil;
  lastResult := GdipCreatePathGradientI(Points, Count, WrapMode, brush);
  SetNativeBrush(brush);
end;

constructor TGPPathGradientBrush.Create(path: TGPGraphicsPath);
var brush           : GpPathGradient;
begin
  brush := nil;
  lastResult := GdipCreatePathGradientFromPath(path.nativePath, brush);
  SetNativeBrush(brush);
end;

function TGPPathGradientBrush.GetCenterColor(out color: TGPColor): TStatus;
begin
  SetStatus(GdipGetPathGradientCenterColor(GpPathGradient(nativeBrush), color));
  Result := lastResult;
end;

function TGPPathGradientBrush.SetCenterColor(color: TGPColor): TStatus;
begin
  SetStatus(GdipSetPathGradientCenterColor(GpPathGradient(nativeBrush), color));
  Result := lastResult;
end;

function TGPPathGradientBrush.GetPointCount: Integer;
begin
  SetStatus(GdipGetPathGradientPointCount(GpPathGradient(nativeBrush), Result));
end;

function TGPPathGradientBrush.GetSurroundColorCount: Integer;
begin
  SetStatus(GdipGetPathGradientSurroundColorCount(GpPathGradient(nativeBrush), Result));
end;

function TGPPathGradientBrush.GetSurroundColors(colors: PARGB; var Count: Integer): TStatus;
var
  count1            : Integer;
begin
  if not Assigned(colors) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;

  SetStatus(GdipGetPathGradientSurroundColorCount(GpPathGradient(nativeBrush), count1));

  if (lastResult <> Ok) then begin
    Result := lastResult;
    exit;
  end;

  if ((Count < count1) or (count1 <= 0)) then begin
    Result := SetStatus(InsufficientBuffer);
    exit;
  end;

  SetStatus(GdipGetPathGradientSurroundColorsWithCount(GpPathGradient(nativeBrush), colors, count1));
  if (lastResult = Ok) then
    Count := count1;

  Result := lastResult;
end;

function TGPPathGradientBrush.SetSurroundColors(colors: PARGB; var Count: Integer): TStatus;
var
  count1            : Integer;
type
  TDynArrDWORD = array of DWORD;
begin
  if (colors = nil) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;

  count1 := GetPointCount;

  if ((Count > count1) or (count1 <= 0)) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;

  count1 := Count;

  SetStatus(GdipSetPathGradientSurroundColorsWithCount(
    GpPathGradient(nativeBrush), colors, count1));

  if (lastResult = Ok) then Count := count1;
  Result := lastResult;
end;

function TGPPathGradientBrush.GetGraphicsPath(path: TGPGraphicsPath): TStatus;
begin
  if (path = nil) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;
  Result := SetStatus(GdipGetPathGradientPath(GpPathGradient(nativeBrush), path.nativePath));
end;

function TGPPathGradientBrush.SetGraphicsPath(path: TGPGraphicsPath): TStatus;
begin
  if (path = nil) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;
  Result := SetStatus(GdipSetPathGradientPath(GpPathGradient(nativeBrush), path.nativePath));
end;

function TGPPathGradientBrush.GetCenterPoint(out point: TGPPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPoint(GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.GetCenterPoint(out point: TGPPoint): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPointI(GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.SetCenterPoint(point: TGPPointF): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPoint(GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.SetCenterPoint(point: TGPPoint): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPointI(GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.GetRectangle(out Rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRect(GpPathGradient(nativeBrush), @Rect));
end;

function TGPPathGradientBrush.GetRectangle(out Rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRectI(GpPathGradient(nativeBrush), @Rect));
end;

function TGPPathGradientBrush.SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientGammaCorrection(GpPathGradient(nativeBrush),
    useGammaCorrection));
end;

function TGPPathGradientBrush.GetGammaCorrection: BOOL;
begin
  SetStatus(GdipGetPathGradientGammaCorrection(GpPathGradient(nativeBrush), Result));
end;

function TGPPathGradientBrush.GetBlendCount: Integer;
var Count           : Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientBlendCount(GpPathGradient(nativeBrush), Count));
  Result := Count;
end;

function TGPPathGradientBrush.GetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientBlend(
    GpPathGradient(nativeBrush),
    blendFactors, blendPositions, Count));
end;

function TGPPathGradientBrush.SetBlend(blendFactors, blendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientBlend(
    GpPathGradient(nativeBrush),
    blendFactors, blendPositions, Count));
end;

function TGPPathGradientBrush.GetInterpolationColorCount: Integer;
var Count           : Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientPresetBlendCount(GpPathGradient(nativeBrush), Count));
  Result := Count;
end;

function TGPPathGradientBrush.SetInterpolationColors(presetColors: PARGB;
  blendPositions: PSingle; Count: Integer): TStatus;
var
  Status            : TStatus;
begin
  if ((Count <= 0) or (presetColors = nil)) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;

  Status := SetStatus(GdipSetPathGradientPresetBlend(GpPathGradient(nativeBrush),
    presetColors, blendPositions, Count));
  Result := Status;
end;

function TGPPathGradientBrush.GetInterpolationColors(presetColors: PARGB;
  blendPositions: PSingle; Count: Integer): TStatus;
var
  Status            : GpStatus;
  i                 : Integer;
  argbs             : PARGB;
begin
  if ((Count <= 0) or (presetColors = nil)) then begin
    Result := SetStatus(InvalidParameter);
    exit;
  end;
  getmem(argbs, Count * sizeof(ARGB));
  if (argbs = nil) then begin
    Result := SetStatus(OutOfMemory);
    exit;
  end;

  Status := SetStatus(GdipGetPathGradientPresetBlend(nativeBrush, argbs,
    blendPositions, Count));

  for i := 0 to Count - 1 do
    TColorDynArray(presetColors)[i] := TColorDynArray(argbs)[i];

  freemem(argbs);

  Result := Status;
end;

function TGPPathGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientSigmaBlend(GpPathGradient(nativeBrush), focus, scale));
end;

function TGPPathGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientLinearBlend(GpPathGradient(nativeBrush), focus, scale));
end;

function TGPPathGradientBrush.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientTransform(GpPathGradient(nativeBrush),
    matrix.nativeMatrix));
end;

function TGPPathGradientBrush.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientTransform(
    GpPathGradient(nativeBrush),
    matrix.nativeMatrix));
end;

function TGPPathGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetPathGradientTransform(
    GpPathGradient(nativeBrush)));
end;

function TGPPathGradientBrush.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPathGradientTransform(
    GpPathGradient(nativeBrush),
    matrix.nativeMatrix,
    order));
end;

function TGPPathGradientBrush.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePathGradientTransform(
    GpPathGradient(nativeBrush),
    dx, dy, order));
end;

function TGPPathGradientBrush.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePathGradientTransform(
    GpPathGradient(nativeBrush),
    sx, sy, order));
end;

function TGPPathGradientBrush.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePathGradientTransform(
    GpPathGradient(nativeBrush),
    angle, order));
end;

function TGPPathGradientBrush.GetFocusScales(out xScale, yScale: Single): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientFocusScales(
    GpPathGradient(nativeBrush), xScale, yScale));
end;

function TGPPathGradientBrush.SetFocusScales(xScale, yScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientFocusScales(
    GpPathGradient(nativeBrush), xScale, yScale));
end;

function TGPPathGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetPathGradientWrapMode(GpPathGradient(nativeBrush), Result));
end;

function TGPPathGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientWrapMode(
    GpPathGradient(nativeBrush), WrapMode));
end;

constructor TGPPathGradientBrush.Create;
begin
  // écrase la fonction parent
end;

initialization
  begin
    // Initialize StartupInput structure
    StartupInput.DebugEventCallback := nil;
    StartupInput.SuppressBackgroundThread := False;
    StartupInput.SuppressExternalCodecs := False;
    StartupInput.GdiplusVersion := 1;
    // Initialize GDI+
    GdiplusStartup(gdiplusToken, @StartupInput, nil);

  end;

finalization
  begin

    if Assigned(GenericSansSerifFontFamily) then GenericSansSerifFontFamily.Free;
    if Assigned(GenericSerifFontFamily) then GenericSerifFontFamily.Free;
    if Assigned(GenericMonospaceFontFamily) then GenericMonospaceFontFamily.Free;

    if Assigned(GenericTypographicStringFormatBuffer) then GenericTypographicStringFormatBuffer.Free;
    if Assigned(GenericDefaultStringFormatBuffer) then GenericDefaultStringFormatBuffer.Free;

    // Close GDI +
    GdiplusShutdown(gdiplusToken);
  end;

end.
