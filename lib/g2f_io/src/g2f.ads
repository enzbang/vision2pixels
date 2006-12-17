------------------------------------------------------------------------------
--                              G2F_IO                                      --
--                                                                          --
--                         Copyright (C) 2004                               --
--                            Ali Bendriss                                  --
--                                                                          --
--  Author: Ali Bendriss                                                    --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C_Streams;
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;

package G2F is

   package C renames Interfaces.C;
   package C_Ext renames Interfaces.C.Extensions;

   pragma Linker_Options ("-lMagick");
   pragma Linker_Options ("-lm");
   pragma Linker_Options ("-L/usr/X11R6/lib/");

   type Image is private;
   type Image_Info is private;

   type Image_Ptr is access Image;
   pragma Convention (C, Image_Ptr);

   type Image_Info_Ptr is access all Image_Info;
   pragma Convention (C, Image_Info_Ptr);

   type Blob_Info_Ptr is private;

   GetExceptionInfo_Error : exception;

   procedure Destroy_Magick;
   --  destroys the ImageMagick environment

   type Quantum_8 is new C.unsigned_char;
   for Quantum_8'Size use 8;
   pragma Convention (C, Quantum_8);

   type Quantum_16 is new C.unsigned_short;
   for Quantum_16'Size use 16;
   pragma Convention (C, Quantum_16);

   type Quantum_32 is new C.unsigned;
   for Quantum_32'Size use 32;
   pragma Convention (C, Quantum_32);

   -- Set the quantum_depth according to your ImageMagick
   -- Quantum configuration;

   subtype Quantum_Depth is Quantum_16;
   subtype Quantum is Quantum_16;

   type Compression_Type is (
      UndefinedCompression,
      NoCompression,
      BZipCompression,
      FaxCompression,
      Group4Compression,
      JPEGCompression,
      JPEG2000Compression,
      LosslessJPEGCompression,
      LZWCompression,
      RLECompression,
      ZipCompression);
   pragma Convention (C, Compression_Type);

   type Interlace_Type is (
      UndefinedInterlace,
      NoInterlace,
      LineInterlace,
      PlaneInterlace,
      PartitionInterlace);
   pragma Convention (C, Interlace_Type);

   type Resolution_Type is (
      UndefinedResolution,
      PixelsPerInchResolution,
      PixelsPerCentimeterResolution);
   pragma Convention (C, Resolution_Type);

   type Filter_Types is (
      Undefined_Filter,
      Point_Filter,
      Box_Filter,
      Triangle_Filter,
      Hermite_Filter,
      Hanning_Filter,
      Hamming_Filter,
      Blackman_Filter,
      Gaussian_Filter,
      Quadratic_Filter,
      Cubic_Filter,
      Catrom_Filter,
      Mitchell_Filter,
      Lanczos_Filter,
      Bessel_Filter,
      Sinc_Filter);
   pragma Convention (C, Filter_Types);

   for Filter_Types use
     (Undefined_Filter => 0,
      Point_Filter     => 1,
      Box_Filter       => 2,
      Triangle_Filter  => 3,
      Hermite_Filter   => 4,
      Hanning_Filter   => 5,
      Hamming_Filter   => 6,
      Blackman_Filter  => 7,
      Gaussian_Filter  => 8,
      Quadratic_Filter => 9,
      Cubic_Filter     => 10,
      Catrom_Filter    => 11,
      Mitchell_Filter  => 12,
      Lanczos_Filter   => 13,
      Bessel_Filter    => 14,
      Sinc_Filter      => 15);

   type Channel_Type is new C.Int;
   pragma Convention (C, Channel_Type);

   UndefinedChannel : constant := 16#0000#;
   RedChannel       : constant := 16#0001#;
   GrayChannel      : constant := 16#0001#;
   CyanChannel      : constant := 16#0001#;
   GreenChannel     : constant := 16#0002#;
   MagentaChannel   : constant := 16#0002#;
   BlueChannel      : constant := 16#0004#;
   YellowChannel    : constant := 16#0004#;
   AlphaChannel     : constant := 16#0008#;
   OpacityChannel   : constant := 16#0008#;
   MatteChannel     : constant := 16#0008#;  -- Deprecated
   BlackChannel     : constant := 16#0020#;
   IndexChannel     : constant := 16#0020#;
   AllChannels      : constant := 16#00ff#;

   procedure Put_Magick_Exception;
   procedure Put_Image_Exception (I : in Image_Ptr);

private

   procedure Free (I : in out Image_Info_Ptr);
   procedure Free (I : in out Image_Ptr);

   type File_Ptr is access Interfaces.C_Streams.FILEs;
   pragma Convention (C, File_Ptr);

   type Endian_Type is (UndefinedEndian, LSBEndian, MSBEndian);
   pragma Convention (C, Endian_Type);

   type Pixel_Packet is record --  Litle endian specifique
      Blue, Green, Red, Opacity : Quantum;
   end record;
   pragma Convention (C, Pixel_Packet);

   type Colorspace_Type is
     (UndefinedColorspace,
      RGBColorspace,
      GRAYColorspace,
      TransparentColorspace,
      OHTAColorspace,
      LABColorSpace,
      XYZColorspace,
      YCbCrColorspace,
      YCCColorspace,
      YIQColorspace,
      YPbPrColorspace,
      YUVColorspace,
      CMYKColorspace,
      SRGBColorspace,
      HSBColorspace,
      HSLColorspace,
      HWBColorspace,
      Rec601LumaColorspace,
      Rec709LumaColorspace,
      LogColorspace);
   pragma Convention (C, Colorspace_Type);

   type Image_Type is
     (UndefinedType,
      BilevelType,
      GrayscaleType,
      GrayscaleMatteType,
      PaletteType,
      PaletteMatteType,
      TrueColorType,
      TrueColorMatteType,
      ColorSeparationType,
      ColorSeparationMatteType,
      OptimizeType);
   pragma Convention (C, Image_Type);

   type Orientation_Type is
     (UndefinedOrientation,
      TopLeftOrientation,
      TopRightOrientation,
      BottomRightOrientation,
      BottomLeftOrientation,
      LeftTopOrientation,
      RightTopOrientation,
      RightBottomOrientation,
      LeftBottomOrientation);
   pragma Convention (C, Orientation_Type);

   type Preview_Type is
     (UndefinedPreview,
      RotatePreview,
      ShearPreview,
      RollPreview,
      HuePreview,
      SaturationPreview,
      BrightnessPreview,
      GammaPreview,
      SpiffPreview,
      DullPreview,
      GrayscalePreview,
      QuantizePreview,
      DespecklePreview,
      ReduceNoisePreview,
      AddNoisePreview,
      SharpenPreview,
      BlurPreview,
      ThresholdPreview,
      EdgeDetectPreview,
      SpreadPreview,
      SolarizePreview,
      ShadePreview,
      RaisePreview,
      SegmentPreview,
      SwirlPreview,
      ImplodePreview,
      WavePreview,
      OilPaintPreview,
      CharcoalDrawingPreview,
      JPEGPreview);
   pragma Convention (C, Preview_Type);

   subtype MaxTextExtent is C.size_t range 1 .. 4_096;

   type MagickOffsetType is new C.long;
   type MagickSizeType is new C.long;

   type MagickBooleanType is access procedure
     (Str  : in C.Strings.chars_ptr;
      Off  : in MagickOffsetType;
      Size : in MagickSizeType;
      V    : in C_Ext.void_ptr);
   pragma Convention (C, MagickBooleanType);

   type StreamHandler is access procedure
     (Image : in Image_Ptr;
      V     : in C_Ext.void_ptr;
      Size  : in C.size_t);
   pragma Convention (C, StreamHandler);

   type Image_Info is record
      Compression      : Compression_Type;
      Orientation      : Orientation_Type;
      Temporary,
      Adjoin,
      Affirm,
      Antialias        : MagickBooleanType;
      Size,
      Extract,
      Page,
      Scenes           : Interfaces.C.Strings.chars_ptr;
      Scene,
      Number_Scenes,
      Depth            : C.unsigned_long;
      Interlace        : Interlace_Type;
      Endian           : Endian_Type;
      Units            : Resolution_Type;
      Quality          : C.unsigned_long;
      Sampling_Factor,
      Server_Name,
      Font,
      Texture,
      Density          : Interfaces.C.Strings.chars_ptr;
      Pointsize,
      Fuzz             : C.double;
      Background_Color,
      Border_Color,
      Matte_Color      : Pixel_Packet;
      Dither,
      Monochrome       : MagickBooleanType;
      Colors           : C.unsigned_long;
      Colorspace       : Colorspace_Type;
      ImageType        : Image_Type;
      PreviewType      : Preview_Type;
      Group            : C.long;
      Ping,
      Verbose          : MagickBooleanType;
      View,
      Authenticate     : Interfaces.C.Strings.chars_ptr;
      Channel          : Channel_Type;
      Attributes       : Image_Ptr;
      Options          : C_Ext.void_ptr;
      Progress_Monitor : MagickBooleanType;
      Client_Data,
      Cache            : C_Ext.void_ptr;
      Stream           : StreamHandler;
      File             : File_Ptr;
      Blob             : C_Ext.void_ptr;
      Length           : C.size_t;
      Magick,
      Unique,
      Zero,
      Filename         : C.char_array (MaxTextExtent);
      Debug            : MagickBooleanType; --  Deprecated
      Tile             : Interfaces.C.Strings.chars_ptr;
      Subimage,
      Subrange         : C.unsigned_long;
      Pen              : Pixel_Packet;
      Signature        : C.unsigned_long;
   end record;
   pragma Convention (C, Image_Info);

   ----------
   -- Image
   ----------

   type Class_Type is (UndefinedClass, DirectClass, PseudoClass);
   pragma Convention (C, Class_Type);

   type Pixel_Packet_Ptr is access all Pixel_Packet;
   pragma Convention (C, Pixel_Packet_Ptr);

   type Primary_Info is record
      X : C.double;
      Y : C.double;
      Z : C.double;
   end record;
   pragma Convention (C, Primary_Info);

   type Chromaticity_Info is record
      Red_Primary   : Primary_Info;
      Green_Primary : Primary_Info;
      Blue_Primary  : Primary_Info;
      White_Point   : Primary_Info;
   end record;
   pragma Convention (C, Chromaticity_Info);

   type Unsigned_Char_Ptr is access C.unsigned_char;
   pragma Convention (C, Unsigned_Char_Ptr);

   type Profile_Info is record
      Name      : Interfaces.C.Strings.chars_ptr;
      Length    : C.size_t;
      Info      : Unsigned_Char_Ptr;
      Signature : C.unsigned_long;
   end record;
   pragma Convention (C, Profile_Info);

   type Profile_Info_Ptr is access Profile_Info;
   pragma Convention (C, Profile_Info_Ptr);

   type Rendering_Intent is
     (UndefinedIntent,
      SaturationIntent,
      PerceptualIntent,
      AbsoluteIntent,
      RelativeIntent);
   pragma Convention (C, Rendering_Intent);

   type Rectangle_Info is record
      Width, Height : C.unsigned_long;
      X, Y          : C.long;
   end record;
   pragma Convention (C, Rectangle_Info);

   type Gravity_Type is
     (ForgetGravity,
      NorthWestGravity,
      NorthGravity,
      NorthEastGravity,
      WestGravity,
      CenterGravity,
      EastGravity,
      SouthWestGravity,
      SouthGravity,
      SouthEastGravity,
      StaticGravity);
   pragma Convention (C, Gravity_Type);

   type Composite_Operator is
     (UndefinedCompositeOp,
      OverCompositeOp,
      InCompositeOp,
      OutCompositeOp,
      AtopCompositeOp,
      XorCompositeOp,
      PlusCompositeOp,
      MinusCompositeOp,
      AddCompositeOp,
      SubtractCompositeOp,
      DifferenceCompositeOp,
      MultiplyCompositeOp,
      BumpmapCompositeOp,
      CopyCompositeOp,
      CopyRedCompositeOp,
      CopyGreenCompositeOp,
      CopyBlueCompositeOp,
      CopyOpacityCompositeOp,
      ClearCompositeOp,
      DissolveCompositeOp,
      DisplaceCompositeOp,
      ModulateCompositeOp,
      ThresholdCompositeOp,
      NoCompositeOp,
      DarkenCompositeOp,
      LightenCompositeOp,
      HueCompositeOp,
      SaturateCompositeOp,
      ColorizeCompositeOp,
      LuminizeCompositeOp,
      ScreenCompositeOp,
      OverlayCompositeOp,
      CopyCyanCompositeOp,
      CopyMagentaCompositeOp,
      CopyYellowCompositeOp,
      CopyBlackCompositeOp);
   pragma Convention (C, Composite_Operator);

   type Dispose_Type is
     (UndefinedDispose,
      NoneDispose,
      BackgroundDispose,
      PreviousDispose);
   pragma Convention (C, Dispose_Type);

   type Error_Info is record
      Mean_Error_Per_Pixel,
      Normalized_Mean_Error,
      Normalized_Maximum_Error : C.double;
   end record;
   pragma Convention (C, Error_Info);

   type Timer is record
      Start, Stop, Total : C.double;
   end record;
   pragma Convention (C, Timer);

   type Timer_State is
     (UndefinedTimerState,
      StoppedTimerState,
      RunningTimerState);
   pragma Convention (C, Timer_State);

   type Timer_Info is record
      User,
      Elapsed   : Timer;
      State     : Timer_State;
      Signature : C.unsigned_long;
   end record;
   pragma Convention (C, Timer_Info);

   type Image_Attribute;
   type Image_Attribute_Ptr is access all Image_Attribute;
   pragma Convention (C, Image_Attribute_Ptr);

   type Image_Attribute is record
      Key,
      Value       : Interfaces.C.Strings.chars_ptr;
      Compression : C.unsigned;
      Previous,
      Next        : Image_Attribute_Ptr;
   end record;
   pragma Convention (C, Image_Attribute);

   type T_Buffer is array (0 .. 10) of C.unsigned_char;
   type Ascii_85_Info is record
      Offset,
      Line_Break : C.long;
      Buffer     : T_Buffer;
   end record;
   pragma Convention (C, Ascii_85_Info);

   type Ascii_85_Info_Ptr is access Ascii_85_Info;
   pragma Convention (C, Ascii_85_Info_Ptr);

   type Exception_Type is
     (UndefinedException,
      WarningException,
      ResourceLimitWarning,
      TypeWarning,
      OptionWarning,
      DelegateWarning,
      MissingDelegateWarning,
      CorruptImageWarning,
      FileOpenWarning,
      BlobWarning,
      StreamWarning,
      CacheWarning,
      CoderWarning,
      ModuleWarning,
      DrawWarning,
      ImageWarning,
      XServerWarning,
      MonitorWarning,
      RegistryWarning,
      ConfigureWarning,
      ErrorException,
      ResourceLimitError,
      TypeError,
      OptionError,
      DelegateError,
      MissingDelegateError,
      CorruptImageError,
      FileOpenError,
      BlobError,
      StreamError,
      CacheError,
      CoderError,
      ModuleError,
      DrawError,
      ImageError,
      XServerError,
      MonitorError,
      RegistryError,
      ConfigureError,
      FatalErrorException,
      ResourceLimitFatalError,
      TypeFatalError,
      OptionFatalError,
      DelegateFatalError,
      MissingDelegateFatalError,
      CorruptImageFatalError,
      FileOpenFatalError,
      BlobFatalError,
      StreamFatalError,
      CacheFatalError,
      CoderFatalError,
      ModuleFatalError,
      DrawFatalError,
      ImageFatalError,
      XServerFatalError,
      MonitorFatalError,
      RegistryFatalError,
      ConfigureFatalError);
   pragma Convention (C, Exception_Type);

   type sigjmp_buf is array (Integer range 0 .. 38) of C.int;
   pragma Convention (C, sigjmp_buf);

   type pthread_t_struct is record
      context    : sigjmp_buf;
      pbody      : sigjmp_buf;
      errno      : C.int;
      ret        : C.int;
      stack_base : System.Address;
   end record;
   pragma Convention (C, pthread_t_struct);

   type pthread_t is access all pthread_t_struct;
   pragma Convention (C, pthread_t);

   type queue_t is record
      head : System.Address;
      tail : System.Address;
   end record;
   pragma Convention (C, queue_t);

   type pthread_mutex_t is record
      queue                 : queue_t;
      lock                  : C.plain_char;
      owner                 : System.Address;
      flags                 : C.int;
      prio_ceiling          : C.int;
      protocol              : C.int;
      prev_max_ceiling_prio : C.int;
   end record;
   pragma Convention (C, pthread_mutex_t);

   type Semaphore_Info is record
      Mutex     : pthread_mutex_t;
      Id        : pthread_t;
      Lock      : C.unsigned;
      Signature : C.unsigned_long;
   end record;
   pragma Convention (C, Semaphore_Info);

   type Semaphore_Info_Ptr is access all Semaphore_Info;
   pragma Convention (C, Semaphore_Info_Ptr);

   type Exception_Info is record
      Severity            : C.int; --Exception_Type;
      Error_Number        : C.int;
      Reason, Description : Interfaces.C.Strings.chars_ptr;
      Exceptions          : C_Ext.Void_Ptr;
      Relinquish          : MagickBooleanType;
      Semaphore           : Semaphore_Info_Ptr;
      Signature           : C.unsigned_long;
   end record;
   pragma Convention (C, Exception_Info);

   type Exception_Info_Ptr is access Exception_Info;
   pragma Convention (C, Exception_Info_Ptr);

   type Off_T is new Long_Integer;
   pragma Convention (C, Off_T);

   type Stream_Type is
     (UndefinedStream,
      FileStream,
      StandardStream,
      PipeStream,
      ZipStream,
      BZipStream,
      FifoStream,
      BlobStream);
   pragma Convention (C, Stream_Type);

   type Blob_Info is record
      Length,
      Extent,
      Quantum         : C.size_t;
      Mapped,
      Eof             : C.unsigned;
      Offset,
      Size            : Off_T;
      Exempt,
      Status,
      Temporary       : C.unsigned;
      S_Type          : Stream_Type;
      File            : File_Ptr;
      Stream          : C.unsigned;
      Data            : Unsigned_Char_Ptr;
      Debug           : C.unsigned;
      Semaphore       : Semaphore_Info_Ptr;
      Reference_Count : C.long;
      Signature       : C.unsigned_long;
   end record;
   pragma Convention (C, Blob_Info);

   type Blob_Info_Ptr is access Blob_Info;
   pragma Convention (C, Blob_Info_Ptr);

   type InterpolatePixelMethod is
     (UndefinedInterpolatePixel,
      AverageInterpolatePixel,
      BicubicInterpolatePixel,
      BilinearInterpolatePixel,
      FilterInterpolatePixel,
      IntegerInterpolatePixel,
      MeshInterpolatePixel,
      NearestNeighborInterpolatePixel);
   pragma Convention (C, InterpolatePixelMethod);

   type Image is record
      Storage_Class            : Class_Type;
      Colorspace               : Colorspace_Type;
      Compression              : Compression_Type;
      Quality                  : C.unsigned_long;
      Orientation              : Orientation_Type;
      Taint,
      Matte                    : MagickBooleanType;
      Columns,
      Rows                     : C.unsigned_long;
      Depth,
      Colors                   : C.unsigned_long;
      Colormap                 : Pixel_Packet_Ptr;
      Background_Color,
      Border_Color,
      Matte_Color              : Pixel_Packet;
      Gamma                    : C.double;
      Chromaticity             : Chromaticity_Info;
      RenderingIntent          : Rendering_Intent;
      Profiles                 : C_Ext.void_ptr;
      Units                    : Resolution_Type;
      Montage,
      Directory,
      Geometry                 : Interfaces.C.Strings.chars_ptr;
      Offset                   : C.long;
      X_Resolution,
      Y_Resolution             : C.double;
      Page,
      Extract_Info,
      Tile_Info                : Rectangle_Info; --  deprecated
      Bias,
      Blur,
      Fuzz                     : C.double;
      Filter                   : Filter_Types;
      Interlace                : Interlace_Type;
      Endian                   : Endian_Type;
      Gravity                  : Gravity_Type;
      Compose                  : Composite_Operator;
      Dispose                  : Dispose_Type;
      Clip_Mask                : Image_Ptr;
      Scene,
      Image_Delay,
      Ticks_Per_Second,
      Iterations,
      Total_Colors             : C.unsigned_long;
      Start_Loop               : C.long;
      Error                    : Error_Info;
      Timer                    : Timer_Info;
      Progress_Monitor         : MagickBooleanType; -- MagickProgressMonitor;
      Client_Data              : C_Ext.void_ptr;
      Cache                    : C_Ext.void_ptr;
      Attributes               : C_Ext.void_ptr;
      Ascii85                  : Ascii_85_Info_Ptr;
      Blob                     : Blob_Info_Ptr;
      Filename,
      Magick_Filename,
      Magick                   : C.char_array (MaxTextExtent);
      Magick_Columns,
      Magick_Rows              : C.unsigned_long;
      Image_Exception          : Exception_Info;
      Debug                    : MagickBooleanType;
      Reference_Count          : C.long;
      Semaphore                : Semaphore_Info_Ptr;
      ColorProfile,
      IptcProfile              : Profile_Info;
      GenericProfile           : Profile_Info_Ptr;
      GenericProfiles          : C.unsigned_long;
      Signature                : C.unsigned_long;
      Previous,
      List,
      Next                     : Image_Ptr;
      Interpolate              : InterpolatePixelMethod;
      Black_Point_Compensation : MagickBooleanType;
      Transparent_Color        : Pixel_Packet;
   end record;
   pragma Convention (C, Image);

   procedure Initialize_Magick;

   function Get_Exception_Info
     (G_E_I : in Exception_Info) return  Exception_Info_Ptr;

   Ex_Info     : Exception_Info;
   Ex_Info_Ptr : Exception_Info_Ptr := new Exception_Info;

end G2F;
