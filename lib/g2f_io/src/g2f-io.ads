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

package G2F.IO is

   type Supported_Image_Formats is
     (Magick_8BIM,
      Magick_8BIMTEXT,
      Magick_8BIMWTEXT,
      Magick_APP1,
      Magick_APP1JPEG,
      Magick_ART,
      Magick_AVI,
      Magick_AVS,
      Magick_BIE,
      Magick_BMP,
      Magick_BMP2,
      Magick_BMP3,
      Magick_CACHE,
      Magick_CAPTION,
      Magick_CMYK,
      Magick_CMYKA,
      Magick_CUR,
      Magick_CUT,
      Magick_DCM,
      Magick_DICO,
      Magick_DCX,
      Magick_DP,
      Magick_DPX,
      Magick_EPD,
      Magick_EPI,
      Magick_EPS,
      Magick_EPS2,
      Magick_EPS3,
      Magick_EPSF,
      Magick_EPSI,
      Magick_EP,
      Magick_EXIF,
      Magick_FAX,
      Magick_FITS,
      Magick_FPX,
      Magick_FRACTAL,
      Magick_G3,
      Magick_GIF,
      Magick_GIF87,
      Magick_GRADIENT,
      Magick_GRANITE,
      Magick_GRAY,
      Magick_HISTOGRAM,
      Magick_HTM,
      Magick_HTML,
      Magick_ICB,
      Magick_ICC,
      Magick_ICM,
      Magick_ICO,
      Magick_ICON,
      Magick_IPTC,
      Magick_IPTCTEXT,
      Magick_IPTCWTEXT,
      Magick_JBG,
      Magick_JBIG,
      Magick_JNG,
      Magick_JP2,
      Magick_JPC,
      Magick_JPEG,
      Magick_JPG,
      Magick_LABEL,
      Magick_LOGO,
      Magick_M2,
      Magick_MAGICK,
      Magick_MAP,
      Magick_MAT,
      Magick_MATTE,
      Magick_MIFF,
      Magick_MNG,
      Magick_MONO,
      Magick_MPC,
      Magick_MPE,
      Magick_MP,
      Magick_MSL,
      Magick_MTV,
      Magick_MVG,
      Magick_NETSCAPE,
      Magick_NULL,
      Magick_OTB,
      Magick_P7,
      Magick_PAL,
      Magick_PALM,
      Magick_PATTERN,
      Magick_PBM,
      Magick_PCD,
      Magick_PCDS,
      Magick_PCL,
      Magick_PCT,
      Magick_PCX,
      Magick_PDB,
      Magick_PD,
      Magick_PFA,
      Magick_PFB,
      Magick_PGM,
      Magick_PGX,
      Magick_PICON,
      Magick_PICT,
      Magick_PIX,
      Magick_PLASMA,
      Magick_PM,
      Magick_PNG,
      Magick_PNG24,
      Magick_PNG32,
      Magick_PNG8,
      Magick_PNM,
      Magick_PPM,
      Magick_PREVIEW,
      Magick_PS,
      Magick_PS2,
      Magick_PS3,
      Magick_PSD,
      Magick_PTIF,
      Magick_PWP,
      Magick_RAS,
      Magick_RGB,
      Magick_RGBA,
      Magick_RLA,
      Magick_RLE,
      Magick_SCT,
      Magick_SFW,
      Magick_SGI,
      Magick_SHTML,
      Magick_STEGANO,
      Magick_SUN,
      Magick_SVG,
      Magick_TEXT,
      Magick_TGA,
      Magick_TIF,
      Magick_TIFF,
      Magick_TILE,
      Magick_TIM,
      Magick_TTF,
      Magick_TXT,
      Magick_UIL,
      Magick_UYVY,
      Magick_VDA,
      Magick_VICAR,
      Magick_VID,
      Magick_VIFF,
      Magick_VST,
      Magick_WBMP,
      Magick_WPG,
      Magick_X,
      Magick_XBM,
      Magick_XC,
      Magick_XCF,
      Magick_XMP,
      Magick_XPM,
      Magick_XV,
      Magick_XWD,
      Magick_YUV);

   pragma Convention (C, Supported_Image_Formats);

   Depth_Error : exception;

   type Image_Size_T is new Natural range 0 .. Natural'Last;
   type Image_Size is record
      X, Y : Image_Size_T;
   end record;

   type Depth is new Positive range 1 .. 32;
   -- ImageMagick support 8/16/32 bit depth

   procedure Set_Filename (I : in Image_Info_Ptr; S : in String);
   --  Set image filename

   procedure Set_Filename (I : in Image_Ptr; S : in String);
   --  Set image filename

   function Get_Filename (I : in Image_Info_Ptr) return String;
   --  Get image filename

   function Get_Filename (I : in Image_Ptr) return String;
   --  Get image filename

   procedure Set_Format
     (I      : in Image_Info_Ptr;
      Format : in Supported_Image_Formats);
   --  Set image format

   procedure Set_Format
     (I      : in Image_Ptr;
      Format : in Supported_Image_Formats);
   --  Set image format

   function Get_Format (I : in Image_Ptr) return String;
   --  Get image format

   function Get_Format (I : in Image_Info_Ptr) return String;
   --  Get image format

   procedure Set_Compression
     (I : in Image_Info_Ptr;
      C : in Compression_Type);
   --  Set image compression

   procedure Set_Compression (I : in Image_Ptr; C : in Compression_Type);
   --  Set image compression

   function Get_Compression (I : in Image_Info_Ptr) return Compression_Type;
   --  Get image compression

   function Get_Compression (I : in Image_Ptr) return Compression_Type;
   --  Get image compression

   procedure Set_Depth (I : in Image_Info_Ptr; D : in Depth);
   procedure Set_Depth (I : in Image_Ptr; D : in Depth);
   --  translates the pixel quantums across all of the channels so that if they
   --  are later divided to fit within the specified bit depth, that no
   --  additional information is lost ( i.e. no remainder will result from the
   --  division ) . Note that any subsequent image processing is likely to
   --  increase the effective depth of the image channels. A non-zero value is
   --  returned if the operation is successful. Check the exception member of
   --  image to determine the cause for any failure.

   function Get_Depth (I : in Image_Info_Ptr) return Depth;
   --  Get image depth

   function Get_Depth (I : in Image_Ptr) return Depth;
   --  Get image depth

   procedure Set_Image_Size (I : in Image_Info_Ptr; Im_S : in Image_Size);
   --  Set image size (usefull when reading raw format)

   function Get_Image_Size (I : in Image_Ptr) return Image_Size;
   --  Get iamge size

end G2f.IO;
