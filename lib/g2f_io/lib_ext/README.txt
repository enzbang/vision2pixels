
Windows
=======

On Windows the import libraries libjpeg.dll.a and libjpeg.dll.a are built
using the provided definition files (*.def). It is required to install
the ImageMagick binary distribution for the application to run
properly. ImageMagick DLL binary distributions for Windows can be found
here:

  http://www.imagemagick.org/script/binary-releases.php#windows

Vision2Pixels has been tested with ImageMagick-6.3.5-0-Q16-windows-dll.exe.


GNU/Linux
=========

On GNU/Linux the build will use the libraries as provided by the
system. On GNU/Debian the ImageMagic library can be installed with
the simple command:

   $ aptitude install imagemagick
