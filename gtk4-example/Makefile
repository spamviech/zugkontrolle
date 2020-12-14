.DEFAULT_GOAL := all

.PHONY = all clean

# Libs and CFlags are results from pkg-config on windows
# they are provided in expanded form, since it only works in an msys shell
# original command:
# gcc `pkg-config --cflags gtk4` -o example-0 example-0.c `pkg-config --libs gtk4`

# detect OS
# https://stackoverflow.com/questions/714100/os-detecting-makefile/14777895#14777895
ifeq '$(findstring ;,$(PATH))' ';'
	detected_OS := Windows
else
	detected_OS := $(shell uname 2>/dev/null || echo Unknown)
	detected_OS := $(patsubst CYGWIN%,Cygwin,$(detected_OS))
	detected_OS := $(patsubst MSYS%,MSYS,$(detected_OS))
	detected_OS := $(patsubst MINGW%,MSYS,$(detected_OS))
endif

LibsMSys = -L/mingw64/lib -L/mingw64/lib/../lib -L/mingw64/lib -lgtk-4 -lgdi32 -limm32 -lshell32 -lole32 -Wl,-luuid -lwinmm -ldwmapi -lsetupapi -lcfgmgr32 -lvulkan -lepoxy -lopengl32 -lgdi32 -lmsimg32 -lpangocairo-1.0 -lm -lgdi32 -lpangoft2-1.0 -lm -lpangowin32-1.0 -lm -lgdi32 -lpango-1.0 -lm -lfribidi -lthai -ldatrie -lgdk_pixbuf-2.0 -lm -lcairo-gobject -lcairo -lm -lgdi32 -lmsimg32 -llzo2 -lfontconfig -liconv -lexpat -lfreetype -lbz2 -lpng16 -lz -lharfbuzz -lm -lusp10 -lgdi32 -lrpcrt4 -ldwrite -lgraphite2 -lbrotlidec -lbrotlicommon -lpixman-1 -lm -pthread -lgraphene-1.0 -lm -pthread -lgio-2.0 -lintl -lshlwapi -ldnsapi -liphlpapi -lws2_32 -lgmodule-2.0 -pthread -lintl -lz -lgobject-2.0 -lintl -lffi -lglib-2.0 -lintl -lws2_32 -lole32 -lwinmm -lshlwapi -pthread -lm -lpcre

LibsLinux = -L/usr/local/lib/x86_64-linux-gnu -lgtk-4 -lpangocairo-1.0 -lpango-1.0 -lharfbuzz -lgdk_pixbuf-2.0 -lcairo-gobject -lcairo -lgraphene-1.0 -latk-1.0 -lgio-2.0 -lgobject-2.0 -lglib-2.0

ifeq ($(detected_OS),Windows)
	Libs = $(LibsMSys)
else ifeq ($(detected_OS),MSYS)
	Libs = $(shell pkg-config --libs gtk4)
else
	Libs = $(shell pkg-config --libs gtk+-4.0)
endif

CFlagsMSys = -mfpmath=sse -msse -msse2 -pthread -mms-bitfields -I/mingw64/include/gtk-4.0 -I/mingw64/include -I/mingw64/include/cairo -I/mingw64/include/pango-1.0 -I/mingw64/include/fribidi -I/mingw64/include -I/mingw64/include/gdk-pixbuf-2.0 -I/mingw64/include/cairo -I/mingw64/include/lzo -I/mingw64/include -I/mingw64/include/freetype2 -I/mingw64/include -I/mingw64/include/libpng16 -I/mingw64/include/harfbuzz -I/mingw64/include -I/mingw64/include/pixman-1 -I/mingw64/include/graphene-1.0 -I/mingw64/lib/graphene-1.0/include -I/mingw64/include -I/mingw64/include/glib-2.0 -I/mingw64/lib/glib-2.0/include -I/mingw64/include

CFlagsLinux = -mfpmath=sse -msse -msse2 -pthread -I/usr/local/include/gtk-4.0 -I/usr/local/include/gio-unix-2.0 -I/usr/local/include -I/usr/local/include/pango-1.0 -I/usr/local/include/gdk-pixbuf-2.0 -I/usr/local/include/cairo -I/usr/local/include/graphene-1.0 -I/usr/local/lib/x86_64-linux-gnu/graphene-1.0/include -I/usr/local/include/atk-1.0 -I/usr/local/include -I/usr/local/include/glib-2.0 -I/usr/local/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/at-spi2-atk/2.0 -I/usr/include/at-spi-2.0 -I/usr/include/dbus-1.0 -I/usr/lib/x86_64-linux-gnu/dbus-1.0/include -I/usr/include/fribidi -I/usr/include/harfbuzz -I/usr/include/lzo -I/usr/include/uuid -I/usr/include/freetype2 -I/usr/include/libpng16 -I/usr/include/pixman-1 -I/usr/include/libmount -I/usr/include/blkid

ifeq ($(detected_OS),Windows)
	CFlags = $(CFlagsMSys)
else ifeq ($(detected_OS),MSYS)
	CFlags = $(shell pkg-config --cflags gtk4)
else
	CFlags = $(shell pkg-config --cflags gtk+-4.0)
endif


all: example-0

example-0: example-0.o
	gcc -o example-0 example-0.o $(Libs)

example-0.o:
	gcc $(CFlags) -c example-0.c

clean:
	rm -f example-0.o
