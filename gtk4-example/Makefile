.DEFAULT_GOAL := all

.PHONY = all clean

# Libs and CFlags are results from pkg-config on windows
# they are provided in expanded form, since it only works in an msys shell
# original command:
# gcc `pkg-config --cflags gtk4` -o example-0 example-0.c `pkg-config --libs gtk4`

Libs = -L/mingw64/lib -L/mingw64/lib/../lib -L/mingw64/lib -lgtk-4 -lgdi32 -limm32 -lshell32 -lole32 -Wl,-luuid -lwinmm -ldwmapi -lsetupapi -lcfgmgr32 -lvulkan -lepoxy -lopengl32 -lgdi32 -lmsimg32 -lpangocairo-1.0 -lm -lgdi32 -lpangoft2-1.0 -lm -lpangowin32-1.0 -lm -lgdi32 -lpango-1.0 -lm -lfribidi -lthai -ldatrie -lgdk_pixbuf-2.0 -lm -lcairo-gobject -lcairo -lm -lgdi32 -lmsimg32 -llzo2 -lfontconfig -liconv -lexpat -lfreetype -lbz2 -lpng16 -lz -lharfbuzz -lm -lusp10 -lgdi32 -lrpcrt4 -ldwrite -lgraphite2 -lbrotlidec -lbrotlicommon -lpixman-1 -lm -pthread -lgraphene-1.0 -lm -pthread -lgio-2.0 -lintl -lshlwapi -ldnsapi -liphlpapi -lws2_32 -lgmodule-2.0 -pthread -lintl -lz -lgobject-2.0 -lintl -lffi -lglib-2.0 -lintl -lws2_32 -lole32 -lwinmm -lshlwapi -pthread -lm -lpcre

CFlags = -mfpmath=sse -msse -msse2 -pthread -mms-bitfields -I/mingw64/include/gtk-4.0 -I/mingw64/include -I/mingw64/include/cairo -I/mingw64/include/pango-1.0 -I/mingw64/include/fribidi -I/mingw64/include -I/mingw64/include/gdk-pixbuf-2.0 -I/mingw64/include/cairo -I/mingw64/include/lzo -I/mingw64/include -I/mingw64/include/freetype2 -I/mingw64/include -I/mingw64/include/libpng16 -I/mingw64/include/harfbuzz -I/mingw64/include -I/mingw64/include/pixman-1 -I/mingw64/include/graphene-1.0 -I/mingw64/lib/graphene-1.0/include -I/mingw64/include -I/mingw64/include/glib-2.0 -I/mingw64/lib/glib-2.0/include -I/mingw64/include

all: example-0

example-0: example-0.o
	gcc -o example-0 example-0.o $(Libs)

example-0.o:
	gcc $(CFlags) -c example-0.c

clean:
	rm -f example-0.o
