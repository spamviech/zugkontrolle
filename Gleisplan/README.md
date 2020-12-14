# Zugkontrolle-Gtk

after installing gtk4 (including all dependencies) run the following commands to rename the pkg-config file so gi-gtk finds it.

```sh
cd /usr/local/lib/x86_64-linux-gnu/pkgconfig/
sudo cp gtk+-4.0.pc gtk4.pc
```

still fails to compile gi-gdk
probably fails to find gir files

