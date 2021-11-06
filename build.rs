use cfg_aliases::cfg_aliases;

fn main() {
    // Setup cfg aliases
    cfg_aliases! {
        // Raspberry Pi 0/1 or 2/3/4
        raspi: { all(
                    any(target_arch = "arm", target_arch = "aarch64"),
                    target_vendor = "unknown",
                    target_os = "linux",
                    target_env = "gnu"
                ) },
    }

    // Setup windows binary icon
    #[cfg(windows)]
    {
        embed_resource::compile("resources.rc");
    }
}
